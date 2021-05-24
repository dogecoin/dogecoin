{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Currency values
--
module Cardano.Api.Value
  ( Lovelace(..)

    -- * Multi-asset values
  , Quantity(..)
  , PolicyId(..)
  , AssetName(..)
  , AssetId(..)
  , Value
  , selectAsset
  , valueFromList
  , valueToList
  , filterValue
  , negateValue
  , calcMinimumDeposit

    -- ** Ada \/ Lovelace specifically
  , quantityToLovelace
  , lovelaceToQuantity
  , selectLovelace
  , lovelaceToValue
  , valueToLovelace

    -- ** Alternative nested representation
  , ValueNestedRep(..)
  , ValueNestedBundle(..)
  , valueToNestedRep
  , valueFromNestedRep

    -- * Internal conversion functions
  , toByronLovelace
  , fromByronLovelace
  , toShelleyLovelace
  , fromShelleyLovelace
  , fromShelleyDeltaLovelace
  , toMaryValue
  , fromMaryValue

    -- * Data family instances
  , AsType(..)
  ) where

import           Prelude

import           Data.Aeson hiding (Value)
import qualified Data.Aeson as Aeson
import           Data.Aeson.Types (Parser, toJSONKeyText)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Merge.Strict as Map
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.String (IsString (..))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import qualified Cardano.Chain.Common as Byron

import qualified Cardano.Ledger.Coin as Shelley
import qualified Cardano.Ledger.Mary.Value as Mary
import qualified Cardano.Ledger.ShelleyMA.Rules.Utxo as Shelley

import           Ouroboros.Consensus.Shelley.Protocol.Crypto (StandardCrypto)

import           Cardano.Api.HasTypeProxy
import           Cardano.Api.Script
import           Cardano.Api.SerialiseRaw


-- ----------------------------------------------------------------------------
-- Lovelace
--

newtype Lovelace = Lovelace Integer
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num, ToJSON, FromJSON)

instance Semigroup Lovelace where
  Lovelace a <> Lovelace b = Lovelace (a + b)

instance Monoid Lovelace where
  mempty = Lovelace 0


toByronLovelace :: Lovelace -> Maybe Byron.Lovelace
toByronLovelace (Lovelace x) =
    case Byron.integerToLovelace x of
      Left  _  -> Nothing
      Right x' -> Just x'

fromByronLovelace :: Byron.Lovelace -> Lovelace
fromByronLovelace = Lovelace . Byron.lovelaceToInteger

toShelleyLovelace :: Lovelace -> Shelley.Coin
toShelleyLovelace (Lovelace l) = Shelley.Coin l
--TODO: validate bounds

fromShelleyLovelace :: Shelley.Coin -> Lovelace
fromShelleyLovelace (Shelley.Coin l) = Lovelace l

fromShelleyDeltaLovelace :: Shelley.DeltaCoin -> Lovelace
fromShelleyDeltaLovelace (Shelley.DeltaCoin d) = Lovelace d


-- ----------------------------------------------------------------------------
-- Multi asset Value
--

newtype Quantity = Quantity Integer
  deriving newtype (Eq, Ord, Num, Show, ToJSON, FromJSON)

instance Semigroup Quantity where
  Quantity a <> Quantity b = Quantity (a + b)

instance Monoid Quantity where
  mempty = Quantity 0

lovelaceToQuantity :: Lovelace -> Quantity
lovelaceToQuantity (Lovelace x) = Quantity x

quantityToLovelace :: Quantity -> Lovelace
quantityToLovelace (Quantity x) = Lovelace x


newtype PolicyId = PolicyId ScriptHash
  deriving stock (Eq, Ord)
  deriving (Show, IsString) via UsingRawBytesHex PolicyId

instance HasTypeProxy PolicyId where
    data AsType PolicyId = AsPolicyId
    proxyToAsType _ = AsPolicyId

instance SerialiseAsRawBytes PolicyId where
    serialiseToRawBytes (PolicyId sh) = serialiseToRawBytes sh
    deserialiseFromRawBytes AsPolicyId bs =
      PolicyId <$> deserialiseFromRawBytes AsScriptHash bs

newtype AssetName = AssetName ByteString
    deriving stock (Eq, Ord)
    deriving newtype (Show)

instance IsString AssetName where
    fromString s
      | let bs = Text.encodeUtf8 (Text.pack s)
      , BS.length bs <= 32 = AssetName (BSC.pack s)
      | otherwise          = error "fromString: AssetName over 32 bytes"

instance HasTypeProxy AssetName where
    data AsType AssetName = AsAssetName
    proxyToAsType _ = AsAssetName

instance SerialiseAsRawBytes AssetName where
    serialiseToRawBytes (AssetName bs) = bs
    deserialiseFromRawBytes AsAssetName bs
      | BS.length bs <= 32 = Just (AssetName bs)
      | otherwise          = Nothing

instance ToJSON AssetName where
  toJSON (AssetName an) = Aeson.String $ Text.decodeUtf8 an

instance FromJSON AssetName where
  parseJSON = withText "AssetName" (return . AssetName . Text.encodeUtf8)

instance ToJSONKey AssetName where
  toJSONKey = toJSONKeyText (\(AssetName asset) -> Text.decodeUtf8 asset)

instance FromJSONKey AssetName where
  fromJSONKey = FromJSONKeyText (AssetName . Text.encodeUtf8)


data AssetId = AdaAssetId
             | AssetId !PolicyId !AssetName
  deriving (Eq, Ord, Show)


newtype Value = Value (Map AssetId Quantity)
  deriving Eq

instance Show Value where
  showsPrec d v = showParen (d > 10) $
    showString "valueFromList " . shows (valueToList v)

instance Semigroup Value where
  Value a <> Value b = Value (mergeAssetMaps a b)

instance Monoid Value where
  mempty = Value Map.empty


{-# NOINLINE mergeAssetMaps #-} -- as per advice in Data.Map.Merge docs
mergeAssetMaps :: Map AssetId Quantity
               -> Map AssetId Quantity
               -> Map AssetId Quantity
mergeAssetMaps =
    Map.merge
      Map.preserveMissing
      Map.preserveMissing
      (Map.zipWithMaybeMatched mergeQuantity)
  where
    mergeQuantity :: AssetId -> Quantity -> Quantity -> Maybe Quantity
    mergeQuantity _k a b =
      case a <> b of
        Quantity 0 -> Nothing
        c          -> Just c

instance ToJSON Value where
  toJSON = toJSON . valueToNestedRep

instance FromJSON Value where
  parseJSON v = valueFromNestedRep <$> parseJSON v


selectAsset :: Value -> (AssetId -> Quantity)
selectAsset (Value m) a = Map.findWithDefault mempty a m

valueFromList :: [(AssetId, Quantity)] -> Value
valueFromList = Value
              . Map.filter (/= 0)
              . Map.fromListWith (<>)

valueToList :: Value -> [(AssetId, Quantity)]
valueToList (Value m) = Map.toList m

-- | This lets you write @a - b@ as @a <> negateValue b@.
--
negateValue :: Value -> Value
negateValue (Value m) = Value (Map.map negate m)

filterValue :: (AssetId -> Bool) -> Value -> Value
filterValue p (Value m) = Value (Map.filterWithKey (\k _v -> p k) m)

selectLovelace :: Value -> Lovelace
selectLovelace = quantityToLovelace . flip selectAsset AdaAssetId

lovelaceToValue :: Lovelace -> Value
lovelaceToValue = Value . Map.singleton AdaAssetId . lovelaceToQuantity

-- | Check if the 'Value' consists of /only/ 'Lovelace' and no other assets,
-- and if so then return the Lovelace.
--
-- See also 'selectLovelace' to select the Lovelace quantity from the Value,
-- ignoring other assets.
--
valueToLovelace :: Value -> Maybe Lovelace
valueToLovelace v =
    case valueToList v of
      []                -> Just (Lovelace 0)
      [(AdaAssetId, q)] -> Just (quantityToLovelace q)
      _                 -> Nothing

toMaryValue :: Value -> Mary.Value StandardCrypto
toMaryValue v =
    Mary.Value lovelace other
  where
    Quantity lovelace = selectAsset v AdaAssetId
      --TODO: write QC tests to show it's ok to use Map.fromAscListWith here
    other = Map.fromListWith Map.union
              [ (toMaryPolicyID pid, Map.singleton (toMaryAssetName name) q)
              | (AssetId pid name, Quantity q) <- valueToList v ]

    toMaryPolicyID :: PolicyId -> Mary.PolicyID StandardCrypto
    toMaryPolicyID (PolicyId sh) = Mary.PolicyID (toShelleyScriptHash sh)

    toMaryAssetName :: AssetName -> Mary.AssetName
    toMaryAssetName (AssetName n) = Mary.AssetName n


fromMaryValue :: Mary.Value StandardCrypto -> Value
fromMaryValue (Mary.Value lovelace other) =
    Value $
      --TODO: write QC tests to show it's ok to use Map.fromAscList here
      Map.fromList $
        [ (AdaAssetId, Quantity lovelace) | lovelace /= 0 ]
     ++ [ (AssetId (fromMaryPolicyID pid) (fromMaryAssetName name), Quantity q)
        | (pid, as) <- Map.toList other
        , (name, q) <- Map.toList as ]
  where
    fromMaryPolicyID :: Mary.PolicyID StandardCrypto -> PolicyId
    fromMaryPolicyID (Mary.PolicyID sh) = PolicyId (fromShelleyScriptHash sh)

    fromMaryAssetName :: Mary.AssetName -> AssetName
    fromMaryAssetName (Mary.AssetName n) = AssetName n

-- | Calculate cost of making a UTxO entry for a given 'Value' and
-- mininimum UTxO value derived from the 'ProtocolParameters'
calcMinimumDeposit :: Value -> Lovelace -> Lovelace
calcMinimumDeposit v minUTxo =
  fromShelleyLovelace $ Shelley.scaledMinDeposit (toMaryValue v) (toShelleyLovelace minUTxo)

-- ----------------------------------------------------------------------------
-- An alternative nested representation
--

-- | An alternative nested representation for 'Value' that groups assets that
-- share a 'PolicyId'.
--
newtype ValueNestedRep = ValueNestedRep [ValueNestedBundle]
  deriving (Eq, Ord, Show)

-- | A bundle within a 'ValueNestedRep' for a single 'PolicyId', or for the
-- special case of ada.
--
data ValueNestedBundle = ValueNestedBundleAda Quantity
                       | ValueNestedBundle PolicyId (Map AssetName Quantity)
  deriving (Eq, Ord, Show)


valueToNestedRep :: Value -> ValueNestedRep
valueToNestedRep v =
    -- unflatten all the non-ada assets, and add ada separately
    ValueNestedRep $
        [ ValueNestedBundleAda q | let q = selectAsset v AdaAssetId, q /= 0 ]
     ++ [ ValueNestedBundle pId qs | (pId, qs) <- Map.toList nonAdaAssets ]
  where
    nonAdaAssets :: Map PolicyId (Map AssetName Quantity)
    nonAdaAssets =
      Map.fromListWith (Map.unionWith (<>))
        [ (pId, Map.singleton aName q)
        | (AssetId pId aName, q) <- valueToList v ]

valueFromNestedRep :: ValueNestedRep -> Value
valueFromNestedRep (ValueNestedRep bundles) =
    valueFromList
      [ (aId, q)
      | bundle   <- bundles
      , (aId, q) <- case bundle of
                      ValueNestedBundleAda  q  -> [ (AdaAssetId, q) ]
                      ValueNestedBundle pId qs -> [ (AssetId pId aName, q)
                                                  | (aName, q) <- Map.toList qs ]
      ]

instance ToJSON ValueNestedRep where
  toJSON (ValueNestedRep bundles) = object $ map toPair bundles
    where
     toPair :: ValueNestedBundle -> (Text, Aeson.Value)
     toPair (ValueNestedBundleAda q) = ("lovelace", toJSON q)
     toPair (ValueNestedBundle pid assets) = (renderPolicyId pid, toJSON assets)

     renderPolicyId :: PolicyId -> Text
     renderPolicyId (PolicyId sh) = serialiseToRawBytesHexText sh

instance FromJSON ValueNestedRep where
  parseJSON =
      withObject "ValueNestedRep" $ \obj ->
        ValueNestedRep <$> sequenceA [ parsePid keyValTuple
                                   | keyValTuple <- HashMap.toList obj ]
    where
      parsePid :: (Text, Aeson.Value) -> Parser ValueNestedBundle
      parsePid ("lovelace", q) = ValueNestedBundleAda <$> parseJSON q
      parsePid (pid, q) =
        case deserialiseFromRawBytesHex AsScriptHash (Text.encodeUtf8 pid) of
          Just sHash -> ValueNestedBundle (PolicyId sHash) <$> parseJSON q
          Nothing -> fail $ "Failure when deserialising PolicyId: "
                         <> Text.unpack pid
