{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.CLI.Shelley.Orphans () where

import           Cardano.Prelude

import           Control.Monad
import           Control.SetAlgebra as SetAlgebra
import           Data.Aeson
import qualified Data.Aeson as Aeson
import           Data.Aeson.Types (toJSONKeyText)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Cardano.Api.Orphans ()

import           Cardano.Crypto.Hash.Class as Crypto
import           Cardano.Ledger.Alonzo.Language (Language)

import           Ouroboros.Consensus.Byron.Ledger.Block (ByronHash (..))
import           Ouroboros.Consensus.HardFork.Combinator (OneEraHash (..))
import           Ouroboros.Consensus.Shelley.Eras (StandardCrypto)
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyHash (..))
import           Ouroboros.Network.Block (BlockNo (..), HeaderHash, Tip (..))

import           Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash (..))

import qualified Shelley.Spec.Ledger.API.Protocol as Ledger
import           Shelley.Spec.Ledger.BlockChain (HashHeader (..))
import qualified Shelley.Spec.Ledger.Credential as Ledger
import qualified Shelley.Spec.Ledger.Delegation.Certificates as Ledger
import qualified Shelley.Spec.Ledger.EpochBoundary as Ledger
import qualified Shelley.Spec.Ledger.Rewards as Ledger
import qualified Shelley.Spec.Ledger.STS.Prtcl as Ledger
import qualified Shelley.Spec.Ledger.STS.Tickn as Ledger
import           Shelley.Spec.Ledger.TxBody (TxId (..))

import qualified Cardano.Ledger.Mary.Value as Ledger.Mary

import qualified Cardano.Ledger.Alonzo.Language as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import           Cardano.Ledger.Alonzo.Translation (AlonzoGenesis (..))
import qualified Cardano.Ledger.Alonzo.Translation as Alonzo

import qualified PlutusCore.Evaluation.Machine.ExBudgeting as Plutus
import qualified PlutusCore.Evaluation.Machine.ExBudgetingDefaults as Plutus

import           Data.MemoBytes (MemoBytes)

instance ToJSON (OneEraHash xs) where
  toJSON = toJSON
         . Text.decodeLatin1
         . Base16.encode
         . SBS.fromShort
         . getOneEraHash

deriving newtype instance ToJSON ByronHash

-- This instance is temporarily duplicated in cardano-config

instance ToJSON (HeaderHash blk) => ToJSON (Tip blk) where
  toJSON TipGenesis = object [ "genesis" .= True ]
  toJSON (Tip slotNo headerHash blockNo) =
    object
      [ "slotNo"     .= slotNo
      , "headerHash" .= headerHash
      , "blockNo"    .= blockNo
      ]

-- This instance is temporarily duplicated in cardano-config
deriving newtype instance ToJSON BlockNo

--
-- Simple newtype wrappers JSON conversion
--

deriving newtype instance ToJSON (TxId era)

deriving newtype instance ToJSON (ShelleyHash era)
deriving newtype instance ToJSON (HashHeader era)

deriving newtype instance ToJSON (AuxiliaryDataHash StandardCrypto)
deriving newtype instance ToJSON Ledger.LogWeight
deriving newtype instance ToJSON (Ledger.PoolDistr StandardCrypto)

deriving newtype instance ToJSON (Ledger.Stake StandardCrypto)

deriving instance ToJSON (Ledger.StakeReference StandardCrypto)

deriving instance ToJSON (Ledger.PrtclState StandardCrypto)
deriving instance ToJSON Ledger.TicknState
deriving instance ToJSON (Ledger.ChainDepState StandardCrypto)

deriving instance ToJSONKey Ledger.Ptr

deriving newtype  instance ToJSON    (Ledger.Mary.PolicyID StandardCrypto)

instance (ToJSONKey k, ToJSON v) => ToJSON (SetAlgebra.BiMap v k v) where
  toJSON = toJSON . SetAlgebra.forwards -- to normal Map




-- We defer parsing of the cost model so that we can
-- read it as a filepath. This is to reduce further pollution
-- of the genesis file.
instance FromJSON Alonzo.AlonzoGenesis where
  parseJSON =
    withObject "Alonzo Genesis" $ \o -> do
      adaPerUTxOWord       <- o .:  "adaPerUTxOWord"
      cModels              <- o .:? "costModels"
      prices               <- o .:  "executionPrices"
      maxTxExUnits         <- o .:  "maxTxExUnits"
      maxBlockExUnits      <- o .:  "maxBlockExUnits"
      maxValSize           <- o .:  "maxValueSize"
      collateralPercentage <- o .:  "collateralPercentage"
      maxCollateralInputs  <- o .:  "maxCollateralInputs"
      case cModels of
        Nothing ->
          case Plutus.extractModelParams Plutus.defaultCostModel of
            Just m ->
              return Alonzo.AlonzoGenesis {
                adaPerUTxOWord,
                costmdls = Map.singleton Alonzo.PlutusV1 (Alonzo.CostModel m),
                prices,
                maxTxExUnits,
                maxBlockExUnits,
                maxValSize,
                collateralPercentage,
                maxCollateralInputs
              }
            Nothing -> fail "Failed to extract the cost model params from Plutus.defaultCostModel"
        Just costmdls ->
          return Alonzo.AlonzoGenesis {
            adaPerUTxOWord,
            costmdls,
            prices,
            maxTxExUnits,
            maxBlockExUnits,
            maxValSize,
            collateralPercentage,
            maxCollateralInputs
          }


-- We don't render the cost model so that we can
-- render it later in 'AlonzoGenWrapper' as a filepath
-- and keep the cost model (which is chunky) as a separate file.
instance ToJSON AlonzoGenesis where
  toJSON v = object
      [ "adaPerUTxOWord" .= adaPerUTxOWord v
      , "costModels" .= costmdls v
      , "executionPrices" .= prices v
      , "maxTxExUnits" .= maxTxExUnits v
      , "maxBlockExUnits" .= maxBlockExUnits v
      , "maxValueSize" .= maxValSize v
      , "collateralPercentage" .= collateralPercentage v
      , "maxCollateralInputs" .= maxCollateralInputs v
      ]

instance ToJSON Alonzo.ExUnits
deriving instance FromJSON Alonzo.ExUnits

instance ToJSON Language where
  toJSON Alonzo.PlutusV1 = Aeson.String "PlutusV1"

instance FromJSON Language  where
  parseJSON v =
    case v of
      Aeson.String "PlutusV1" -> return Alonzo.PlutusV1
      wrong -> fail $ "Error decoding Language. \
                      \Expected a JSON string but got: " <> show wrong

instance ToJSON Alonzo.CostModel
instance FromJSON Alonzo.CostModel

instance FromJSON (Data.MemoBytes.MemoBytes (Map Text Integer))
instance ToJSON (Data.MemoBytes.MemoBytes (Map Text Integer))


instance ToJSONKey Language where
  toJSONKey = toJSONKeyText (Text.decodeLatin1 . LBS.toStrict . encode)

instance FromJSONKey Language where
  fromJSONKey = FromJSONKeyText parseLang
   where
     parseLang :: Text -> Language
     parseLang lang =
       case eitherDecode $ LBS.fromStrict $ Text.encodeUtf8 lang of
         Left err -> panic $ Text.pack err
         Right lang' -> lang'



instance ToJSON Alonzo.Prices
deriving instance FromJSON Alonzo.Prices

instance ToJSON SBS.ShortByteString where
  toJSON = Aeson.String
             . Text.decodeLatin1
             . Base16.encode
             . SBS.fromShort


instance FromJSON SBS.ShortByteString where
  parseJSON v = case v of
                  Aeson.String b16 ->
                    case Base16.decode $ Text.encodeUtf8 b16 of
                      Right decoded -> return $ SBS.toShort decoded
                      Left err -> fail err
                  wrong -> fail $ "Error decoding ShortByteString. \
                                  \Expected a JSON string but got: " <> show wrong
