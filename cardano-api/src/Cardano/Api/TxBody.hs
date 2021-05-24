{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}


-- | Transaction bodies
--
module Cardano.Api.TxBody (

    -- * Transaction bodies
    TxBody(..),
    getTransactionBodyContent,
    makeTransactionBody,
    TxBodyContent(..),
    TxBodyError(..),

    -- ** Transitional utils
    makeByronTransaction,

    -- * Transaction Ids
    TxId(..),
    getTxId,

    -- * Transaction inputs
    TxIn(..),
    TxIx(..),
    genesisUTxOPseudoTxIn,

    -- * Transaction outputs
    TxOut(..),
    TxOutValue(..),
    serialiseAddressForTxOut,
    TxOutDatumHash(..),

    -- * Other transaction body types
    TxFee(..),
    TxValidityLowerBound(..),
    TxValidityUpperBound(..),
    TxMetadataInEra(..),
    TxAuxScripts(..),
    TxAuxScriptData(..),
    TxWithdrawals(..),
    TxCertificates(..),
    TxUpdateProposal(..),
    TxMintValue(..),

    -- ** Building vs viewing transactions
    BuildTxWith(..),
    BuildTx,
    ViewTx,

    -- * Era-dependent transaction body features
    MultiAssetSupportedInEra(..),
    OnlyAdaSupportedInEra(..),
    TxFeesExplicitInEra(..),
    TxFeesImplicitInEra(..),
    ValidityUpperBoundSupportedInEra(..),
    ValidityNoUpperBoundSupportedInEra(..),
    ValidityLowerBoundSupportedInEra(..),
    TxMetadataSupportedInEra(..),
    AuxScriptsSupportedInEra(..),
    ScriptDataSupportedInEra(..),
    WithdrawalsSupportedInEra(..),
    CertificatesSupportedInEra(..),
    UpdateProposalSupportedInEra(..),

    -- ** Feature availability functions
    multiAssetSupportedInEra,
    txFeesExplicitInEra,
    validityUpperBoundSupportedInEra,
    validityNoUpperBoundSupportedInEra,
    validityLowerBoundSupportedInEra,
    txMetadataSupportedInEra,
    auxScriptsSupportedInEra,
    scriptDataSupportedInEra,
    withdrawalsSupportedInEra,
    certificatesSupportedInEra,
    updateProposalSupportedInEra,

    -- * Internal conversion functions & types
    toShelleyTxId,
    toShelleyTxIn,
    toShelleyTxOut,
    fromShelleyTxId,
    fromShelleyTxIn,
    fromShelleyTxOut,
    fromTxOut,

    -- * Data family instances
    AsType(AsTxId, AsTxBody, AsByronTxBody, AsShelleyTxBody, AsMaryTxBody),

    -- * Conversion functions
    fromByronTxIn,
  ) where

import           Prelude

import           Control.Monad (guard)
import           Data.Aeson (ToJSON (..), object, (.=))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Types (ToJSONKey (..), toJSONKeyText)
import           Data.Bifunctor (first)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import           Data.Foldable (toList)
import           Data.List (intercalate)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Sequence.Strict as Seq
import qualified Data.Set as Set
import           Data.String (IsString)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Word (Word64)
import           GHC.Generics

import           Cardano.Binary (Annotated (..), reAnnotate, recoverBytes)
import qualified Cardano.Binary as CBOR
import qualified Shelley.Spec.Ledger.Serialization as CBOR (decodeNullMaybe, encodeNullMaybe)

import qualified Cardano.Crypto.Hash.Class as Crypto
import           Cardano.Slotting.Slot (SlotNo (..))

import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Chain.UTxO as Byron
import qualified Cardano.Crypto.Hashing as Byron

import qualified Cardano.Ledger.AuxiliaryData as Ledger (hashAuxiliaryData)
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.SafeHash as SafeHash
import qualified Cardano.Ledger.Shelley.Constraints as Ledger
import           Ouroboros.Consensus.Shelley.Eras
                   (StandardShelley, StandardAllegra,
                    StandardMary, StandardAlonzo)
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (StandardCrypto)

import qualified Shelley.Spec.Ledger.Address as Shelley
import           Shelley.Spec.Ledger.BaseTypes (StrictMaybe (..), maybeToStrictMaybe)
import qualified Shelley.Spec.Ledger.Credential as Shelley
import qualified Shelley.Spec.Ledger.Genesis as Shelley
import qualified Shelley.Spec.Ledger.Keys as Shelley
import qualified Shelley.Spec.Ledger.Metadata as Shelley
import qualified Shelley.Spec.Ledger.PParams as Shelley
import qualified Shelley.Spec.Ledger.Tx as Shelley
import qualified Shelley.Spec.Ledger.TxBody as Shelley
import qualified Shelley.Spec.Ledger.UTxO as Shelley

import qualified Cardano.Ledger.ShelleyMA.AuxiliaryData as Allegra
import qualified Cardano.Ledger.ShelleyMA.AuxiliaryData as Mary
import qualified Cardano.Ledger.ShelleyMA.TxBody as Allegra
import qualified Cardano.Ledger.ShelleyMA.TxBody as Mary
import           Cardano.Ledger.Val (isZero)

import qualified Cardano.Ledger.Alonzo as Alonzo
import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo

import           Cardano.Api.Address
import           Cardano.Api.Certificate
import           Cardano.Api.Eras
import           Cardano.Api.Error
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.Hash
import           Cardano.Api.KeysByron
import           Cardano.Api.KeysShelley
import           Cardano.Api.NetworkId
import           Cardano.Api.ProtocolParameters
import           Cardano.Api.Script
import           Cardano.Api.SerialiseBech32
import           Cardano.Api.SerialiseCBOR
import           Cardano.Api.SerialiseRaw
import           Cardano.Api.SerialiseTextEnvelope
import           Cardano.Api.TxMetadata
import           Cardano.Api.Utils
import           Cardano.Api.Value


-- ----------------------------------------------------------------------------
-- Transaction Ids
--

newtype TxId = TxId (Shelley.Hash StandardCrypto Shelley.EraIndependentTxBody)
  deriving stock (Eq, Ord, Show)
  deriving newtype (IsString)
               -- We use the Shelley representation and convert the Byron one

instance ToJSON TxId where
  toJSON = Aeson.String . serialiseToRawBytesHexText

instance HasTypeProxy TxId where
    data AsType TxId = AsTxId
    proxyToAsType _ = AsTxId

instance SerialiseAsRawBytes TxId where
    serialiseToRawBytes (TxId h) = Crypto.hashToBytes h
    deserialiseFromRawBytes AsTxId bs = TxId <$> Crypto.hashFromBytes bs

toByronTxId :: TxId -> Byron.TxId
toByronTxId (TxId h) =
    Byron.unsafeHashFromBytes (Crypto.hashToBytes h)

toShelleyTxId :: TxId -> Shelley.TxId StandardCrypto
toShelleyTxId (TxId h) =
    Shelley.TxId (SafeHash.unsafeMakeSafeHash (Crypto.castHash h))

fromShelleyTxId :: Shelley.TxId StandardCrypto -> TxId
fromShelleyTxId (Shelley.TxId h) =
    TxId (Crypto.castHash (SafeHash.extractHash h))

-- | Calculate the transaction identifier for a 'TxBody'.
--
getTxId :: forall era. TxBody era -> TxId
getTxId (ByronTxBody tx) =
    TxId
  . fromMaybe impossible
  . Crypto.hashFromBytesShort
  . Byron.abstractHashToShort
  . Byron.hashDecoded
  $ tx
  where
    impossible =
      error "getTxId: byron and shelley hash sizes do not match"

getTxId (ShelleyTxBody era tx _ _) =
    case era of
      ShelleyBasedEraShelley -> getTxIdShelley tx
      ShelleyBasedEraAllegra -> getTxIdShelley tx
      ShelleyBasedEraMary    -> getTxIdShelley tx
      ShelleyBasedEraAlonzo  -> getTxIdShelley tx
  where
    getTxIdShelley :: Ledger.Crypto (ShelleyLedgerEra era) ~ StandardCrypto
                   => Ledger.UsesTxBody (ShelleyLedgerEra era)
                   => Ledger.TxBody (ShelleyLedgerEra era) -> TxId
    getTxIdShelley =
        TxId
      . Crypto.castHash
      . (\(Shelley.TxId txhash) -> SafeHash.extractHash txhash)
      . (Shelley.txid @(ShelleyLedgerEra era))


-- ----------------------------------------------------------------------------
-- Transaction inputs
--

data TxIn = TxIn TxId TxIx
  deriving (Eq, Ord, Show)

instance ToJSON TxIn where
  toJSON txIn = Aeson.String $ renderTxIn txIn

instance ToJSONKey TxIn where
  toJSONKey = toJSONKeyText renderTxIn

renderTxIn :: TxIn -> Text
renderTxIn (TxIn txId (TxIx ix)) =
  serialiseToRawBytesHexText txId <> "#" <> Text.pack (show ix)


newtype TxIx = TxIx Word
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum)
  deriving newtype ToJSON

fromByronTxIn :: Byron.TxIn -> TxIn
fromByronTxIn (Byron.TxInUtxo txId index) =
  let shortBs = Byron.abstractHashToShort txId
      mApiHash = Crypto.hashFromBytesShort shortBs
  in case mApiHash of
       Just apiHash -> TxIn (TxId apiHash) (TxIx . fromIntegral $ toInteger index)
       Nothing -> error $ "Error converting Byron era TxId: " <> show txId

toByronTxIn :: TxIn -> Byron.TxIn
toByronTxIn (TxIn txid (TxIx txix)) =
    Byron.TxInUtxo (toByronTxId txid) (fromIntegral txix)

toShelleyTxIn :: TxIn -> Shelley.TxIn StandardCrypto
toShelleyTxIn (TxIn txid (TxIx txix)) =
    Shelley.TxIn (toShelleyTxId txid) (fromIntegral txix)

fromShelleyTxIn :: Shelley.TxIn StandardCrypto -> TxIn
fromShelleyTxIn (Shelley.TxIn txid txix) =
    TxIn (fromShelleyTxId txid) (TxIx (fromIntegral txix))


-- ----------------------------------------------------------------------------
-- Transaction outputs
--

data TxOut era = TxOut (AddressInEra era)
                       (TxOutValue era)
                       (TxOutDatumHash era)
  deriving Generic

instance IsCardanoEra era => ToJSON (TxOut era) where
  toJSON (TxOut addr val TxOutDatumHashNone) =
    object [ "address" .= serialiseAddressForTxOut addr
           , "value"   .= toJSON val
           ]
  toJSON (TxOut addr val (TxOutDatumHash _ d)) =
    object [ "address" .= serialiseAddressForTxOut addr
           , "value"   .= toJSON val
           , "data"    .= toJSON d
           ]

serialiseAddressForTxOut :: AddressInEra era -> Text
serialiseAddressForTxOut (AddressInEra addrType addr) =
  case addrType of
    ByronAddressInAnyEra  -> serialiseToRawBytesHexText addr
    ShelleyAddressInEra _ -> serialiseToBech32 addr


deriving instance Eq   (TxOut era)
deriving instance Show (TxOut era)


fromByronTxOut :: Byron.TxOut -> TxOut ByronEra
fromByronTxOut (Byron.TxOut addr value) =
  TxOut
    (AddressInEra ByronAddressInAnyEra (ByronAddress addr))
    (TxOutAdaOnly AdaOnlyInByronEra (fromByronLovelace value))
     TxOutDatumHashNone


toByronTxOut :: TxOut ByronEra -> Maybe Byron.TxOut
toByronTxOut (TxOut (AddressInEra ByronAddressInAnyEra (ByronAddress addr))
                    (TxOutAdaOnly AdaOnlyInByronEra value) _) =
    Byron.TxOut addr <$> toByronLovelace value

toByronTxOut (TxOut (AddressInEra ByronAddressInAnyEra (ByronAddress _))
                    (TxOutValue era _) _) = case era of {}

toByronTxOut (TxOut (AddressInEra (ShelleyAddressInEra era) ShelleyAddress{})
                    _ _) = case era of {}


toShelleyTxOut :: forall era ledgerera.
                 (ShelleyLedgerEra era ~ ledgerera,
                  IsShelleyBasedEra era, Ledger.ShelleyBased ledgerera)
               => TxOut era -> Ledger.TxOut ledgerera
toShelleyTxOut (TxOut _ (TxOutAdaOnly AdaOnlyInByronEra _) _) =
    case shelleyBasedEra :: ShelleyBasedEra era of {}

toShelleyTxOut (TxOut addr (TxOutAdaOnly AdaOnlyInShelleyEra value) _) =
    Shelley.TxOut (toShelleyAddr addr) (toShelleyLovelace value)

toShelleyTxOut (TxOut addr (TxOutAdaOnly AdaOnlyInAllegraEra value) _) =
    Shelley.TxOut (toShelleyAddr addr) (toShelleyLovelace value)

toShelleyTxOut (TxOut addr (TxOutValue MultiAssetInMaryEra value) _) =
    Shelley.TxOut (toShelleyAddr addr) (toMaryValue value)

toShelleyTxOut (TxOut addr (TxOutValue MultiAssetInAlonzoEra value) txoutdata) =
    Alonzo.TxOut (toShelleyAddr addr) (toMaryValue value)
                 (toAlonzoTxOutDataHash txoutdata)

fromShelleyTxOut :: Shelley.TxOut StandardShelley -> TxOut ShelleyEra
fromShelleyTxOut = fromTxOut ShelleyBasedEraShelley

fromTxOut
  :: ShelleyLedgerEra era ~ ledgerera
  => ShelleyBasedEra era
  -> Core.TxOut ledgerera
  -> TxOut era
fromTxOut shelleyBasedEra' ledgerTxOut =
  case shelleyBasedEra' of
    ShelleyBasedEraShelley ->
        TxOut (fromShelleyAddr addr)
              (TxOutAdaOnly AdaOnlyInShelleyEra
                            (fromShelleyLovelace value))
               TxOutDatumHashNone
      where
        Shelley.TxOut addr value = ledgerTxOut

    ShelleyBasedEraAllegra ->
        TxOut (fromShelleyAddr addr)
              (TxOutAdaOnly AdaOnlyInAllegraEra
                            (fromShelleyLovelace value))
               TxOutDatumHashNone
      where
        Shelley.TxOut addr value = ledgerTxOut

    ShelleyBasedEraMary ->
        TxOut (fromShelleyAddr addr)
              (TxOutValue MultiAssetInMaryEra
                          (fromMaryValue value))
               TxOutDatumHashNone
      where
        Shelley.TxOut addr value = ledgerTxOut

    ShelleyBasedEraAlonzo ->
       TxOut (fromShelleyAddr addr)
             (TxOutValue MultiAssetInAlonzoEra
                         (fromMaryValue value))
             (fromAlonzoTxOutDataHash ScriptDataInAlonzoEra datahash)
      where
        Alonzo.TxOut addr value datahash = ledgerTxOut

toAlonzoTxOutDataHash :: TxOutDatumHash era
                      -> StrictMaybe (Alonzo.DataHash StandardCrypto)
toAlonzoTxOutDataHash TxOutDatumHashNone    = SNothing
toAlonzoTxOutDataHash (TxOutDatumHash _ (ScriptDataHash dh)) = SJust dh

fromAlonzoTxOutDataHash :: ScriptDataSupportedInEra era
                        -> StrictMaybe (Alonzo.DataHash StandardCrypto)
                        -> TxOutDatumHash era
fromAlonzoTxOutDataHash _    SNothing  = TxOutDatumHashNone
fromAlonzoTxOutDataHash era (SJust dh) = TxOutDatumHash era (ScriptDataHash dh)


-- ----------------------------------------------------------------------------
-- Era-dependent transaction body features
--

-- | A representation of whether the era supports multi-asset transactions.
--
-- The Mary and subsequent eras support multi-asset transactions.
--
-- The negation of this is 'OnlyAdaSupportedInEra'.
--
data MultiAssetSupportedInEra era where

     -- | Multi-asset transactions are supported in the 'Mary' era.
     MultiAssetInMaryEra :: MultiAssetSupportedInEra MaryEra

     -- | Multi-asset transactions are supported in the 'Alonzo' era.
     MultiAssetInAlonzoEra :: MultiAssetSupportedInEra AlonzoEra

deriving instance Eq   (MultiAssetSupportedInEra era)
deriving instance Show (MultiAssetSupportedInEra era)

instance ToJSON (MultiAssetSupportedInEra era) where
  toJSON = Aeson.String . Text.pack . show

-- | A representation of whether the era supports only ada transactions.
--
-- Prior to the Mary era only ada transactions are supported. Multi-assets are
-- supported from the Mary era onwards.
--
-- This is the negation of 'MultiAssetSupportedInEra'. It exists since we need
-- evidence to be positive.
--
data OnlyAdaSupportedInEra era where

     AdaOnlyInByronEra   :: OnlyAdaSupportedInEra ByronEra
     AdaOnlyInShelleyEra :: OnlyAdaSupportedInEra ShelleyEra
     AdaOnlyInAllegraEra :: OnlyAdaSupportedInEra AllegraEra

deriving instance Eq   (OnlyAdaSupportedInEra era)
deriving instance Show (OnlyAdaSupportedInEra era)

multiAssetSupportedInEra :: CardanoEra era
                         -> Either (OnlyAdaSupportedInEra era)
                                   (MultiAssetSupportedInEra era)
multiAssetSupportedInEra ByronEra   = Left AdaOnlyInByronEra
multiAssetSupportedInEra ShelleyEra = Left AdaOnlyInShelleyEra
multiAssetSupportedInEra AllegraEra = Left AdaOnlyInAllegraEra
multiAssetSupportedInEra MaryEra    = Right MultiAssetInMaryEra
multiAssetSupportedInEra AlonzoEra  = Right MultiAssetInAlonzoEra


-- | A representation of whether the era requires explicitly specified fees in
-- transactions.
--
-- The Byron era tx fees are implicit (as the difference bettween the sum of
-- outputs and sum of inputs), but all later eras the fees are specified in the
-- transaction explicitly.
--
data TxFeesExplicitInEra era where

     TxFeesExplicitInShelleyEra :: TxFeesExplicitInEra ShelleyEra
     TxFeesExplicitInAllegraEra :: TxFeesExplicitInEra AllegraEra
     TxFeesExplicitInMaryEra    :: TxFeesExplicitInEra MaryEra
     TxFeesExplicitInAlonzoEra  :: TxFeesExplicitInEra AlonzoEra

deriving instance Eq   (TxFeesExplicitInEra era)
deriving instance Show (TxFeesExplicitInEra era)

-- | A representation of whether the era requires implicitly specified fees in
-- transactions.
--
-- This is the negation of 'TxFeesExplicitInEra'.
--
data TxFeesImplicitInEra era where
     TxFeesImplicitInByronEra :: TxFeesImplicitInEra ByronEra

deriving instance Eq   (TxFeesImplicitInEra era)
deriving instance Show (TxFeesImplicitInEra era)

txFeesExplicitInEra :: CardanoEra era
                    -> Either (TxFeesImplicitInEra era)
                              (TxFeesExplicitInEra era)
txFeesExplicitInEra ByronEra   = Left  TxFeesImplicitInByronEra
txFeesExplicitInEra ShelleyEra = Right TxFeesExplicitInShelleyEra
txFeesExplicitInEra AllegraEra = Right TxFeesExplicitInAllegraEra
txFeesExplicitInEra MaryEra    = Right TxFeesExplicitInMaryEra
txFeesExplicitInEra AlonzoEra  = Right TxFeesExplicitInAlonzoEra


-- | A representation of whether the era supports transactions with an upper
-- bound on the range of slots in which they are valid.
--
-- The Shelley and subsequent eras support an upper bound on the validity
-- range. In the Shelley era specifically it is actually required. It is
-- optional in later eras.
--
data ValidityUpperBoundSupportedInEra era where

     ValidityUpperBoundInShelleyEra :: ValidityUpperBoundSupportedInEra ShelleyEra
     ValidityUpperBoundInAllegraEra :: ValidityUpperBoundSupportedInEra AllegraEra
     ValidityUpperBoundInMaryEra    :: ValidityUpperBoundSupportedInEra MaryEra
     ValidityUpperBoundInAlonzoEra  :: ValidityUpperBoundSupportedInEra AlonzoEra

deriving instance Eq   (ValidityUpperBoundSupportedInEra era)
deriving instance Show (ValidityUpperBoundSupportedInEra era)

validityUpperBoundSupportedInEra :: CardanoEra era
                                 -> Maybe (ValidityUpperBoundSupportedInEra era)
validityUpperBoundSupportedInEra ByronEra   = Nothing
validityUpperBoundSupportedInEra ShelleyEra = Just ValidityUpperBoundInShelleyEra
validityUpperBoundSupportedInEra AllegraEra = Just ValidityUpperBoundInAllegraEra
validityUpperBoundSupportedInEra MaryEra    = Just ValidityUpperBoundInMaryEra
validityUpperBoundSupportedInEra AlonzoEra  = Just ValidityUpperBoundInAlonzoEra
  

-- | A representation of whether the era supports transactions having /no/
-- upper bound on the range of slots in which they are valid.
--
-- Note that the 'ShelleyEra' /does not support/ omitting a validity upper
-- bound. It was introduced as a /required/ field in Shelley and then made
-- optional in Allegra and subsequent eras.
--
-- The Byron era supports this by virtue of the fact that it does not support
-- validity ranges at all.
--
data ValidityNoUpperBoundSupportedInEra era where

     ValidityNoUpperBoundInByronEra   :: ValidityNoUpperBoundSupportedInEra ByronEra
     ValidityNoUpperBoundInAllegraEra :: ValidityNoUpperBoundSupportedInEra AllegraEra
     ValidityNoUpperBoundInMaryEra    :: ValidityNoUpperBoundSupportedInEra MaryEra
     ValidityNoUpperBoundInAlonzoEra  :: ValidityNoUpperBoundSupportedInEra AlonzoEra

deriving instance Eq   (ValidityNoUpperBoundSupportedInEra era)
deriving instance Show (ValidityNoUpperBoundSupportedInEra era)

validityNoUpperBoundSupportedInEra :: CardanoEra era
                                   -> Maybe (ValidityNoUpperBoundSupportedInEra era)
validityNoUpperBoundSupportedInEra ByronEra   = Just ValidityNoUpperBoundInByronEra
validityNoUpperBoundSupportedInEra ShelleyEra = Nothing
validityNoUpperBoundSupportedInEra AllegraEra = Just ValidityNoUpperBoundInAllegraEra
validityNoUpperBoundSupportedInEra MaryEra    = Just ValidityNoUpperBoundInMaryEra
validityNoUpperBoundSupportedInEra AlonzoEra  = Just ValidityNoUpperBoundInAlonzoEra


-- | A representation of whether the era supports transactions with a lower
-- bound on the range of slots in which they are valid.
--
-- The Allegra and subsequent eras support an optional lower bound on the
-- validity range. No equivalent of 'ValidityNoUpperBoundSupportedInEra' is
-- needed since all eras support having no lower bound.
--
data ValidityLowerBoundSupportedInEra era where

     ValidityLowerBoundInAllegraEra :: ValidityLowerBoundSupportedInEra AllegraEra
     ValidityLowerBoundInMaryEra    :: ValidityLowerBoundSupportedInEra MaryEra
     ValidityLowerBoundInAlonzoEra  :: ValidityLowerBoundSupportedInEra AlonzoEra

deriving instance Eq   (ValidityLowerBoundSupportedInEra era)
deriving instance Show (ValidityLowerBoundSupportedInEra era)

validityLowerBoundSupportedInEra :: CardanoEra era
                                 -> Maybe (ValidityLowerBoundSupportedInEra era)
validityLowerBoundSupportedInEra ByronEra   = Nothing
validityLowerBoundSupportedInEra ShelleyEra = Nothing
validityLowerBoundSupportedInEra AllegraEra = Just ValidityLowerBoundInAllegraEra
validityLowerBoundSupportedInEra MaryEra    = Just ValidityLowerBoundInMaryEra
validityLowerBoundSupportedInEra AlonzoEra  = Just ValidityLowerBoundInAlonzoEra

-- | A representation of whether the era supports transaction metadata.
--
-- Transaction metadata is supported from the Shelley era onwards.
--
data TxMetadataSupportedInEra era where

     TxMetadataInShelleyEra :: TxMetadataSupportedInEra ShelleyEra
     TxMetadataInAllegraEra :: TxMetadataSupportedInEra AllegraEra
     TxMetadataInMaryEra    :: TxMetadataSupportedInEra MaryEra
     TxMetadataInAlonzoEra  :: TxMetadataSupportedInEra AlonzoEra

deriving instance Eq   (TxMetadataSupportedInEra era)
deriving instance Show (TxMetadataSupportedInEra era)

txMetadataSupportedInEra :: CardanoEra era
                         -> Maybe (TxMetadataSupportedInEra era)
txMetadataSupportedInEra ByronEra   = Nothing
txMetadataSupportedInEra ShelleyEra = Just TxMetadataInShelleyEra
txMetadataSupportedInEra AllegraEra = Just TxMetadataInAllegraEra
txMetadataSupportedInEra MaryEra    = Just TxMetadataInMaryEra
txMetadataSupportedInEra AlonzoEra  = Just TxMetadataInAlonzoEra


-- | A representation of whether the era supports auxiliary scripts in
-- transactions.
--
-- Auxiliary scripts are supported from the Allegra era onwards.
--
data AuxScriptsSupportedInEra era where

     AuxScriptsInAllegraEra :: AuxScriptsSupportedInEra AllegraEra
     AuxScriptsInMaryEra    :: AuxScriptsSupportedInEra MaryEra
     AuxScriptsInAlonzoEra  :: AuxScriptsSupportedInEra AlonzoEra

deriving instance Eq   (AuxScriptsSupportedInEra era)
deriving instance Show (AuxScriptsSupportedInEra era)

auxScriptsSupportedInEra :: CardanoEra era
                         -> Maybe (AuxScriptsSupportedInEra era)
auxScriptsSupportedInEra ByronEra   = Nothing
auxScriptsSupportedInEra ShelleyEra = Nothing
auxScriptsSupportedInEra AllegraEra = Just AuxScriptsInAllegraEra
auxScriptsSupportedInEra MaryEra    = Just AuxScriptsInMaryEra
auxScriptsSupportedInEra AlonzoEra  = Just AuxScriptsInAlonzoEra


-- | A representation of whether the era supports multi-asset transactions.
--
-- The Mary and subsequent eras support multi-asset transactions.
--
-- The negation of this is 'OnlyAdaSupportedInEra'.
--
data ScriptDataSupportedInEra era where

     -- | Script data is supported in transactions in the 'Alonzo' era.
     ScriptDataInAlonzoEra :: ScriptDataSupportedInEra AlonzoEra

deriving instance Eq   (ScriptDataSupportedInEra era)
deriving instance Show (ScriptDataSupportedInEra era)

scriptDataSupportedInEra :: CardanoEra era
                         -> Maybe (ScriptDataSupportedInEra era)
scriptDataSupportedInEra ByronEra   = Nothing
scriptDataSupportedInEra ShelleyEra = Nothing
scriptDataSupportedInEra AllegraEra = Nothing
scriptDataSupportedInEra MaryEra    = Nothing
scriptDataSupportedInEra AlonzoEra  = Just ScriptDataInAlonzoEra


-- | A representation of whether the era supports withdrawals from reward
-- accounts.
--
-- The Shelley and subsequent eras support stake addresses, their associated
-- reward accounts and support for withdrawals from them.
--
data WithdrawalsSupportedInEra era where

     WithdrawalsInShelleyEra :: WithdrawalsSupportedInEra ShelleyEra
     WithdrawalsInAllegraEra :: WithdrawalsSupportedInEra AllegraEra
     WithdrawalsInMaryEra    :: WithdrawalsSupportedInEra MaryEra
     WithdrawalsInAlonzoEra  :: WithdrawalsSupportedInEra AlonzoEra

deriving instance Eq   (WithdrawalsSupportedInEra era)
deriving instance Show (WithdrawalsSupportedInEra era)

withdrawalsSupportedInEra :: CardanoEra era
                          -> Maybe (WithdrawalsSupportedInEra era)
withdrawalsSupportedInEra ByronEra   = Nothing
withdrawalsSupportedInEra ShelleyEra = Just WithdrawalsInShelleyEra
withdrawalsSupportedInEra AllegraEra = Just WithdrawalsInAllegraEra
withdrawalsSupportedInEra MaryEra    = Just WithdrawalsInMaryEra
withdrawalsSupportedInEra AlonzoEra  = Just WithdrawalsInAlonzoEra


-- | A representation of whether the era supports 'Certificate's embedded in
-- transactions.
--
-- The Shelley and subsequent eras support such certificates.
--
data CertificatesSupportedInEra era where

     CertificatesInShelleyEra :: CertificatesSupportedInEra ShelleyEra
     CertificatesInAllegraEra :: CertificatesSupportedInEra AllegraEra
     CertificatesInMaryEra    :: CertificatesSupportedInEra MaryEra
     CertificatesInAlonzoEra  :: CertificatesSupportedInEra AlonzoEra

deriving instance Eq   (CertificatesSupportedInEra era)
deriving instance Show (CertificatesSupportedInEra era)

certificatesSupportedInEra :: CardanoEra era
                           -> Maybe (CertificatesSupportedInEra era)
certificatesSupportedInEra ByronEra   = Nothing
certificatesSupportedInEra ShelleyEra = Just CertificatesInShelleyEra
certificatesSupportedInEra AllegraEra = Just CertificatesInAllegraEra
certificatesSupportedInEra MaryEra    = Just CertificatesInMaryEra
certificatesSupportedInEra AlonzoEra  = Just CertificatesInAlonzoEra


-- | A representation of whether the era supports 'UpdateProposal's embedded in
-- transactions.
--
-- The Shelley and subsequent eras support such update proposals. They Byron
-- era has a notion of an update proposal, but it is a standalone chain object
-- and not embedded in a transaction.
--
data UpdateProposalSupportedInEra era where

     UpdateProposalInShelleyEra :: UpdateProposalSupportedInEra ShelleyEra
     UpdateProposalInAllegraEra :: UpdateProposalSupportedInEra AllegraEra
     UpdateProposalInMaryEra    :: UpdateProposalSupportedInEra MaryEra
     UpdateProposalInAlonzoEra  :: UpdateProposalSupportedInEra AlonzoEra

deriving instance Eq   (UpdateProposalSupportedInEra era)
deriving instance Show (UpdateProposalSupportedInEra era)

updateProposalSupportedInEra :: CardanoEra era
                             -> Maybe (UpdateProposalSupportedInEra era)
updateProposalSupportedInEra ByronEra   = Nothing
updateProposalSupportedInEra ShelleyEra = Just UpdateProposalInShelleyEra
updateProposalSupportedInEra AllegraEra = Just UpdateProposalInAllegraEra
updateProposalSupportedInEra MaryEra    = Just UpdateProposalInMaryEra
updateProposalSupportedInEra AlonzoEra  = Just UpdateProposalInAlonzoEra


-- ----------------------------------------------------------------------------
-- Building vs viewing transactions
--

data BuildTx
data ViewTx

data BuildTxWith build a where

     ViewTx      ::      BuildTxWith ViewTx  a
     BuildTxWith :: a -> BuildTxWith BuildTx a

deriving instance Eq   a => Eq   (BuildTxWith build a)
deriving instance Show a => Show (BuildTxWith build a)


-- ----------------------------------------------------------------------------
-- Transaction output values (era-dependent)
--

data TxOutValue era where

     TxOutAdaOnly :: OnlyAdaSupportedInEra era -> Lovelace -> TxOutValue era

     TxOutValue   :: MultiAssetSupportedInEra era -> Value -> TxOutValue era

deriving instance Eq   (TxOutValue era)
deriving instance Show (TxOutValue era)
deriving instance Generic (TxOutValue era)

instance ToJSON (TxOutValue era) where
  toJSON (TxOutAdaOnly _ ll) = toJSON ll
  toJSON (TxOutValue _ val) = toJSON val


-- ----------------------------------------------------------------------------
-- Transaction output datum (era-dependent)
--

data TxOutDatumHash era where

     TxOutDatumHashNone :: TxOutDatumHash era

     TxOutDatumHash     :: ScriptDataSupportedInEra era
                        -> Hash ScriptData
                        -> TxOutDatumHash era

deriving instance Eq   (TxOutDatumHash era)
deriving instance Show (TxOutDatumHash era)
deriving instance Generic (TxOutDatumHash era)


-- ----------------------------------------------------------------------------
-- Transaction fees
--

data TxFee era where

     TxFeeImplicit :: TxFeesImplicitInEra era -> TxFee era

     TxFeeExplicit :: TxFeesExplicitInEra era -> Lovelace -> TxFee era

deriving instance Eq   (TxFee era)
deriving instance Show (TxFee era)


-- ----------------------------------------------------------------------------
-- Transaction validity range
--

-- | This was formerly known as the TTL.
--
data TxValidityUpperBound era where

     TxValidityNoUpperBound :: ValidityNoUpperBoundSupportedInEra era
                            -> TxValidityUpperBound era

     TxValidityUpperBound   :: ValidityUpperBoundSupportedInEra era
                            -> SlotNo
                            -> TxValidityUpperBound era

deriving instance Eq   (TxValidityUpperBound era)
deriving instance Show (TxValidityUpperBound era)


data TxValidityLowerBound era where

     TxValidityNoLowerBound :: TxValidityLowerBound era

     TxValidityLowerBound   :: ValidityLowerBoundSupportedInEra era
                            -> SlotNo
                            -> TxValidityLowerBound era

deriving instance Eq   (TxValidityLowerBound era)
deriving instance Show (TxValidityLowerBound era)


-- ----------------------------------------------------------------------------
-- Transaction metadata (era-dependent)
--

data TxMetadataInEra era where

     TxMetadataNone  :: TxMetadataInEra era

     TxMetadataInEra :: TxMetadataSupportedInEra era
                     -> TxMetadata
                     -> TxMetadataInEra era

deriving instance Eq   (TxMetadataInEra era)
deriving instance Show (TxMetadataInEra era)


-- ----------------------------------------------------------------------------
-- Auxiliary scripts (era-dependent)
--

data TxAuxScripts era where

     TxAuxScriptsNone :: TxAuxScripts era

     TxAuxScripts     :: AuxScriptsSupportedInEra era
                      -> [ScriptInEra era]
                      -> TxAuxScripts era

deriving instance Eq   (TxAuxScripts era)
deriving instance Show (TxAuxScripts era)


-- ----------------------------------------------------------------------------
-- Auxiliary script data (era-dependent)
--

data TxAuxScriptData era where

     TxAuxScriptDataNone :: TxAuxScriptData era

     TxAuxScriptData     :: ScriptDataSupportedInEra era
                         -> [ScriptData]
                         -> TxAuxScriptData era

deriving instance Eq   (TxAuxScriptData era)
deriving instance Show (TxAuxScriptData era)


-- ----------------------------------------------------------------------------
-- Withdrawals within transactions (era-dependent)
--

data TxWithdrawals build era where

     TxWithdrawalsNone :: TxWithdrawals build era

     TxWithdrawals     :: WithdrawalsSupportedInEra era
                       -> [(StakeAddress, Lovelace,
                            BuildTxWith build (Witness WitCtxStake era))]
                       -> TxWithdrawals build era

deriving instance Eq   (TxWithdrawals build era)
deriving instance Show (TxWithdrawals build era)


-- ----------------------------------------------------------------------------
-- Certificates within transactions (era-dependent)
--

data TxCertificates build era where

     TxCertificatesNone :: TxCertificates build era

     TxCertificates     :: CertificatesSupportedInEra era
                        -> [Certificate]
                        -> BuildTxWith build
                             (Map StakeCredential (Witness WitCtxStake era))
                        -> TxCertificates build era

deriving instance Eq   (TxCertificates build era)
deriving instance Show (TxCertificates build era)


-- ----------------------------------------------------------------------------
-- Transaction metadata (era-dependent)
--

data TxUpdateProposal era where

     TxUpdateProposalNone :: TxUpdateProposal era

     TxUpdateProposal     :: UpdateProposalSupportedInEra era
                          -> UpdateProposal
                          -> TxUpdateProposal era

deriving instance Eq   (TxUpdateProposal era)
deriving instance Show (TxUpdateProposal era)


-- ----------------------------------------------------------------------------
-- Value minting within transactions (era-dependent)
--

data TxMintValue build era where

     TxMintNone  :: TxMintValue build era

     TxMintValue :: MultiAssetSupportedInEra era
                 -> Value
                 -> BuildTxWith build (Map PolicyId (Witness WitCtxMint era))
                 -> TxMintValue build era

deriving instance Eq   (TxMintValue build era)
deriving instance Show (TxMintValue build era)


-- ----------------------------------------------------------------------------
-- Transaction body content
--

data TxBodyContent build era =
     TxBodyContent {
       txIns            :: [(TxIn, BuildTxWith build (Witness WitCtxTxIn era))],
       txOuts           :: [TxOut era],
       txFee            :: TxFee era,
       txValidityRange  :: (TxValidityLowerBound era,
                            TxValidityUpperBound era),
       txMetadata       :: TxMetadataInEra era,
       txAuxScripts     :: TxAuxScripts era,
     --txAuxScriptData  :: TxAuxScriptData era, -- TODO alonzo
       txWithdrawals    :: TxWithdrawals  build era,
       txCertificates   :: TxCertificates build era,
       txUpdateProposal :: TxUpdateProposal era,
       txMintValue      :: TxMintValue    build era
     }


-- ----------------------------------------------------------------------------
-- Transaction bodies
--

data TxBody era where

     ByronTxBody
       :: Annotated Byron.Tx ByteString
       -> TxBody ByronEra

     ShelleyTxBody
       :: ShelleyBasedEra era
       -> Ledger.TxBody (ShelleyLedgerEra era)

          -- We include the scripts along with the tx body, rather than the
          -- witnesses set, since they need to be known when building the body.
       -> [Ledger.Script (ShelleyLedgerEra era)]

          -- TODO alonzo: we will probably want to or need to put the Alonzo data and
          -- redeemers in the tx body here

          -- The 'Ledger.AuxiliaryData' consists of one or several things,
          -- depending on era:
          -- + transaction metadata  (in Shelley and later)
          -- + auxiliary scripts     (in Allegra and later)
          -- + auxiliary script data (in Allonzo and later)
       -> Maybe (Ledger.AuxiliaryData (ShelleyLedgerEra era))

       -> TxBody era
     -- The 'ShelleyBasedEra' GADT tells us what era we are in.
     -- The 'ShelleyLedgerEra' type family maps that to the era type from the
     -- ledger lib. The 'Ledger.TxBody' type family maps that to a specific
     -- tx body type, which is different for each Shelley-based era.


-- The GADT in the ShelleyTxBody case requires a custom instance
instance Eq (TxBody era) where
    (==) (ByronTxBody txbodyA)
         (ByronTxBody txbodyB) = txbodyA == txbodyB

    (==) (ShelleyTxBody era txbodyA txscriptsA txmetadataA)
         (ShelleyTxBody _   txbodyB txscriptsB txmetadataB) =
         case era of
           ShelleyBasedEraShelley -> txbodyA     == txbodyB
                                  && txscriptsA  == txscriptsB
                                  && txmetadataA == txmetadataB

           ShelleyBasedEraAllegra -> txbodyA     == txbodyB
                                  && txscriptsA  == txscriptsB
                                  && txmetadataA == txmetadataB

           ShelleyBasedEraMary    -> txbodyA     == txbodyB
                                  && txscriptsA  == txscriptsB
                                  && txmetadataA == txmetadataB
           ShelleyBasedEraAlonzo  -> txbodyA     == txbodyB
                                  && txscriptsA  == txscriptsB
                                  && txmetadataA == txmetadataB

    (==) ByronTxBody{} (ShelleyTxBody era _ _ _) = case era of {}


-- The GADT in the ShelleyTxBody case requires a custom instance
instance Show (TxBody era) where
    showsPrec p (ByronTxBody txbody) =
      showParen (p >= 11)
        ( showString "ByronTxBody "
        . showsPrec 11 txbody
        )

    showsPrec p (ShelleyTxBody ShelleyBasedEraShelley
                               txbody txscripts txmetadata) =
      showParen (p >= 11)
        ( showString "ShelleyTxBody ShelleyBasedEraShelley "
        . showsPrec 11 txbody
        . showChar ' '
        . showsPrec 11 txscripts
        . showChar ' '
        . showsPrec 11 txmetadata
        )

    showsPrec p (ShelleyTxBody ShelleyBasedEraAllegra
                               txbody txscripts txmetadata) =
      showParen (p >= 11)
        ( showString "ShelleyTxBody ShelleyBasedEraAllegra "
        . showsPrec 11 txbody
        . showChar ' '
        . showsPrec 11 txscripts
        . showChar ' '
        . showsPrec 11 txmetadata
        )

    showsPrec p (ShelleyTxBody ShelleyBasedEraMary
                               txbody txscripts txmetadata) =
      showParen (p >= 11)
        ( showString "ShelleyTxBody ShelleyBasedEraMary "
        . showsPrec 11 txbody
        . showChar ' '
        . showsPrec 11 txscripts
        . showChar ' '
        . showsPrec 11 txmetadata
        )

    showsPrec p (ShelleyTxBody ShelleyBasedEraAlonzo
                               txbody txscripts txmetadata) =
      showParen (p >= 11)
        ( showString "ShelleyTxBody ShelleyBasedEraMary "
        . showsPrec 11 txbody
        . showChar ' '
        . showsPrec 11 txscripts
        . showChar ' '
        . showsPrec 11 txmetadata
        )


instance HasTypeProxy era => HasTypeProxy (TxBody era) where
    data AsType (TxBody era) = AsTxBody (AsType era)
    proxyToAsType _ = AsTxBody (proxyToAsType (Proxy :: Proxy era))

pattern AsByronTxBody :: AsType (TxBody ByronEra)
pattern AsByronTxBody   = AsTxBody AsByronEra
{-# COMPLETE AsByronTxBody #-}

pattern AsShelleyTxBody :: AsType (TxBody ShelleyEra)
pattern AsShelleyTxBody = AsTxBody AsShelleyEra
{-# COMPLETE AsShelleyTxBody #-}

pattern AsMaryTxBody :: AsType (TxBody MaryEra)
pattern AsMaryTxBody = AsTxBody AsMaryEra
{-# COMPLETE AsMaryTxBody #-}

instance IsCardanoEra era => SerialiseAsCBOR (TxBody era) where

    serialiseToCBOR (ByronTxBody txbody) =
      recoverBytes txbody

    serialiseToCBOR (ShelleyTxBody era txbody txscripts txmetadata) =
      case era of
        -- Use the same serialisation impl, but at different types:
        ShelleyBasedEraShelley -> serialiseShelleyBasedTxBody txbody txscripts txmetadata
        ShelleyBasedEraAllegra -> serialiseShelleyBasedTxBody txbody txscripts txmetadata
        ShelleyBasedEraMary    -> serialiseShelleyBasedTxBody txbody txscripts txmetadata
        ShelleyBasedEraAlonzo  -> serialiseShelleyBasedTxBody txbody txscripts txmetadata

    deserialiseFromCBOR _ bs =
      case cardanoEra :: CardanoEra era of
        ByronEra ->
          ByronTxBody <$>
            CBOR.decodeFullAnnotatedBytes
              "Byron TxBody"
              CBOR.fromCBORAnnotated
              (LBS.fromStrict bs)

        -- Use the same derialisation impl, but at different types:
        ShelleyEra -> deserialiseShelleyBasedTxBody
                        (ShelleyTxBody ShelleyBasedEraShelley) bs
        AllegraEra -> deserialiseShelleyBasedTxBody
                        (ShelleyTxBody ShelleyBasedEraAllegra) bs
        MaryEra    -> deserialiseShelleyBasedTxBody
                        (ShelleyTxBody ShelleyBasedEraMary) bs
        AlonzoEra  -> deserialiseShelleyBasedTxBody
                        (ShelleyTxBody ShelleyBasedEraAlonzo) bs

-- | The serialisation format for the different Shelley-based eras are not the
-- same, but they can be handled generally with one overloaded implementation.
--
serialiseShelleyBasedTxBody :: forall txbody script metadata.
                                (ToCBOR txbody, ToCBOR script, ToCBOR metadata)
                            => txbody
                            -> [script]
                            -> Maybe metadata
                            -> ByteString
serialiseShelleyBasedTxBody txbody txscripts txmetadata =
    CBOR.serializeEncoding' $
        CBOR.encodeListLen 3
     <> CBOR.toCBOR txbody
     <> CBOR.toCBOR txscripts
     <> CBOR.encodeNullMaybe CBOR.toCBOR txmetadata

deserialiseShelleyBasedTxBody :: forall txbody script metadata pair.
                                (FromCBOR (CBOR.Annotator txbody),
                                 FromCBOR (CBOR.Annotator script),
                                 FromCBOR (CBOR.Annotator metadata))
                              => (txbody -> [script] -> Maybe metadata -> pair)
                              -> ByteString
                              -> Either CBOR.DecoderError pair
deserialiseShelleyBasedTxBody mkTxBody bs =
    CBOR.decodeAnnotator
      "Shelley TxBody"
      decodeAnnotatedTuple
      (LBS.fromStrict bs)
  where
    decodeAnnotatedTuple :: CBOR.Decoder s (CBOR.Annotator pair)
    decodeAnnotatedTuple =  do
      CBOR.decodeListLenOf 3
      txbody     <- fromCBOR
      txscripts  <- fromCBOR
      txmetadata <- CBOR.decodeNullMaybe fromCBOR
      return $ CBOR.Annotator $ \fbs ->
        mkTxBody
          (CBOR.runAnnotator txbody fbs)
          (map (`CBOR.runAnnotator` fbs) txscripts)
          (CBOR.runAnnotator <$> txmetadata <*> pure fbs)

instance IsCardanoEra era => HasTextEnvelope (TxBody era) where
    textEnvelopeType _ =
      case cardanoEra :: CardanoEra era of
        ByronEra   -> "TxUnsignedByron"
        ShelleyEra -> "TxUnsignedShelley"
        AllegraEra -> "TxBodyAllegra"
        MaryEra    -> "TxBodyMary"
        AlonzoEra  -> "TxBodyAlonzo"


-- ----------------------------------------------------------------------------
-- Constructing transaction bodies
--

data TxBodyError era =
       TxBodyEmptyTxIns
     | TxBodyEmptyTxOuts
     | TxBodyOutputNegative Quantity (TxOut era)
     | TxBodyOutputOverflow Quantity (TxOut era)
     | TxBodyMetadataError [(Word64, TxMetadataRangeError)]
     | TxBodyMintAdaError
     | TxBodyAuxDataHashInvalidError
     | TxBodyMintBeforeMaryError
     deriving Show

instance Error (TxBodyError era) where
    displayError TxBodyEmptyTxIns  = "Transaction body has no inputs"
    displayError TxBodyEmptyTxOuts = "Transaction body has no outputs"
    displayError (TxBodyOutputNegative (Quantity q) txout) =
      "Negative quantity (" ++ show q ++ ") in transaction output: " ++
      show txout
    displayError (TxBodyOutputOverflow (Quantity q) txout) =
      "Quantity too large (" ++ show q ++ " >= 2^64) in transaction output: " ++
      show txout
    displayError (TxBodyMetadataError [(k, err)]) =
      "Error in metadata entry " ++ show k ++ ": " ++ displayError err
    displayError (TxBodyMetadataError errs) =
      "Error in metadata entries: " ++
      intercalate "; "
        [ show k ++ ": " ++ displayError err
        | (k, err) <- errs ]
    displayError TxBodyMintAdaError =
      "Transaction cannot mint ada, only non-ada assets"
    displayError TxBodyMintBeforeMaryError =
      "Transaction can mint in Mary era or later"
    displayError TxBodyAuxDataHashInvalidError =
      "Auxiliary data hash is invalid"


makeTransactionBody :: forall era.
                       IsCardanoEra era
                    => TxBodyContent BuildTx era
                    -> Either (TxBodyError era) (TxBody era)
makeTransactionBody =
    case cardanoEraStyle (cardanoEra :: CardanoEra era) of
      LegacyByronEra      -> makeByronTransactionBody
      ShelleyBasedEra era -> makeShelleyTransactionBody era


getTransactionBodyContent
  :: TxBody era -> Either (TxBodyError era) (TxBodyContent ViewTx era)
getTransactionBodyContent = \case
  ByronTxBody body ->
    Right $ getByronTxBodyContent body
  ShelleyTxBody era body _scripts mAux ->
    fromLedgerTxBody era body mAux


fromLedgerTxBody
  :: ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> Maybe (Ledger.AuxiliaryData (ShelleyLedgerEra era))
  -> Either (TxBodyError era) (TxBodyContent ViewTx era)
fromLedgerTxBody era body mAux = do
  checkAuxiliaryDataHash era body mAux
  txMintValue <- fromLedgerTxMintValue era body
  pure
    TxBodyContent
      { txIns            = fromLedgerTxIns            era body
      , txOuts           = fromLedgerTxOuts           era body
      , txFee            = fromLedgerTxFee            era body
      , txValidityRange  = fromLedgerTxValidityRange  era body
      , txWithdrawals    = fromLedgerTxWithdrawals    era body
      , txCertificates   = fromLedgerTxCertificates   era body
      , txUpdateProposal = fromLedgerTxUpdateProposal era body
      , txMintValue
      , txMetadata
      , txAuxScripts
      }
  where
    (txMetadata, txAuxScripts) = fromLedgerTxAuxiliaryData era mAux
    -- TODO alonzo ^^ also return TxAuxScriptData as 3rd component


checkAuxiliaryDataHash
  :: ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> Maybe (Ledger.AuxiliaryData (ShelleyLedgerEra era))
  -> Either (TxBodyError era) ()
checkAuxiliaryDataHash era body mAux =
  guard hashEquality ?! TxBodyAuxDataHashInvalidError
  where
    mAux' = maybeToStrictMaybe mAux
    hashEquality =
      case era of
        ShelleyBasedEraShelley ->
          Shelley._mdHash body ==
          (Ledger.hashAuxiliaryData @StandardShelley <$> mAux')
        ShelleyBasedEraAllegra ->
          Allegra.adHash' body ==
          (Ledger.hashAuxiliaryData @StandardAllegra <$> mAux')
        ShelleyBasedEraMary ->
          Mary.adHash' body ==
          (Ledger.hashAuxiliaryData @StandardMary <$> mAux')
        ShelleyBasedEraAlonzo ->
          Alonzo.adHash' body ==
          (Ledger.hashAuxiliaryData @StandardAlonzo <$> mAux')

fromLedgerTxIns
  :: ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> [(TxIn,BuildTxWith ViewTx (Witness WitCtxTxIn era))]
fromLedgerTxIns era body =
  [ (fromShelleyTxIn input, ViewTx)
  | input <-
      case era of
        ShelleyBasedEraShelley -> toList $ Shelley._inputs body
        ShelleyBasedEraAllegra -> toList $ Allegra.inputs' body
        ShelleyBasedEraMary    -> toList $ Mary.inputs'    body
        ShelleyBasedEraAlonzo  -> toList $ Alonzo.inputs'  body
  ]


fromLedgerTxOuts
  :: ShelleyBasedEra era -> Ledger.TxBody (ShelleyLedgerEra era) -> [TxOut era]
fromLedgerTxOuts era body =
  fromTxOut era <$>
  case era of
    ShelleyBasedEraShelley -> toList $ Shelley._outputs body
    ShelleyBasedEraAllegra -> toList $ Allegra.outputs' body
    ShelleyBasedEraMary    -> toList $ Mary.outputs'    body
    ShelleyBasedEraAlonzo  -> toList $ Alonzo.outputs'  body


fromLedgerTxFee
  :: ShelleyBasedEra era -> Ledger.TxBody (ShelleyLedgerEra era) -> TxFee era
fromLedgerTxFee era body =
  case era of
    ShelleyBasedEraShelley ->
      TxFeeExplicit TxFeesExplicitInShelleyEra $
      fromShelleyLovelace $ Shelley._txfee body
    ShelleyBasedEraAllegra ->
      TxFeeExplicit TxFeesExplicitInAllegraEra $
      fromShelleyLovelace $ Allegra.txfee' body
    ShelleyBasedEraMary ->
      TxFeeExplicit TxFeesExplicitInMaryEra $
      fromShelleyLovelace $ Mary.txfee' body
    ShelleyBasedEraAlonzo ->
      TxFeeExplicit TxFeesExplicitInAlonzoEra $
      fromShelleyLovelace $ Alonzo.txfee' body

fromLedgerTxValidityRange
  :: ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> (TxValidityLowerBound era, TxValidityUpperBound era)
fromLedgerTxValidityRange era body =
  case era of
    ShelleyBasedEraShelley ->
      ( TxValidityNoLowerBound
      , TxValidityUpperBound ValidityUpperBoundInShelleyEra $ Shelley._ttl body
      )

    ShelleyBasedEraAllegra ->
      ( case invalidBefore of
          SNothing -> TxValidityNoLowerBound
          SJust s  -> TxValidityLowerBound ValidityLowerBoundInAllegraEra s
      , case invalidHereafter of
          SNothing -> TxValidityNoUpperBound ValidityNoUpperBoundInAllegraEra
          SJust s  -> TxValidityUpperBound   ValidityUpperBoundInAllegraEra s
      )
      where
        Allegra.ValidityInterval{invalidBefore, invalidHereafter} =
          Allegra.vldt' body

    ShelleyBasedEraMary ->
      ( case invalidBefore of
          SNothing -> TxValidityNoLowerBound
          SJust s  -> TxValidityLowerBound ValidityLowerBoundInMaryEra s
      , case invalidHereafter of
          SNothing -> TxValidityNoUpperBound ValidityNoUpperBoundInMaryEra
          SJust s  -> TxValidityUpperBound   ValidityUpperBoundInMaryEra s
      )
      where
        Mary.ValidityInterval{invalidBefore, invalidHereafter} = Mary.vldt' body

    ShelleyBasedEraAlonzo ->
      ( case invalidBefore of
          SNothing -> TxValidityNoLowerBound
          SJust s  -> TxValidityLowerBound ValidityLowerBoundInAlonzoEra s
      , case invalidHereafter of
          SNothing -> TxValidityNoUpperBound ValidityNoUpperBoundInAlonzoEra
          SJust s  -> TxValidityUpperBound   ValidityUpperBoundInAlonzoEra s
      )
      where
        Mary.ValidityInterval{invalidBefore, invalidHereafter} = Alonzo.vldt' body


fromLedgerAuxiliaryData
  :: ShelleyBasedEra era
  -> Ledger.AuxiliaryData (ShelleyLedgerEra era)
  -> (Map Word64 TxMetadataValue, [ScriptInEra era])
fromLedgerAuxiliaryData ShelleyBasedEraShelley (Shelley.Metadata metadata) =
  (fromShelleyMetadata metadata, [])
fromLedgerAuxiliaryData ShelleyBasedEraAllegra (Allegra.AuxiliaryData ms ss) =
  ( fromShelleyMetadata ms
  , fromShelleyBasedScript ShelleyBasedEraAllegra <$> toList ss
  )
fromLedgerAuxiliaryData ShelleyBasedEraMary (Mary.AuxiliaryData ms ss) =
  ( fromShelleyMetadata ms
  , fromShelleyBasedScript ShelleyBasedEraMary <$> toList ss
  )
fromLedgerAuxiliaryData ShelleyBasedEraAlonzo (Alonzo.AuxiliaryData ms ss _ds) =
  ( fromShelleyMetadata ms
  , fromShelleyBasedScript ShelleyBasedEraAlonzo <$> toList ss
  )
  -- TODO alonzo: cover the Alonzo era auxiliary data, see txAuxScriptData above

fromLedgerTxAuxiliaryData
  :: ShelleyBasedEra era
  -> Maybe (Ledger.AuxiliaryData (ShelleyLedgerEra era))
  -> (TxMetadataInEra era, TxAuxScripts era)
fromLedgerTxAuxiliaryData _ Nothing = (TxMetadataNone, TxAuxScriptsNone)
fromLedgerTxAuxiliaryData era (Just auxData) =
  case era of
    ShelleyBasedEraShelley ->
      ( if null ms then
          TxMetadataNone
        else
          TxMetadataInEra TxMetadataInShelleyEra $ TxMetadata ms
      , TxAuxScriptsNone
      )
    ShelleyBasedEraAllegra ->
      ( if null ms then
          TxMetadataNone
        else
          TxMetadataInEra TxMetadataInAllegraEra $ TxMetadata ms
      , case ss of
          [] -> TxAuxScriptsNone
          _  -> TxAuxScripts AuxScriptsInAllegraEra ss
      )
    ShelleyBasedEraMary ->
      ( if null ms then
          TxMetadataNone
        else
          TxMetadataInEra TxMetadataInMaryEra $ TxMetadata ms
      , case ss of
          [] -> TxAuxScriptsNone
          _  -> TxAuxScripts AuxScriptsInMaryEra ss
      )
    ShelleyBasedEraAlonzo ->
      ( if null ms then
          TxMetadataNone
        else
          TxMetadataInEra TxMetadataInAlonzoEra $ TxMetadata ms
      , case ss of
          [] -> TxAuxScriptsNone
          _  -> TxAuxScripts AuxScriptsInAlonzoEra ss
      )
  where
    (ms, ss) = fromLedgerAuxiliaryData era auxData


fromLedgerTxWithdrawals
  :: ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> TxWithdrawals ViewTx era
fromLedgerTxWithdrawals era body =
  case era of
    ShelleyBasedEraShelley
      | null (Shelley.unWdrl withdrawals) -> TxWithdrawalsNone
      | otherwise ->
          TxWithdrawals WithdrawalsInShelleyEra $
          fromShelleyWithdrawal withdrawals
      where
        withdrawals = Shelley._wdrls body

    ShelleyBasedEraAllegra
      | null (Shelley.unWdrl withdrawals) -> TxWithdrawalsNone
      | otherwise ->
          TxWithdrawals WithdrawalsInAllegraEra $
          fromShelleyWithdrawal withdrawals
      where
        withdrawals = Allegra.wdrls' body

    ShelleyBasedEraMary
      | null (Shelley.unWdrl withdrawals) -> TxWithdrawalsNone
      | otherwise ->
          TxWithdrawals WithdrawalsInMaryEra $ fromShelleyWithdrawal withdrawals
      where
        withdrawals = Mary.wdrls' body

    ShelleyBasedEraAlonzo
      | null (Shelley.unWdrl withdrawals) -> TxWithdrawalsNone
      | otherwise ->
          TxWithdrawals WithdrawalsInAlonzoEra $ fromShelleyWithdrawal withdrawals
      where
        withdrawals = Alonzo.wdrls' body

fromLedgerTxCertificates
  :: ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> TxCertificates ViewTx era
fromLedgerTxCertificates era body =
  case era of
    ShelleyBasedEraShelley
      | null certificates -> TxCertificatesNone
      | otherwise ->
          TxCertificates
            CertificatesInShelleyEra
            (map fromShelleyCertificate $ toList certificates)
            ViewTx
      where
        certificates = Shelley._certs body

    ShelleyBasedEraAllegra
      | null certificates -> TxCertificatesNone
      | otherwise ->
          TxCertificates
            CertificatesInAllegraEra
            (map fromShelleyCertificate $ toList certificates)
            ViewTx
      where
        certificates = Allegra.certs' body

    ShelleyBasedEraMary
      | null certificates -> TxCertificatesNone
      | otherwise ->
          TxCertificates
            CertificatesInMaryEra
            (map fromShelleyCertificate $ toList certificates)
            ViewTx
      where
        certificates = Mary.certs' body

    ShelleyBasedEraAlonzo
      | null certificates -> TxCertificatesNone
      | otherwise ->
          TxCertificates
            CertificatesInAlonzoEra
            (map fromShelleyCertificate $ toList certificates)
            ViewTx
      where
        certificates = Alonzo.certs' body

fromLedgerTxUpdateProposal
  :: ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> TxUpdateProposal era
fromLedgerTxUpdateProposal era body =
  case era of
    ShelleyBasedEraShelley ->
      case Shelley._txUpdate body of
        SNothing -> TxUpdateProposalNone
        SJust p ->
          TxUpdateProposal UpdateProposalInShelleyEra $ fromShelleyUpdate p

    ShelleyBasedEraAllegra ->
      case Allegra.update' body of
        SNothing -> TxUpdateProposalNone
        SJust p ->
          TxUpdateProposal UpdateProposalInAllegraEra $ fromShelleyUpdate p

    ShelleyBasedEraMary ->
      case Mary.update' body of
        SNothing -> TxUpdateProposalNone
        SJust p ->
          TxUpdateProposal UpdateProposalInMaryEra $ fromShelleyUpdate p

    ShelleyBasedEraAlonzo ->
      case Alonzo.update' body of
        SNothing -> TxUpdateProposalNone
        SJust _p ->
          error "fromLedgerTxUpdateProposal: Alonzo era not implemented yet"
--        TxUpdateProposal UpdateProposalInAlonzoEra $ fromShelleyUpdate p
--        TODO alonzo: fromShelleyUpdate currently is not era-generic and expects
--              Ledger.PParamsDelta ledgerera ~ Shelley.PParamsUpdate ledgerera


fromLedgerTxMintValue
  :: ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> Either (TxBodyError era) (TxMintValue ViewTx era)
fromLedgerTxMintValue era body =
  case era of
    ShelleyBasedEraShelley -> pure TxMintNone

    ShelleyBasedEraAllegra ->
      TxMintNone <$
      guard (isZero $ Allegra.mint' body) ?! TxBodyMintBeforeMaryError

    ShelleyBasedEraMary
      | isZero mint -> pure TxMintNone
      | otherwise   ->
          pure $ TxMintValue MultiAssetInMaryEra (fromMaryValue mint) ViewTx
      where
        mint = Mary.mint' body

    ShelleyBasedEraAlonzo
      | isZero mint -> pure TxMintNone
      | otherwise   ->
          pure $ TxMintValue MultiAssetInAlonzoEra (fromMaryValue mint) ViewTx
      where
        mint = Alonzo.mint' body


makeByronTransactionBody :: TxBodyContent BuildTx ByronEra
                         -> Either (TxBodyError ByronEra) (TxBody ByronEra)
makeByronTransactionBody TxBodyContent { txIns, txOuts } = do
    ins'  <- NonEmpty.nonEmpty txIns      ?! TxBodyEmptyTxIns
    let ins'' = NonEmpty.map (toByronTxIn . fst) ins'

    outs'  <- NonEmpty.nonEmpty txOuts    ?! TxBodyEmptyTxOuts
    outs'' <- traverse
                (\out -> toByronTxOut out ?! classifyRangeError out)
                outs'
    return $
      ByronTxBody $
        reAnnotate $
          Annotated
            (Byron.UnsafeTx ins'' outs'' (Byron.mkAttributes ()))
            ()
  where
    classifyRangeError :: TxOut ByronEra -> TxBodyError ByronEra
    classifyRangeError
      txout@(TxOut (AddressInEra ByronAddressInAnyEra ByronAddress{})
                   (TxOutAdaOnly AdaOnlyInByronEra value) _)
      | value < 0        = TxBodyOutputNegative (lovelaceToQuantity value) txout
      | otherwise        = TxBodyOutputOverflow (lovelaceToQuantity value) txout

    classifyRangeError
      (TxOut (AddressInEra ByronAddressInAnyEra (ByronAddress _))
             (TxOutValue era _) _) = case era of {}

    classifyRangeError
      (TxOut (AddressInEra (ShelleyAddressInEra era) ShelleyAddress{})
             _ _) = case era of {}

getByronTxBodyContent :: Annotated Byron.Tx ByteString
                      -> TxBodyContent ViewTx ByronEra
getByronTxBodyContent (Annotated Byron.UnsafeTx{txInputs, txOutputs} _) =
  makeByronTransactionBodyContent
    [(fromByronTxIn input, ViewTx) | input <- toList txInputs]
    (fromByronTxOut <$> toList txOutputs)

makeShelleyTransactionBody :: ShelleyBasedEra era
                           -> TxBodyContent BuildTx era
                           -> Either (TxBodyError era) (TxBody era)
makeShelleyTransactionBody era@ShelleyBasedEraShelley
                           txbodycontent@TxBodyContent {
                             txIns,
                             txOuts,
                             txFee,
                             txValidityRange = (_, upperBound),
                             txMetadata,
                             txWithdrawals,
                             txCertificates,
                             txUpdateProposal
                           } = do

    guard (not (null txIns)) ?! TxBodyEmptyTxIns
    sequence_
      [ do guard (v >= 0) ?! TxBodyOutputNegative (lovelaceToQuantity v) txout
           guard (v <= maxTxOut) ?! TxBodyOutputOverflow (lovelaceToQuantity v) txout
      | let maxTxOut = fromIntegral (maxBound :: Word64) :: Lovelace
      , txout@(TxOut _ (TxOutAdaOnly AdaOnlyInShelleyEra v) _) <- txOuts ]
    case txMetadata of
      TxMetadataNone      -> return ()
      TxMetadataInEra _ m -> first TxBodyMetadataError (validateTxMetadata m)

    return $
      ShelleyTxBody era
        (Shelley.TxBody
          (Set.fromList (map (toShelleyTxIn . fst) txIns))
          (Seq.fromList (map toShelleyTxOut txOuts))
          (case txCertificates of
             TxCertificatesNone    -> Seq.empty
             TxCertificates _ cs _ -> Seq.fromList (map toShelleyCertificate cs))
          (case txWithdrawals of
             TxWithdrawalsNone  -> Shelley.Wdrl Map.empty
             TxWithdrawals _ ws -> toShelleyWithdrawal ws)
          (case txFee of
             TxFeeImplicit era'  -> case era' of {}
             TxFeeExplicit _ fee -> toShelleyLovelace fee)
          (case upperBound of
             TxValidityNoUpperBound era' -> case era' of {}
             TxValidityUpperBound _ ttl  -> ttl)
          (case txUpdateProposal of
             TxUpdateProposalNone -> SNothing
             TxUpdateProposal _ p -> SJust (toShelleyUpdate p))
          (maybeToStrictMaybe
            (Ledger.hashAuxiliaryData @StandardShelley <$> txAuxData)))
        (map toShelleySimpleScript (collectTxBodySimpleScripts txbodycontent))
        txAuxData
  where
    txAuxData :: Maybe (Ledger.AuxiliaryData StandardShelley)
    txAuxData
      | Map.null ms = Nothing
      | otherwise   = Just (toShelleyAuxiliaryData ms)
      where
        ms = case txMetadata of
               TxMetadataNone                     -> Map.empty
               TxMetadataInEra _ (TxMetadata ms') -> ms'

makeShelleyTransactionBody era@ShelleyBasedEraAllegra
                           txbodycontent@TxBodyContent {
                             txIns,
                             txOuts,
                             txFee,
                             txValidityRange = (lowerBound, upperBound),
                             txMetadata,
                             txAuxScripts,
                             txWithdrawals,
                             txCertificates,
                             txUpdateProposal
                           } = do

    guard (not (null txIns)) ?! TxBodyEmptyTxIns
    sequence_
      [ do guard (v >= 0) ?! TxBodyOutputNegative (lovelaceToQuantity v) txout
           guard (v <= maxTxOut) ?! TxBodyOutputOverflow (lovelaceToQuantity v) txout
      | let maxTxOut = fromIntegral (maxBound :: Word64) :: Lovelace
      , txout@(TxOut _ (TxOutAdaOnly AdaOnlyInAllegraEra v) _) <- txOuts
      ]
    case txMetadata of
      TxMetadataNone      -> return ()
      TxMetadataInEra _ m -> validateTxMetadata m ?!. TxBodyMetadataError

    return $
      ShelleyTxBody era
        (Allegra.TxBody
          (Set.fromList (map (toShelleyTxIn . fst) txIns))
          (Seq.fromList (map toShelleyTxOut txOuts))
          (case txCertificates of
             TxCertificatesNone    -> Seq.empty
             TxCertificates _ cs _ -> Seq.fromList (map toShelleyCertificate cs))
          (case txWithdrawals of
             TxWithdrawalsNone  -> Shelley.Wdrl Map.empty
             TxWithdrawals _ ws -> toShelleyWithdrawal ws)
          (case txFee of
             TxFeeImplicit era'  -> case era' of {}
             TxFeeExplicit _ fee -> toShelleyLovelace fee)
          (Allegra.ValidityInterval {
             invalidBefore    = case lowerBound of
                                          TxValidityNoLowerBound   -> SNothing
                                          TxValidityLowerBound _ s -> SJust s,
             invalidHereafter = case upperBound of
                                          TxValidityNoUpperBound _ -> SNothing
                                          TxValidityUpperBound _ s -> SJust s
           })
          (case txUpdateProposal of
             TxUpdateProposalNone -> SNothing
             TxUpdateProposal _ p -> SJust (toShelleyUpdate p))
          (maybeToStrictMaybe
            (Ledger.hashAuxiliaryData @StandardAllegra <$> txAuxData))
          mempty) -- No minting in Allegra, only Mary
        (map toShelleySimpleScript (collectTxBodySimpleScripts txbodycontent))
        txAuxData
  where
    txAuxData :: Maybe (Ledger.AuxiliaryData StandardAllegra)
    txAuxData
      | Map.null ms
      , null ss   = Nothing
      | otherwise = Just (toAllegraAuxiliaryData ms ss)
      where
        ms = case txMetadata of
               TxMetadataNone                     -> Map.empty
               TxMetadataInEra _ (TxMetadata ms') -> ms'
        ss = case txAuxScripts of
               TxAuxScriptsNone   -> []
               TxAuxScripts _ ss' -> ss'

makeShelleyTransactionBody era@ShelleyBasedEraMary
                           txbodycontent@TxBodyContent {
                             txIns,
                             txOuts,
                             txFee,
                             txValidityRange = (lowerBound, upperBound),
                             txMetadata,
                             txAuxScripts,
                             txWithdrawals,
                             txCertificates,
                             txUpdateProposal,
                             txMintValue
                           } = do

    guard (not (null txIns)) ?! TxBodyEmptyTxIns
    sequence_
      [ do allPositive
           allWithinMaxBound
      | let maxTxOut = fromIntegral (maxBound :: Word64) :: Quantity
      , txout@(TxOut _ (TxOutValue MultiAssetInMaryEra v) _) <- txOuts
      , let allPositive       = case [ q | (_,q) <- valueToList v, q < 0 ] of
                                  []  -> Right ()
                                  q:_ -> Left (TxBodyOutputNegative q txout)
            allWithinMaxBound = case [ q | (_,q) <- valueToList v, q > maxTxOut ] of
                                  []  -> Right ()
                                  q:_ -> Left (TxBodyOutputOverflow q txout)
      ]
    case txMetadata of
      TxMetadataNone      -> return ()
      TxMetadataInEra _ m -> validateTxMetadata m ?!. TxBodyMetadataError
    case txMintValue of
      TxMintNone        -> return ()
      TxMintValue _ v _ -> guard (selectLovelace v == 0) ?! TxBodyMintAdaError

    return $
      ShelleyTxBody era
        (Allegra.TxBody
          (Set.fromList (map (toShelleyTxIn . fst) txIns))
          (Seq.fromList (map toShelleyTxOut txOuts))
          (case txCertificates of
             TxCertificatesNone    -> Seq.empty
             TxCertificates _ cs _ -> Seq.fromList (map toShelleyCertificate cs))
          (case txWithdrawals of
             TxWithdrawalsNone  -> Shelley.Wdrl Map.empty
             TxWithdrawals _ ws -> toShelleyWithdrawal ws)
          (case txFee of
             TxFeeImplicit era'  -> case era' of {}
             TxFeeExplicit _ fee -> toShelleyLovelace fee)
          (Allegra.ValidityInterval {
             invalidBefore    = case lowerBound of
                                          TxValidityNoLowerBound   -> SNothing
                                          TxValidityLowerBound _ s -> SJust s,
             invalidHereafter = case upperBound of
                                          TxValidityNoUpperBound _ -> SNothing
                                          TxValidityUpperBound _ s -> SJust s
           })
          (case txUpdateProposal of
             TxUpdateProposalNone -> SNothing
             TxUpdateProposal _ p -> SJust (toShelleyUpdate p))
          (maybeToStrictMaybe
            (Ledger.hashAuxiliaryData @StandardMary <$> txAuxData))
          (case txMintValue of
             TxMintNone        -> mempty
             TxMintValue _ v _ -> toMaryValue v))
        (map toShelleySimpleScript (collectTxBodySimpleScripts txbodycontent))
        txAuxData
  where
    txAuxData :: Maybe (Ledger.AuxiliaryData StandardMary)
    txAuxData
      | Map.null ms
      , null ss   = Nothing
      | otherwise = Just (toAllegraAuxiliaryData ms ss)
      where
        ms = case txMetadata of
               TxMetadataNone                     -> Map.empty
               TxMetadataInEra _ (TxMetadata ms') -> ms'
        ss = case txAuxScripts of
               TxAuxScriptsNone   -> []
               TxAuxScripts _ ss' -> ss'

makeShelleyTransactionBody era@ShelleyBasedEraAlonzo
                           txbodycontent@TxBodyContent {
                             txIns,
                             txOuts,
                             txFee,
                             txValidityRange = (lowerBound, upperBound),
                             txMetadata,
                             txAuxScripts,
                             txWithdrawals,
                             txCertificates,
                             txUpdateProposal,
                             txMintValue
                           } = do

    guard (not (null txIns)) ?! TxBodyEmptyTxIns
    sequence_
      [ do allPositive
           allWithinMaxBound
      | let maxTxOut = fromIntegral (maxBound :: Word64) :: Quantity
      , txout@(TxOut _ (TxOutValue MultiAssetInAlonzoEra v) _) <- txOuts
      , let allPositive       = case [ q | (_,q) <- valueToList v, q < 0 ] of
                                  []  -> Right ()
                                  q:_ -> Left (TxBodyOutputNegative q txout)
            allWithinMaxBound = case [ q | (_,q) <- valueToList v, q > maxTxOut ] of
                                  []  -> Right ()
                                  q:_ -> Left (TxBodyOutputOverflow q txout)
      ]
    case txMetadata of
      TxMetadataNone      -> return ()
      TxMetadataInEra _ m -> validateTxMetadata m ?!. TxBodyMetadataError
    case txMintValue of
      TxMintNone        -> return ()
      TxMintValue _ v _ -> guard (selectLovelace v == 0) ?! TxBodyMintAdaError

    return $
      ShelleyTxBody era
        (Alonzo.TxBody
          (Set.fromList (map (toShelleyTxIn . fst) txIns))
          (error "TODO alonzo: add support for collateral")
          (Seq.fromList (map toShelleyTxOut txOuts))
          (case txCertificates of
             TxCertificatesNone    -> Seq.empty
             TxCertificates _ cs _ -> Seq.fromList (map toShelleyCertificate cs))
          (case txWithdrawals of
             TxWithdrawalsNone  -> Shelley.Wdrl Map.empty
             TxWithdrawals _ ws -> toShelleyWithdrawal ws)
          (case txFee of
             TxFeeImplicit era'  -> case era' of {}
             TxFeeExplicit _ fee -> toShelleyLovelace fee)
          (Allegra.ValidityInterval {
             invalidBefore    = case lowerBound of
                                          TxValidityNoLowerBound   -> SNothing
                                          TxValidityLowerBound _ s -> SJust s,
             invalidHereafter = case upperBound of
                                          TxValidityNoUpperBound _ -> SNothing
                                          TxValidityUpperBound _ s -> SJust s
           })
          (case txUpdateProposal of
             TxUpdateProposalNone -> SNothing
             TxUpdateProposal _ p -> SJust (toAlonzoUpdate p))
          (error "TODO alonzo: extra key hashes for required witnesses")
          (case txMintValue of
             TxMintNone        -> mempty
             TxMintValue _ v _ -> toMaryValue v)
          (error "TODO: Alonzo optional protocol param hash")
          (maybeToStrictMaybe
            (Ledger.hashAuxiliaryData @StandardAlonzo <$> txAuxData))
          (error "TODO alonzo: optional network"))
        (map toShelleySimpleScript (collectTxBodySimpleScripts txbodycontent))
        txAuxData
  where
    txAuxData :: Maybe (Ledger.AuxiliaryData StandardAlonzo)
    txAuxData
      | Map.null ms
      , null ss
      , null ds   = Nothing
      | otherwise = Just (toAlonzoAuxiliaryData ms ss ds)
      where
        ms = case txMetadata of
               TxMetadataNone                     -> Map.empty
               TxMetadataInEra _ (TxMetadata ms') -> ms'
        ss = case txAuxScripts of
               TxAuxScriptsNone   -> []
               TxAuxScripts _ ss' -> ss'
        ds :: [ScriptData]
        ds = error "TODO alonzo: support the Alonzo era aux data"
             -- TODO alonzo: txAuxScriptData

    toAlonzoUpdate :: UpdateProposal -> Shelley.Update ledgerera
    toAlonzoUpdate = error "TODO alonzo: toAlonzoUpdate"
    -- TODO alonzo: ^^ and move the definition next to toShelleyUpdate
    -- and\/or merge it with toShelleyUpdate to make it era-generic
    -- must assume Ledger.PParamsDelta ledgerera ~ Alonzo.PParamsDelta ledgerera


data SimpleScriptInEra era where
     SimpleScriptInEra :: ScriptLanguageInEra lang era
                       -> SimpleScriptVersion lang
                       -> SimpleScript lang
                       -> SimpleScriptInEra era

{-# ANN collectTxBodySimpleScripts ("HLint: ignore Reduce duplication" :: Text) #-}

collectTxBodySimpleScripts :: TxBodyContent BuildTx era
                           -> [SimpleScriptInEra era]
collectTxBodySimpleScripts TxBodyContent {
                             txIns,
                             txWithdrawals,
                             txCertificates,
                             txMintValue
                           } =
    [ script
    | (_, BuildTxWith witness) <- txIns
    , script <- simpleScriptInEra witness ]

 ++ [ script
    | TxWithdrawals _ withdrawals <- [txWithdrawals]
    , (_, _, BuildTxWith witness) <- withdrawals
    , script <- simpleScriptInEra witness ]

 ++ [ script
    | TxCertificates _ _ (BuildTxWith witnesses) <- [txCertificates]
    , witness <- Map.elems witnesses
    , script <- simpleScriptInEra witness ]

 ++ [ script
    | TxMintValue _ _ (BuildTxWith witnesses) <- [txMintValue]
    , witness <- Map.elems witnesses
    , script <- simpleScriptInEra witness ]

  where
    simpleScriptInEra :: Witness witctx era -> [SimpleScriptInEra era]
    simpleScriptInEra (ScriptWitness
                         _ (SimpleScriptWitness langInEra version script)) =
      [SimpleScriptInEra langInEra version script]

    simpleScriptInEra _ = []


toShelleySimpleScript :: SimpleScriptInEra era
                      -> Ledger.Script (ShelleyLedgerEra era)
toShelleySimpleScript (SimpleScriptInEra langInEra version script) =
    toShelleyScript (ScriptInEra langInEra (SimpleScript version script))

toShelleyWithdrawal :: [(StakeAddress, Lovelace, a)] -> Shelley.Wdrl StandardCrypto
toShelleyWithdrawal withdrawals =
    Shelley.Wdrl $
      Map.fromList
        [ (toShelleyStakeAddr stakeAddr, toShelleyLovelace value)
        | (stakeAddr, value, _) <- withdrawals ]


fromShelleyWithdrawal
  :: Shelley.Wdrl StandardCrypto
  -> [(StakeAddress, Lovelace, BuildTxWith ViewTx (Witness WitCtxStake era))]
fromShelleyWithdrawal (Shelley.Wdrl withdrawals) =
  [ (fromShelleyStakeAddr stakeAddr, fromShelleyLovelace value, ViewTx)
  | (stakeAddr, value) <- Map.assocs withdrawals
  ]


-- | In the Shelley era the auxiliary data consists only of the tx metadata
toShelleyAuxiliaryData :: Map Word64 TxMetadataValue
                       -> Ledger.AuxiliaryData StandardShelley
toShelleyAuxiliaryData m =
    Shelley.Metadata
      (toShelleyMetadata m)


-- | In the Allegra and Mary eras the auxiliary data consists of the tx metadata
-- and the axiliary scripts.
--
toAllegraAuxiliaryData :: forall era ledgerera.
                          ShelleyLedgerEra era ~ ledgerera
                       => Ledger.AuxiliaryData ledgerera ~ Allegra.AuxiliaryData ledgerera
                       => Ledger.AnnotatedData (Ledger.Script ledgerera)
                       => Ord (Ledger.Script ledgerera)
                       => Map Word64 TxMetadataValue
                       -> [ScriptInEra era]
                       -> Ledger.AuxiliaryData ledgerera
toAllegraAuxiliaryData m ss =
    Allegra.AuxiliaryData
      (toShelleyMetadata m)
      (Seq.fromList (map toShelleyScript ss))


-- | In the Alonzo and later eras the auxiliary data consists of the tx metadata
-- and the axiliary scripts, and the axiliary script data.
--
toAlonzoAuxiliaryData :: forall era ledgerera.
                         ShelleyLedgerEra era ~ ledgerera
                      => Ledger.AuxiliaryData ledgerera ~ Alonzo.AuxiliaryData ledgerera
                      => Ledger.Script ledgerera ~ Alonzo.Script ledgerera
                      => Ledger.Era ledgerera
                      => Map Word64 TxMetadataValue
                      -> [ScriptInEra era]
                      -> [ScriptData]
                      -> Ledger.AuxiliaryData ledgerera
toAlonzoAuxiliaryData m ss ds =
    Alonzo.AuxiliaryData
      (toShelleyMetadata m)
      (Seq.fromList (map toShelleyScript ss))
      (Set.fromList (map toAlonzoScriptData ds))


-- ----------------------------------------------------------------------------
-- Transitional utility functions for making transaction bodies
--

-- | Transitional function to help the CLI move to the updated TxBody API.
--
makeByronTransaction :: [TxIn]
                     -> [TxOut ByronEra]
                     -> Either (TxBodyError ByronEra) (TxBody ByronEra)
makeByronTransaction txIns txOuts =
  makeTransactionBody $
    makeByronTransactionBodyContent
      [(txin, BuildTxWith (KeyWitness KeyWitnessForSpending)) | txin <- txIns]
      txOuts
{-# DEPRECATED makeByronTransaction "Use makeTransactionBody" #-}


makeByronTransactionBodyContent
  :: [(TxIn, BuildTxWith build (Witness WitCtxTxIn ByronEra))]
  -> [TxOut ByronEra]
  -> TxBodyContent build ByronEra
makeByronTransactionBodyContent txIns txOuts =
  TxBodyContent {
    txIns,
    txOuts,
    txFee            = TxFeeImplicit TxFeesImplicitInByronEra,
    txValidityRange  = (TxValidityNoLowerBound,
                        TxValidityNoUpperBound
                          ValidityNoUpperBoundInByronEra),
    txMetadata       = TxMetadataNone,
    txAuxScripts     = TxAuxScriptsNone,
    txWithdrawals    = TxWithdrawalsNone,
    txCertificates   = TxCertificatesNone,
    txUpdateProposal = TxUpdateProposalNone,
    txMintValue      = TxMintNone
  }


-- ----------------------------------------------------------------------------
-- Other utilities helpful with making transaction bodies
--

-- | Compute the 'TxIn' of the initial UTxO pseudo-transaction corresponding
-- to the given address in the genesis initial funds.
--
-- The Shelley initial UTxO is constructed from the 'sgInitialFunds' which
-- is not a full UTxO but just a map from addresses to coin values.
--
-- This gets turned into a UTxO by making a pseudo-transaction for each address,
-- with the 0th output being the coin value. So to spend from the initial UTxO
-- we need this same 'TxIn' to use as an input to the spending transaction.
--
genesisUTxOPseudoTxIn :: NetworkId -> Hash GenesisUTxOKey -> TxIn
genesisUTxOPseudoTxIn nw (GenesisUTxOKeyHash kh) =
    --TODO: should handle Byron UTxO case too.
    fromShelleyTxIn (Shelley.initialFundsPseudoTxIn addr)
  where
    addr :: Shelley.Addr StandardCrypto
    addr = Shelley.Addr
             (toShelleyNetwork nw)
             (Shelley.KeyHashObj kh)
             Shelley.StakeRefNull
