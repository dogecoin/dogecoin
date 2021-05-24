{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- The Shelley ledger uses promoted data kinds which we have to use, but we do
-- not export any from this API. We also use them unticked as nature intended.
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

-- | Complete, signed transactions
--
module Cardano.Api.Tx (

    -- * Signing transactions
    -- | Creating transaction witnesses one by one, or all in one go.
    Tx(.., Tx),
    getTxBody,
    getTxWitnesses,

    -- ** Signing in one go
    ShelleySigningKey(..),
    toShelleySigningKey,
    signByronTransaction,
    signShelleyTransaction,
    -- ** Incremental signing and separate witnesses
    makeSignedTransaction,
    KeyWitness(..),
    makeByronKeyWitness,
    ShelleyWitnessSigningKey(..),
    makeShelleyKeyWitness,
    WitnessNetworkIdOrByronAddress (..),
    makeShelleyBootstrapWitness,
    makeShelleySignature,
    getShelleyKeyWitnessVerificationKey,

    -- * Data family instances
    AsType(AsTx, AsByronTx, AsShelleyTx,
           AsKeyWitness, AsByronWitness, AsShelleyWitness),
  ) where

import           Prelude

import           Data.Maybe

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import           Data.Functor.Identity (Identity)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vector
--
-- Common types, consensus, network
--
import           Cardano.Binary (Annotated (..))
import qualified Cardano.Binary as CBOR
import qualified Cardano.Prelude as CBOR (cborError)

--
-- Crypto API used by consensus and Shelley (and should be used by Byron)
--
import qualified Cardano.Crypto.DSIGN.Class as Crypto
import qualified Cardano.Crypto.Util as Crypto
import qualified Cardano.Crypto.Wallet as Crypto.HD

--
-- Byron imports
--
import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Chain.UTxO as Byron
import qualified Cardano.Crypto.Hashing as Byron
import qualified Cardano.Crypto.ProtocolMagic as Byron
import qualified Cardano.Crypto.Signing as Byron

--
-- Shelley imports
--
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (StandardCrypto)

import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.SafeHash as Ledger
import qualified Cardano.Ledger.Shelley.Constraints as Shelley
import qualified Shelley.Spec.Ledger.TxBody as Ledger (EraIndependentTxBody)

import qualified Shelley.Spec.Ledger.Address.Bootstrap as Shelley
import           Shelley.Spec.Ledger.BaseTypes (maybeToStrictMaybe, strictMaybeToMaybe)
import qualified Shelley.Spec.Ledger.Keys as Shelley
import qualified Shelley.Spec.Ledger.Tx as Shelley

import qualified Cardano.Ledger.Alonzo as Alonzo
import qualified Cardano.Ledger.Alonzo.TxWitness as Alonzo

import           Cardano.Api.Address
import           Cardano.Api.Certificate
import           Cardano.Api.Eras
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.Key
import           Cardano.Api.KeysByron
import           Cardano.Api.KeysShelley
import           Cardano.Api.NetworkId
import           Cardano.Api.SerialiseCBOR
import           Cardano.Api.SerialiseTextEnvelope
import           Cardano.Api.TxBody


-- ----------------------------------------------------------------------------
-- Signed transactions
--

data Tx era where

     ByronTx
       :: Byron.ATxAux ByteString
       -> Tx ByronEra

     ShelleyTx
       :: ShelleyBasedEra era
       -> Ledger.Tx (ShelleyLedgerEra era)
       -> Tx era

-- The GADT in the ShelleyTx case requires a custom instance
instance Eq (Tx era) where
    (==) (ByronTx txA)
         (ByronTx txB) = txA == txB

    (==) (ShelleyTx era txA)
         (ShelleyTx _   txB) =
      case era of
        ShelleyBasedEraShelley -> txA == txB
        ShelleyBasedEraAllegra -> txA == txB
        ShelleyBasedEraMary    -> txA == txB
        ShelleyBasedEraAlonzo  -> txA == txB

    (==) ByronTx{} (ShelleyTx era _) = case era of {}

-- The GADT in the ShelleyTx case requires a custom instance
instance Show (Tx era) where
    showsPrec p (ByronTx tx) =
      showParen (p >= 11) $
        showString "ByronTx "
      . showsPrec 11 tx

    showsPrec p (ShelleyTx ShelleyBasedEraShelley tx) =
      showParen (p >= 11) $
        showString "ShelleyTx ShelleyBasedEraShelley "
      . showsPrec 11 tx

    showsPrec p (ShelleyTx ShelleyBasedEraAllegra tx) =
      showParen (p >= 11) $
        showString "ShelleyTx ShelleyBasedEraAllegra "
      . showsPrec 11 tx

    showsPrec p (ShelleyTx ShelleyBasedEraMary tx) =
      showParen (p >= 11) $
        showString "ShelleyTx ShelleyBasedEraMary "
      . showsPrec 11 tx

    showsPrec p (ShelleyTx ShelleyBasedEraAlonzo tx) =
      showParen (p >= 11) $
        showString "ShelleyTx ShelleyBasedEraAlonzo "
      . showsPrec 11 tx


instance HasTypeProxy era => HasTypeProxy (Tx era) where
    data AsType (Tx era) = AsTx (AsType era)
    proxyToAsType _ = AsTx (proxyToAsType (Proxy :: Proxy era))

pattern AsByronTx :: AsType (Tx ByronEra)
pattern AsByronTx   = AsTx AsByronEra
{-# COMPLETE AsByronTx #-}

pattern AsShelleyTx :: AsType (Tx ShelleyEra)
pattern AsShelleyTx = AsTx AsShelleyEra
{-# COMPLETE AsShelleyTx #-}


instance IsCardanoEra era => SerialiseAsCBOR (Tx era) where
    serialiseToCBOR (ByronTx tx) = CBOR.recoverBytes tx

    serialiseToCBOR (ShelleyTx era tx) =
      case era of
        ShelleyBasedEraShelley -> serialiseShelleyBasedTx tx
        ShelleyBasedEraAllegra -> serialiseShelleyBasedTx tx
        ShelleyBasedEraMary    -> serialiseShelleyBasedTx tx
        ShelleyBasedEraAlonzo  -> serialiseShelleyBasedTx tx

    deserialiseFromCBOR _ bs =
      case cardanoEra :: CardanoEra era of
        ByronEra ->
          ByronTx <$>
            CBOR.decodeFullAnnotatedBytes
              "Byron Tx" fromCBOR (LBS.fromStrict bs)

        -- Use the same derialisation impl, but at different types:
        ShelleyEra -> deserialiseShelleyBasedTx
                        (ShelleyTx ShelleyBasedEraShelley) bs
        AllegraEra -> deserialiseShelleyBasedTx
                        (ShelleyTx ShelleyBasedEraAllegra) bs
        MaryEra    -> deserialiseShelleyBasedTx
                        (ShelleyTx ShelleyBasedEraMary) bs
        AlonzoEra  -> deserialiseShelleyBasedTx
                        (ShelleyTx ShelleyBasedEraAlonzo) bs

-- | The serialisation format for the different Shelley-based eras are not the
-- same, but they can be handled generally with one overloaded implementation.
--
serialiseShelleyBasedTx :: ToCBOR tx => tx -> ByteString
serialiseShelleyBasedTx = CBOR.serialize'

deserialiseShelleyBasedTx :: FromCBOR (CBOR.Annotator tx)
                          => (tx -> tx')
                          -> ByteString
                          -> Either CBOR.DecoderError tx'
deserialiseShelleyBasedTx mkTx bs =
    mkTx <$> CBOR.decodeAnnotator "Shelley Tx" fromCBOR (LBS.fromStrict bs)


instance IsCardanoEra era => HasTextEnvelope (Tx era) where
    textEnvelopeType _ =
      case cardanoEra :: CardanoEra era of
        ByronEra   -> "TxSignedByron"
        ShelleyEra -> "TxSignedShelley"
        AllegraEra -> "Tx AllegraEra"
        MaryEra    -> "Tx MaryEra"
        AlonzoEra  -> "Tx AlonzoEra"


data KeyWitness era where

     ByronKeyWitness
       :: Byron.TxInWitness
       -> KeyWitness ByronEra

     ShelleyBootstrapWitness
       :: ShelleyBasedEra era
       -> Shelley.BootstrapWitness StandardCrypto
       -> KeyWitness era

     ShelleyKeyWitness
       :: ShelleyBasedEra era
       -> Shelley.WitVKey Shelley.Witness StandardCrypto
       -> KeyWitness era


-- The GADT in the Shelley cases requires a custom instance
instance Eq (KeyWitness era) where
    (==) (ByronKeyWitness wA)
         (ByronKeyWitness wB) = wA == wB

    (==) (ShelleyBootstrapWitness era wA)
         (ShelleyBootstrapWitness _   wB) =
      case era of
        ShelleyBasedEraShelley -> wA == wB
        ShelleyBasedEraAllegra -> wA == wB
        ShelleyBasedEraMary    -> wA == wB
        ShelleyBasedEraAlonzo  -> wA == wB

    (==) (ShelleyKeyWitness era wA)
         (ShelleyKeyWitness _   wB) =
      case era of
        ShelleyBasedEraShelley -> wA == wB
        ShelleyBasedEraAllegra -> wA == wB
        ShelleyBasedEraMary    -> wA == wB
        ShelleyBasedEraAlonzo  -> wA == wB

    (==) _ _ = False

-- The GADT in the ShelleyTx case requires a custom instance
--TODO: once we start providing custom patterns we should do the show in terms
-- of those. It'll be less verbose too!
instance Show (KeyWitness era) where
    showsPrec p (ByronKeyWitness tx) =
      showParen (p >= 11) $
        showString "ByronKeyWitness "
      . showsPrec 11 tx

    showsPrec p (ShelleyBootstrapWitness ShelleyBasedEraShelley tx) =
      showParen (p >= 11) $
        showString "ShelleyBootstrapWitness ShelleyBasedEraShelley "
      . showsPrec 11 tx

    showsPrec p (ShelleyBootstrapWitness ShelleyBasedEraAllegra tx) =
      showParen (p >= 11) $
        showString "ShelleyBootstrapWitness ShelleyBasedEraAllegra "
      . showsPrec 11 tx

    showsPrec p (ShelleyBootstrapWitness ShelleyBasedEraMary tx) =
      showParen (p >= 11) $
        showString "ShelleyBootstrapWitness ShelleyBasedEraMary "
      . showsPrec 11 tx

    showsPrec p (ShelleyBootstrapWitness ShelleyBasedEraAlonzo tx) =
      showParen (p >= 11) $
        showString "ShelleyBootstrapWitness ShelleyBasedEraAlonzo "
      . showsPrec 11 tx

    showsPrec p (ShelleyKeyWitness ShelleyBasedEraShelley tx) =
      showParen (p >= 11) $
        showString "ShelleyKeyWitness ShelleyBasedEraShelley "
      . showsPrec 11 tx

    showsPrec p (ShelleyKeyWitness ShelleyBasedEraAllegra tx) =
      showParen (p >= 11) $
        showString "ShelleyKeyWitness ShelleyBasedEraAllegra "
      . showsPrec 11 tx

    showsPrec p (ShelleyKeyWitness ShelleyBasedEraMary tx) =
      showParen (p >= 11) $
        showString "ShelleyKeyWitness ShelleyBasedEraMary "
      . showsPrec 11 tx

    showsPrec p (ShelleyKeyWitness ShelleyBasedEraAlonzo tx) =
      showParen (p >= 11) $
        showString "ShelleyKeyWitness ShelleyBasedEraAlonzo "
      . showsPrec 11 tx


instance HasTypeProxy era => HasTypeProxy (KeyWitness era) where
    data AsType (KeyWitness era) = AsKeyWitness (AsType era)
    proxyToAsType _ = AsKeyWitness (proxyToAsType (Proxy :: Proxy era))

pattern AsByronWitness :: AsType (KeyWitness ByronEra)
pattern AsByronWitness   = AsKeyWitness AsByronEra
{-# COMPLETE AsByronWitness #-}

pattern AsShelleyWitness :: AsType (KeyWitness ShelleyEra)
pattern AsShelleyWitness = AsKeyWitness AsShelleyEra
{-# COMPLETE AsShelleyWitness #-}


instance IsCardanoEra era => SerialiseAsCBOR (KeyWitness era) where
    serialiseToCBOR (ByronKeyWitness wit) = CBOR.serialize' wit

    serialiseToCBOR (ShelleyKeyWitness _era wit) =
      CBOR.serializeEncoding' $
      encodeShelleyBasedKeyWitness wit

    serialiseToCBOR (ShelleyBootstrapWitness _era wit) =
      CBOR.serializeEncoding' $
      encodeShelleyBasedBootstrapWitness wit

    deserialiseFromCBOR _ bs =
      case cardanoEra :: CardanoEra era of
        ByronEra ->
          ByronKeyWitness <$> CBOR.decodeFull' bs

        -- Use the same derialisation impl, but at different types:
        ShelleyEra -> decodeShelleyBasedWitness ShelleyBasedEraShelley bs
        AllegraEra -> decodeShelleyBasedWitness ShelleyBasedEraAllegra bs
        MaryEra    -> decodeShelleyBasedWitness ShelleyBasedEraMary    bs
        AlonzoEra  -> decodeShelleyBasedWitness ShelleyBasedEraAlonzo  bs


encodeShelleyBasedKeyWitness :: ToCBOR w => w -> CBOR.Encoding
encodeShelleyBasedKeyWitness wit =
    CBOR.encodeListLen 2 <> CBOR.encodeWord 0 <> toCBOR wit

encodeShelleyBasedBootstrapWitness :: ToCBOR w => w -> CBOR.Encoding
encodeShelleyBasedBootstrapWitness wit =
    CBOR.encodeListLen 2 <> CBOR.encodeWord 1 <> toCBOR wit

decodeShelleyBasedWitness :: forall era.
                             ShelleyBasedEra era
                          -> ByteString
                          -> Either CBOR.DecoderError (KeyWitness era)
decodeShelleyBasedWitness era =
    CBOR.decodeAnnotator "Shelley Witness" decode . LBS.fromStrict
  where
    decode :: CBOR.Decoder s (CBOR.Annotator (KeyWitness era))
    decode =  do
      CBOR.decodeListLenOf 2
      t <- CBOR.decodeWord
      case t of
        0 -> fmap (fmap (ShelleyKeyWitness era)) fromCBOR
        1 -> fmap (fmap (ShelleyBootstrapWitness era)) fromCBOR
        _ -> CBOR.cborError $ CBOR.DecoderErrorUnknownTag
                                "Shelley Witness" (fromIntegral t)


instance IsCardanoEra era => HasTextEnvelope (KeyWitness era) where
    textEnvelopeType _ =
      case cardanoEra :: CardanoEra era of
        ByronEra   -> "TxWitnessByron"
        ShelleyEra -> "TxWitnessShelley"
        AllegraEra -> "TxWitness AllegraEra"
        MaryEra    -> "TxWitness MaryEra"
        AlonzoEra  -> "TxWitness AlonzoEra"


pattern Tx :: Ledger.Era era => TxBody era -> [KeyWitness era] -> Tx era
pattern Tx txbody ws <- (getTxBodyAndWitnesses -> (txbody, ws))
  where
    Tx txbody ws = makeSignedTransaction ws txbody

getTxBodyAndWitnesses :: Tx era -> (TxBody era, [KeyWitness era])
getTxBodyAndWitnesses tx = (getTxBody tx, getTxWitnesses tx)

getTxBody :: forall era. Tx era -> TxBody era
getTxBody (ByronTx Byron.ATxAux { Byron.aTaTx = txbody }) =
    ByronTxBody txbody

getTxBody (ShelleyTx era tx) =
    case era of
      ShelleyBasedEraShelley -> getShelleyTxBody tx
      ShelleyBasedEraAllegra -> getShelleyTxBody tx
      ShelleyBasedEraMary    -> getShelleyTxBody tx
      ShelleyBasedEraAlonzo  -> getAlonzoTxBody  tx
  where
    getShelleyTxBody :: forall ledgerera.
                        ShelleyLedgerEra era ~ ledgerera
                     => Ledger.Witnesses ledgerera ~ Shelley.WitnessSetHKD Identity ledgerera
                     => Shelley.ShelleyBased ledgerera
                     => Ledger.Tx ledgerera
                     -> TxBody era
    getShelleyTxBody Shelley.Tx {
                       Shelley.body       = txbody,
                       Shelley.auxiliaryData = txAuxiliaryData,
                       Shelley.wits = Shelley.WitnessSet
                                              _addrWits
                                               msigWits
                                              _bootWits
                     } =
      ShelleyTxBody era txbody
                    (Map.elems msigWits)
                    (strictMaybeToMaybe txAuxiliaryData)

    getAlonzoTxBody :: forall ledgerera.
                       ShelleyLedgerEra era ~ ledgerera
                    => Ledger.Witnesses ledgerera ~ Alonzo.TxWitness ledgerera
                    => Shelley.ShelleyBased ledgerera
                    => Ledger.Tx ledgerera
                    -> TxBody era
    getAlonzoTxBody Shelley.Tx {
                      Shelley.body = txbody,
                      Shelley.wits = Alonzo.TxWitness'
                                     _addrWits
                                     _bootWits
                                     txscripts
                                     _txdats
                                     _txrdmrs,
                      Shelley.auxiliaryData = auxiliaryData
                    } =
      ShelleyTxBody era txbody
                    (Map.elems txscripts)
                    (strictMaybeToMaybe auxiliaryData)
                    --TODO: we will probably want to put the Alonzo data and
                    -- redeemer in the tx body here, and so that will use
                    -- the _txdats and _txrdmrs above.

getTxWitnesses :: forall era. Tx era -> [KeyWitness era]
getTxWitnesses (ByronTx Byron.ATxAux { Byron.aTaWitness = witnesses }) =
    map ByronKeyWitness
  . Vector.toList
  . unAnnotated
  $ witnesses

getTxWitnesses (ShelleyTx era tx) =
    case era of
      ShelleyBasedEraShelley -> getShelleyTxWitnesses tx
      ShelleyBasedEraAllegra -> getShelleyTxWitnesses tx
      ShelleyBasedEraMary    -> getShelleyTxWitnesses tx
      ShelleyBasedEraAlonzo  -> getAlonzoTxWitnesses  tx
  where
    getShelleyTxWitnesses :: forall ledgerera.
                             Ledger.Crypto ledgerera ~ StandardCrypto
                          => Ledger.Witnesses ledgerera ~ Shelley.WitnessSetHKD Identity ledgerera
                          => ToCBOR (Ledger.Witnesses ledgerera)
                          => Shelley.ShelleyBased ledgerera
                          => Ledger.Tx ledgerera
                          -> [KeyWitness era]
    getShelleyTxWitnesses Shelley.Tx {
                            Shelley.wits =
                              Shelley.WitnessSet
                                addrWits
                               _msigWits
                                bootWits
                          } =
        map (ShelleyBootstrapWitness era) (Set.elems bootWits)
     ++ map (ShelleyKeyWitness       era) (Set.elems addrWits)

    getAlonzoTxWitnesses :: forall ledgerera.
                            Ledger.Crypto ledgerera ~ StandardCrypto
                         => Ledger.Witnesses ledgerera ~ Alonzo.TxWitness ledgerera
                         => Shelley.ShelleyBased ledgerera
                         => Ledger.Tx ledgerera
                         -> [KeyWitness era]
    getAlonzoTxWitnesses Shelley.Tx {
                           Shelley.wits =
                             Alonzo.TxWitness'
                               addrWits
                               bootWits
                               _txscripts
                               _txdats
                               _txrdmrs
                         } =
        map (ShelleyBootstrapWitness era) (Set.elems bootWits)
     ++ map (ShelleyKeyWitness       era) (Set.elems addrWits)


makeSignedTransaction :: forall era.
                         [KeyWitness era]
                      -> TxBody era
                      -> Tx era
makeSignedTransaction witnesses (ByronTxBody txbody) =
    ByronTx
  . Byron.annotateTxAux
  $ Byron.mkTxAux
      (unAnnotated txbody)
      (Vector.fromList [ w | ByronKeyWitness w <- witnesses ])

makeSignedTransaction witnesses (ShelleyTxBody era txbody txscripts txmetadata) =
    case era of
      ShelleyBasedEraShelley -> makeShelleySignedTransaction txbody
      ShelleyBasedEraAllegra -> makeShelleySignedTransaction txbody
      ShelleyBasedEraMary    -> makeShelleySignedTransaction txbody
      ShelleyBasedEraAlonzo  -> makeAlonzoSignedTransaction  txbody
  where
    makeShelleySignedTransaction
      :: forall ledgerera.
         ShelleyLedgerEra era ~ ledgerera
      => Ledger.Crypto ledgerera ~ StandardCrypto
      => Ledger.Witnesses ledgerera ~ Shelley.WitnessSetHKD Identity ledgerera
      => ToCBOR (Ledger.Witnesses ledgerera)
      => Shelley.ShelleyBased ledgerera
      => Shelley.ValidateScript ledgerera
      => Ledger.TxBody ledgerera
      -> Tx era
    makeShelleySignedTransaction txbody' =
      ShelleyTx era $
        Shelley.Tx
          txbody'
          (Shelley.WitnessSet
            (Set.fromList [ w | ShelleyKeyWitness _ w <- witnesses ])
            (Map.fromList [ (Ledger.hashScript @ledgerera sw, sw)
                          | sw <- txscripts ])
            (Set.fromList [ w | ShelleyBootstrapWitness _ w <- witnesses ]))
          (maybeToStrictMaybe txmetadata)

    makeAlonzoSignedTransaction
      :: forall ledgerera.
         ShelleyLedgerEra era ~ ledgerera
      => Ledger.Crypto ledgerera ~ StandardCrypto
      => Ledger.Witnesses ledgerera ~ Alonzo.TxWitness ledgerera
      => Ledger.Script ledgerera ~ Alonzo.Script ledgerera
      => Shelley.ShelleyBased ledgerera
      => Shelley.ValidateScript ledgerera
      => Ledger.TxBody ledgerera
      -> Tx era
    makeAlonzoSignedTransaction txbody' =
      ShelleyTx era $
        Shelley.Tx
          txbody'
          (Alonzo.TxWitness
            (Set.fromList [ w | ShelleyKeyWitness _ w <- witnesses ])
            (Set.fromList [ w | ShelleyBootstrapWitness _ w <- witnesses ])
            (Map.fromList [ (Ledger.hashScript @ledgerera sw, sw)
                          | sw <- txscripts ])
            (error "TODO alonzo: makeAlonzoSignedTransaction: datums")
            (error "TODO alonzo: makeAlonzoSignedTransaction: redeemers"))
          (maybeToStrictMaybe txmetadata)


makeByronKeyWitness :: forall key.
                       IsByronKey key
                    => NetworkId
                    -> TxBody ByronEra
                    -> SigningKey key
                    -> KeyWitness ByronEra
makeByronKeyWitness _ (ShelleyTxBody era _ _ _) = case era of {}
makeByronKeyWitness nw (ByronTxBody txbody) =
    let txhash :: Byron.Hash Byron.Tx
        txhash = Byron.hashDecoded txbody

        pm :: Byron.ProtocolMagicId
        pm = toByronProtocolMagicId nw

        -- To allow sharing of the txhash computation across many signatures we
        -- define and share the txhash outside the lambda for the signing key:
     in case byronKeyFormat :: ByronKeyFormat key of
          ByronLegacyKeyFormat ->
            \(ByronSigningKeyLegacy sk) -> witness sk pm txhash
          ByronModernKeyFormat ->
            \(ByronSigningKey sk) -> witness sk pm txhash
 where
   witness :: Byron.SigningKey
           -> Byron.ProtocolMagicId
           -> Byron.Hash Byron.Tx
           -> KeyWitness ByronEra
   witness sk pm txHash =
     ByronKeyWitness $
       Byron.VKWitness
         (Byron.toVerification sk)
         (Byron.sign pm Byron.SignTx sk (Byron.TxSigData txHash))

-- | Either a network ID or a Byron address to be used in constructing a
-- Shelley bootstrap witness.
data WitnessNetworkIdOrByronAddress
  = WitnessNetworkId !NetworkId
  -- ^ Network ID.
  --
  -- If this value is used in the construction of a Shelley bootstrap witness,
  -- the result will not consist of a derivation path. If that is required,
  -- specify a 'WitnessByronAddress' value instead.
  | WitnessByronAddress !(Address ByronAddr)
  -- ^ Byron address.
  --
  -- If this value is used in the construction of a Shelley bootstrap witness,
  -- both the network ID and derivation path will be extracted from the
  -- address and used in the construction of the witness.

makeShelleyBootstrapWitness :: forall era.
                               IsShelleyBasedEra era
                            => WitnessNetworkIdOrByronAddress
                            -> TxBody era
                            -> SigningKey ByronKey
                            -> KeyWitness era
makeShelleyBootstrapWitness _ ByronTxBody{} _ =
    case shelleyBasedEra :: ShelleyBasedEra era of {}

makeShelleyBootstrapWitness nwOrAddr (ShelleyTxBody era txbody _ _) sk =
    case era of
      ShelleyBasedEraShelley ->
        makeShelleyBasedBootstrapWitness era nwOrAddr txbody sk
      ShelleyBasedEraAllegra ->
        makeShelleyBasedBootstrapWitness era nwOrAddr txbody sk
      ShelleyBasedEraMary    ->
        makeShelleyBasedBootstrapWitness era nwOrAddr txbody sk
      ShelleyBasedEraAlonzo  ->
        makeShelleyBasedBootstrapWitness era nwOrAddr txbody sk

makeShelleyBasedBootstrapWitness :: forall era.
                                    (Ledger.HashAnnotated
                                       (Ledger.TxBody (ShelleyLedgerEra era))
                                       Ledger.EraIndependentTxBody
                                       StandardCrypto)
                                 => ShelleyBasedEra era
                                 -> WitnessNetworkIdOrByronAddress
                                 -> Ledger.TxBody (ShelleyLedgerEra era)
                                 -> SigningKey ByronKey
                                 -> KeyWitness era
makeShelleyBasedBootstrapWitness era nwOrAddr txbody (ByronSigningKey sk) =
    ShelleyBootstrapWitness era $
      -- Byron era witnesses were weird. This reveals all that weirdness.
      Shelley.BootstrapWitness {
        Shelley.bwKey        = vk,
        Shelley.bwSig        = signature,
        Shelley.bwChainCode  = chainCode,
        Shelley.bwAttributes = attributes
      }
  where
    -- Starting with the easy bits: we /can/ convert the Byron verification key
    -- to a the pair of a Shelley verification key plus the chain code.
    --
    (vk, chainCode) = Shelley.unpackByronVKey (Byron.toVerification sk)

    -- Now the hairy bits.
    --
    -- Byron era signing keys were all /extended/ ed25519 keys. We have to
    -- produce a signature using this extended signing key directly. They
    -- /cannot/ be converted to a plain (non-extended) signing keys. Since we
    -- now support extended signing keys for the Shelley too, we are able to
    -- reuse that here.
    --
    signature :: Shelley.SignedDSIGN StandardCrypto
                  (Shelley.Hash StandardCrypto Ledger.EraIndependentTxBody)
    signature = makeShelleySignature
                  txhash
                  -- Make the signature with the extended key directly:
                  (ShelleyExtendedSigningKey (Byron.unSigningKey sk))

    txhash :: Shelley.Hash StandardCrypto Ledger.EraIndependentTxBody
    txhash = Ledger.extractHash (Ledger.hashAnnotated txbody)
    --TODO: use Shelley.eraIndTxBodyHash txbody once that function has a
    -- suitably general type.

    -- And finally we need to provide the extra suffix bytes necessary to
    -- reconstruct the mini-Merkel tree that is a Byron address. The suffix
    -- bytes are the serialised address attributes.
    attributes =
      CBOR.serialize' $
        Byron.mkAttributes Byron.AddrAttributes {
          Byron.aaVKDerivationPath = derivationPath,
          Byron.aaNetworkMagic     = networkMagic
        }

    -- The 'WitnessNetworkIdOrByronAddress' value converted to an 'Either'.
    eitherNwOrAddr :: Either NetworkId (Address ByronAddr)
    eitherNwOrAddr =
      case nwOrAddr of
        WitnessNetworkId nw -> Left nw
        WitnessByronAddress addr -> Right addr

    unByronAddr :: Address ByronAddr -> Byron.Address
    unByronAddr (ByronAddress addr) = addr

    unAddrAttrs :: Address ByronAddr -> Byron.AddrAttributes
    unAddrAttrs = Byron.attrData . Byron.addrAttributes . unByronAddr

    derivationPath :: Maybe Byron.HDAddressPayload
    derivationPath =
      either
        (const Nothing)
        (Byron.aaVKDerivationPath . unAddrAttrs)
        eitherNwOrAddr

    networkMagic :: Byron.NetworkMagic
    networkMagic =
      either
        toByronNetworkMagic
        (Byron.aaNetworkMagic . unAddrAttrs)
        eitherNwOrAddr


data ShelleyWitnessSigningKey =
       WitnessPaymentKey         (SigningKey PaymentKey)
     | WitnessPaymentExtendedKey (SigningKey PaymentExtendedKey)
     | WitnessStakeKey           (SigningKey StakeKey)
     | WitnessStakeExtendedKey   (SigningKey StakeExtendedKey)
     | WitnessStakePoolKey       (SigningKey StakePoolKey)
     | WitnessGenesisKey         (SigningKey GenesisKey)
     | WitnessGenesisExtendedKey (SigningKey GenesisExtendedKey)
     | WitnessGenesisDelegateKey (SigningKey GenesisDelegateKey)
     | WitnessGenesisDelegateExtendedKey
                                 (SigningKey GenesisDelegateExtendedKey)
     | WitnessGenesisUTxOKey     (SigningKey GenesisUTxOKey)


makeShelleyKeyWitness :: forall era
                      .  IsShelleyBasedEra era
                      => TxBody era
                      -> ShelleyWitnessSigningKey
                      -> KeyWitness era
makeShelleyKeyWitness (ShelleyTxBody era txbody _ _) =
    case era of
      ShelleyBasedEraShelley -> makeShelleyBasedKeyWitness txbody
      ShelleyBasedEraAllegra -> makeShelleyBasedKeyWitness txbody
      ShelleyBasedEraMary    -> makeShelleyBasedKeyWitness txbody
      ShelleyBasedEraAlonzo  -> makeShelleyBasedKeyWitness txbody
  where
    makeShelleyBasedKeyWitness :: Shelley.ShelleyBased ledgerera
                               => ShelleyLedgerEra era ~ ledgerera
                               => Ledger.TxBody ledgerera
                               -> ShelleyWitnessSigningKey
                               -> KeyWitness era
    makeShelleyBasedKeyWitness txbody' =

     let txhash :: Shelley.Hash StandardCrypto Ledger.EraIndependentTxBody
         txhash = Ledger.extractHash (Ledger.hashAnnotated txbody')

        -- To allow sharing of the txhash computation across many signatures we
        -- define and share the txhash outside the lambda for the signing key:
     in \wsk ->
        let sk        = toShelleySigningKey wsk
            vk        = getShelleyKeyWitnessVerificationKey sk
            signature = makeShelleySignature txhash sk
         in ShelleyKeyWitness era $
              Shelley.WitVKey vk signature

makeShelleyKeyWitness ByronTxBody{} =
    case shelleyBasedEra :: ShelleyBasedEra era of {}


-- | We support making key witnesses with both normal and extended signing keys.
--
data ShelleySigningKey =
       -- | A normal ed25519 signing key
       ShelleyNormalSigningKey   (Shelley.SignKeyDSIGN StandardCrypto)

       -- | An extended ed25519 signing key
     | ShelleyExtendedSigningKey Crypto.HD.XPrv


toShelleySigningKey :: ShelleyWitnessSigningKey -> ShelleySigningKey
toShelleySigningKey key = case key of
  WitnessPaymentKey     (PaymentSigningKey     sk) -> ShelleyNormalSigningKey sk
  WitnessStakeKey       (StakeSigningKey       sk) -> ShelleyNormalSigningKey sk
  WitnessStakePoolKey   (StakePoolSigningKey   sk) -> ShelleyNormalSigningKey sk
  WitnessGenesisKey     (GenesisSigningKey     sk) -> ShelleyNormalSigningKey sk
  WitnessGenesisUTxOKey (GenesisUTxOSigningKey sk) -> ShelleyNormalSigningKey sk
  WitnessGenesisDelegateKey (GenesisDelegateSigningKey sk) ->
    ShelleyNormalSigningKey sk

  -- The cases for extended keys
  WitnessPaymentExtendedKey (PaymentExtendedSigningKey sk) ->
    ShelleyExtendedSigningKey sk

  WitnessStakeExtendedKey (StakeExtendedSigningKey sk) ->
    ShelleyExtendedSigningKey sk

  WitnessGenesisExtendedKey (GenesisExtendedSigningKey sk) ->
    ShelleyExtendedSigningKey sk

  WitnessGenesisDelegateExtendedKey (GenesisDelegateExtendedSigningKey sk) ->
    ShelleyExtendedSigningKey sk


getShelleyKeyWitnessVerificationKey
  :: ShelleySigningKey
  -> Shelley.VKey Shelley.Witness StandardCrypto
getShelleyKeyWitnessVerificationKey (ShelleyNormalSigningKey sk) =
      (Shelley.coerceKeyRole :: Shelley.VKey Shelley.Payment StandardCrypto
                             -> Shelley.VKey Shelley.Witness StandardCrypto)
    . (\(PaymentVerificationKey vk) -> vk)
    . getVerificationKey
    . PaymentSigningKey
    $ sk

getShelleyKeyWitnessVerificationKey (ShelleyExtendedSigningKey sk) =
      (Shelley.coerceKeyRole :: Shelley.VKey Shelley.Payment StandardCrypto
                             -> Shelley.VKey Shelley.Witness StandardCrypto)
    . (\(PaymentVerificationKey vk) -> vk)
    . (castVerificationKey :: VerificationKey PaymentExtendedKey
                           -> VerificationKey PaymentKey)
    . getVerificationKey
    . PaymentExtendedSigningKey
    $ sk


makeShelleySignature
  :: Crypto.SignableRepresentation tosign
  => tosign
  -> ShelleySigningKey
  -> Shelley.SignedDSIGN StandardCrypto tosign
makeShelleySignature tosign (ShelleyNormalSigningKey sk) =
    Crypto.signedDSIGN () tosign sk

makeShelleySignature tosign (ShelleyExtendedSigningKey sk) =
    fromXSignature $
      Crypto.HD.sign
        BS.empty  -- passphrase for (unused) in-memory encryption
        sk
        (Crypto.getSignableRepresentation tosign)
  where
    fromXSignature :: Crypto.HD.XSignature
                   -> Shelley.SignedDSIGN StandardCrypto b
    fromXSignature =
        Crypto.SignedDSIGN
      . fromMaybe impossible
      . Crypto.rawDeserialiseSigDSIGN
      . Crypto.HD.unXSignature

    impossible =
      error "makeShelleyKeyWitnessSignature: byron and shelley signature sizes do not match"


-- order of signing keys must match txins
signByronTransaction :: NetworkId
                     -> TxBody ByronEra
                     -> [SigningKey ByronKey]
                     -> Tx ByronEra
signByronTransaction nw txbody sks =
    makeSignedTransaction witnesses txbody
  where
    witnesses = map (makeByronKeyWitness nw txbody) sks

-- signing keys is a set
signShelleyTransaction :: IsShelleyBasedEra era
                       => TxBody era
                       -> [ShelleyWitnessSigningKey]
                       -> Tx era
signShelleyTransaction txbody sks =
    makeSignedTransaction witnesses txbody
  where
    witnesses = map (makeShelleyKeyWitness txbody) sks
