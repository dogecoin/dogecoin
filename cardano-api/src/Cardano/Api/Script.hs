{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.Script (
    -- * Languages
    SimpleScriptV1,
    SimpleScriptV2,
    ScriptLanguage(..),
    SimpleScriptVersion(..),
    PlutusScriptVersion(..),
    AnyScriptLanguage(..),
    AnyPlutusScriptVersion(..),
    IsScriptLanguage(..),
    IsSimpleScriptLanguage(..),

    -- * Scripts in a specific language
    Script(..),

    -- * Scripts in any language
    ScriptInAnyLang(..),
    toScriptInAnyLang,

    -- * Scripts in an era
    ScriptInEra(..),
    toScriptInEra,
    eraOfScriptInEra,

    -- * Use of a script in an era as a witness
    WitCtxTxIn, WitCtxMint, WitCtxStake,
    ScriptWitness(..),
    Witness(..),
    KeyWitnessInCtx(..),
    ScriptWitnessInCtx(..),
    ScriptDatum(..),
    ScriptRedeemer,

    -- ** Languages supported in each era
    ScriptLanguageInEra(..),
    scriptLanguageSupportedInEra,
    languageOfScriptLanguageInEra,
    eraOfScriptLanguageInEra,

    -- * The simple script language
    SimpleScript(..),
    TimeLocksSupported(..),
    timeLocksSupported,
    adjustSimpleScriptVersion,

    -- * The Plutus script language
    PlutusScript(..),

    -- * Script data
    ScriptData(..),

    -- * Script execution units
    ExecutionUnits(..),

    -- * Script hashes
    ScriptHash(..),
    hashScript,

    -- * Internal conversion functions
    toShelleyScript,
    fromShelleyBasedScript,
    toShelleyMultiSig,
    fromShelleyMultiSig,
    toAllegraTimelock,
    fromAllegraTimelock,
    toAlonzoExUnits,
    fromAlonzoExUnits,
    toShelleyScriptHash,
    fromShelleyScriptHash,
    toAlonzoScriptData,
    fromAlonzoScriptData,

    -- * Data family instances
    AsType(..),
    Hash(..),
  ) where

import           Prelude

import           Data.Word (Word64)
import qualified Data.ByteString.Lazy as LBS
import           Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as SBS
import           Data.Foldable (toList)
import           Data.Scientific (toBoundedInteger)
import           Data.String (IsString)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Type.Equality (TestEquality (..), (:~:) (Refl))
import           Data.Typeable (Typeable)

import           Data.Aeson (Value (..), object, (.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Aeson.Encoding as Aeson
import qualified Data.Sequence.Strict as Seq
import           Data.Vector (Vector)
import qualified Data.Vector as Vector

import           Control.Applicative
import           Control.Monad

import qualified Cardano.Binary as CBOR

import qualified Cardano.Crypto.Hash.Class as Crypto

import           Cardano.Slotting.Slot (SlotNo)

import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Era  as Ledger
import qualified Cardano.Ledger.SafeHash as Ledger

import qualified Cardano.Ledger.ShelleyMA.Timelocks as Timelock
import           Ouroboros.Consensus.Shelley.Eras (StandardCrypto)
import qualified Shelley.Spec.Ledger.Keys as Shelley
import qualified Shelley.Spec.Ledger.Scripts as Shelley

import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Alonzo.Data as Alonzo

import           Cardano.Api.Eras
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.Hash
import           Cardano.Api.KeysShelley
import           Cardano.Api.SerialiseCBOR
import           Cardano.Api.SerialiseJSON
import           Cardano.Api.SerialiseRaw
import           Cardano.Api.SerialiseTextEnvelope

{- HLINT ignore "Use section" -}


-- ----------------------------------------------------------------------------
-- Types for script language and version
--

-- | The original simple script language which supports
--
-- * require a signature from a given key (by verification key hash)
-- * n-way and combinator
-- * n-way or combinator
-- * m-of-n combinator
--
-- This version of the language was introduced in the 'ShelleyEra'.
--
data SimpleScriptV1

-- | The second version of the simple script language. It has all the features
-- of 'SimpleScriptV1' plus new atomic predicates:
--
-- * require the time be before a given slot number
-- * require the time be after a given slot number
--
-- This version of the language was introduced in the 'AllegraEra'.
--
data SimpleScriptV2

-- | Place holder type to show what the pattern is to extend to multiple
-- languages, not just multiple versions of a single language.
--
data PlutusScriptV1

instance HasTypeProxy SimpleScriptV1 where
    data AsType SimpleScriptV1 = AsSimpleScriptV1
    proxyToAsType _ = AsSimpleScriptV1

instance HasTypeProxy SimpleScriptV2 where
    data AsType SimpleScriptV2 = AsSimpleScriptV2
    proxyToAsType _ = AsSimpleScriptV2

instance HasTypeProxy PlutusScriptV1 where
    data AsType PlutusScriptV1 = AsPlutusScriptV1
    proxyToAsType _ = AsPlutusScriptV1


-- ----------------------------------------------------------------------------
-- Value level representation for script languages
--
data ScriptLanguage lang where

     SimpleScriptLanguage :: SimpleScriptVersion lang -> ScriptLanguage lang

     PlutusScriptLanguage :: PlutusScriptVersion lang -> ScriptLanguage lang

deriving instance (Eq   (ScriptLanguage lang))
deriving instance (Show (ScriptLanguage lang))

instance TestEquality ScriptLanguage where
    testEquality (SimpleScriptLanguage lang)
                 (SimpleScriptLanguage lang') = testEquality lang lang'

    testEquality (PlutusScriptLanguage lang)
                 (PlutusScriptLanguage lang') = testEquality lang lang'

    testEquality  _ _ = Nothing


data SimpleScriptVersion lang where

     SimpleScriptV1 :: SimpleScriptVersion SimpleScriptV1
     SimpleScriptV2 :: SimpleScriptVersion SimpleScriptV2

deriving instance (Eq   (SimpleScriptVersion lang))
deriving instance (Show (SimpleScriptVersion lang))

instance TestEquality SimpleScriptVersion where
    testEquality SimpleScriptV1 SimpleScriptV1 = Just Refl
    testEquality SimpleScriptV2 SimpleScriptV2 = Just Refl
    testEquality _              _              = Nothing


data PlutusScriptVersion lang where
    PlutusScriptV1 :: PlutusScriptVersion PlutusScriptV1

deriving instance (Eq   (PlutusScriptVersion lang))
deriving instance (Show (PlutusScriptVersion lang))

instance TestEquality PlutusScriptVersion where
    testEquality PlutusScriptV1 PlutusScriptV1 = Just Refl


data AnyScriptLanguage where
     AnyScriptLanguage :: ScriptLanguage lang -> AnyScriptLanguage

deriving instance (Show AnyScriptLanguage)

instance Eq AnyScriptLanguage where
    a == b = fromEnum a == fromEnum b

instance Ord AnyScriptLanguage where
    compare a b = compare (fromEnum a) (fromEnum b)

instance Enum AnyScriptLanguage where
    toEnum 0 = AnyScriptLanguage (SimpleScriptLanguage SimpleScriptV1)
    toEnum 1 = AnyScriptLanguage (SimpleScriptLanguage SimpleScriptV2)
    toEnum 2 = AnyScriptLanguage (PlutusScriptLanguage PlutusScriptV1)
    toEnum _ = error "AnyScriptLanguage.toEnum: bad argument"

    fromEnum (AnyScriptLanguage (SimpleScriptLanguage SimpleScriptV1)) = 0
    fromEnum (AnyScriptLanguage (SimpleScriptLanguage SimpleScriptV2)) = 1
    fromEnum (AnyScriptLanguage (PlutusScriptLanguage PlutusScriptV1)) = 2

instance Bounded AnyScriptLanguage where
    minBound = AnyScriptLanguage (SimpleScriptLanguage SimpleScriptV1)
    maxBound = AnyScriptLanguage (PlutusScriptLanguage PlutusScriptV1)


data AnyPlutusScriptVersion where
     AnyPlutusScriptVersion :: PlutusScriptVersion lang
                            -> AnyPlutusScriptVersion

deriving instance (Show AnyPlutusScriptVersion)

instance Eq AnyPlutusScriptVersion where
    a == b = fromEnum a == fromEnum b

instance Ord AnyPlutusScriptVersion where
    compare a b = compare (fromEnum a) (fromEnum b)

instance Enum AnyPlutusScriptVersion where
    toEnum 0 = AnyPlutusScriptVersion PlutusScriptV1
    toEnum _ = error "AnyPlutusScriptVersion.toEnum: bad argument"

    fromEnum (AnyPlutusScriptVersion PlutusScriptV1) = 0

instance Bounded AnyPlutusScriptVersion where
    minBound = AnyPlutusScriptVersion PlutusScriptV1
    maxBound = AnyPlutusScriptVersion PlutusScriptV1

instance ToJSON AnyPlutusScriptVersion where
    toJSON (AnyPlutusScriptVersion PlutusScriptV1) =
      Aeson.String "PlutusScriptV1"

parsePlutusScriptVersion :: Text -> Aeson.Parser AnyPlutusScriptVersion
parsePlutusScriptVersion t =
  case t of
    "PlutusScriptV1" -> return (AnyPlutusScriptVersion PlutusScriptV1)
    _                -> fail "Expected PlutusScriptV1"

instance FromJSON AnyPlutusScriptVersion where
    parseJSON = Aeson.withText "PlutusScriptVersion" parsePlutusScriptVersion

instance Aeson.FromJSONKey AnyPlutusScriptVersion where
    fromJSONKey = Aeson.FromJSONKeyTextParser parsePlutusScriptVersion

instance Aeson.ToJSONKey AnyPlutusScriptVersion where
    toJSONKey = Aeson.ToJSONKeyText toText toAesonEncoding
      where
        toText :: AnyPlutusScriptVersion -> Text
        toText (AnyPlutusScriptVersion PlutusScriptV1) = "PlutusScriptV1"
        toAesonEncoding = Aeson.text . toText

class HasTypeProxy lang => IsScriptLanguage lang where
    scriptLanguage :: ScriptLanguage lang

instance IsScriptLanguage SimpleScriptV1 where
    scriptLanguage = SimpleScriptLanguage SimpleScriptV1

instance IsScriptLanguage SimpleScriptV2 where
    scriptLanguage = SimpleScriptLanguage SimpleScriptV2

instance IsScriptLanguage PlutusScriptV1 where
    scriptLanguage = PlutusScriptLanguage PlutusScriptV1


class IsScriptLanguage lang => IsSimpleScriptLanguage lang where
    simpleScriptVersion :: SimpleScriptVersion lang

instance IsSimpleScriptLanguage SimpleScriptV1 where
    simpleScriptVersion = SimpleScriptV1

instance IsSimpleScriptLanguage SimpleScriptV2 where
    simpleScriptVersion = SimpleScriptV2


class IsScriptLanguage lang => IsPlutusScriptLanguage lang where
    plutusScriptVersion :: PlutusScriptVersion lang

instance IsPlutusScriptLanguage PlutusScriptV1 where
    plutusScriptVersion = PlutusScriptV1


-- ----------------------------------------------------------------------------
-- Script type: covering all script languages
--

-- | A script in a particular language.
--
-- See also 'ScriptInAnyLang' for a script in any of the known languages.
--
-- See also 'ScriptInEra' for a script in a language that is available within
-- a particular era.
--
-- Note that some but not all scripts have an external JSON syntax, hence this
-- type has no JSON serialisation instances. The 'SimpleScript' family of
-- languages do have a JSON syntax and thus have 'ToJSON'\/'FromJSON' instances.
--
data Script lang where

     SimpleScript :: !(SimpleScriptVersion lang)
                  -> !(SimpleScript lang)
                  -> Script lang

     PlutusScript :: !(PlutusScriptVersion lang)
                  -> !(PlutusScript lang)
                  -> Script lang

deriving instance (Eq   (Script lang))
deriving instance (Show (Script lang))

instance HasTypeProxy lang => HasTypeProxy (Script lang) where
    data AsType (Script lang) = AsScript (AsType lang)
    proxyToAsType _ = AsScript (proxyToAsType (Proxy :: Proxy lang))

instance IsScriptLanguage lang => SerialiseAsCBOR (Script lang) where
    serialiseToCBOR (SimpleScript SimpleScriptV1 s) =
      CBOR.serialize' (toShelleyMultiSig s)

    serialiseToCBOR (SimpleScript SimpleScriptV2 s) =
      CBOR.serialize' (toAllegraTimelock s :: Timelock.Timelock StandardCrypto)

    serialiseToCBOR (PlutusScript PlutusScriptV1 s) =
      CBOR.serialize' s

    deserialiseFromCBOR _ bs =
      case scriptLanguage :: ScriptLanguage lang of
        SimpleScriptLanguage SimpleScriptV1 ->
              SimpleScript SimpleScriptV1
            . fromShelleyMultiSig
          <$> CBOR.decodeAnnotator "Script" fromCBOR (LBS.fromStrict bs)

        SimpleScriptLanguage SimpleScriptV2 ->
              SimpleScript SimpleScriptV2
            . (fromAllegraTimelock TimeLocksInSimpleScriptV2
                                :: Timelock.Timelock StandardCrypto
                                -> SimpleScript SimpleScriptV2)
          <$> CBOR.decodeAnnotator "Script" fromCBOR (LBS.fromStrict bs)

        PlutusScriptLanguage PlutusScriptV1 ->
              PlutusScript PlutusScriptV1
          <$> CBOR.decodeFull' bs


instance IsScriptLanguage lang => HasTextEnvelope (Script lang) where
    textEnvelopeType _ =
      case scriptLanguage :: ScriptLanguage lang of
        SimpleScriptLanguage SimpleScriptV1 -> "SimpleScriptV1"
        SimpleScriptLanguage SimpleScriptV2 -> "SimpleScriptV2"
        PlutusScriptLanguage PlutusScriptV1 -> "PlutusScriptV1"


-- ----------------------------------------------------------------------------
-- Scripts in any language
--

-- | Sometimes it is necessary to handle all languages without making static
-- type distinctions between languages. For example, when reading external
-- input, or before the era context is known.
--
-- Use 'toScriptInEra' to convert to a script in the context of an era.
--
data ScriptInAnyLang where
     ScriptInAnyLang :: ScriptLanguage lang
                     -> Script lang
                     -> ScriptInAnyLang

deriving instance Show ScriptInAnyLang

-- The GADT in the ScriptInAnyLang constructor requires a custom Eq instance
instance Eq ScriptInAnyLang where
    (==) (ScriptInAnyLang lang  script)
         (ScriptInAnyLang lang' script') =
      case testEquality lang lang' of
        Nothing   -> False
        Just Refl -> script == script'


-- | Convert a script in a specific statically-known language to a
-- 'ScriptInAnyLang'.
--
-- No inverse to this is provided, just do case analysis on the 'ScriptLanguage'
-- field within the 'ScriptInAnyLang' constructor.
--
toScriptInAnyLang :: Script lang -> ScriptInAnyLang
toScriptInAnyLang s@(SimpleScript v _) =
    ScriptInAnyLang (SimpleScriptLanguage v) s
toScriptInAnyLang s@(PlutusScript v _) =
    ScriptInAnyLang (PlutusScriptLanguage v) s

instance HasTypeProxy ScriptInAnyLang where
    data AsType ScriptInAnyLang = AsScriptInAnyLang
    proxyToAsType _ = AsScriptInAnyLang


-- ----------------------------------------------------------------------------
-- Scripts in the context of a ledger era
--

data ScriptInEra era where
     ScriptInEra :: ScriptLanguageInEra lang era
                 -> Script lang
                 -> ScriptInEra era

deriving instance Show (ScriptInEra era)

-- The GADT in the ScriptInEra constructor requires a custom instance
instance Eq (ScriptInEra era) where
    (==) (ScriptInEra langInEra  script)
         (ScriptInEra langInEra' script') =
      case testEquality (languageOfScriptLanguageInEra langInEra)
                        (languageOfScriptLanguageInEra langInEra') of
        Nothing   -> False
        Just Refl -> script == script'


data ScriptLanguageInEra lang era where

     SimpleScriptV1InShelley :: ScriptLanguageInEra SimpleScriptV1 ShelleyEra
     SimpleScriptV1InAllegra :: ScriptLanguageInEra SimpleScriptV1 AllegraEra
     SimpleScriptV1InMary    :: ScriptLanguageInEra SimpleScriptV1 MaryEra
     SimpleScriptV1InAlonzo  :: ScriptLanguageInEra SimpleScriptV1 AlonzoEra

     SimpleScriptV2InAllegra :: ScriptLanguageInEra SimpleScriptV2 AllegraEra
     SimpleScriptV2InMary    :: ScriptLanguageInEra SimpleScriptV2 MaryEra
     SimpleScriptV2InAlonzo  :: ScriptLanguageInEra SimpleScriptV2 AlonzoEra

     PlutusScriptV1InAlonzo  :: ScriptLanguageInEra PlutusScriptV1 AlonzoEra

deriving instance Eq   (ScriptLanguageInEra lang era)
deriving instance Show (ScriptLanguageInEra lang era)

instance HasTypeProxy era => HasTypeProxy (ScriptInEra era) where
    data AsType (ScriptInEra era) = AsScriptInEra (AsType era)
    proxyToAsType _ = AsScriptInEra (proxyToAsType (Proxy :: Proxy era))


-- | Check if a given script language is supported in a given era, and if so
-- return the evidence.
--
scriptLanguageSupportedInEra :: CardanoEra era
                             -> ScriptLanguage lang
                             -> Maybe (ScriptLanguageInEra lang era)
scriptLanguageSupportedInEra era lang =
    case (era, lang) of
      (ShelleyEra, SimpleScriptLanguage SimpleScriptV1) ->
        Just SimpleScriptV1InShelley

      (AllegraEra, SimpleScriptLanguage SimpleScriptV1) ->
        Just SimpleScriptV1InAllegra

      (MaryEra, SimpleScriptLanguage SimpleScriptV1) ->
        Just SimpleScriptV1InMary

      (AllegraEra, SimpleScriptLanguage SimpleScriptV2) ->
        Just SimpleScriptV2InAllegra

      (MaryEra, SimpleScriptLanguage SimpleScriptV2) ->
        Just SimpleScriptV2InMary

      (AlonzoEra, SimpleScriptLanguage SimpleScriptV2) ->
        Just SimpleScriptV2InAlonzo

      (AlonzoEra, PlutusScriptLanguage PlutusScriptV1) ->
        Just PlutusScriptV1InAlonzo

      _ -> Nothing

languageOfScriptLanguageInEra :: ScriptLanguageInEra lang era
                              -> ScriptLanguage lang
languageOfScriptLanguageInEra langInEra =
    case langInEra of
      SimpleScriptV1InShelley -> SimpleScriptLanguage SimpleScriptV1
      SimpleScriptV1InAllegra -> SimpleScriptLanguage SimpleScriptV1
      SimpleScriptV1InMary    -> SimpleScriptLanguage SimpleScriptV1
      SimpleScriptV1InAlonzo  -> SimpleScriptLanguage SimpleScriptV1

      SimpleScriptV2InAllegra -> SimpleScriptLanguage SimpleScriptV2
      SimpleScriptV2InMary    -> SimpleScriptLanguage SimpleScriptV2
      SimpleScriptV2InAlonzo  -> SimpleScriptLanguage SimpleScriptV2

      PlutusScriptV1InAlonzo  -> PlutusScriptLanguage PlutusScriptV1

eraOfScriptLanguageInEra :: ScriptLanguageInEra lang era
                         -> ShelleyBasedEra era
eraOfScriptLanguageInEra langInEra =
    case langInEra of
      SimpleScriptV1InShelley -> ShelleyBasedEraShelley

      SimpleScriptV1InAllegra -> ShelleyBasedEraAllegra
      SimpleScriptV2InAllegra -> ShelleyBasedEraAllegra

      SimpleScriptV1InMary    -> ShelleyBasedEraMary
      SimpleScriptV2InMary    -> ShelleyBasedEraMary

      SimpleScriptV1InAlonzo  -> ShelleyBasedEraAlonzo
      SimpleScriptV2InAlonzo  -> ShelleyBasedEraAlonzo

      PlutusScriptV1InAlonzo  -> ShelleyBasedEraAlonzo


-- | Given a target era and a script in some language, check if the language is
-- supported in that era, and if so return a 'ScriptInEra'.
--
toScriptInEra :: CardanoEra era -> ScriptInAnyLang -> Maybe (ScriptInEra era)
toScriptInEra era (ScriptInAnyLang lang s) = do
    lang' <- scriptLanguageSupportedInEra era lang
    return (ScriptInEra lang' s)

eraOfScriptInEra :: ScriptInEra era -> ShelleyBasedEra era
eraOfScriptInEra (ScriptInEra langInEra _) = eraOfScriptLanguageInEra langInEra


-- ----------------------------------------------------------------------------
-- Scripts used in a transaction (in an era) to witness authorised use
--

-- | A tag type for the context in which a script is used in a transaction.
--
-- This type tags the context as being to witness a transaction input.
--
data WitCtxTxIn

-- | A tag type for the context in which a script is used in a transaction.
--
-- This type tags the context as being to witness minting.
--
data WitCtxMint

-- | A tag type for the context in which a script is used in a transaction.
--
-- This type tags the context as being to witness the use of stake addresses in
-- both certificates and withdrawals.
--
data WitCtxStake

-- | A /use/ of a script within a transaction body to witness that something is
-- being used in an authorised manner. That can be
--
-- * spending a transaction input
-- * minting tokens
-- * using a certificate (stake address certs specifically)
-- * withdrawing from a reward account
--
-- For simple script languages, the use of the script is the same in all
-- contexts. For Plutus scripts, using a script involves supplying a redeemer.
-- In addition, Plutus scripts used for spending inputs must also supply the
-- datum value used when originally creating the TxOut that is now being spent.
--
data ScriptWitness witctx era where

     SimpleScriptWitness :: ScriptLanguageInEra lang era
                         -> SimpleScriptVersion lang
                         -> SimpleScript        lang
                         -> ScriptWitness witctx era

     PlutusScriptWitness :: ScriptLanguageInEra  lang era
                         -> PlutusScriptVersion  lang
                         -> PlutusScript         lang
                         -> ScriptDatum witctx
                         -> ScriptRedeemer
                         -> ExecutionUnits
                         -> ScriptWitness witctx era

deriving instance Show (ScriptWitness witctx era)

-- The GADT in the SimpleScriptWitness constructor requires a custom instance
instance Eq (ScriptWitness witctx era) where
    (==) (SimpleScriptWitness langInEra  version  script)
         (SimpleScriptWitness langInEra' version' script') =
      case testEquality (languageOfScriptLanguageInEra langInEra)
                        (languageOfScriptLanguageInEra langInEra') of
        Nothing   -> False
        Just Refl -> version == version' && script == script'

    (==) (PlutusScriptWitness langInEra  version   script
                              datum      redeemer  execUnits)
         (PlutusScriptWitness langInEra' version'  script'
                              datum'     redeemer' execUnits') =
      case testEquality (languageOfScriptLanguageInEra langInEra)
                        (languageOfScriptLanguageInEra langInEra') of
        Nothing   -> False
        Just Refl ->    version   == version'
                     && script    == script'
                     && datum     == datum'
                     && redeemer  == redeemer'
                     && execUnits == execUnits'

    (==)  _ _ = False

data ScriptDatum witctx where
     ScriptDatumForTxIn    :: ScriptData -> ScriptDatum WitCtxTxIn
     NoScriptDatumForMint  ::               ScriptDatum WitCtxMint
     NoScriptDatumForStake ::               ScriptDatum WitCtxStake

deriving instance Eq   (ScriptDatum witctx)
deriving instance Show (ScriptDatum witctx)


-- ----------------------------------------------------------------------------
-- The kind of witness to use, key (signature) or script
--

data Witness witctx era where

     KeyWitness    :: KeyWitnessInCtx witctx
                   -> Witness         witctx era

     ScriptWitness :: ScriptWitnessInCtx witctx
                   -> ScriptWitness      witctx era
                   -> Witness            witctx era

deriving instance Eq   (Witness witctx era)
deriving instance Show (Witness witctx era)

data KeyWitnessInCtx witctx where

     KeyWitnessForSpending  :: KeyWitnessInCtx WitCtxTxIn
     KeyWitnessForStakeAddr :: KeyWitnessInCtx WitCtxStake

data ScriptWitnessInCtx witctx where

     ScriptWitnessForSpending  :: ScriptWitnessInCtx WitCtxTxIn
     ScriptWitnessForMinting   :: ScriptWitnessInCtx WitCtxMint
     ScriptWitnessForStakeAddr :: ScriptWitnessInCtx WitCtxStake

deriving instance Eq   (KeyWitnessInCtx witctx)
deriving instance Show (KeyWitnessInCtx witctx)

deriving instance Eq   (ScriptWitnessInCtx witctx)
deriving instance Show (ScriptWitnessInCtx witctx)


-- ----------------------------------------------------------------------------
-- Script data
--

type ScriptRedeemer = ScriptData

-- TODO alonzo: Placeholder type to re-present the Alonzo.Data type
data ScriptData = ScriptData
  deriving (Eq, Show)

instance HasTypeProxy ScriptData where
    data AsType ScriptData = AsScriptData
    proxyToAsType _ = AsScriptData

toAlonzoScriptData :: ScriptData -> Alonzo.Data ledgerera
toAlonzoScriptData = error "TODO alonzo: toShelleyScriptData"

fromAlonzoScriptData :: Alonzo.Data ledgerera -> ScriptData
fromAlonzoScriptData = error "TODO alonzo: fromShelleyScriptData"


newtype instance Hash ScriptData =
    ScriptDataHash (Alonzo.DataHash StandardCrypto)
  deriving stock (Eq, Ord)
  deriving (Show, IsString) via UsingRawBytesHex (Hash ScriptData)

instance SerialiseAsRawBytes (Hash ScriptData) where
    serialiseToRawBytes (ScriptDataHash dh) =
      Crypto.hashToBytes (Ledger.extractHash dh)

    deserialiseFromRawBytes (AsHash AsScriptData) bs =
      ScriptDataHash . Ledger.unsafeMakeSafeHash <$> Crypto.hashFromBytes bs

instance ToJSON (Hash ScriptData) where
    toJSON = toJSON . serialiseToRawBytesHexText

instance Aeson.ToJSONKey (Hash ScriptData) where
    toJSONKey = Aeson.toJSONKeyText serialiseToRawBytesHexText


-- ----------------------------------------------------------------------------
-- Script execution units
--

-- | The units for how long a script executes for and how much memory it uses.
-- This is used to declare the resources used by a particular use of a script.
--
-- This type is also used to describe the limits for the maximum overall
-- execution units per transaction or per block.
--
data ExecutionUnits =
     ExecutionUnits {
        -- | This corresponds roughly to the time to execute a script.
        executionSteps  :: Word64,

        -- | This corresponds roughly to the peak memory used during script
        -- execution.
        executionMemory :: Word64
     }
  deriving (Eq, Show)

instance ToJSON ExecutionUnits where
  toJSON ExecutionUnits{executionSteps, executionMemory} =
    object [ "steps"  .= executionSteps
           , "memory" .= executionMemory ]

instance FromJSON ExecutionUnits where
  parseJSON =
    Aeson.withObject "ExecutionUnits" $ \o ->
      ExecutionUnits
        <$> o .: "steps"
        <*> o .: "memory"

toAlonzoExUnits :: ExecutionUnits -> Alonzo.ExUnits
toAlonzoExUnits ExecutionUnits{executionSteps, executionMemory} =
  Alonzo.ExUnits {
    Alonzo.exUnitsSteps = executionSteps,
    Alonzo.exUnitsMem   = executionMemory
  }

fromAlonzoExUnits :: Alonzo.ExUnits -> ExecutionUnits
fromAlonzoExUnits Alonzo.ExUnits{Alonzo.exUnitsSteps, Alonzo.exUnitsMem} =
  ExecutionUnits {
    executionSteps  = exUnitsSteps,
    executionMemory = exUnitsMem
  }


-- ----------------------------------------------------------------------------
-- Script Hash
--

-- | We have this type separate from the 'Hash' type to avoid the script
-- hash type being parametrised by the era. The representation is era
-- independent, and there are many places where we want to use a script
-- hash where we don't want things to be era-parametrised.
--
newtype ScriptHash = ScriptHash (Shelley.ScriptHash StandardCrypto)
  deriving stock (Eq, Ord)
  deriving (Show, IsString) via UsingRawBytesHex ScriptHash

instance HasTypeProxy ScriptHash where
    data AsType ScriptHash = AsScriptHash
    proxyToAsType _ = AsScriptHash

instance SerialiseAsRawBytes ScriptHash where
    serialiseToRawBytes (ScriptHash (Shelley.ScriptHash h)) =
      Crypto.hashToBytes h

    deserialiseFromRawBytes AsScriptHash bs =
      ScriptHash . Shelley.ScriptHash <$> Crypto.hashFromBytes bs


hashScript :: Script lang -> ScriptHash
hashScript (SimpleScript SimpleScriptV1 s) =
    -- For V1, we convert to the Shelley-era version specifically and hash that.
    -- Later ledger eras have to be compatible anyway.
    ScriptHash
  . Ledger.hashScript @(ShelleyLedgerEra ShelleyEra)
  . toShelleyMultiSig
  $ s

hashScript (SimpleScript SimpleScriptV2 s) =
    -- For V2, we convert to the Allegra-era version specifically and hash that.
    -- Later ledger eras have to be compatible anyway.
    ScriptHash
  . Ledger.hashScript @(ShelleyLedgerEra AllegraEra)
  . (toAllegraTimelock :: SimpleScript SimpleScriptV2
                       -> Timelock.Timelock StandardCrypto)
  $ s

hashScript (PlutusScript PlutusScriptV1 (PlutusScriptSerialised script)) =
    -- For Plutus V1, we convert to the Alonzo-era version specifically and
    -- hash that. Later ledger eras have to be compatible anyway.
    ScriptHash
  . Ledger.hashScript @(ShelleyLedgerEra AlonzoEra)
  $ Alonzo.PlutusScript script

toShelleyScriptHash :: ScriptHash -> Shelley.ScriptHash StandardCrypto
toShelleyScriptHash (ScriptHash h) =  h

fromShelleyScriptHash :: Shelley.ScriptHash StandardCrypto -> ScriptHash
fromShelleyScriptHash = ScriptHash


-- ----------------------------------------------------------------------------
-- The simple native script language
--

data SimpleScript lang where

     RequireSignature  :: !(Hash PaymentKey)
                       -> SimpleScript lang

     RequireTimeBefore :: !(TimeLocksSupported lang)
                       -> !SlotNo
                       -> SimpleScript lang

     RequireTimeAfter  :: !(TimeLocksSupported lang)
                       -> !SlotNo
                       -> SimpleScript lang

     RequireAllOf      ::        [SimpleScript lang] -> SimpleScript lang
     RequireAnyOf      ::        [SimpleScript lang] -> SimpleScript lang
     RequireMOf        :: Int -> [SimpleScript lang] -> SimpleScript lang

deriving instance Eq   (SimpleScript lang)
deriving instance Show (SimpleScript lang)

instance HasTypeProxy lang => HasTypeProxy (SimpleScript lang) where
    data AsType (SimpleScript lang) = AsSimpleScript (AsType lang)
    proxyToAsType _ = AsSimpleScript (proxyToAsType (Proxy :: Proxy lang))


-- | Time lock feature in the 'SimpleScript' language.
--
-- The constructors of this type serve as evidence that the timelocks feature
-- is supported in particular versions of the language.
--
data TimeLocksSupported lang where
     TimeLocksInSimpleScriptV2 :: TimeLocksSupported SimpleScriptV2

deriving instance Eq   (TimeLocksSupported lang)
deriving instance Show (TimeLocksSupported lang)

timeLocksSupported :: SimpleScriptVersion lang
                   -> Maybe (TimeLocksSupported lang)
timeLocksSupported SimpleScriptV1 = Nothing
timeLocksSupported SimpleScriptV2 = Just TimeLocksInSimpleScriptV2


-- | Try converting the 'SimpleScript' into a different version of the language.
--
-- This will work when the script only uses the features of the target language
-- version. For example converting from 'SimpleScriptV2' to 'SimpleScriptV1'
-- will work if the script happens not to use time locks feature. On the other
-- hand converting 'SimpleScriptV1' to 'SimpleScriptV2' will always work because
-- it is backwards compatible.
--
adjustSimpleScriptVersion :: SimpleScriptVersion lang'
                          -> SimpleScript lang
                          -> Maybe (SimpleScript lang')
adjustSimpleScriptVersion target = go
  where
    go (RequireSignature sig) = pure (RequireSignature sig)

    go (RequireTimeBefore _ slot) = do
      supported <- timeLocksSupported target
      pure (RequireTimeBefore supported slot)

    go (RequireTimeAfter _ slot) = do
      supported <- timeLocksSupported target
      pure (RequireTimeAfter supported slot)

    go (RequireAllOf ss) = RequireAllOf <$> traverse go ss
    go (RequireAnyOf ss) = RequireAnyOf <$> traverse go ss
    go (RequireMOf m ss) = RequireMOf m <$> traverse go ss


-- ----------------------------------------------------------------------------
-- The Plutus script language
--

-- | Plutus scripts.
--
-- Note that Plutus scripts have a binary serialisation but no JSON
-- serialisation.
--
data PlutusScript lang where
     PlutusScriptSerialised :: ShortByteString -> PlutusScript lang

deriving instance Eq (PlutusScript lang)
deriving instance Show (PlutusScript lang)

instance HasTypeProxy lang => HasTypeProxy (PlutusScript lang) where
    data AsType (PlutusScript lang) = AsPlutusScript (AsType lang)
    proxyToAsType _ = AsPlutusScript (proxyToAsType (Proxy :: Proxy lang))

instance HasTypeProxy lang => SerialiseAsRawBytes (PlutusScript lang) where
    serialiseToRawBytes (PlutusScriptSerialised sbs) = SBS.fromShort sbs

    deserialiseFromRawBytes (AsPlutusScript _) bs =
      -- TODO alonzo: validate the script syntax and fail decoding if invalid
      Just (PlutusScriptSerialised (SBS.toShort bs))

instance Typeable lang => ToCBOR (PlutusScript lang) where
    toCBOR (PlutusScriptSerialised sbs) = toCBOR sbs

instance Typeable lang => FromCBOR (PlutusScript lang) where
    -- TODO alonzo: validate the script syntax and fail decoding if invalid
    fromCBOR = PlutusScriptSerialised <$> fromCBOR

instance (HasTypeProxy lang, Typeable lang) =>
         SerialiseAsCBOR (PlutusScript lang)

instance (IsPlutusScriptLanguage lang, Typeable lang) =>
         HasTextEnvelope (PlutusScript lang) where
    textEnvelopeType _ =
      case plutusScriptVersion :: PlutusScriptVersion lang of
        PlutusScriptV1 -> "PlutusScriptV1"


-- ----------------------------------------------------------------------------
-- Conversion functions
--

toShelleyScript :: ScriptInEra era -> Ledger.Script (ShelleyLedgerEra era)
toShelleyScript (ScriptInEra langInEra (SimpleScript SimpleScriptV1 script)) =
    case langInEra of
      SimpleScriptV1InShelley -> toShelleyMultiSig script
      SimpleScriptV1InAllegra -> toAllegraTimelock script
      SimpleScriptV1InMary    -> toAllegraTimelock script
      SimpleScriptV1InAlonzo  -> Alonzo.TimelockScript (toAllegraTimelock script)

toShelleyScript (ScriptInEra langInEra (SimpleScript SimpleScriptV2 script)) =
    case langInEra of
      SimpleScriptV2InAllegra -> toAllegraTimelock script
      SimpleScriptV2InMary    -> toAllegraTimelock script
      SimpleScriptV2InAlonzo  -> Alonzo.TimelockScript (toAllegraTimelock script)

toShelleyScript (ScriptInEra langInEra (PlutusScript PlutusScriptV1
                                         (PlutusScriptSerialised script))) =
    case langInEra of
      PlutusScriptV1InAlonzo  -> Alonzo.PlutusScript script

fromShelleyBasedScript  :: ShelleyBasedEra era
                        -> Ledger.Script (ShelleyLedgerEra era)
                        -> ScriptInEra era
fromShelleyBasedScript era script =
  case era of
    ShelleyBasedEraShelley ->
      ScriptInEra SimpleScriptV1InShelley $
      SimpleScript SimpleScriptV1 $
      fromShelleyMultiSig script
    ShelleyBasedEraAllegra ->
      ScriptInEra SimpleScriptV2InAllegra $
      SimpleScript SimpleScriptV2 $
      fromAllegraTimelock TimeLocksInSimpleScriptV2 script
    ShelleyBasedEraMary ->
      ScriptInEra SimpleScriptV2InMary $
      SimpleScript SimpleScriptV2 $
      fromAllegraTimelock TimeLocksInSimpleScriptV2 script
    ShelleyBasedEraAlonzo ->
      case script of
        Alonzo.TimelockScript s ->
          ScriptInEra SimpleScriptV2InAlonzo $
          SimpleScript SimpleScriptV2 $
          fromAllegraTimelock TimeLocksInSimpleScriptV2 s
        Alonzo.PlutusScript s ->
          ScriptInEra PlutusScriptV1InAlonzo $
          PlutusScript PlutusScriptV1 $
          PlutusScriptSerialised s


-- | Conversion for the 'Shelley.MultiSig' language used by the Shelley era.
--
toShelleyMultiSig :: SimpleScript SimpleScriptV1
                  -> Shelley.MultiSig StandardCrypto
toShelleyMultiSig = go
  where
    go :: SimpleScript SimpleScriptV1 -> Shelley.MultiSig StandardCrypto
    go (RequireSignature (PaymentKeyHash kh))
                        = Shelley.RequireSignature (Shelley.coerceKeyRole kh)
    go (RequireAllOf s) = Shelley.RequireAllOf (map go s)
    go (RequireAnyOf s) = Shelley.RequireAnyOf (map go s)
    go (RequireMOf m s) = Shelley.RequireMOf m (map go s)

-- | Conversion for the 'Shelley.MultiSig' language used by the Shelley era.
--
fromShelleyMultiSig :: Shelley.MultiSig StandardCrypto -> SimpleScript lang
fromShelleyMultiSig = go
  where
    go (Shelley.RequireSignature kh)
                                = RequireSignature
                                    (PaymentKeyHash (Shelley.coerceKeyRole kh))
    go (Shelley.RequireAllOf s) = RequireAllOf (map go s)
    go (Shelley.RequireAnyOf s) = RequireAnyOf (map go s)
    go (Shelley.RequireMOf m s) = RequireMOf m (map go s)

-- | Conversion for the 'Timelock.Timelock' language that is shared between the
-- Allegra and Mary eras.
--
toAllegraTimelock :: forall lang.
                     SimpleScript lang -> Timelock.Timelock StandardCrypto
toAllegraTimelock = go
  where
    go :: SimpleScript lang -> Timelock.Timelock StandardCrypto
    go (RequireSignature (PaymentKeyHash kh))
                        = Timelock.RequireSignature (Shelley.coerceKeyRole kh)
    go (RequireAllOf s) = Timelock.RequireAllOf (Seq.fromList (map go s))
    go (RequireAnyOf s) = Timelock.RequireAnyOf (Seq.fromList (map go s))
    go (RequireMOf m s) = Timelock.RequireMOf m (Seq.fromList (map go s))
    go (RequireTimeBefore _ t) = Timelock.RequireTimeExpire t
    go (RequireTimeAfter  _ t) = Timelock.RequireTimeStart  t

-- | Conversion for the 'Timelock.Timelock' language that is shared between the
-- Allegra and Mary eras.
--
fromAllegraTimelock :: TimeLocksSupported lang
                    -> Timelock.Timelock StandardCrypto
                    -> SimpleScript lang
fromAllegraTimelock timelocks = go
  where
    go (Timelock.RequireSignature kh) = RequireSignature
                                          (PaymentKeyHash (Shelley.coerceKeyRole kh))
    go (Timelock.RequireTimeExpire t) = RequireTimeBefore timelocks t
    go (Timelock.RequireTimeStart  t) = RequireTimeAfter  timelocks t
    go (Timelock.RequireAllOf      s) = RequireAllOf (map go (toList s))
    go (Timelock.RequireAnyOf      s) = RequireAnyOf (map go (toList s))
    go (Timelock.RequireMOf      i s) = RequireMOf i (map go (toList s))


-- ----------------------------------------------------------------------------
-- JSON serialisation
--

-- Remember that Plutus scripts do not have a JSON syntax, and so do not have
-- and JSON instances. The only JSON format they support is via the
-- HasTextEnvelope class which just wraps the binary format.
--
-- Because of this the 'Script' type also does not have any JSON instances, but
-- the 'SimpleScript' type does.

instance ToJSON (SimpleScript lang) where
  toJSON (RequireSignature pKeyHash) =
    object [ "type"    .= String "sig"
           , "keyHash" .= serialiseToRawBytesHexText pKeyHash
           ]
  toJSON (RequireTimeBefore _ slot) =
    object [ "type" .= String "before"
           , "slot" .= slot
           ]
  toJSON (RequireTimeAfter _ slot) =
    object [ "type" .= String "after"
           , "slot" .= slot
           ]
  toJSON (RequireAnyOf reqScripts) =
    object [ "type" .= String "any", "scripts" .= map toJSON reqScripts ]
  toJSON (RequireAllOf reqScripts) =
    object [ "type" .= String "all", "scripts" .= map toJSON reqScripts ]
  toJSON (RequireMOf reqNum reqScripts) =
    object [ "type" .= String "atLeast"
           , "required" .= reqNum
           , "scripts" .= map toJSON reqScripts
           ]


instance IsSimpleScriptLanguage lang => FromJSON (SimpleScript lang) where
  parseJSON = parseSimpleScript simpleScriptVersion


parseSimpleScript :: SimpleScriptVersion lang
                  -> Value -> Aeson.Parser (SimpleScript lang)
parseSimpleScript lang v = parseScriptSig          v
                       <|> parseScriptBefore  lang v
                       <|> parseScriptAfter   lang v
                       <|> parseScriptAny     lang v
                       <|> parseScriptAll     lang v
                       <|> parseScriptAtLeast lang v

parseScriptAny :: SimpleScriptVersion lang
               -> Value -> Aeson.Parser (SimpleScript lang)
parseScriptAny lang =
    Aeson.withObject "any" $ \obj -> do
      t <- obj .: "type"
      case t :: Text of
        "any" -> do vs <- obj .: "scripts"
                    RequireAnyOf <$> gatherSimpleScriptTerms lang vs
        _ -> fail "\"any\" script value not found"

parseScriptAll :: SimpleScriptVersion lang
               -> Value -> Aeson.Parser (SimpleScript lang)
parseScriptAll lang =
    Aeson.withObject "all" $ \obj -> do
      t <- obj .: "type"
      case t :: Text of
        "all" -> do vs <- obj .: "scripts"
                    RequireAllOf <$> gatherSimpleScriptTerms lang vs
        _ -> fail "\"all\" script value not found"

parseScriptAtLeast :: SimpleScriptVersion lang
                   -> Value -> Aeson.Parser (SimpleScript lang)
parseScriptAtLeast lang =
    Aeson.withObject "atLeast" $ \obj -> do
      v <- obj .: "type"
      case v :: Text of
        "atLeast" -> do
          r  <- obj .: "required"
          vs <- obj .: "scripts"
          case r of
            Number sci ->
              case toBoundedInteger sci of
                Just reqInt ->
                  do scripts <- gatherSimpleScriptTerms lang vs
                     let numScripts = length scripts
                     when
                       (reqInt > numScripts)
                       (fail $ "Required number of script signatures exceeds the number of scripts."
                             <> " Required number: " <> show reqInt
                             <> " Number of scripts: " <> show numScripts)
                     return $ RequireMOf reqInt scripts
                Nothing -> fail $ "Error in \"required\" key: "
                                <> show sci <> " is not a valid Int"
            _ -> fail "\"required\" value should be an integer"
        _        -> fail "\"atLeast\" script value not found"

gatherSimpleScriptTerms :: SimpleScriptVersion lang
                        -> Vector Value -> Aeson.Parser [SimpleScript lang]
gatherSimpleScriptTerms lang = mapM (parseSimpleScript lang) . Vector.toList

parseScriptSig :: Value -> Aeson.Parser (SimpleScript lang)
parseScriptSig =
    Aeson.withObject "sig" $ \obj -> do
      v <- obj .: "type"
      case v :: Text of
        "sig" -> do k <- obj .: "keyHash"
                    RequireSignature <$> parsePaymentKeyHash k
        _     -> fail "\"sig\" script value not found"

parseScriptBefore :: SimpleScriptVersion lang
                  -> Value -> Aeson.Parser (SimpleScript lang)
parseScriptBefore lang =
    Aeson.withObject "before" $ \obj -> do
      v <- obj .: "type"
      case v :: Text of
        "before" ->
          case timeLocksSupported lang of
            Just supported -> RequireTimeBefore supported <$> obj .: "slot"
            Nothing -> fail ("type \"before\" not supported in " ++ show lang)
        _ -> fail "\"before\" script value not found"

parseScriptAfter :: SimpleScriptVersion lang
                 -> Value -> Aeson.Parser (SimpleScript lang)
parseScriptAfter lang =
    Aeson.withObject "after" $ \obj -> do
      v <- obj .: "type"
      case v :: Text of
        "after" ->
          case timeLocksSupported lang of
            Just supported -> RequireTimeAfter supported <$> obj .: "slot"
            Nothing -> fail ("type \"after\" not supported in " ++ show lang)
        _       -> fail "\"after\" script value not found"

parsePaymentKeyHash :: Text -> Aeson.Parser (Hash PaymentKey)
parsePaymentKeyHash txt =
    case deserialiseFromRawBytesHex (AsHash AsPaymentKey) (Text.encodeUtf8 txt) of
      Just payKeyHash -> return payKeyHash
      Nothing -> fail $ "Error deserialising payment key hash: " <> Text.unpack txt
