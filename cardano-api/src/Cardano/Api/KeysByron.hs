{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Byron key types and their 'Key' class instances
--
module Cardano.Api.KeysByron (

    -- * Key types
    ByronKey,
    ByronKeyLegacy,

    -- * Data family instances
    AsType(..),
    VerificationKey(..),
    SigningKey(..),
    Hash(..),

    -- * Legacy format
    IsByronKey(..),
    ByronKeyFormat(..),

    SomeByronSigningKey(..),
    toByronSigningKey
  ) where

import           Cardano.Prelude (cborError, toCborError)
import           Prelude

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import           Control.Monad
import qualified Data.ByteString.Lazy as LB
import           Data.String (IsString)
import           Data.Text (Text)
import qualified Data.Text as Text

import qualified Cardano.Crypto.DSIGN.Class as Crypto
import qualified Cardano.Crypto.Seed as Crypto
import qualified Cardano.Crypto.Signing as Crypto
import qualified Cardano.Crypto.Wallet as Crypto.HD

import           Cardano.Binary (toStrictByteString)
import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Crypto.Hashing as Byron
import qualified Cardano.Crypto.Signing as Byron
import qualified Cardano.Crypto.Wallet as Wallet

import           Cardano.Api.HasTypeProxy
import           Cardano.Api.Hash
import           Cardano.Api.Key
import           Cardano.Api.KeysShelley
import           Cardano.Api.SerialiseCBOR
import           Cardano.Api.SerialiseRaw
import           Cardano.Api.SerialiseTextEnvelope


-- | Byron-era payment keys. Used for Byron addresses and witnessing
-- transactions that spend from these addresses.
--
-- These use Ed25519 but with a 32byte \"chaincode\" used in HD derivation.
-- The inclusion of the chaincode is a design mistake but one that cannot
-- be corrected for the Byron era. The Shelley era 'PaymentKey's do not include
-- a chaincode. It is safe to use a zero or random chaincode for new Byron keys.
--
-- This is a type level tag, used with other interfaces like 'Key'.
--
data ByronKey
data ByronKeyLegacy

class IsByronKey key where
    byronKeyFormat :: ByronKeyFormat key

data ByronKeyFormat key where
  ByronLegacyKeyFormat :: ByronKeyFormat ByronKeyLegacy
  ByronModernKeyFormat :: ByronKeyFormat ByronKey

data SomeByronSigningKey
  = AByronSigningKeyLegacy (SigningKey ByronKeyLegacy)
  | AByronSigningKey (SigningKey ByronKey)

toByronSigningKey :: SomeByronSigningKey -> Byron.SigningKey
toByronSigningKey bWit =
  case bWit of
    AByronSigningKeyLegacy (ByronSigningKeyLegacy sKey) -> sKey
    AByronSigningKey (ByronSigningKey sKey) -> sKey

--
-- Byron key
--

instance Key ByronKey where

    newtype VerificationKey ByronKey =
           ByronVerificationKey Byron.VerificationKey
      deriving stock Eq
      deriving (Show, IsString) via UsingRawBytesHex (VerificationKey ByronKey)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    newtype SigningKey ByronKey =
           ByronSigningKey Byron.SigningKey
      deriving (Show, IsString) via UsingRawBytesHex (SigningKey ByronKey)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    deterministicSigningKey :: AsType ByronKey -> Crypto.Seed -> SigningKey ByronKey
    deterministicSigningKey AsByronKey seed =
       ByronSigningKey (snd (Crypto.runMonadRandomWithSeed seed Byron.keyGen))

    deterministicSigningKeySeedSize :: AsType ByronKey -> Word
    deterministicSigningKeySeedSize AsByronKey = 32

    getVerificationKey :: SigningKey ByronKey -> VerificationKey ByronKey
    getVerificationKey (ByronSigningKey sk) =
      ByronVerificationKey (Byron.toVerification sk)

    verificationKeyHash :: VerificationKey ByronKey -> Hash ByronKey
    verificationKeyHash (ByronVerificationKey vkey) =
      ByronKeyHash (Byron.hashKey vkey)

instance HasTypeProxy ByronKey where
    data AsType ByronKey = AsByronKey
    proxyToAsType _ = AsByronKey

instance HasTextEnvelope (VerificationKey ByronKey) where
    textEnvelopeType _ = "PaymentVerificationKeyByron_ed25519_bip32"

instance HasTextEnvelope (SigningKey ByronKey) where
    textEnvelopeType _ = "PaymentSigningKeyByron_ed25519_bip32"

instance SerialiseAsRawBytes (VerificationKey ByronKey) where
    serialiseToRawBytes (ByronVerificationKey (Byron.VerificationKey xvk)) =
      Crypto.HD.unXPub xvk

    deserialiseFromRawBytes (AsVerificationKey AsByronKey) bs =
      either (const Nothing) (Just . ByronVerificationKey . Byron.VerificationKey)
             (Crypto.HD.xpub bs)

instance SerialiseAsRawBytes (SigningKey ByronKey) where
    serialiseToRawBytes (ByronSigningKey (Byron.SigningKey xsk)) =
      toStrictByteString $ Crypto.toCBORXPrv xsk

    deserialiseFromRawBytes (AsSigningKey AsByronKey) bs =
      either (const Nothing) (Just . ByronSigningKey . Byron.SigningKey)
             (snd <$> CBOR.deserialiseFromBytes Byron.fromCBORXPrv (LB.fromStrict bs))

newtype instance Hash ByronKey = ByronKeyHash Byron.KeyHash
  deriving (Eq, Ord)
  deriving (Show, IsString) via UsingRawBytesHex (Hash ByronKey)

instance SerialiseAsRawBytes (Hash ByronKey) where
    serialiseToRawBytes (ByronKeyHash (Byron.KeyHash vkh)) =
      Byron.abstractHashToBytes vkh

    deserialiseFromRawBytes (AsHash AsByronKey) bs =
      ByronKeyHash . Byron.KeyHash <$> Byron.abstractHashFromBytes bs

instance CastVerificationKeyRole ByronKey PaymentExtendedKey where
    castVerificationKey (ByronVerificationKey vk) =
        PaymentExtendedVerificationKey
          (Byron.unVerificationKey vk)

instance CastVerificationKeyRole ByronKey PaymentKey where
    castVerificationKey =
        (castVerificationKey :: VerificationKey PaymentExtendedKey
                             -> VerificationKey PaymentKey)
      . (castVerificationKey :: VerificationKey ByronKey
                             -> VerificationKey PaymentExtendedKey)

instance IsByronKey ByronKey where
  byronKeyFormat = ByronModernKeyFormat

--
-- Legacy Byron key
--

instance Key ByronKeyLegacy where

    newtype VerificationKey ByronKeyLegacy =
           ByronVerificationKeyLegacy Byron.VerificationKey
      deriving stock (Eq)
      deriving (Show, IsString) via UsingRawBytesHex (VerificationKey ByronKeyLegacy)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    newtype SigningKey ByronKeyLegacy =
           ByronSigningKeyLegacy Byron.SigningKey
      deriving (Show, IsString) via UsingRawBytesHex (SigningKey ByronKeyLegacy)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    deterministicSigningKey :: AsType ByronKeyLegacy -> Crypto.Seed -> SigningKey ByronKeyLegacy
    deterministicSigningKey _ _ = error "Please generate a non legacy Byron key instead"

    deterministicSigningKeySeedSize :: AsType ByronKeyLegacy -> Word
    deterministicSigningKeySeedSize AsByronKeyLegacy = 32

    getVerificationKey :: SigningKey ByronKeyLegacy -> VerificationKey ByronKeyLegacy
    getVerificationKey (ByronSigningKeyLegacy sk) =
      ByronVerificationKeyLegacy (Byron.toVerification sk)

    verificationKeyHash :: VerificationKey ByronKeyLegacy -> Hash ByronKeyLegacy
    verificationKeyHash (ByronVerificationKeyLegacy vkey) =
      ByronKeyHashLegacy (Byron.hashKey vkey)

instance HasTypeProxy ByronKeyLegacy where
  data AsType ByronKeyLegacy = AsByronKeyLegacy
  proxyToAsType _ = AsByronKeyLegacy

instance HasTextEnvelope (VerificationKey ByronKeyLegacy) where
    textEnvelopeType _ = "PaymentVerificationKeyByronLegacy_ed25519_bip32"

instance HasTextEnvelope (SigningKey ByronKeyLegacy) where
    textEnvelopeType _ = "PaymentSigningKeyByronLegacy_ed25519_bip32"

newtype instance Hash ByronKeyLegacy = ByronKeyHashLegacy Byron.KeyHash
  deriving (Eq, Ord)
  deriving (Show, IsString) via UsingRawBytesHex (Hash ByronKeyLegacy)

instance SerialiseAsRawBytes (Hash ByronKeyLegacy) where
    serialiseToRawBytes (ByronKeyHashLegacy (Byron.KeyHash vkh)) =
      Byron.abstractHashToBytes vkh

    deserialiseFromRawBytes (AsHash AsByronKeyLegacy) bs =
      ByronKeyHashLegacy . Byron.KeyHash <$> Byron.abstractHashFromBytes bs

instance SerialiseAsRawBytes (VerificationKey ByronKeyLegacy) where
    serialiseToRawBytes (ByronVerificationKeyLegacy (Byron.VerificationKey xvk)) =
      Crypto.HD.unXPub xvk

    deserialiseFromRawBytes (AsVerificationKey AsByronKeyLegacy) bs =
      either (const Nothing) (Just . ByronVerificationKeyLegacy . Byron.VerificationKey)
             (Crypto.HD.xpub bs)

instance SerialiseAsRawBytes (SigningKey ByronKeyLegacy) where
    serialiseToRawBytes (ByronSigningKeyLegacy (Byron.SigningKey xsk)) =
      Crypto.HD.unXPrv xsk

    deserialiseFromRawBytes (AsSigningKey AsByronKeyLegacy) bs =
      either (const Nothing) (Just . ByronSigningKeyLegacy . snd)
             (CBOR.deserialiseFromBytes decodeLegacyDelegateKey $ LB.fromStrict bs)
     where
      -- Stolen from: cardano-sl/binary/src/Pos/Binary/Class/Core.hs
      -- | Enforces that the input size is the same as the decoded one, failing in
      -- case it's not.
      enforceSize :: Text -> Int -> CBOR.Decoder s ()
      enforceSize lbl requestedSize = CBOR.decodeListLenCanonical >>= matchSize requestedSize lbl

      -- Stolen from: cardano-sl/binary/src/Pos/Binary/Class/Core.hs
      -- | Compare two sizes, failing if they are not equal.
      matchSize :: Int -> Text -> Int -> CBOR.Decoder s ()
      matchSize requestedSize lbl actualSize =
        when (actualSize /= requestedSize) $
          cborError ( lbl <> " failed the size check. Expected " <> Text.pack (show requestedSize)
                          <> ", found " <> Text.pack (show actualSize)
                    )

      decodeXPrv :: CBOR.Decoder s Wallet.XPrv
      decodeXPrv = CBOR.decodeBytesCanonical >>= toCborError . Wallet.xprv


      -- | Decoder for a Byron/Classic signing key.
      --   Lifted from cardano-sl legacy codebase.
      decodeLegacyDelegateKey :: CBOR.Decoder s Byron.SigningKey
      decodeLegacyDelegateKey = do
          enforceSize "UserSecret" 4
          _    <- do
            enforceSize "vss" 1
            CBOR.decodeBytes
          pkey <- do
            enforceSize "pkey" 1
            Byron.SigningKey <$> decodeXPrv
          _    <- do
            CBOR.decodeListLenIndef
            CBOR.decodeSequenceLenIndef (flip (:)) [] reverse CBOR.decodeNull
          _    <- do
            enforceSize "wallet" 0
          pure pkey

instance CastVerificationKeyRole ByronKeyLegacy ByronKey where
    castVerificationKey (ByronVerificationKeyLegacy vk) =
        ByronVerificationKey vk

instance IsByronKey ByronKeyLegacy where
  byronKeyFormat = ByronLegacyKeyFormat
