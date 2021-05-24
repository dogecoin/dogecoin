{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | BIP32-Ed25519 digital signatures.
module Cardano.Api.Crypto.Ed25519Bip32
  ( Ed25519Bip32DSIGN
  , SigDSIGN (..)
  , SignKeyDSIGN (..)
  , VerKeyDSIGN (..)

    -- * Serialisation
  , xPrvToBytes
  , xPrvFromBytes
  )
where

import           Cardano.Prelude hiding (show)
import           Prelude (show)

import           Data.ByteArray as BA (ByteArrayAccess, ScrubbedBytes, convert)
import qualified Data.ByteString as BS
import           NoThunks.Class (InspectHeap (..), NoThunks)

import           Cardano.Binary (FromCBOR (..), ToCBOR (..))
import qualified Cardano.Crypto.Wallet as CC

import           Cardano.Crypto.DSIGN.Class
import           Cardano.Crypto.Seed
import           Cardano.Crypto.Util (SignableRepresentation (..))

import qualified Crypto.ECC.Edwards25519 as Ed25519
import           Crypto.Error (eitherCryptoError)


data Ed25519Bip32DSIGN

instance DSIGNAlgorithm Ed25519Bip32DSIGN where

    type SeedSizeDSIGN    Ed25519Bip32DSIGN = 32

    -- | BIP32-Ed25519 extended verification key size is 64 octets.
    type SizeVerKeyDSIGN  Ed25519Bip32DSIGN = 64

    -- | BIP32-Ed25519 extended signing key size is 96 octets.
    type SizeSignKeyDSIGN Ed25519Bip32DSIGN = 96

    -- | BIP32-Ed25519 extended signature size is 64 octets.
    type SizeSigDSIGN     Ed25519Bip32DSIGN = 64

    --
    -- Key and signature types
    --

    newtype VerKeyDSIGN Ed25519Bip32DSIGN = VerKeyEd25519Bip32DSIGN CC.XPub
        deriving (Show, Eq, Generic)
        deriving newtype NFData
        deriving NoThunks via InspectHeap CC.XPub

    newtype SignKeyDSIGN Ed25519Bip32DSIGN = SignKeyEd25519Bip32DSIGN CC.XPrv
        deriving (Generic, ByteArrayAccess)
        deriving newtype NFData
        deriving NoThunks via InspectHeap CC.XPrv

    newtype SigDSIGN Ed25519Bip32DSIGN = SigEd25519Bip32DSIGN CC.XSignature
        deriving (Show, Eq, Generic, ByteArrayAccess)
        deriving NoThunks via InspectHeap CC.XSignature

    --
    -- Metadata and basic key operations
    --

    algorithmNameDSIGN _ = "ed25519_bip32"

    deriveVerKeyDSIGN (SignKeyEd25519Bip32DSIGN sk) =
      VerKeyEd25519Bip32DSIGN $ CC.toXPub sk

    --
    -- Core algorithm operations
    --

    type Signable Ed25519Bip32DSIGN = SignableRepresentation

    signDSIGN () a (SignKeyEd25519Bip32DSIGN sk) =
      SigEd25519Bip32DSIGN $
        CC.sign (mempty :: ScrubbedBytes) sk (getSignableRepresentation a)

    verifyDSIGN () (VerKeyEd25519Bip32DSIGN vk) a (SigEd25519Bip32DSIGN sig) =
      if CC.verify vk (getSignableRepresentation a) sig
        then Right ()
        else Left "Verification failed"

    --
    -- Key generation
    --

    genKeyDSIGN seed =
      SignKeyEd25519Bip32DSIGN $
        CC.generateNew
          (getSeedBytes seed)
          (mempty :: ScrubbedBytes)
          (mempty :: ScrubbedBytes)

    --
    -- raw serialise/deserialise
    --

    rawSerialiseVerKeyDSIGN (VerKeyEd25519Bip32DSIGN vk) = CC.unXPub vk
    rawSerialiseSignKeyDSIGN (SignKeyEd25519Bip32DSIGN sk) = xPrvToBytes sk
    rawSerialiseSigDSIGN = BA.convert

    rawDeserialiseVerKeyDSIGN =
      either (const Nothing) (Just . VerKeyEd25519Bip32DSIGN) . CC.xpub
    rawDeserialiseSignKeyDSIGN =
      fmap SignKeyEd25519Bip32DSIGN . xPrvFromBytes
    rawDeserialiseSigDSIGN =
      either (const Nothing) (Just . SigEd25519Bip32DSIGN) . CC.xsignature


instance Show (SignKeyDSIGN Ed25519Bip32DSIGN) where
  show (SignKeyEd25519Bip32DSIGN sk) = show $ xPrvToBytes sk

instance ToCBOR (VerKeyDSIGN Ed25519Bip32DSIGN) where
  toCBOR = encodeVerKeyDSIGN
  encodedSizeExpr _ = encodedVerKeyDSIGNSizeExpr

instance FromCBOR (VerKeyDSIGN Ed25519Bip32DSIGN) where
  fromCBOR = decodeVerKeyDSIGN

instance ToCBOR (SignKeyDSIGN Ed25519Bip32DSIGN) where
  toCBOR = encodeSignKeyDSIGN
  encodedSizeExpr _ = encodedSignKeyDESIGNSizeExpr

instance FromCBOR (SignKeyDSIGN Ed25519Bip32DSIGN) where
  fromCBOR = decodeSignKeyDSIGN

instance ToCBOR (SigDSIGN Ed25519Bip32DSIGN) where
  toCBOR = encodeSigDSIGN
  encodedSizeExpr _ = encodedSigDSIGNSizeExpr

instance FromCBOR (SigDSIGN Ed25519Bip32DSIGN) where
  fromCBOR = decodeSigDSIGN


-- | Serialise an 'CC.XPrv' to a 'ByteString' (96 bytes).
--
-- In @cardano-crypto@, an 'CC.XPrv' was originally serialised using the
-- following 128-byte binary format:
--
-- +---------------------------------+-----------------------+-----------------------+
-- | Extended Private Key (64 bytes) | Public Key (32 bytes) | Chain Code (32 bytes) |
-- +---------------------------------+-----------------------+-----------------------+
--
-- However, this function serialises an 'CC.XPrv' using a more compact 96-byte
-- binary format:
--
-- +---------------------------------+-----------------------+
-- | Extended Private Key (64 bytes) | Chain Code (32 bytes) |
-- +---------------------------------+-----------------------+
--
xPrvToBytes :: CC.XPrv -> ByteString
xPrvToBytes xPrv = privateKeyBytes <> chainCodeBytes
  where
    privateKeyBytes :: ByteString
    privateKeyBytes = BS.take 64 (CC.unXPrv xPrv)

    chainCodeBytes :: ByteString
    chainCodeBytes = BS.drop 96 (CC.unXPrv xPrv)

-- | Deserialise an 'CC.XPrv' from a 'ByteString' (96 bytes).
--
-- In @cardano-crypto@, an 'CC.XPrv' was originally deserialised using the
-- following 128-byte binary format:
--
-- +---------------------------------+-----------------------+-----------------------+
-- | Extended Private Key (64 bytes) | Public Key (32 bytes) | Chain Code (32 bytes) |
-- +---------------------------------+-----------------------+-----------------------+
--
-- However, this function deserialises an 'CC.XPrv' using a more compact
-- 96-byte binary format:
--
-- +---------------------------------+-----------------------+
-- | Extended Private Key (64 bytes) | Chain Code (32 bytes) |
-- +---------------------------------+-----------------------+
--
xPrvFromBytes :: ByteString -> Maybe CC.XPrv
xPrvFromBytes bytes
    | BS.length bytes /= 96 = Nothing
    | otherwise = do
        let (prv, cc) = BS.splitAt 64 bytes
        pub <- ed25519ScalarMult (BS.take 32 prv)
        eitherToMaybe $ CC.xprv $ prv <> pub <> cc
  where
    eitherToMaybe :: Either a b -> Maybe b
    eitherToMaybe = either (const Nothing) Just

    ed25519ScalarMult :: ByteString -> Maybe ByteString
    ed25519ScalarMult bs = do
      scalar <- eitherToMaybe . eitherCryptoError $ Ed25519.scalarDecodeLong bs
      pure $ Ed25519.pointEncode $ Ed25519.toPoint scalar
