{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.Key
  ( Key(..)
  , generateSigningKey
  , CastVerificationKeyRole(..)
  , CastSigningKeyRole(..)
  , AsType(AsVerificationKey, AsSigningKey)
  ) where

import Prelude

import           Data.Kind (Type)

import qualified Cardano.Crypto.DSIGN.Class as Crypto
import qualified Cardano.Crypto.Seed as Crypto

import           Cardano.Api.Hash
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.SerialiseRaw
import           Cardano.Api.SerialiseTextEnvelope


-- | An interface for cryptographic keys used for signatures with a 'SigningKey'
-- and a 'VerificationKey' key.
--
-- This interface does not provide actual signing or verifying functions since
-- this API is concerned with the management of keys: generating and
-- serialising.
--
class (Eq (VerificationKey keyrole),
       Show (VerificationKey keyrole),
       SerialiseAsRawBytes (Hash keyrole),
       HasTextEnvelope (VerificationKey keyrole),
       HasTextEnvelope (SigningKey keyrole))
    => Key keyrole where

    -- | The type of cryptographic verification key, for each key role.
    data VerificationKey keyrole :: Type

    -- | The type of cryptographic signing key, for each key role.
    data SigningKey keyrole :: Type

    -- | Get the corresponding verification key from a signing key.
    getVerificationKey :: SigningKey keyrole -> VerificationKey keyrole

    -- | Generate a 'SigningKey' deterministically, given a 'Crypto.Seed'. The
    -- required size of the seed is given by 'deterministicSigningKeySeedSize'.
    --
    deterministicSigningKey :: AsType keyrole -> Crypto.Seed -> SigningKey keyrole
    deterministicSigningKeySeedSize :: AsType keyrole -> Word

    verificationKeyHash :: VerificationKey keyrole -> Hash keyrole


-- TODO: We should move this into the Key type class, with the existing impl as the default impl.
-- For KES we can then override it to keep the seed and key in mlocked memory at all times.
-- | Generate a 'SigningKey' using a seed from operating system entropy.
--
generateSigningKey :: Key keyrole => AsType keyrole -> IO (SigningKey keyrole)
generateSigningKey keytype = do
    seed <- Crypto.readSeedFromSystemEntropy seedSize
    return $! deterministicSigningKey keytype seed
  where
    seedSize = deterministicSigningKeySeedSize keytype


instance HasTypeProxy a => HasTypeProxy (VerificationKey a) where
    data AsType (VerificationKey a) = AsVerificationKey (AsType a)
    proxyToAsType _ = AsVerificationKey (proxyToAsType (Proxy :: Proxy a))

instance HasTypeProxy a => HasTypeProxy (SigningKey a) where
    data AsType (SigningKey a) = AsSigningKey (AsType a)
    proxyToAsType _ = AsSigningKey (proxyToAsType (Proxy :: Proxy a))


-- | Some key roles share the same representation and it is sometimes
-- legitimate to change the role of a key.
--
class CastVerificationKeyRole keyroleA keyroleB where

    -- | Change the role of a 'VerificationKey', if the representation permits.
    castVerificationKey :: VerificationKey keyroleA -> VerificationKey keyroleB

class CastSigningKeyRole keyroleA keyroleB where

    -- | Change the role of a 'SigningKey', if the representation permits.
    castSigningKey :: SigningKey keyroleA -> SigningKey keyroleB

