{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Api.Typed.Orphans () where

import           Cardano.Prelude

import           Cardano.Api.Shelley
import           Cardano.Crypto.Hash hiding (Hash)
import           Cardano.Crypto.KES
import           Cardano.Crypto.Libsodium (SodiumHashAlgorithm)

import           Test.Cardano.Crypto.Orphans ()

-- Signing Key instances

deriving instance Eq (SigningKey ByronKey)
deriving instance Eq (SigningKey PaymentKey)
deriving instance Eq (SigningKey StakeKey)
deriving instance Eq (SigningKey StakePoolKey)
deriving instance Eq (SigningKey GenesisKey)
deriving instance Eq (SigningKey GenesisDelegateKey)
deriving instance Eq (SigningKey GenesisUTxOKey)
deriving instance Eq (SigningKey KesKey)
deriving instance Eq (SigningKey VrfKey)


instance ( KESAlgorithm d
         , SodiumHashAlgorithm h
         , SizeHash h ~ SeedSizeKES d
         ) => Eq (SignKeyKES (SumKES h d)) where
  k1 == k2 = rawSerialiseSignKeyKES k1 == rawSerialiseSignKeyKES k2
