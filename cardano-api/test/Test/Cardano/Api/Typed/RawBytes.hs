{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Api.Typed.RawBytes
  ( tests
  ) where

import           Cardano.Api
import           Cardano.Prelude
import           Hedgehog (Property, discover)
import           Test.Cardano.Api.Typed.Gen
import           Test.Cardano.Api.Typed.Orphans ()
import           Test.Tasty (TestTree)
import           Test.Tasty.Hedgehog.Group (fromGroup)

import qualified Hedgehog as H

{- HLINT ignore "Use camelCase" -}

-- Address CBOR round trips

prop_roundtrip_shelley_address_raw :: Property
prop_roundtrip_shelley_address_raw =
  roundtrip_raw_bytes AsShelleyAddress genAddressShelley


prop_roundtrip_byron_address_raw :: Property
prop_roundtrip_byron_address_raw =
  roundtrip_raw_bytes AsByronAddress genAddressByron

prop_roundtrip_stake_address_raw :: Property
prop_roundtrip_stake_address_raw =
  roundtrip_raw_bytes AsStakeAddress genStakeAddress

prop_roundtrip_script_hash_raw :: Property
prop_roundtrip_script_hash_raw =
  roundtrip_raw_bytes AsScriptHash genScriptHash

prop_roundtrip_verification_ByronKey_hash_raw :: Property
prop_roundtrip_verification_ByronKey_hash_raw =
  roundtrip_verification_key_hash_raw AsByronKey

prop_roundtrip_verification_PaymentKey_hash_raw :: Property
prop_roundtrip_verification_PaymentKey_hash_raw =
  roundtrip_verification_key_hash_raw AsPaymentKey

prop_roundtrip_verification_StakeKey_hash_raw :: Property
prop_roundtrip_verification_StakeKey_hash_raw =
  roundtrip_verification_key_hash_raw AsStakeKey

prop_roundtrip_verification_StakePoolKey_hash_raw :: Property
prop_roundtrip_verification_StakePoolKey_hash_raw =
  roundtrip_verification_key_hash_raw AsStakePoolKey

prop_roundtrip_verification_GenesisKey_hash_raw :: Property
prop_roundtrip_verification_GenesisKey_hash_raw =
  roundtrip_verification_key_hash_raw AsGenesisKey

prop_roundtrip_verification_GenesisDelegateKey_hash_raw :: Property
prop_roundtrip_verification_GenesisDelegateKey_hash_raw =
  roundtrip_verification_key_hash_raw AsGenesisDelegateKey

prop_roundtrip_verification_KesKey_hash_raw :: Property
prop_roundtrip_verification_KesKey_hash_raw =
  roundtrip_verification_key_hash_raw AsKesKey

prop_roundtrip_verification_VrfKey_hash_raw :: Property
prop_roundtrip_verification_VrfKey_hash_raw =
  roundtrip_verification_key_hash_raw AsVrfKey

prop_roundtrip_verification_GenesisUTxOKey_hash_raw :: Property
prop_roundtrip_verification_GenesisUTxOKey_hash_raw =
  roundtrip_verification_key_hash_raw AsGenesisUTxOKey

-- -----------------------------------------------------------------------------

roundtrip_raw_bytes
  :: ( SerialiseAsRawBytes a
     , Eq a
     , Show a) => AsType a -> H.Gen a -> Property
roundtrip_raw_bytes asType g =
  H.property $ do
    v <- H.forAll g
    H.tripping v serialiseToRawBytes (deserialiseFromRawBytes asType)

roundtrip_verification_key_hash_raw
  :: (Key keyrole, Eq (Hash keyrole), Show (Hash keyrole))
  => AsType keyrole -> Property
roundtrip_verification_key_hash_raw roletoken =
  H.property $ do
    vKey <- H.forAll $ genVerificationKey roletoken
    let vKeyHash = verificationKeyHash vKey
    H.tripping vKeyHash serialiseToRawBytes (deserialiseFromRawBytes (AsHash roletoken))

-- -----------------------------------------------------------------------------

tests :: TestTree
tests = fromGroup $$discover
