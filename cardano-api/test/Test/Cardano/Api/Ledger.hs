{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Api.Ledger
  ( tests
  ) where

import           Cardano.Prelude
import           Hedgehog (Property, discover)
import           Ouroboros.Consensus.Shelley.Eras (StandardCrypto)
import           Shelley.Spec.Ledger.Address (deserialiseAddr, serialiseAddr)
import           Test.Cardano.Api.Genesis
import           Test.Shelley.Spec.Ledger.Serialisation.Generators.Genesis (genAddress)
import           Test.Tasty (TestTree)
import           Test.Tasty.Hedgehog.Group (fromGroup)

import qualified Hedgehog as H
import qualified Hedgehog.Extras.Aeson as H

prop_golden_ShelleyGenesis :: Property
prop_golden_ShelleyGenesis = H.goldenTestJsonValuePretty exampleShelleyGenesis "test/Golden/ShelleyGenesis"

-- Keep this here to make sure serialiseAddr/deserialiseAddr are working.
-- They are defined in the Shelley executable spec and have been wrong at
-- least once.
prop_roundtrip_Address_CBOR :: Property
prop_roundtrip_Address_CBOR = H.property $ do
  -- If this fails, FundPair and ShelleyGenesis can also fail.
  addr <- H.forAll (genAddress @StandardCrypto)
  H.tripping addr serialiseAddr deserialiseAddr

-- -----------------------------------------------------------------------------

tests :: TestTree
tests = fromGroup $$discover
