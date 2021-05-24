{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Cardano.Node.Json
  ( tests
  ) where

import           Cardano.Prelude

import           Data.Aeson (decode, encode, fromJSON, toJSON)

import           Hedgehog (Property, discover)
import qualified Hedgehog

import           Test.Cardano.Node.Gen

prop_roundtrip_NodeIPv4Address_JSON :: Property
prop_roundtrip_NodeIPv4Address_JSON =
  Hedgehog.property $ do
    na <- Hedgehog.forAll genNodeIPv4Address
    Hedgehog.tripping na toJSON fromJSON
    Hedgehog.tripping na encode decode

prop_roundtrip_NodeIPv6Address_JSON :: Property
prop_roundtrip_NodeIPv6Address_JSON =
  Hedgehog.property $ do
    na <- Hedgehog.forAll genNodeIPv6Address
    Hedgehog.tripping na toJSON fromJSON
    Hedgehog.tripping na encode decode

prop_roundtrip_NodeIPAddress_JSON :: Property
prop_roundtrip_NodeIPAddress_JSON =
  Hedgehog.property $ do
    na <- Hedgehog.forAll genNodeIPAddress
    Hedgehog.tripping na toJSON fromJSON
    Hedgehog.tripping na encode decode

prop_roundtrip_NodeHostAddress_JSON :: Property
prop_roundtrip_NodeHostAddress_JSON =
  Hedgehog.property $ do
    nha <- Hedgehog.forAll genNodeHostIPAddress
    Hedgehog.tripping nha toJSON fromJSON
    Hedgehog.tripping nha encode decode

prop_roundtrip_NodeSetup_JSON :: Property
prop_roundtrip_NodeSetup_JSON =
  Hedgehog.property $ do
    ns <- Hedgehog.forAll genNodeSetup
    Hedgehog.tripping ns toJSON fromJSON
    Hedgehog.tripping ns encode decode

prop_roundtrip_NetworkTopology_JSON :: Property
prop_roundtrip_NetworkTopology_JSON =
  Hedgehog.property $ do
    ntop <- Hedgehog.forAll genNetworkTopology
    Hedgehog.tripping ntop toJSON fromJSON
    Hedgehog.tripping ntop encode decode


-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  Hedgehog.checkParallel $$discover
