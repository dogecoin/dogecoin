{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Api.Typed.JSON
  ( tests
  ) where

import           Cardano.Prelude

import           Data.Aeson

import           Hedgehog (Property, discover, forAll, tripping)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import           Test.Tasty (TestTree)
import           Test.Tasty.Hedgehog.Group (fromGroup)

import           Test.Cardano.Api.Typed.Gen


{- HLINT ignore "Use camelCase" -}

prop_roundtrip_praos_nonce_JSON :: Property
prop_roundtrip_praos_nonce_JSON = H.property $ do
  pNonce <- forAll $ Gen.just genMaybePraosNonce
  tripping pNonce encode eitherDecode

prop_roundtrip_protocol_parameters_JSON :: Property
prop_roundtrip_protocol_parameters_JSON = H.property $ do
  pp <- forAll genProtocolParameters
  tripping pp encode eitherDecode

-- -----------------------------------------------------------------------------

tests :: TestTree
tests = fromGroup $$discover
