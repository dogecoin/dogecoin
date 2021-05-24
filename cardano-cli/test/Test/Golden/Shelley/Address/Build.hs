{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Address.Build
  ( golden_shelleyAddressBuild
  ) where

import           Cardano.Prelude
import           Hedgehog (Property)
import           Test.OptParse as OP

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

golden_shelleyAddressBuild :: Property
golden_shelleyAddressBuild = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  addressVKeyFile <- noteInputFile "test/data/golden/shelley/keys/payment_keys/verification_key"
  addressSKeyFile <- noteInputFile "test/data/golden/shelley/keys/stake_keys/verification_key"
  goldenStakingAddressHexFile <- noteInputFile "test/data/golden/shelley/addresses/staking-address.hex"
  goldenEnterpriseAddressHexFile <- noteInputFile "test/data/golden/shelley/addresses/enterprise-address.hex"
  stakingAddressHexFile <- noteTempFile tempDir "staking-address.hex"
  enterpriseAddressHexFile <- noteTempFile tempDir "enterprise-address.hex"

  void $ H.readFile addressVKeyFile

  stakingAddressText <- execCardanoCLI
    [ "address","build"
    , "--testnet-magic", "14"
    , "--payment-verification-key-file", addressVKeyFile
    , "--staking-verification-key-file", addressSKeyFile
    ]

  goldenStakingAddressHex <- H.readFile goldenStakingAddressHexFile

  H.writeFile stakingAddressHexFile stakingAddressText

  equivalence stakingAddressText goldenStakingAddressHex

  void $ H.readFile addressSKeyFile

  enterpriseAddressText <- execCardanoCLI
    [ "address","build"
    , "--testnet-magic", "14"
    , "--payment-verification-key-file", addressVKeyFile
    , "--staking-verification-key-file", addressSKeyFile
    ]

  goldenEnterpriseAddressHex <- H.readFile goldenEnterpriseAddressHexFile

  H.writeFile enterpriseAddressHexFile enterpriseAddressText

  equivalence enterpriseAddressText goldenEnterpriseAddressHex
