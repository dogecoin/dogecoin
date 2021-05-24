{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.StakeAddress.Build
  ( golden_shelleyStakeAddressBuild
  ) where

import           Cardano.Prelude
import           Hedgehog (Property)
import           Test.OptParse as OP

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

golden_shelleyStakeAddressBuild :: Property
golden_shelleyStakeAddressBuild = propertyOnce . H.moduleWorkspace "tmp" $ \_ -> do
  verificationKeyFile <- noteInputFile "test/data/golden/shelley/keys/stake_keys/verification_key"
  goldenRewardAddressFile <- noteInputFile "test/data/golden/shelley/keys/stake_keys/reward_address"

  rewardAddress <- execCardanoCLI
    [ "stake-address","build"
    , "--mainnet"
    , "--staking-verification-key-file", verificationKeyFile
    ]

  goldenRewardsAddress <- H.readFile goldenRewardAddressFile

  equivalence rewardAddress goldenRewardsAddress
