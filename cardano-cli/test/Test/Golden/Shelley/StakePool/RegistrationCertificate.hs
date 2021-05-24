{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.StakePool.RegistrationCertificate
  ( golden_shelleyStakePoolRegistrationCertificate
  ) where

import           Cardano.Prelude
import           Hedgehog (Property)
import           Test.OptParse

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

golden_shelleyStakePoolRegistrationCertificate :: Property
golden_shelleyStakePoolRegistrationCertificate = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  operatorVerificationKeyFile <- noteInputFile "test/data/golden/shelley/node-pool/operator.vkey"
  vrfVerificationKeyFile <- noteInputFile "test/data/golden/shelley/node-pool/vrf.vkey"
  ownerVerificationKeyFile <- noteInputFile "test/data/golden/shelley/node-pool/owner.vkey"
  registrationCertFile <- noteTempFile tempDir "registration.cert"

  void $ execCardanoCLI
    [ "stake-pool","registration-certificate"
    , "--testnet-magic", "42"
    , "--pool-pledge", "0"
    , "--pool-cost", "0"
    , "--pool-margin", "0"
    , "--cold-verification-key-file", operatorVerificationKeyFile
    , "--vrf-verification-key-file", vrfVerificationKeyFile
    , "--reward-account-verification-key-file", ownerVerificationKeyFile
    , "--pool-owner-stake-verification-key-file", ownerVerificationKeyFile
    , "--out-file", registrationCertFile
    ]

  H.assertFileOccurences 1 "Stake Pool Registration Certificate" registrationCertFile

  H.assertEndsWithSingleNewline registrationCertFile
