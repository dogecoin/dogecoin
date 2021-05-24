{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.StakeAddress.RegistrationCertificate
  ( golden_shelleyStakeAddressRegistrationCertificate
  ) where

import           Cardano.Prelude
import           Hedgehog (Property)
import           Test.OptParse

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

golden_shelleyStakeAddressRegistrationCertificate :: Property
golden_shelleyStakeAddressRegistrationCertificate = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  keyGenStakingVerificationKeyFile <- noteInputFile "test/data/golden/shelley/keys/stake_keys/verification_key"
  registrationCertFile <- noteTempFile tempDir "registration.cert"

  void $ execCardanoCLI
    [ "stake-address","registration-certificate"
    , "--staking-verification-key-file", keyGenStakingVerificationKeyFile
    , "--out-file", registrationCertFile
    ]

  H.assertFileOccurences 1 "Stake Address Registration Certificate" registrationCertFile

  H.assertEndsWithSingleNewline registrationCertFile
