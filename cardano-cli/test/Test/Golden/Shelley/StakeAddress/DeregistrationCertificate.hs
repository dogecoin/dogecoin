{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.StakeAddress.DeregistrationCertificate
  ( golden_shelleyStakeAddressDeregistrationCertificate
  ) where

import           Cardano.Prelude
import           Hedgehog (Property)
import           Test.OptParse

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

golden_shelleyStakeAddressDeregistrationCertificate :: Property
golden_shelleyStakeAddressDeregistrationCertificate = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  verificationKeyFile <- noteInputFile "test/data/golden/shelley/keys/stake_keys/verification_key"
  deregistrationCertFile <- noteTempFile tempDir "deregistrationCertFile"

  void $ execCardanoCLI
    [ "stake-address","deregistration-certificate"
    , "--staking-verification-key-file", verificationKeyFile
    , "--out-file", deregistrationCertFile
    ]

  H.assertFileOccurences 1 "Stake Address Deregistration Certificate" deregistrationCertFile

  H.assertEndsWithSingleNewline deregistrationCertFile
