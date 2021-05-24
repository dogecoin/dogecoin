{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.StakeAddress.KeyGen
  ( golden_shelleyStakeAddressKeyGen
  ) where

import           Cardano.Prelude
import           Hedgehog (Property)
import           Test.OptParse

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

golden_shelleyStakeAddressKeyGen :: Property
golden_shelleyStakeAddressKeyGen = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  verificationKeyFile <- noteTempFile tempDir "kes.vkey"
  signingKeyFile <- noteTempFile tempDir "kes.skey"

  void $ execCardanoCLI
    [ "stake-address","key-gen"
    , "--verification-key-file", verificationKeyFile
    , "--signing-key-file", signingKeyFile
    ]

  H.assertFileOccurences 1 "StakeVerificationKeyShelley_ed25519" verificationKeyFile
  H.assertFileOccurences 1 "StakeSigningKeyShelley_ed25519" signingKeyFile

  H.assertEndsWithSingleNewline verificationKeyFile
  H.assertEndsWithSingleNewline signingKeyFile
