{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Node.KeyGenVrf
  ( golden_shelleyNodeKeyGenVrf
  ) where

import           Cardano.Prelude
import           Hedgehog (Property)
import           Test.OptParse

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

golden_shelleyNodeKeyGenVrf :: Property
golden_shelleyNodeKeyGenVrf = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  verificationKey <- noteTempFile tempDir "kes.vkey"
  signingKey <- noteTempFile tempDir "kes.skey"

  void $ execCardanoCLI
    [ "node","key-gen-VRF"
    , "--verification-key-file", verificationKey
    , "--signing-key-file", signingKey
    ]

  H.assertFileOccurences 1 "VRF Verification Key" verificationKey
  H.assertFileOccurences 1 "VRF Signing Key" signingKey

  H.assertEndsWithSingleNewline verificationKey
  H.assertEndsWithSingleNewline signingKey
