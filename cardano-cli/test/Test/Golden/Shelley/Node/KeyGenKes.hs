{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Node.KeyGenKes
  ( golden_shelleyNodeKeyGenKes
  ) where

import           Cardano.Prelude
import           Hedgehog (Property)
import           Test.OptParse

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

golden_shelleyNodeKeyGenKes :: Property
golden_shelleyNodeKeyGenKes = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  verificationKey <- noteTempFile tempDir "kes.vkey"
  signingKey <- noteTempFile tempDir "kes.skey"

  void $ execCardanoCLI
    [ "node","key-gen-KES"
    , "--verification-key-file", verificationKey
    , "--signing-key-file", signingKey
    ]

  H.assertFileOccurences 1 "KesVerificationKey_ed25519_kes_2^6" verificationKey
  H.assertFileOccurences 1 "KesSigningKey_ed25519_kes_2^6" signingKey

  H.assertEndsWithSingleNewline verificationKey
  H.assertEndsWithSingleNewline signingKey
