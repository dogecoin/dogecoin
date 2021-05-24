{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Node.KeyGen
  ( golden_shelleyNodeKeyGen
  ) where

import           Cardano.Prelude
import           Hedgehog (Property)
import           Test.OptParse

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

golden_shelleyNodeKeyGen :: Property
golden_shelleyNodeKeyGen = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  verificationKeyFile <- noteTempFile tempDir "key-gen.vkey"
  signingKeyFile <- noteTempFile tempDir "key-gen.skey"
  opCertCounterFile <- noteTempFile tempDir "op-cert.counter"

  void $ execCardanoCLI
    [ "node","key-gen"
    , "--verification-key-file", verificationKeyFile
    , "--signing-key-file", signingKeyFile
    , "--operational-certificate-issue-counter", opCertCounterFile
    ]

  H.assertFileOccurences 1 "StakePoolVerificationKey_ed25519" verificationKeyFile
  H.assertFileOccurences 1 "StakePoolSigningKey_ed25519" signingKeyFile
  H.assertFileOccurences 1 "NodeOperationalCertificateIssueCounter" opCertCounterFile

  H.assertEndsWithSingleNewline verificationKeyFile
  H.assertEndsWithSingleNewline signingKeyFile
  H.assertEndsWithSingleNewline opCertCounterFile
