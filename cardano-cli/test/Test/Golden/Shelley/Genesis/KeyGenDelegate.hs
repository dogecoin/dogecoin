{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Genesis.KeyGenDelegate
  ( golden_shelleyGenesisKeyGenDelegate
  ) where

import           Cardano.Prelude
import           Hedgehog (Property)
import           Test.OptParse

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

golden_shelleyGenesisKeyGenDelegate :: Property
golden_shelleyGenesisKeyGenDelegate = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  verificationKeyFile <- noteTempFile tempDir "key-gen.vkey"
  signingKeyFile <- noteTempFile tempDir "key-gen.skey"
  operationalCertificateIssueCounterFile <- noteTempFile tempDir "op-cert.counter"

  void $ execCardanoCLI
    [ "genesis","key-gen-delegate"
    , "--verification-key-file", verificationKeyFile
    , "--signing-key-file", signingKeyFile
    , "--operational-certificate-issue-counter", operationalCertificateIssueCounterFile
    ]

  H.assertFileOccurences 1 "GenesisDelegateVerificationKey_ed25519" verificationKeyFile
  H.assertFileOccurences 1 "GenesisDelegateSigningKey_ed25519" signingKeyFile
  H.assertFileOccurences 1 "NodeOperationalCertificateIssueCounter" operationalCertificateIssueCounterFile

  H.assertFileOccurences 1 "Genesis delegate operator key" verificationKeyFile
  H.assertFileOccurences 1 "Genesis delegate operator key" signingKeyFile

  H.assertEndsWithSingleNewline verificationKeyFile
  H.assertEndsWithSingleNewline signingKeyFile
  H.assertEndsWithSingleNewline operationalCertificateIssueCounterFile
