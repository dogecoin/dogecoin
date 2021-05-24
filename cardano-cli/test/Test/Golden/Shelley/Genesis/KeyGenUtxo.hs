{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Genesis.KeyGenUtxo
  ( golden_shelleyGenesisKeyGenUtxo
  ) where

import           Cardano.Prelude
import           Hedgehog (Property)
import           Test.OptParse

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

golden_shelleyGenesisKeyGenUtxo :: Property
golden_shelleyGenesisKeyGenUtxo = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  utxoVerificationKeyFile <- noteTempFile tempDir "utxo.vkey"
  utxoSigningKeyFile <- noteTempFile tempDir "utxo.skey"

  void $ execCardanoCLI
    [ "genesis","key-gen-utxo"
    , "--verification-key-file", utxoVerificationKeyFile
    , "--signing-key-file", utxoSigningKeyFile
    ]

  H.assertFileOccurences 1 "GenesisUTxOVerificationKey_ed25519" utxoVerificationKeyFile
  H.assertFileOccurences 1 "GenesisUTxOSigningKey_ed25519" utxoSigningKeyFile

  H.assertEndsWithSingleNewline utxoVerificationKeyFile
  H.assertEndsWithSingleNewline utxoSigningKeyFile
