{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Transaction.Assemble
  ( golden_shelleyTransactionAssembleWitness_SigningKey
  ) where

import           Cardano.Prelude
import           Hedgehog (Property)
import           Test.OptParse

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

-- Check that we can assemble a txbody and a tx witness to form a transaction

golden_shelleyTransactionAssembleWitness_SigningKey :: Property
golden_shelleyTransactionAssembleWitness_SigningKey = propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  witnessTx <- noteTempFile tempDir "single-signing-key-witness-tx"
  txBodyFile <- noteInputFile "test/data/golden/shelley/tx/txbody"
  signingKeyWitnessFile <- noteInputFile "test/data/golden/shelley/witnesses/singleSigningKeyWitness"
  void $ execCardanoCLI
    [ "transaction","sign-witness"
    , "--tx-body-file", txBodyFile
    , "--witness-file", signingKeyWitnessFile
    , "--witness-file", signingKeyWitnessFile
    , "--out-file", witnessTx
    ]

  H.assertFileOccurences 1 "Tx MaryEra" witnessTx
