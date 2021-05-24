{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Transaction.CalculateMinFee
  ( golden_shelleyTransactionCalculateMinFee
  ) where

import           Cardano.Prelude
import           Hedgehog (Property)
import           Test.OptParse

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

golden_shelleyTransactionCalculateMinFee :: Property
golden_shelleyTransactionCalculateMinFee = propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  protocolParamsJsonFile <- noteInputFile "test/data/golden/shelley/transaction-calculate-min-fee/protocol-params.json"
  txBodyFile <- noteInputFile "test/data/golden/shelley/tx/txbody"
  minFeeTxtFile <- noteTempFile tempDir "min-fee.txt"

  minFeeTxt <- execCardanoCLI
    [ "transaction","calculate-min-fee"
    , "--tx-in-count", "32"
    , "--tx-out-count", "27"
    , "--byron-witness-count", "5"
    , "--witness-count", "10"
    , "--testnet-magic", "4036000900"
    , "--protocol-params-file", protocolParamsJsonFile
    , "--tx-body-file", txBodyFile
    ]

  H.writeFile minFeeTxtFile minFeeTxt

  H.assertFileOccurences 1 "5083100" minFeeTxtFile
  H.assertFileLines (== 1) minFeeTxtFile
  H.assertEndsWithSingleNewline minFeeTxtFile
