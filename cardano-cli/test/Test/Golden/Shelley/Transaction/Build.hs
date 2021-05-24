{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Transaction.Build
  ( golden_shelleyTransactionBuild
  , golden_shelleyTransactionBuild_TxInScriptWitnessed
  , golden_shelleyTransactionBuild_Minting
  , golden_shelleyTransactionBuild_CertificateScriptWitnessed
  , golden_shelleyTransactionBuild_WithdrawalScriptWitnessed
  ) where

import           Cardano.Prelude
import           Prelude (String)

import           Hedgehog (Property)
import           Test.OptParse

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

txOut :: String
txOut = "addr1q94cxl99qvtwunsqqv6g9mgj3zrawtpt4edsgwxkjtwpy5dsezcht90tmwfur7t5hc9fk8hjd3r5vjwec2h8vmk3xh8s7er7t3+100"

txIn :: String
txIn = "2392d2b1200b5139fe555c81261697b29a8ccf561c5c783d46e78a479d977053#0"

golden_shelleyTransactionBuild :: Property
golden_shelleyTransactionBuild =
  propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    txBodyOutFile <- noteTempFile tempDir "tx-body-out"

    void $ execCardanoCLI
      [ "transaction","build-raw"
      , "--tx-in", txIn
      , "--tx-out", txOut
      , "--fee", "12"
      , "--tx-body-file", txBodyOutFile
      ]

    H.assertFileOccurences 1 "TxBodyMary" txBodyOutFile

    H.assertEndsWithSingleNewline txBodyOutFile


golden_shelleyTransactionBuild_CertificateScriptWitnessed :: Property
golden_shelleyTransactionBuild_CertificateScriptWitnessed =
  propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    let deregcert = "test/data/golden/shelley/certificates/stake_address_deregistration_certificate"
        scriptWit = "test/data/golden/shelley/multisig/scripts/any"

    txBodyOutFile <- noteTempFile tempDir "tx-body-out"

    void $ execCardanoCLI
      [ "transaction","build-raw"
      , "--tx-in", txIn
      , "--tx-out", txOut
      , "--certificate-file", deregcert, "--certificate-script-file", scriptWit
      , "--fee", "12"
      , "--tx-body-file", txBodyOutFile
      ]

    H.assertFileOccurences 1 "TxBodyMary" txBodyOutFile

    H.assertEndsWithSingleNewline txBodyOutFile

golden_shelleyTransactionBuild_Minting :: Property
golden_shelleyTransactionBuild_Minting =
  propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    let scriptWit = "test/data/golden/shelley/multisig/scripts/any"

    polid <- execCardanoCLI
               [ "transaction"
               , "policyid"
               , "--script-file"
               , scriptWit
               ]

    let dummyMA = filter (/= '\n') $ "50 " ++ polid ++ ".ethereum"

    txBodyOutFile <- noteTempFile tempDir "tx-body-out"

    void $ execCardanoCLI
      [ "transaction","build-raw"
      , "--tx-in", txIn
      , "--tx-out", txOut ++ "+" ++ dummyMA, "--minting-script-file", scriptWit
      , "--mint", dummyMA
      , "--fee", "12"
      , "--tx-body-file", txBodyOutFile
      ]

    H.assertFileOccurences 1 "TxBodyMary" txBodyOutFile

    H.assertEndsWithSingleNewline txBodyOutFile

golden_shelleyTransactionBuild_WithdrawalScriptWitnessed :: Property
golden_shelleyTransactionBuild_WithdrawalScriptWitnessed =
  propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    txBodyOutFile <- noteTempFile tempDir "tx-body-out"

    stakeAddress <- H.readFile "test/data/golden/shelley/keys/stake_keys/reward_address"

    let withdrawal = filter (/= '\n') $ stakeAddress <> "+100"
        scriptWit = "test/data/golden/shelley/multisig/scripts/any"

    void $ execCardanoCLI
      [ "transaction","build-raw"
      , "--tx-in", txIn
      , "--tx-out", txOut
      , "--withdrawal", withdrawal, "--withdrawal-script-file", scriptWit
      , "--fee", "12"
      , "--tx-body-file", txBodyOutFile
      ]

    H.assertFileOccurences 1 "TxBodyMary" txBodyOutFile

    H.assertEndsWithSingleNewline txBodyOutFile

golden_shelleyTransactionBuild_TxInScriptWitnessed :: Property
golden_shelleyTransactionBuild_TxInScriptWitnessed =
  propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    let scriptWit = "test/data/golden/shelley/multisig/scripts/any"

    txBodyOutFile <- noteTempFile tempDir "tx-body-out"

    void $ execCardanoCLI
      [ "transaction","build-raw"
      , "--tx-in", txIn, "--txin-script-file", scriptWit
      , "--tx-out", txOut
      , "--fee", "12"
      , "--tx-body-file", txBodyOutFile
      ]

    H.assertFileOccurences 1 "TxBodyMary" txBodyOutFile

    H.assertEndsWithSingleNewline txBodyOutFile


