{-# LANGUAGE OverloadedStrings #-}

module Test.Cli.Pioneers.Exercise2
  ( tests
  ) where

import           Cardano.Prelude
import           Hedgehog (Property)
import           Test.OptParse

import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

-- | 1. We generate a payment signing key
--   2. We create a tx body
--   3. We sign the tx body with the generated payment signing key
prop_createTransaction :: Property
prop_createTransaction = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  -- Key filepaths
  paymentVerKey <- noteTempFile tempDir "payment-verification-key-file"
  paymentSignKey <- noteTempFile tempDir "payment-signing-key-file"
  transactionBodyFile <- noteTempFile tempDir "transaction-body"
  transactionFile <- noteTempFile tempDir "transaction-file"

  -- Generate payment signing key to sign transaction
  void $ execCardanoCLI
    [ "address","key-gen"
    , "--verification-key-file", paymentVerKey
    , "--signing-key-file", paymentSignKey
    ]

  H.assertFilesExist [paymentVerKey, paymentSignKey]

  -- Create transaction body
  void $ execCardanoCLI
    [ "transaction", "build-raw"
    , "--tx-in", "91999ea21177b33ebe6b8690724a0c026d410a11ad7521caa350abdafa5394c3#0"
    , "--auxiliary-script-file", "test/data/golden/shelley/multisig/scripts/all"
    , "--tx-in", "91999ea21177b33ebe6b8690724a0c026d410a11ad7521caa350abdafa5394c3#0"
    , "--auxiliary-script-file", "test/data/golden/shelley/multisig/scripts/all"
    , "--tx-out", "addr1v9wmu83pzajplrtpsq6tsqdgwr98x888trpmah2u0ezznsge7del3+100000000"
    , "--fee", "1000000"
    , "--invalid-hereafter", "500000"
    , "--out-file", transactionBodyFile
    ]

  H.assertFilesExist [transactionBodyFile]

  -- Sign transaction
  void $ execCardanoCLI
    [ "transaction", "sign"
    , "--tx-body-file", transactionBodyFile
    , "--signing-key-file", paymentSignKey
    , "--mainnet"
    , "--out-file", transactionFile
    ]

  H.assertFilesExist [paymentVerKey, paymentSignKey, transactionBodyFile, transactionFile]

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  H.checkSequential
    $ H.Group "Pioneers Example 2"
        [ ("prop_createTransaction", prop_createTransaction)
        ]
