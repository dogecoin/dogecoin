{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Genesis.InitialTxIn
  ( golden_shelleyGenesisInitialTxIn
  ) where

import           Cardano.Prelude
import           Hedgehog (Property)
import           Test.OptParse

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

golden_shelleyGenesisInitialTxIn :: Property
golden_shelleyGenesisInitialTxIn = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  verificationKeyFile <- noteInputFile "test/data/golden/shelley/keys/genesis_verification_keys/genesis-utxo.vkey"
  goldenUtxoHashFile <- noteInputFile "test/data/golden/shelley/keys/genesis_utxo_hashes/utxo_hash"
  utxoHashFile <- noteTempFile tempDir "utxo_hash"

  utxoHash <- execCardanoCLI
    [ "genesis","initial-txin"
    , "--testnet-magic", "16"
    , "--verification-key-file", verificationKeyFile
    ]

  H.writeFile utxoHashFile utxoHash

  goldenUtxoHash <- H.readFile goldenUtxoHashFile

  equivalence utxoHash goldenUtxoHash
