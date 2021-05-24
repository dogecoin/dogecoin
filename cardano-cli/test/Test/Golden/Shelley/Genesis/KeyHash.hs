{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Genesis.KeyHash
  ( golden_shelleyGenesisKeyHash
  ) where

import           Cardano.Prelude
import           Hedgehog (Property, (===))
import           Test.OptParse as OP

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

golden_shelleyGenesisKeyHash :: Property
golden_shelleyGenesisKeyHash = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  referenceVerificationKey <- noteInputFile "test/data/golden/shelley/keys/genesis_keys/verification_key"
  goldenGenesisVerificationKeyHashFile <- noteInputFile "test/data/golden/shelley/keys/genesis_keys/verification_key.key-hash"
  genesisVerificationKeyHashFile <- noteTempFile tempDir "key-hash.hex"

  genesisVerificationKeyHash <- execCardanoCLI
    [ "genesis","key-hash"
    , "--verification-key-file", referenceVerificationKey
    ]

  H.writeFile genesisVerificationKeyHashFile genesisVerificationKeyHash

  goldenGenesisVerificationKeyHash <- H.readFile goldenGenesisVerificationKeyHashFile

  genesisVerificationKeyHash === goldenGenesisVerificationKeyHash
