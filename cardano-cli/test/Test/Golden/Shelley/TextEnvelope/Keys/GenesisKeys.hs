{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.TextEnvelope.Keys.GenesisKeys
  ( golden_shelleyGenesisKeys
  ) where

import           Cardano.Api (AsType (..), HasTextEnvelope (..))
import           Cardano.Prelude
import           Hedgehog (Property)
import           Test.OptParse

import qualified Hedgehog.Extras.Test.Base as H

{- HLINT ignore "Use camelCase" -}

-- | 1. Generate a key pair
--   2. Check for the existence of the key pair
--   3. Check the TextEnvelope serialization format has not changed
golden_shelleyGenesisKeys :: Property
golden_shelleyGenesisKeys = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  -- Reference keys
  referenceVerKey <- noteInputFile "test/data/golden/shelley/keys/genesis_keys/verification_key"
  referenceSignKey <- noteInputFile "test/data/golden/shelley/keys/genesis_keys/signing_key"

  -- Key filepaths
  verKey <- noteTempFile tempDir "genesis-verification-key-file"
  signKey <- noteTempFile tempDir "genesis-signing-key-file"

  -- Generate payment verification key
  void $ execCardanoCLI
    [ "genesis","key-gen-genesis"
    , "--verification-key-file", verKey
    , "--signing-key-file", signKey
    ]

  let signingKeyType = textEnvelopeType (AsSigningKey AsGenesisKey)
      verificationKeyType = textEnvelopeType (AsVerificationKey AsGenesisKey)

  -- Check the newly created files have not deviated from the
  -- golden files
  checkTextEnvelopeFormat verificationKeyType referenceVerKey verKey
  checkTextEnvelopeFormat signingKeyType referenceSignKey signKey
