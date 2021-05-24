{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.TextEnvelope.Keys.StakeKeys
  ( golden_shelleyStakeKeys
  ) where

import           Cardano.Api (AsType (..), HasTextEnvelope (..))
import           Cardano.Prelude
import           Hedgehog (Property)
import           Test.OptParse

import qualified Hedgehog.Extras.Test.Base as H

{- HLINT ignore "Use camelCase" -}

-- | 1. Generate a key pair
--   2. Check for the existence of the key pair
--   3. Check the TextEnvelope serialization format has not changed.
golden_shelleyStakeKeys :: Property
golden_shelleyStakeKeys = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  -- Reference keys
  referenceVerKey <- noteInputFile "test/data/golden/shelley/keys/stake_keys/verification_key"
  referenceSignKey <- noteInputFile "test/data/golden/shelley/keys/stake_keys/signing_key"

  -- Key filepaths
  verKey <- noteTempFile tempDir "stake-verification-key-file"
  signKey <- noteTempFile tempDir "stake-signing-key-file"

  -- Generate stake key pair
  void $ execCardanoCLI
    [ "stake-address","key-gen"
    , "--verification-key-file", verKey
    , "--signing-key-file", signKey
    ]

  let signingKeyType = textEnvelopeType (AsSigningKey AsStakeKey)
      verificationKeyType = textEnvelopeType (AsVerificationKey AsStakeKey)

  -- Check the newly created files have not deviated from the
  -- golden files
  checkTextEnvelopeFormat verificationKeyType referenceVerKey verKey
  checkTextEnvelopeFormat signingKeyType referenceSignKey signKey
