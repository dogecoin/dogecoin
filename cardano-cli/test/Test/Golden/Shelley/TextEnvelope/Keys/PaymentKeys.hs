{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.TextEnvelope.Keys.PaymentKeys
  ( golden_shelleyPaymentKeys
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
golden_shelleyPaymentKeys :: Property
golden_shelleyPaymentKeys = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  -- Reference keys
  referenceVerKey <- noteInputFile "test/data/golden/shelley/keys/payment_keys/verification_key"
  referenceSignKey <- noteInputFile "test/data/golden/shelley/keys/payment_keys/signing_key"

  -- Key filepaths
  verKey <- noteTempFile tempDir "payment-verification-key-file"
  signKey <- noteTempFile tempDir "payment-signing-key-file"

  -- Generate payment verification key
  void $ execCardanoCLI
    [ "address","key-gen"
    , "--verification-key-file", verKey
    , "--signing-key-file", signKey
    ]

  let signingKeyType = textEnvelopeType (AsSigningKey AsPaymentKey)
      verificationKeyType = textEnvelopeType (AsVerificationKey AsPaymentKey)

  -- Check the newly created files have not deviated from the
  -- golden files
  checkTextEnvelopeFormat verificationKeyType referenceVerKey verKey
  checkTextEnvelopeFormat signingKeyType referenceSignKey signKey
