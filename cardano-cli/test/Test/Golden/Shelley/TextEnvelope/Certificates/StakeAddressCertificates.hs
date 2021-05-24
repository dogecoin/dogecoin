{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.TextEnvelope.Certificates.StakeAddressCertificates
  ( golden_shelleyStakeAddressCertificates
  ) where

import           Cardano.Api (AsType (..), HasTextEnvelope (..))
import           Cardano.Prelude
import           Hedgehog (Property)
import           Test.OptParse

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

-- | 1. Generate a stake verification key
--   2. Create a stake address registration certificate
--   3. Check the TextEnvelope serialization format has not changed.
golden_shelleyStakeAddressCertificates :: Property
golden_shelleyStakeAddressCertificates = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  -- Reference files
  referenceRegistrationCertificate <- noteInputFile "test/data/golden/shelley/certificates/stake_address_registration_certificate"
  referenceDeregistrationCertificate <- noteInputFile "test/data/golden/shelley/certificates/stake_address_deregistration_certificate"

  -- Key filepaths
  verKey <- noteTempFile tempDir "stake-verification-key-file"
  signKey <- noteTempFile tempDir "stake-signing-key-file"
  deregistrationCertificate <- noteTempFile tempDir "stake-address-deregistration-certificate"
  registrationCertificate <- noteTempFile tempDir "stake-address-registration-certificate"

  -- Generate stake verification key
  void $ execCardanoCLI
    [ "stake-address","key-gen"
    , "--verification-key-file", verKey
    , "--signing-key-file", signKey
    ]

  H.assertFilesExist [verKey, signKey]

  -- Create stake address registration certificate
  void $ execCardanoCLI
    [ "stake-address","registration-certificate"
    , "--stake-verification-key-file", verKey
    , "--out-file", registrationCertificate
    ]

  let registrationCertificateType = textEnvelopeType AsCertificate

  -- Check the newly created files have not deviated from the
  -- golden files
  checkTextEnvelopeFormat registrationCertificateType referenceRegistrationCertificate registrationCertificate

  -- Create stake address deregistration certificate
  void $ execCardanoCLI
    [ "stake-address","deregistration-certificate"
    , "--stake-verification-key-file", verKey
    , "--out-file", deregistrationCertificate
    ]

  -- Check the newly created files have not deviated from the
  -- golden files
  checkTextEnvelopeFormat registrationCertificateType referenceDeregistrationCertificate deregistrationCertificate

-- TODO: After delegation-certificate command is fixed to take a hash instead of a verfication key
{-
  -- Create stake address delegation certificate
  void $ execCardanoCLI
    [ "stake-address","delegation-certificate"
    , "--stake-verification-key-file", verKey
    , "--cold-verification-key-file", verKey --TODO: Should be stake pool's hash
    , "--out-file", deregistrationCertificate
    ]

  -- Check the newly created files have not deviated from the
  -- golden files
  checkTextEnvelopeFormat registrationCertificateType referenceDeregistrationCertificate deregistrationCertificate
-}
