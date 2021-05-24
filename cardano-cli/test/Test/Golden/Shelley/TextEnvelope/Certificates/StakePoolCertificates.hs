{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.TextEnvelope.Certificates.StakePoolCertificates
  ( golden_shelleyStakePoolCertificates
  ) where

import           Cardano.Api (AsType (..), HasTextEnvelope (..))
import           Cardano.Prelude
import           Hedgehog (Property)
import           Test.OptParse

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

-- | 1. Create cold key pair.
--   2. Create stake key pair.
--   3. Create VRF key pair.
--   4. Create stake pool registration certificate.
--   5. Create stake pool deregistration/retirement certificate.
--   6. Check the TextEnvelope serialization format has not changed.
golden_shelleyStakePoolCertificates :: Property
golden_shelleyStakePoolCertificates = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  -- Reference files
  referenceRegistrationCertificate <- noteInputFile "test/data/golden/shelley/certificates/stake_pool_registration_certificate"
  referenceDeregistrationCertificate <- noteInputFile "test/data/golden/shelley/certificates/stake_pool_deregistration_certificate"

  -- Key filepaths
  coldVerKey <- noteTempFile tempDir "cold-verification-key-file"
  coldSignKey <- noteTempFile tempDir "cold-signing-key-file"
  operationalCertCounter <- noteTempFile tempDir "operational-certificate-counter-file"
  vrfVerKey <- noteTempFile tempDir "vrf-verification-key-file"
  vrfSignKey <- noteTempFile tempDir "vrf-signing-key-file"
  poolRewardAccountAndOwnerVerKey <- noteTempFile tempDir "reward-account-verification-key-file"
  poolRewardAccountSignKey <- noteTempFile tempDir "reward-account-signing-key-file"
  registrationCertificate <- noteTempFile tempDir "stake-pool-registration-certificate"
  deregistrationCertificate <- noteTempFile tempDir "stake-pool-deregistration-certificate"

  -- Create cold key pair
  void $ execCardanoCLI
    [ "node","key-gen"
    , "--cold-verification-key-file", coldVerKey
    , "--cold-signing-key-file", coldSignKey
    , "--operational-certificate-issue-counter", operationalCertCounter
    ]

  H.assertFilesExist [coldSignKey, coldVerKey, operationalCertCounter]

  -- Generate stake key pair
  void $ execCardanoCLI
    [ "stake-address","key-gen"
    , "--verification-key-file", poolRewardAccountAndOwnerVerKey
    , "--signing-key-file", poolRewardAccountSignKey
    ]

  H.assertFilesExist [poolRewardAccountAndOwnerVerKey, poolRewardAccountSignKey]

  -- Generate vrf verification key
  void $ execCardanoCLI
    [ "node","key-gen-VRF"
    , "--verification-key-file", vrfVerKey
    , "--signing-key-file", vrfSignKey
    ]


  H.assertFilesExist [vrfSignKey, vrfVerKey]

  -- Create stake pool registration certificate
  void $ execCardanoCLI
    [ "stake-pool","registration-certificate"
    , "--cold-verification-key-file", coldVerKey
    , "--vrf-verification-key-file", vrfVerKey
    , "--mainnet"
    , "--pool-cost", "1000"
    , "--pool-pledge", "5000"
    , "--pool-margin", "0.1"
    , "--pool-reward-account-verification-key-file", poolRewardAccountAndOwnerVerKey
    , "--pool-owner-stake-verification-key-file", poolRewardAccountAndOwnerVerKey
    , "--out-file", registrationCertificate
    ]

  H.assertFilesExist [registrationCertificate]

  let registrationCertificateType = textEnvelopeType AsCertificate

  -- Check the newly created files have not deviated from the
  -- golden files
  checkTextEnvelopeFormat registrationCertificateType referenceRegistrationCertificate registrationCertificate

  -- Create stake pool deregistration certificate
  void $ execCardanoCLI
    [ "stake-pool", "deregistration-certificate"
    , "--cold-verification-key-file", coldVerKey
    , "--epoch", "42"
    , "--out-file", deregistrationCertificate
    ]

  H.assertFilesExist [deregistrationCertificate]

  -- Check the newly created files have not deviated from the
  -- golden files
  checkTextEnvelopeFormat registrationCertificateType referenceDeregistrationCertificate deregistrationCertificate
