{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.TextEnvelope.Certificates.MIRCertificate
  ( golden_shelleyMIRCertificate
  ) where

import           Cardano.Api (AsType (..), HasTextEnvelope (..))
import           Cardano.Prelude
import           Hedgehog (Property)
import           Test.OptParse

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

-- | 1. Generate stake key pair
--   2. Create MIR certificate
--   s. Check the TextEnvelope serialization format has not changed.
golden_shelleyMIRCertificate :: Property
golden_shelleyMIRCertificate = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  -- Reference keys
  referenceMIRCertificate <- noteInputFile "test/data/golden/shelley/certificates/mir_certificate"

  -- Key filepaths
  verKey <- noteTempFile tempDir "stake-verification-key-file"
  signKey <- noteTempFile tempDir "stake-signing-key-file"
  mirCertificate <- noteTempFile tempDir "mir-certificate-file"

  -- Generate stake key pair
  void $ execCardanoCLI
    [ "stake-address","key-gen"
    , "--verification-key-file", verKey
    , "--signing-key-file", signKey
    ]

  H.assertFilesExist [verKey, signKey]

  let testAddr = "stake1u9j6axhcpd0exvrthn5dqzqt54g85akqvkn4uqmccm70qsc5hpv9w"
  -- Create MIR certificate
  void $ execCardanoCLI
    [ "governance","create-mir-certificate"
    , "--reserves" --TODO: Should also do "--reserves"
    , "--stake-address", testAddr
    , "--reward", "1000"
    , "--out-file", mirCertificate
    ]

  H.assertFilesExist [mirCertificate]

  let registrationCertificateType = textEnvelopeType AsCertificate

  checkTextEnvelopeFormat registrationCertificateType referenceMIRCertificate mirCertificate
