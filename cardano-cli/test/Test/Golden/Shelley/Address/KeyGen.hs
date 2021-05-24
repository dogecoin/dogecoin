{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Address.KeyGen
  ( golden_shelleyAddressKeyGen
  ) where

import           Cardano.Prelude
import           Hedgehog (Property)
import           Test.OptParse

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

golden_shelleyAddressKeyGen :: Property
golden_shelleyAddressKeyGen = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  addressVKeyFile <- noteTempFile tempDir "address.vkey"
  addressSKeyFile <- noteTempFile tempDir "address.skey"

  void $ execCardanoCLI
    [ "address","key-gen"
    , "--verification-key-file", addressVKeyFile
    , "--signing-key-file", addressSKeyFile
    ]

  void $ H.readFile addressVKeyFile
  void $ H.readFile addressSKeyFile

  H.assertFileOccurences 1 "PaymentVerificationKeyShelley" addressVKeyFile
  H.assertFileOccurences 1 "PaymentSigningKeyShelley_ed25519" addressSKeyFile

  H.assertEndsWithSingleNewline addressVKeyFile
  H.assertEndsWithSingleNewline addressSKeyFile
