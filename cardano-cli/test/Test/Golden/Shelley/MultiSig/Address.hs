{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.MultiSig.Address
  ( golden_shelleyAllMultiSigAddressBuild
  , golden_shelleyAnyMultiSigAddressBuild
  , golden_shelleyAtLeastMultiSigAddressBuild
  ) where

import           Cardano.Prelude
import           Hedgehog (Property)
import           Test.OptParse as OP

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

golden_shelleyAllMultiSigAddressBuild :: Property
golden_shelleyAllMultiSigAddressBuild = propertyOnce . H.moduleWorkspace "tmp" $ \_ -> do
  allMultiSigFp <- noteInputFile "test/data/golden/shelley/multisig/scripts/all"

  allMultiSigAddress <- execCardanoCLI
    [ "address", "build-script"
    , "--script-file", allMultiSigFp
    , "--mainnet"
    ]

  goldenAllMultiSigAddrFp <- noteInputFile "test/data/golden/shelley/multisig/addresses/all"

  goldenAllMs <- H.readFile goldenAllMultiSigAddrFp

  equivalence allMultiSigAddress goldenAllMs

golden_shelleyAnyMultiSigAddressBuild :: Property
golden_shelleyAnyMultiSigAddressBuild = propertyOnce . H.moduleWorkspace "tmp" $ \_ -> do
  anyMultiSigFp <- noteInputFile "test/data/golden/shelley/multisig/scripts/any"

  anyMultiSigAddress <- execCardanoCLI
    [ "address", "build-script"
    , "--script-file", anyMultiSigFp
    , "--mainnet"
    ]

  goldenAnyMultiSigAddrFp <- noteInputFile "test/data/golden/shelley/multisig/addresses/any"

  goldenAnyMs <- H.readFile goldenAnyMultiSigAddrFp

  equivalence anyMultiSigAddress goldenAnyMs

golden_shelleyAtLeastMultiSigAddressBuild :: Property
golden_shelleyAtLeastMultiSigAddressBuild = propertyOnce . H.moduleWorkspace "tmp" $ \_ -> do
  atLeastMultiSigFp <- noteInputFile "test/data/golden/shelley/multisig/scripts/atleast"

  atLeastMultiSigAddress <- execCardanoCLI
    [ "address", "build-script"
    , "--script-file", atLeastMultiSigFp
    , "--mainnet"
    ]

  goldenAtLeastMultiSigAddrFp <- noteInputFile "test/data/golden/shelley/multisig/addresses/atleast"

  goldenAtLeastMs <- H.readFile goldenAtLeastMultiSigAddrFp

  equivalence atLeastMultiSigAddress goldenAtLeastMs
