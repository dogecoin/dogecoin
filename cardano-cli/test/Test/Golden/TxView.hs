{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.TxView (txViewTests) where

import           Cardano.Prelude

import           Hedgehog (Group (..), Property, checkSequential)
import           Hedgehog.Extras.Test.Base (moduleWorkspace, propertyOnce)

import           Test.OptParse (execCardanoCLI, noteTempFile)
import           Test.Utilities (diffVsGoldenFile)

{- HLINT ignore "Use camelCase" -}

txViewTests :: IO Bool
txViewTests =
  checkSequential $
    Group "`transaction view` Goldens"
      [ ("golden_view_shelley", golden_view_shelley)
      , ("golden_view_allegra", golden_view_allegra)
      , ("golden_view_mary",    golden_view_mary)
      ]

golden_view_shelley :: Property
golden_view_shelley =
  propertyOnce $
  moduleWorkspace "tmp" $ \tempDir -> do
    transactionBodyFile <- noteTempFile tempDir "transaction-body-file"

    -- Create transaction body
    void $
      execCardanoCLI
        [ "transaction", "build-raw"
        , "--shelley-era"
        , "--tx-in"
        ,   "fe5dd07fb576bff960d6e066eade5b26cdb5afebe29f76ea58d0a098bce5d891\
            \#29"
        , "--tx-out"
        ,   "addr1v9wmu83pzajplrtpsq6tsqdgwr98x888trpmah2u0ezznsge7del3+31"
        , "--fee", "32"
        , "--invalid-hereafter", "33"
        , "--out-file", transactionBodyFile
        ]

    -- View transaction body
    result <-
      execCardanoCLI
        ["transaction", "view", "--tx-body-file", transactionBodyFile]
    diffVsGoldenFile result "test/data/golden/shelley/transaction-view.out"

golden_view_allegra :: Property
golden_view_allegra =
  propertyOnce $
  moduleWorkspace "tmp" $ \tempDir -> do
    transactionBodyFile <- noteTempFile tempDir "transaction-body-file"

    -- Create transaction body
    void $
      execCardanoCLI
        [ "transaction", "build-raw"
        , "--allegra-era"
        , "--tx-in"
        ,   "fe5dd07fb576bff960d6e066eade5b26cdb5afebe29f76ea58d0a098bce5d891\
            \#94"
        , "--tx-out"
        ,   "addr_test1qrefnr4k09pvge6dq83v6s67ruter8sftmky8qrmkqqsxy7q5psgn8tgqmupq4r79jmxlyk4eqt6z6hj5g8jd8393msqaw47f4\
            \+99"
        , "--fee", "100"
        , "--invalid-hereafter", "101"
        , "--out-file", transactionBodyFile
        ]

    -- View transaction body
    result <-
      execCardanoCLI
        ["transaction", "view", "--tx-body-file", transactionBodyFile]
    diffVsGoldenFile result "test/data/golden/allegra/transaction-view.out"

golden_view_mary :: Property
golden_view_mary =
  propertyOnce $
  moduleWorkspace "tmp" $ \tempDir -> do
    transactionBodyFile <- noteTempFile tempDir "transaction-body-file"

    -- Create transaction body
    void $
      execCardanoCLI
        [ "transaction", "build-raw"
        , "--mary-era"
        , "--tx-in"
        ,   "fe5dd07fb576bff960d6e066eade5b26cdb5afebe29f76ea58d0a098bce5d891\
            \#135"
        , "--tx-out"
        ,   "addr_test1qrefnr4k09pvge6dq83v6s67ruter8sftmky8qrmkqqsxy7q5psgn8tgqmupq4r79jmxlyk4eqt6z6hj5g8jd8393msqaw47f4\
            \+138"
        , "--fee", "139"
        , "--invalid-before", "140"
        , "--mint"
        ,   "142 69596718df8203759c0f9e86f3f79d1dd45bc9d34109a4fccc824e02"
        , "--minting-script-file", "test/data/golden/shelley/multisig/scripts/any"
        , "--out-file", transactionBodyFile
        ]

    -- View transaction body
    result <-
      execCardanoCLI
        ["transaction", "view", "--tx-body-file", transactionBodyFile]
    diffVsGoldenFile result "test/data/golden/mary/transaction-view.out"
