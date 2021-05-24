{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cli.MultiAssetParsing where

import           Cardano.Prelude hiding (filter)

import qualified Data.Text as Text
import qualified Text.Parsec as Parsec (parse)

import           Hedgehog (Property, checkSequential, discover, forAll, property, tripping)
import           Hedgehog.Gen (filter)

import           Cardano.Api (valueToList)
import           Cardano.CLI.Mary.RenderValue (defaultRenderPrettyValueOptions,
                     defaultRenderValueOptions, renderPrettyValue, renderValue)
import           Cardano.CLI.Mary.ValueParser (parseValue)

import           Test.Cardano.Api.Typed.Gen (genValueDefault)

prop_roundtrip_Value_parse_render :: Property
prop_roundtrip_Value_parse_render =
  property $ do
    value <- forAll $ filter (not . null . valueToList) genValueDefault
    tripping
      value
      (renderValue defaultRenderValueOptions)
      (Parsec.parse parseValue "" . Text.unpack)

prop_roundtrip_Value_parse_renderPretty :: Property
prop_roundtrip_Value_parse_renderPretty =
  property $ do
    value <- forAll $ filter (not . null . valueToList) genValueDefault
    tripping
      value
      (renderPrettyValue defaultRenderPrettyValueOptions)
      (Parsec.parse parseValue "" . Text.unpack)

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  checkSequential $$discover
