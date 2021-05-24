{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Api.Gen
  ( genMetadata
  ) where


import           Cardano.Prelude

import qualified Data.Map.Strict as Map

import           Shelley.Spec.Ledger.Metadata (Metadata (..), Metadatum (..))

import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

genMetadata :: Gen (Metadata era)
genMetadata = do
  numberOfIndicies <- Gen.integral (Range.linear 1 15)
  let indexes = map (\i -> fromIntegral i :: Word64) [1..numberOfIndicies]
  mDatums <- Gen.list (Range.singleton numberOfIndicies) genMetadatum
  return . Metadata . Map.fromList $ zip indexes mDatums

genMetadatum :: Gen Metadatum
genMetadatum = do
    int <- Gen.list (Range.linear 1 5) (I <$> Gen.integral (Range.linear 1 100))
    bytes <- Gen.list (Range.linear 1 5) (B <$> Gen.bytes (Range.linear 1 20))
    str <- Gen.list (Range.linear 1 5) (S <$> Gen.text (Range.linear 1 20) Gen.alphaNum)
    let mDatumList = int ++ bytes ++ str

    singleMetadatum <- Gen.element mDatumList

    Gen.choice
      [ return $ List mDatumList
      , return $ Map [(singleMetadatum, singleMetadatum)]
      , return $ Map [(List mDatumList, singleMetadatum)]
      , return $ Map [(singleMetadatum, List mDatumList)]
      ]
