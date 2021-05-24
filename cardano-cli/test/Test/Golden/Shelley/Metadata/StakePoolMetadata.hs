{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Metadata.StakePoolMetadata
  ( golden_stakePoolMetadataHash
  ) where

import           Cardano.Prelude
import           Hedgehog (Property)
import           Test.OptParse as OP

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

golden_stakePoolMetadataHash :: Property
golden_stakePoolMetadataHash = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  referenceStakePoolMetadata <- noteInputFile "test/data/golden/shelley/metadata/stake_pool_metadata_hash"

  stakePoolMetadataFile <- noteTempFile tempDir "stake-pool-metadata.json"
  outputStakePoolMetadataHashFp <- noteTempFile tempDir "stake-pool-metadata-hash.txt"

  -- Write the example stake pool metadata to disk
  liftIO $ writeFile stakePoolMetadataFile exampleStakePoolMetadata

  -- Hash the stake pool metadata
  void $ execCardanoCLI
    [ "stake-pool","metadata-hash"
    , "--pool-metadata-file", stakePoolMetadataFile
    , "--out-file", outputStakePoolMetadataHashFp
    ]

  -- Check that the stake pool metadata hash file content is correct.
  expectedStakePoolMetadataHash <- H.readFile referenceStakePoolMetadata
  actualStakePoolMetadataHash <- H.readFile outputStakePoolMetadataHashFp

  equivalence expectedStakePoolMetadataHash actualStakePoolMetadataHash
  where
    exampleStakePoolMetadata :: Text
    exampleStakePoolMetadata = "{\"homepage\":\"https://iohk.io\",\"name\":\"Genesis Pool C\",\"ticker\":\"GPC\",\"description\":\"Lorem Ipsum Dolor Sit Amet.\"}"
