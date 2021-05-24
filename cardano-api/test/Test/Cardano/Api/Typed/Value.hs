{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Api.Typed.Value
  ( tests
  ) where

import           Prelude

import           Data.Aeson
import           Data.List (groupBy, sort)
import qualified Data.Map.Strict as Map

import           Cardano.Api.Shelley

import           Hedgehog (Property, discover, forAll, property, tripping, (===))
import           Test.Cardano.Api.Typed.Gen

import           Test.Tasty (TestTree)
import           Test.Tasty.Hedgehog.Group (fromGroup)

{- HLINT ignore "Use map once" -}

prop_roundtrip_Value_JSON :: Property
prop_roundtrip_Value_JSON =
  property $ do v <- forAll genValueDefault
                tripping v encode eitherDecode


prop_roundtrip_Value_flatten_unflatten :: Property
prop_roundtrip_Value_flatten_unflatten =
  property $ do v <- forAll genValueDefault
                valueFromNestedRep (valueToNestedRep v) === v

prop_roundtrip_Value_unflatten_flatten :: Property
prop_roundtrip_Value_unflatten_flatten =
    property $ do
      v <- forAll genValueNestedRep
      canonicalise v === valueToNestedRep (valueFromNestedRep v)

canonicalise :: ValueNestedRep -> ValueNestedRep
canonicalise =
    ValueNestedRep
  . filter (not . isZeroOrEmpty)
  . map filterZeros
  . map (foldl1 mergeBundle)
  . groupBy samePolicyId
  . sort
  . (\(ValueNestedRep bundles) -> bundles)
  where
    samePolicyId ValueNestedBundleAda{}
                 ValueNestedBundleAda{} = True
    samePolicyId (ValueNestedBundle pid _)
                 (ValueNestedBundle pid' _) = pid == pid'
    samePolicyId _ _ = False

    -- Merge together bundles that have already been grouped by same PolicyId:
    mergeBundle (ValueNestedBundleAda q)
                (ValueNestedBundleAda q') =
      ValueNestedBundleAda (q <> q')

    mergeBundle (ValueNestedBundle pid  as)
                (ValueNestedBundle pid' as') | pid == pid' =
      ValueNestedBundle pid (Map.unionWith (<>) as as')

    mergeBundle _ _ = error "canonicalise.mergeBundle: impossible"

    filterZeros b@ValueNestedBundleAda{} = b
    filterZeros (ValueNestedBundle pid as) =
      ValueNestedBundle pid (Map.filter (/=0) as)

    isZeroOrEmpty (ValueNestedBundleAda q) = q == 0
    isZeroOrEmpty (ValueNestedBundle _ as) = Map.null as

-- -----------------------------------------------------------------------------

tests :: TestTree
tests = fromGroup $$discover
