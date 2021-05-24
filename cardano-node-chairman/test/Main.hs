{-# LANGUAGE FlexibleInstances #-}

module Main
  ( main
  ) where

import Prelude

import qualified System.Environment as E
import qualified Test.Tasty as T
import qualified Test.Tasty.Ingredients as T
import qualified Test.Tasty.Hedgehog as H

import qualified Spec.Network
import qualified Spec.Chairman.Byron
import qualified Spec.Chairman.ByronShelley
import qualified Spec.Chairman.Shelley

tests :: IO T.TestTree
tests = do
  let t0 = H.testProperty "isPortOpen False" Spec.Network.hprop_isPortOpen_False
  let t1 = H.testProperty "isPortOpen True" Spec.Network.hprop_isPortOpen_True
  let t2 = H.testProperty "chairman" Spec.Chairman.Byron.hprop_chairman
  let t3 = H.testProperty "chairman" Spec.Chairman.ByronShelley.hprop_chairman
  let t4 = H.testProperty "chairman" Spec.Chairman.Shelley.hprop_chairman

  pure $ T.testGroup "test/Spec.hs"
    [ T.testGroup "Spec"
      [ T.testGroup "Chairman"
        [ T.testGroup "Byron" [t2]
        , T.testGroup "ByronShelley" [t3]
        , T.testGroup "Shelley" [t4]
        ]
      , T.testGroup "Network" [t0, t1]
      ]
    ]

ingredients :: [T.Ingredient]
ingredients = T.defaultIngredients

main :: IO ()
main = do
  args <- E.getArgs

  E.withArgs args $ tests >>= T.defaultMainWithIngredients ingredients
