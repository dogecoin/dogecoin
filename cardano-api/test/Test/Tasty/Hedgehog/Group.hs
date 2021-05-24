module Test.Tasty.Hedgehog.Group
  ( fromGroup
  )
where

import           Prelude

import           Hedgehog (Group (..), Property)
import           Hedgehog.Internal.Property (GroupName (..), PropertyName (..))
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

fromGroup :: Group -> TestTree
fromGroup group =
    testGroup (unGroupName $ groupName group) $
      map mkTestTree (groupProperties group)
  where
    mkTestTree :: (PropertyName, Property) -> TestTree
    mkTestTree (propName, prop) = testProperty (unPropertyName propName) prop
