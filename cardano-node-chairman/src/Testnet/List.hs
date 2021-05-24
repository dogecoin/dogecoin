module Testnet.List
  ( dropNth
  ) where

import           Data.Int
import           GHC.Num

-- | Drop the zero-based n-th element from the list.
dropNth :: Int -> [a] -> [a]
dropNth _ [] = []
dropNth 0 (_:as) = as
dropNth i (a:as) = a:dropNth (i - 1) as
