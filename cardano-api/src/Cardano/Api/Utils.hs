-- | Internal utils for the other Api modules
--
module Cardano.Api.Utils
  ( (?!)
  , (?!.)
  ) where

import           Prelude


(?!) :: Maybe a -> e -> Either e a
Nothing ?! e = Left e
Just x  ?! _ = Right x

(?!.) :: Either e a -> (e -> e') -> Either e' a
Left  e ?!. f = Left (f e)
Right x ?!. _ = Right x
