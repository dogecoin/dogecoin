{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.HasTypeProxy
  ( HasTypeProxy(AsType, proxyToAsType)
  , Proxy(..)
  , FromSomeType(..)
  ) where

import           Data.Proxy (Proxy (..))
import           Data.Kind (Type, Constraint)


class HasTypeProxy t where
  -- | A family of singleton types used in this API to indicate which type to
  -- use where it would otherwise be ambiguous or merely unclear.
  --
  -- Values of this type are passed to deserialisation functions for example.
  --
  data AsType t

  proxyToAsType :: Proxy t -> AsType t


data FromSomeType (c :: Type -> Constraint) b where
     FromSomeType :: c a => AsType a -> (a -> b) -> FromSomeType c b

