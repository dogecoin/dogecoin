{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.Hash
  ( Hash
  , CastHash(..)
  , AsType(AsHash)
  ) where

import           Data.Kind (Type)

import           Cardano.Api.HasTypeProxy


data family Hash keyrole :: Type

class CastHash roleA roleB where

    castHash :: Hash roleA -> Hash roleB


instance HasTypeProxy a => HasTypeProxy (Hash a) where
    data AsType (Hash a) = AsHash (AsType a)
    proxyToAsType _ = AsHash (proxyToAsType (Proxy :: Proxy a))

