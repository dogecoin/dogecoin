{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StrictData #-}

module Cardano.TxSubmit.Rest.Types
  ( WebserverConfig(..)
  , toWarpSettings
  ) where

import           Data.Function ((&))
import           Data.Semigroup ((<>))
import           Text.Show (Show (..))

import qualified Network.Wai.Handler.Warp as Warp

------------------------------------------------------------
data WebserverConfig = WebserverConfig
  { wcHost :: Warp.HostPreference
  , wcPort :: Warp.Port
  }

instance Show WebserverConfig where
  show WebserverConfig {wcHost, wcPort} = show wcHost <> ":" <> show wcPort

toWarpSettings :: WebserverConfig -> Warp.Settings
toWarpSettings WebserverConfig {wcHost, wcPort} =
  Warp.defaultSettings & Warp.setHost wcHost & Warp.setPort wcPort
