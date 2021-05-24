{-# LANGUAGE BangPatterns #-}

module Cardano.Node.STM
  ( modifyReadTVar
  , modifyReadTVar'
  , modifyReadTVarIO
  , modifyReadTVarIO'
  ) where

import Data.Function
import Control.Monad
import System.IO (IO)

import qualified Control.Concurrent.STM as STM

-- | Mutate the contents of a TVar and return the new value of the TVar (non-strict).
modifyReadTVar :: STM.TVar a -> (a -> a) -> STM.STM a
modifyReadTVar tv f = do
  old <- STM.readTVar tv
  let new = f old
  STM.writeTVar tv new
  return new

-- | Mutate the contents of a TVar and return the new value of the TVar (strict).
modifyReadTVar' :: STM.TVar a -> (a -> a) -> STM.STM a
modifyReadTVar' tv f = do
  old <- STM.readTVar tv
  let !new = f old
  STM.writeTVar tv new
  return new

-- | Mutate the contents of a TVar and return the new value of the TVar (non-strict).
modifyReadTVarIO :: STM.TVar a -> (a -> a) -> IO a
modifyReadTVarIO tv f = STM.atomically $ modifyReadTVar tv f

-- | Mutate the contents of a TVar and return the new value of the TVar (strict).
modifyReadTVarIO' :: STM.TVar a -> (a -> a) -> IO a
modifyReadTVarIO' tv f = STM.atomically $ modifyReadTVar' tv f
