{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Spec.Network
  ( hprop_isPortOpen_False
  , hprop_isPortOpen_True
  ) where

import           Control.Exception (IOException)
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Data.Bool
import           Data.Either
import           Data.Function
import           Data.Int
import           Hedgehog (Property, (===))
import           Network.Socket (Socket)
import           Prelude (error)
import           System.IO (IO)

import qualified Control.Monad.Trans.Resource as IO
import qualified Data.List as L
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.IO.Network.Socket as IO
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.Network as H
import qualified Network.Socket as IO
import qualified System.Random as IO
import qualified UnliftIO.Exception as IO

hprop_isPortOpen_False :: Property
hprop_isPortOpen_False = H.propertyOnce . H.workspace "temp/network" $ \_ -> do
  -- Check multiple random ports and assert that one is closed.
  -- Multiple random ports are checked because there is a remote possibility a random
  -- port is actually open by another program
  ports <- H.evalIO $ fmap (L.take 10 . IO.randomRs @Int (5000, 9000)) IO.getStdGen
  results <- forM ports H.isPortOpen
  H.assert (False `L.elem` results)

hprop_isPortOpen_True :: Property
hprop_isPortOpen_True = H.propertyOnce . H.workspace "temp/network" $ \_ -> do
  -- Check first random port from multiple possible ports to be successfully bound is open
  -- Multiple random ports are checked because there is a remote possibility a random
  -- port is actually open by another program
  ports <- H.evalIO $ fmap (L.take 10 . IO.randomRs @Int (5000, 9000)) IO.getStdGen
  (socket, port) <- liftIO $ openOnePortFrom ports
  void $ IO.register $ IO.close socket
  result <- H.isPortOpen port
  result === True
  where openOnePortFrom :: [Int] -> IO (Socket, Int)
        openOnePortFrom ports = case ports of
          [] -> error "Could not open any ports"
          (n:ns) -> do
            socketResult <- IO.try . liftIO $ IO.listenOn n
            case socketResult of
              Right socket -> return (socket, n)
              Left (_ :: IOException) -> openOnePortFrom ns
