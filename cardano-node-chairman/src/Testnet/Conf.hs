{-# LANGUAGE RecordWildCards #-}

module Testnet.Conf
  ( Conf(..)
  , mkConf
  ) where

import           Control.Monad
import           Data.Eq
import           Data.Function
import           Data.Int
import           Data.Maybe
import           System.FilePath.Posix ((</>))
import           System.IO (FilePath)
import           Text.Show

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.Process as H
import qualified System.FilePath.Posix as FP
import qualified System.Random as IO

data Conf = Conf
  { tempAbsPath :: FilePath
  , tempRelPath :: FilePath
  , tempBaseAbsPath :: FilePath
  , logDir :: FilePath
  , base :: FilePath
  , socketDir :: FilePath
  , testnetMagic :: Int
  } deriving (Eq, Show)

mkConf :: FilePath -> Maybe Int -> H.Integration Conf
mkConf tempAbsPath maybeMagic = do
  testnetMagic <- H.noteShowIO $ maybe (IO.randomRIO (1000, 2000)) return maybeMagic
  tempBaseAbsPath <- H.noteShow $ FP.takeDirectory tempAbsPath
  tempRelPath <- H.noteShow $ FP.makeRelative tempBaseAbsPath tempAbsPath
  base <- H.noteShowM H.getProjectBase
  socketDir <- H.noteShow $ tempRelPath </> "socket"
  logDir <- H.noteTempFile tempAbsPath "/logs"

  return $ Conf {..}
