{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTSyntax #-}

-- | Class of errors used in the Api.
--
module Cardano.Api.Error
  ( Error(..)
  , throwErrorAsException
  , ErrorAsException(..)
  , FileError(..)
  ) where

import           Prelude

import           Control.Exception (Exception(..), IOException, throwIO)
import           System.IO (Handle)


class Show e => Error e where

    displayError :: e -> String

instance Error () where
    displayError () = ""


-- | The preferred approach is to use 'Except' or 'ExceptT', but you can if
-- necessary use IO exceptions.
--
throwErrorAsException :: Error e => e -> IO a
throwErrorAsException e = throwIO (ErrorAsException e)

data ErrorAsException where
     ErrorAsException :: Error e => e -> ErrorAsException

instance Show ErrorAsException where
    show (ErrorAsException e) = show e

instance Exception ErrorAsException where
    displayException (ErrorAsException e) = displayError e


data FileError e = FileError   FilePath e
                 | FileErrorTempFile
                     FilePath
                     -- ^ Target path
                     FilePath
                     -- ^ Temporary path
                     Handle
                 | FileIOError FilePath IOException
  deriving Show

instance Error e => Error (FileError e) where
  displayError (FileErrorTempFile targetPath tempPath h)=
    "Error creating temporary file at: " ++ tempPath ++
    "/n" ++ "Target path: " ++ targetPath ++
    "/n" ++ "Handle: " ++ show h
  displayError (FileIOError path ioe) =
    path ++ ": " ++ displayException ioe
  displayError (FileError path e) =
    path ++ ": " ++ displayError e


