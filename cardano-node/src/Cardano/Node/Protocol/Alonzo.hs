{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Node.Protocol.Alonzo
  ( AlonzoProtocolInstantiationError(..)
  , renderAlonzoProtocolInstantiationError
    -- * Reusable parts
  , readAlonzoGenesis
  ) where

import           Cardano.Prelude

import           Cardano.Api

import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither, left)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import           System.IO.Error (isDoesNotExistError)

import qualified Cardano.Ledger.Alonzo.Translation as Alonzo


import           Cardano.Node.Orphans ()

import           Cardano.Tracing.OrphanInstances.HardFork ()
import           Cardano.Tracing.OrphanInstances.Shelley ()


--
-- Alonzo genesis
--

-- | In order to avoid introducing a separate Alonzo genesis file, we
-- have added additional fields to the Shelley genesis that are required
-- when hardforking to Alonzo. Unfortunately the 'ShelleyGenesis' 'FromJSON'
-- instance exists in cardano-ledger-specs so we must duplicate code for now.

readAlonzoGenesis
  :: FilePath
  -> ExceptT AlonzoProtocolInstantiationError IO Alonzo.AlonzoGenesis
readAlonzoGenesis fpath = do
  readAndDecode
    `catchError` \err ->
      case err of
        AlonzoGenesisFileError (FileIOError _ ioe)
          | isDoesNotExistError ioe -> left $ GenesisFileNotFound fpath
        _                           -> left err
 where
  readAndDecode :: ExceptT AlonzoProtocolInstantiationError IO Alonzo.AlonzoGenesis
  readAndDecode = do
      lbs <- handleIOExceptT (AlonzoGenesisFileError . FileIOError fpath) $ LBS.readFile fpath
      firstExceptT (AlonzoGenesisDecodeError fpath . Text.pack)
        . hoistEither $ Aeson.eitherDecode' lbs


data AlonzoProtocolInstantiationError
  = InvalidCostModelError !FilePath
  | CostModelExtractionError !FilePath
  | AlonzoCostModelFileError !(FileError ())
  | AlonzoCostModelDecodeError !FilePath !Text
  | AlonzoGenesisFileError !(FileError ())
  | AlonzoGenesisDecodeError !FilePath !Text
  | GenesisFileNotFound !FilePath
  deriving Show

renderAlonzoProtocolInstantiationError :: AlonzoProtocolInstantiationError -> Text
renderAlonzoProtocolInstantiationError (InvalidCostModelError fp) =
  "Invalid cost model: " <> Text.pack (show fp)
renderAlonzoProtocolInstantiationError (CostModelExtractionError fp) =
  "Error extracting the cost model at: " <> Text.pack (show fp)
renderAlonzoProtocolInstantiationError (AlonzoCostModelFileError err) =
  Text.pack $ displayError err
renderAlonzoProtocolInstantiationError (AlonzoCostModelDecodeError fp err) =
  "Error decoding cost model at: " <> Text.pack (show fp) <> " Error: " <> err
renderAlonzoProtocolInstantiationError (AlonzoGenesisFileError err) =
  Text.pack $ displayError err
renderAlonzoProtocolInstantiationError (AlonzoGenesisDecodeError fp err) =
  "Error decoding genesis at: " <> Text.pack fp <> " Error: " <> err
renderAlonzoProtocolInstantiationError (GenesisFileNotFound fp) =
  "Genesis file not found at: " <> Text.pack fp

