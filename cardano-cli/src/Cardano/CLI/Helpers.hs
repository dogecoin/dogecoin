{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.CLI.Helpers
  ( HelpersError(..)
  , ensureNewFile
  , ensureNewFileLBS
  , pPrintCBOR
  , readCBOR
  , renderHelpersError
  , textShow
  , validateCBOR
  ) where

import           Cardano.Prelude

import           Codec.CBOR.Pretty (prettyHexEnc)
import           Codec.CBOR.Read (DeserialiseFailure, deserialiseFromBytes)
import           Codec.CBOR.Term (decodeTerm, encodeTerm)
import           Control.Monad.Trans.Except.Extra (handleIOExceptT, left)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as Text
import           System.Directory (doesPathExist)

import           Cardano.Binary (Decoder, fromCBOR)
import           Cardano.Chain.Block (fromCBORABlockOrBoundary)
import qualified Cardano.Chain.Delegation as Delegation
import qualified Cardano.Chain.Update as Update
import qualified Cardano.Chain.UTxO as UTxO
import           Cardano.CLI.Types


data HelpersError
  = CBORPrettyPrintError !DeserialiseFailure
  | CBORDecodingError !DeserialiseFailure
  | IOError' !FilePath !IOException
  | OutputMustNotAlreadyExist FilePath
  | ReadCBORFileFailure !FilePath !Text
  deriving Show

renderHelpersError :: HelpersError -> Text
renderHelpersError err =
  case err of
    OutputMustNotAlreadyExist fp -> "Output file/directory must not already exist: " <> Text.pack fp
    ReadCBORFileFailure fp err' -> "CBOR read failure at: " <> Text.pack fp <> Text.pack (show err')
    CBORPrettyPrintError err' -> "Error with CBOR decoding: " <> Text.pack (show err')
    CBORDecodingError err' -> "Error with CBOR decoding: " <> Text.pack (show err')
    IOError' fp ioE -> "Error at: " <> Text.pack fp <> " Error: " <> Text.pack (show ioE)

decodeCBOR
  :: LByteString
  -> (forall s. Decoder s a)
  -> Either HelpersError (LB.ByteString, a)
decodeCBOR bs decoder =
  first CBORDecodingError $ deserialiseFromBytes decoder bs

-- | Checks if a path exists and throws and error if it does.
ensureNewFile :: (FilePath -> a -> IO ()) -> FilePath -> a -> ExceptT HelpersError IO ()
ensureNewFile writer outFile blob = do
  exists <- liftIO $ doesPathExist outFile
  when exists $
    left $ OutputMustNotAlreadyExist outFile
  liftIO $ writer outFile blob

ensureNewFileLBS :: FilePath -> ByteString -> ExceptT HelpersError IO ()
ensureNewFileLBS = ensureNewFile BS.writeFile

pPrintCBOR :: LByteString -> ExceptT HelpersError IO ()
pPrintCBOR bs = do
  case deserialiseFromBytes decodeTerm bs of
    Left err -> left $ CBORPrettyPrintError err
    Right (remaining, decodedVal) -> do
      liftIO . putTextLn . toS . prettyHexEnc $ encodeTerm decodedVal
      unless (LB.null remaining) $
        pPrintCBOR remaining

readCBOR :: FilePath -> ExceptT HelpersError IO LByteString
readCBOR fp =
  handleIOExceptT
    (ReadCBORFileFailure fp . toS . displayException)
    (LB.readFile fp)

validateCBOR :: CBORObject -> LByteString -> Either HelpersError Text
validateCBOR cborObject bs =
  case cborObject of
    CBORBlockByron epochSlots -> do
      () <$ decodeCBOR bs (fromCBORABlockOrBoundary epochSlots)
      Right "Valid Byron block."

    CBORDelegationCertificateByron -> do
      () <$ decodeCBOR bs (fromCBOR :: Decoder s Delegation.Certificate)
      Right "Valid Byron delegation certificate."

    CBORTxByron -> do
      () <$ decodeCBOR bs (fromCBOR :: Decoder s UTxO.Tx)
      Right "Valid Byron Tx."

    CBORUpdateProposalByron -> do
      () <$ decodeCBOR bs (fromCBOR :: Decoder s Update.Proposal)
      Right "Valid Byron update proposal."

    CBORVoteByron -> do
      () <$ decodeCBOR bs (fromCBOR :: Decoder s Update.Vote)
      Right "Valid Byron vote."


textShow :: Show a => a -> Text
textShow = Text.pack . show
