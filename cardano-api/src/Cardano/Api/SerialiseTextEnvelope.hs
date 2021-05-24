{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | TextEnvelope Serialisation
--
module Cardano.Api.SerialiseTextEnvelope
  ( HasTextEnvelope(..)
  , TextEnvelope(..)
  , TextEnvelopeType(..)
  , TextEnvelopeDescr(..)
  , textEnvelopeRawCBOR
  , TextEnvelopeError(..)
  , serialiseToTextEnvelope
  , deserialiseFromTextEnvelope
  , readFileTextEnvelope
  , writeFileTextEnvelope
  , writeFileTextEnvelopeWithOwnerPermissions
  , readTextEnvelopeFromFile
  , readTextEnvelopeOfTypeFromFile

    -- * Reading one of several key types
  , FromSomeType(..)
  , deserialiseFromTextEnvelopeAnyOf
  , readFileTextEnvelopeAnyOf

    -- * Data family instances
  , AsType(..)
  ) where

import           Prelude

import           Data.Bifunctor (first)
import           Data.Maybe (fromMaybe)
import           Data.String (IsString)
import qualified Data.ByteString as BS
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.List as List
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text

import qualified Data.Aeson as Aeson
import           Data.Aeson
                   (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import           Data.Aeson.Encode.Pretty
                   (Config (..), encodePretty', defConfig, keyOrder)

import           Control.Monad (unless)
import           Control.Exception (bracketOnError)
import           Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import           Control.Monad.Trans.Except.Extra
                   (hoistEither, firstExceptT, handleIOExceptT)

import           System.Directory (removeFile, renameFile)
import           System.FilePath (splitFileName, (<.>))
import           System.IO (hClose, openTempFile)

import           Cardano.Binary (DecoderError)

import           Cardano.Api.Error
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.SerialiseCBOR


-- ----------------------------------------------------------------------------
-- Text envelopes
--

newtype TextEnvelopeType = TextEnvelopeType String
  deriving (Eq, Show)
  deriving newtype (IsString, Semigroup, ToJSON, FromJSON)

newtype TextEnvelopeDescr = TextEnvelopeDescr String
  deriving (Eq, Show)
  deriving newtype (IsString, Semigroup, ToJSON, FromJSON)

-- | A 'TextEnvelope' is a structured envelope for serialised binary values
-- with an external format with a semi-readable textual format.
--
-- It contains a \"type\" field, e.g. \"PublicKeyByron\" or \"TxSignedShelley\"
-- to indicate the type of the encoded data. This is used as a sanity check
-- and to help readers.
--
-- It also contains a \"title\" field which is free-form, and could be used
-- to indicate the role or purpose to a reader.
--
data TextEnvelope = TextEnvelope
  { teType        :: !TextEnvelopeType
  , teDescription :: !TextEnvelopeDescr
  , teRawCBOR     :: !ByteString
  } deriving (Eq, Show)

instance HasTypeProxy TextEnvelope where
    data AsType TextEnvelope = AsTextEnvelope
    proxyToAsType _ = AsTextEnvelope

instance ToJSON TextEnvelope where
  toJSON TextEnvelope {teType, teDescription, teRawCBOR} =
    object [ "type"        .= teType
           , "description" .= teDescription
           , "cborHex"     .= Text.decodeUtf8 (Base16.encode teRawCBOR)
           ]

instance FromJSON TextEnvelope where
  parseJSON = withObject "TextEnvelope" $ \v ->
                TextEnvelope <$> (v .: "type")
                             <*> (v .: "description")
                             <*> (parseJSONBase16 =<< v .: "cborHex")
    where
      parseJSONBase16 v =
        either fail return . Base16.decode . Text.encodeUtf8 =<< parseJSON v

textEnvelopeJSONConfig :: Config
textEnvelopeJSONConfig = defConfig { confCompare = textEnvelopeJSONKeyOrder }

textEnvelopeJSONKeyOrder :: Text -> Text -> Ordering
textEnvelopeJSONKeyOrder = keyOrder ["type", "description", "cborHex"]


textEnvelopeRawCBOR :: TextEnvelope -> ByteString
textEnvelopeRawCBOR = teRawCBOR


-- | The errors that the pure 'TextEnvelope' parsing\/decoding functions can return.
--
data TextEnvelopeError
  = TextEnvelopeTypeError   ![TextEnvelopeType] !TextEnvelopeType -- ^ expected, actual
  | TextEnvelopeDecodeError !DecoderError
  | TextEnvelopeAesonDecodeError !String
  deriving (Eq, Show)

instance Error TextEnvelopeError where
  displayError tee =
    case tee of
      TextEnvelopeTypeError [TextEnvelopeType expType]
                            (TextEnvelopeType actType) ->
          "TextEnvelope type error: "
       <> " Expected: " <> expType
       <> " Actual: " <> actType

      TextEnvelopeTypeError expTypes (TextEnvelopeType actType) ->
          "TextEnvelope type error: "
       <> " Expected one of: "
       <> List.intercalate ", "
            [ expType | TextEnvelopeType expType <- expTypes ]
       <> " Actual: " <> actType
      TextEnvelopeAesonDecodeError decErr -> "TextEnvelope aeson decode error: " <> decErr
      TextEnvelopeDecodeError decErr -> "TextEnvelope decode error: " <> show decErr


-- | Check that the \"type\" of the 'TextEnvelope' is as expected.
--
-- For example, one might check that the type is \"TxSignedShelley\".
--
expectTextEnvelopeOfType :: TextEnvelopeType -> TextEnvelope -> Either TextEnvelopeError ()
expectTextEnvelopeOfType expectedType TextEnvelope { teType = actualType } =
    unless (expectedType == actualType) $
      Left (TextEnvelopeTypeError [expectedType] actualType)


-- ----------------------------------------------------------------------------
-- Serialisation in text envelope format
--

class SerialiseAsCBOR a => HasTextEnvelope a where
    textEnvelopeType :: AsType a -> TextEnvelopeType

    textEnvelopeDefaultDescr :: a -> TextEnvelopeDescr
    textEnvelopeDefaultDescr _ = ""


serialiseToTextEnvelope :: forall a. HasTextEnvelope a
                        => Maybe TextEnvelopeDescr -> a -> TextEnvelope
serialiseToTextEnvelope mbDescr a =
    TextEnvelope {
      teType    = textEnvelopeType ttoken
    , teDescription   = fromMaybe (textEnvelopeDefaultDescr a) mbDescr
    , teRawCBOR = serialiseToCBOR a
    }
  where
    ttoken :: AsType a
    ttoken = proxyToAsType Proxy


deserialiseFromTextEnvelope :: HasTextEnvelope a
                            => AsType a
                            -> TextEnvelope
                            -> Either TextEnvelopeError a
deserialiseFromTextEnvelope ttoken te = do
    expectTextEnvelopeOfType (textEnvelopeType ttoken) te
    first TextEnvelopeDecodeError $
      deserialiseFromCBOR ttoken (teRawCBOR te) --TODO: You have switched from CBOR to JSON


deserialiseFromTextEnvelopeAnyOf :: [FromSomeType HasTextEnvelope b]
                                 -> TextEnvelope
                                 -> Either TextEnvelopeError b
deserialiseFromTextEnvelopeAnyOf types te =
    case List.find matching types of
      Nothing ->
        Left (TextEnvelopeTypeError expectedTypes actualType)

      Just (FromSomeType ttoken f) ->
        first TextEnvelopeDecodeError $
          f <$> deserialiseFromCBOR ttoken (teRawCBOR te)
  where
    actualType    = teType te
    expectedTypes = [ textEnvelopeType ttoken
                    | FromSomeType ttoken _f <- types ]

    matching (FromSomeType ttoken _f) = actualType == textEnvelopeType ttoken


writeFileWithOwnerPermissions
  :: FilePath
  -> ByteString
  -> IO (Either (FileError ()) ())
writeFileWithOwnerPermissions targetPath a =
    bracketOnError
      (openTempFile targetDir $ targetFile <.> "tmp")
      (\(tmpPath, fHandle) -> do
        hClose fHandle >> removeFile tmpPath
        return . Left $ FileErrorTempFile targetPath tmpPath fHandle)
      (\(tmpPath, fHandle) -> do
          BS.hPut fHandle a
          hClose fHandle
          renameFile tmpPath targetPath
          return $ Right ())
  where
    (targetDir, targetFile) = splitFileName targetPath

writeFileTextEnvelope :: HasTextEnvelope a
                      => FilePath
                      -> Maybe TextEnvelopeDescr
                      -> a
                      -> IO (Either (FileError ()) ())
writeFileTextEnvelope path mbDescr a =
    runExceptT $ do
      handleIOExceptT (FileIOError path) $ BS.writeFile path content
  where
    content = textEnvelopeToJSON mbDescr a


writeFileTextEnvelopeWithOwnerPermissions
  :: HasTextEnvelope a
  => FilePath
  -> Maybe TextEnvelopeDescr
  -> a
  -> IO (Either (FileError ()) ())
writeFileTextEnvelopeWithOwnerPermissions targetPath mbDescr a =
  writeFileWithOwnerPermissions targetPath content
 where
  content = textEnvelopeToJSON mbDescr a


textEnvelopeToJSON :: HasTextEnvelope a =>  Maybe TextEnvelopeDescr -> a -> ByteString
textEnvelopeToJSON mbDescr a  =
  LBS.toStrict $ encodePretty' textEnvelopeJSONConfig
                               (serialiseToTextEnvelope mbDescr a)
              <> "\n"


readFileTextEnvelope :: HasTextEnvelope a
                     => AsType a
                     -> FilePath
                     -> IO (Either (FileError TextEnvelopeError) a)
readFileTextEnvelope ttoken path =
    runExceptT $ do
      content <- handleIOExceptT (FileIOError path) $ BS.readFile path
      firstExceptT (FileError path) $ hoistEither $ do
        te <- first TextEnvelopeAesonDecodeError $ Aeson.eitherDecodeStrict' content
        deserialiseFromTextEnvelope ttoken te


readFileTextEnvelopeAnyOf :: [FromSomeType HasTextEnvelope b]
                          -> FilePath
                          -> IO (Either (FileError TextEnvelopeError) b)
readFileTextEnvelopeAnyOf types path =
    runExceptT $ do
      content <- handleIOExceptT (FileIOError path) $ BS.readFile path
      firstExceptT (FileError path) $ hoistEither $ do
        te <- first TextEnvelopeAesonDecodeError $ Aeson.eitherDecodeStrict' content
        deserialiseFromTextEnvelopeAnyOf types te


readTextEnvelopeFromFile :: FilePath
                         -> IO (Either (FileError TextEnvelopeError) TextEnvelope)
readTextEnvelopeFromFile path =
  runExceptT $ do
    bs <- handleIOExceptT (FileIOError path) $
            BS.readFile path
    firstExceptT (FileError path . TextEnvelopeAesonDecodeError)
      . hoistEither $ Aeson.eitherDecodeStrict' bs


readTextEnvelopeOfTypeFromFile
  :: TextEnvelopeType
  -> FilePath
  -> IO (Either (FileError TextEnvelopeError) TextEnvelope)
readTextEnvelopeOfTypeFromFile expectedType path =
  runExceptT $ do
    te <- ExceptT (readTextEnvelopeFromFile path)
    firstExceptT (FileError path) $ hoistEither $
      expectTextEnvelopeOfType expectedType te
    return te

