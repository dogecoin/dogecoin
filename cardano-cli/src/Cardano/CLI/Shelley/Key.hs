{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Shelley CLI option data types and functions for cryptographic keys.
module Cardano.CLI.Shelley.Key
  ( InputFormat (..)
  , InputDecodeError (..)
  , deserialiseInput
  , deserialiseInputAnyOf
  , renderInputDecodeError

  , readKeyFile
  , readKeyFileAnyOf
  , readKeyFileTextEnvelope

  , readSigningKeyFile
  , readSigningKeyFileAnyOf

  , VerificationKeyOrFile (..)
  , readVerificationKeyOrFile
  , readVerificationKeyOrTextEnvFile

  , VerificationKeyTextOrFile (..)
  , VerificationKeyTextOrFileError (..)
  , readVerificationKeyTextOrFileAnyOf
  , renderVerificationKeyTextOrFileError

  , VerificationKeyOrHashOrFile (..)
  , readVerificationKeyOrHashOrFile
  , readVerificationKeyOrHashOrTextEnvFile

  , PaymentVerifier(..)
  , StakeVerifier(..)
  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Cardano.Api

import           Cardano.CLI.Types

------------------------------------------------------------------------------
-- Formatted/encoded input deserialisation
------------------------------------------------------------------------------

-- | Input format/encoding.
data InputFormat a where
  -- | Bech32 encoding.
  InputFormatBech32 :: SerialiseAsBech32 a => InputFormat a

  -- | Hex/Base16 encoding.
  InputFormatHex :: SerialiseAsRawBytes a => InputFormat a

  -- | Text envelope format.
  InputFormatTextEnvelope :: HasTextEnvelope a => InputFormat a

-- | Input decoding error.
data InputDecodeError
  = InputTextEnvelopeError !TextEnvelopeError
  -- ^ The provided data seems to be a valid text envelope, but some error
  -- occurred in deserialising it.
  | InputBech32DecodeError !Bech32DecodeError
  -- ^ The provided data is valid Bech32, but some error occurred in
  -- deserialising it.
  | InputInvalidError
  -- ^ The provided data does not represent a valid value of the provided
  -- type.
  deriving (Eq, Show)

instance Error InputDecodeError where
  displayError = Text.unpack . renderInputDecodeError

-- | Render an error message for a 'InputDecodeError'.
renderInputDecodeError :: InputDecodeError -> Text
renderInputDecodeError err =
  case err of
    InputTextEnvelopeError textEnvErr ->
      Text.pack (displayError textEnvErr)
    InputBech32DecodeError decodeErr ->
      Text.pack (displayError decodeErr)
    InputInvalidError -> "Invalid key."

-- | The result of a deserialisation function.
--
-- Note that this type isn't intended to be exported, but only used as a
-- helper within the 'deserialiseInput' function.
data DeserialiseInputResult a
  = DeserialiseInputSuccess !a
  -- ^ Input successfully deserialised.
  | DeserialiseInputError !InputDecodeError
  -- ^ The provided data is of the expected format/encoding, but an error
  -- occurred in deserialising it.
  | DeserialiseInputErrorFormatMismatch
  -- ^ The provided data's formatting/encoding does not match that which was
  -- expected. This error is an indication that one could attempt to
  -- deserialise the input again, but instead expecting a different format.

-- | Deserialise an input of some type that is formatted in some way.
deserialiseInput
  :: forall a.
     AsType a
  -> NonEmpty (InputFormat a)
  -> ByteString
  -> Either InputDecodeError a
deserialiseInput asType acceptedFormats inputBs =
    go (NE.toList acceptedFormats)
  where
    inputText :: Text
    inputText = Text.decodeUtf8 inputBs

    go :: [InputFormat a] -> Either InputDecodeError a
    go [] = Left InputInvalidError
    go (kf:kfs) =
      let res =
            case kf of
              InputFormatBech32 -> deserialiseBech32
              InputFormatHex -> deserialiseHex
              InputFormatTextEnvelope -> deserialiseTextEnvelope
      in case res of
        DeserialiseInputSuccess a -> Right a
        DeserialiseInputError err -> Left err
        DeserialiseInputErrorFormatMismatch -> go kfs

    deserialiseTextEnvelope :: HasTextEnvelope a => DeserialiseInputResult a
    deserialiseTextEnvelope = do
      let textEnvRes :: Either TextEnvelopeError a
          textEnvRes =
            deserialiseFromTextEnvelope asType
              =<< first TextEnvelopeAesonDecodeError (Aeson.eitherDecodeStrict' inputBs)
      case textEnvRes of
        Right res -> DeserialiseInputSuccess res

        -- The input was valid a text envelope, but there was a type mismatch
        -- error.
        Left err@TextEnvelopeTypeError{} ->
          DeserialiseInputError (InputTextEnvelopeError err)

        -- The input was not valid a text envelope.
        Left _ -> DeserialiseInputErrorFormatMismatch

    deserialiseBech32 :: SerialiseAsBech32 a => DeserialiseInputResult a
    deserialiseBech32 =
      case deserialiseFromBech32 asType inputText of
        Right res -> DeserialiseInputSuccess res

        -- The input was not valid Bech32.
        Left (Bech32DecodingError _) -> DeserialiseInputErrorFormatMismatch

        -- The input was valid Bech32, but some other error occurred.
        Left err -> DeserialiseInputError $ InputBech32DecodeError err

    deserialiseHex :: SerialiseAsRawBytes a => DeserialiseInputResult a
    deserialiseHex
      | isValidHex inputBs =
          maybe
            (DeserialiseInputError InputInvalidError)
            DeserialiseInputSuccess
            (deserialiseFromRawBytesHex asType inputBs)
      | otherwise = DeserialiseInputErrorFormatMismatch

    isValidHex :: ByteString -> Bool
    isValidHex x =
      all (`elem` hexAlpha) (toLower <$> BSC.unpack x)
        && even (BSC.length x)
      where
        hexAlpha :: [Char]
        hexAlpha = "0123456789abcdef"

-- | Deserialise an input of some type that is formatted in some way.
--
-- The provided 'ByteString' can either be Bech32-encoded or in the text
-- envelope format.
deserialiseInputAnyOf
  :: forall b.
     [FromSomeType SerialiseAsBech32 b]
  -> [FromSomeType HasTextEnvelope b]
  -> ByteString
  -> Either InputDecodeError b
deserialiseInputAnyOf bech32Types textEnvTypes inputBs =
    case deserialiseBech32 `orTry` deserialiseTextEnvelope of
      DeserialiseInputSuccess res -> Right res
      DeserialiseInputError err -> Left err
      DeserialiseInputErrorFormatMismatch -> Left InputInvalidError
  where
    inputText :: Text
    inputText = Text.decodeUtf8 inputBs

    orTry
      :: DeserialiseInputResult b
      -> DeserialiseInputResult b
      -> DeserialiseInputResult b
    orTry x y =
      case x of
        DeserialiseInputSuccess _ -> x
        DeserialiseInputError _ -> x
        DeserialiseInputErrorFormatMismatch -> y

    deserialiseTextEnvelope :: DeserialiseInputResult b
    deserialiseTextEnvelope = do
      let textEnvRes :: Either TextEnvelopeError b
          textEnvRes =
            deserialiseFromTextEnvelopeAnyOf textEnvTypes
              =<< first TextEnvelopeAesonDecodeError (Aeson.eitherDecodeStrict' inputBs)
      case textEnvRes of
        Right res -> DeserialiseInputSuccess res

        -- The input was valid a text envelope, but there was a type mismatch
        -- error.
        Left err@TextEnvelopeTypeError{} ->
          DeserialiseInputError (InputTextEnvelopeError err)

        -- The input was not valid a text envelope.
        Left _ -> DeserialiseInputErrorFormatMismatch

    deserialiseBech32 :: DeserialiseInputResult b
    deserialiseBech32 =
      case deserialiseAnyOfFromBech32 bech32Types inputText of
        Right res -> DeserialiseInputSuccess res

        -- The input was not valid Bech32.
        Left (Bech32DecodingError _) -> DeserialiseInputErrorFormatMismatch

        -- The input was valid Bech32, but some other error occurred.
        Left err -> DeserialiseInputError $ InputBech32DecodeError err

------------------------------------------------------------------------------
-- Cryptographic key deserialisation
------------------------------------------------------------------------------

-- | Read a cryptographic key from a file.
--
-- The contents of the file can either be Bech32-encoded, hex-encoded, or in
-- the text envelope format.
readKeyFile
  :: AsType a
  -> NonEmpty (InputFormat a)
  -> FilePath
  -> IO (Either (FileError InputDecodeError) a)
readKeyFile asType acceptedFormats path =
  runExceptT $ do
    content <- handleIOExceptT (FileIOError path) $ BS.readFile path
    firstExceptT (FileError path) $ hoistEither $
      deserialiseInput asType acceptedFormats content

-- | Read a cryptographic key from a file.
--
-- The contents of the file must be in the text envelope format.
readKeyFileTextEnvelope
  :: HasTextEnvelope a
  => AsType a
  -> FilePath
  -> IO (Either (FileError InputDecodeError) a)
readKeyFileTextEnvelope asType fp =
    first toInputDecodeError <$> readFileTextEnvelope asType fp
  where
    toInputDecodeError
      :: FileError TextEnvelopeError
      -> FileError InputDecodeError
    toInputDecodeError err =
      case err of
        FileIOError path ex -> FileIOError path ex
        FileError path textEnvErr ->
          FileError path (InputTextEnvelopeError textEnvErr)
        FileErrorTempFile targetP tempP h ->
          FileErrorTempFile targetP tempP h

-- | Read a cryptographic key from a file given that it is one of the provided
-- types.
--
-- The contents of the file can either be Bech32-encoded or in the text
-- envelope format.
readKeyFileAnyOf
  :: forall b.
     [FromSomeType SerialiseAsBech32 b]
  -> [FromSomeType HasTextEnvelope b]
  -> FilePath
  -> IO (Either (FileError InputDecodeError) b)
readKeyFileAnyOf bech32Types textEnvTypes path =
  runExceptT $ do
    content <- handleIOExceptT (FileIOError path) $ BS.readFile path
    firstExceptT (FileError path) $ hoistEither $
      deserialiseInputAnyOf bech32Types textEnvTypes content

------------------------------------------------------------------------------
-- Signing key deserialisation
------------------------------------------------------------------------------

-- | Read a signing key from a file.
--
-- The contents of the file can either be Bech32-encoded, hex-encoded, or in
-- the text envelope format.
readSigningKeyFile
  :: forall keyrole.
     ( HasTextEnvelope (SigningKey keyrole)
     , SerialiseAsBech32 (SigningKey keyrole)
     )
  => AsType keyrole
  -> SigningKeyFile
  -> IO (Either (FileError InputDecodeError) (SigningKey keyrole))
readSigningKeyFile asType (SigningKeyFile fp) =
  readKeyFile
    (AsSigningKey asType)
    (NE.fromList [InputFormatBech32, InputFormatHex, InputFormatTextEnvelope])
    fp

-- | Read a signing key from a file given that it is one of the provided types
-- of signing key.
--
-- The contents of the file can either be Bech32-encoded or in the text
-- envelope format.
readSigningKeyFileAnyOf
  :: forall b.
     [FromSomeType SerialiseAsBech32 b]
  -> [FromSomeType HasTextEnvelope b]
  -> SigningKeyFile
  -> IO (Either (FileError InputDecodeError) b)
readSigningKeyFileAnyOf bech32Types textEnvTypes (SigningKeyFile fp) =
  readKeyFileAnyOf bech32Types textEnvTypes fp

------------------------------------------------------------------------------
-- Verification key deserialisation
------------------------------------------------------------------------------

-- | Either a verification key or path to a verification key file.
data VerificationKeyOrFile keyrole
  = VerificationKeyValue !(VerificationKey keyrole)
  -- ^ A verification key.
  | VerificationKeyFilePath !VerificationKeyFile
  -- ^ A path to a verification key file.
  -- Note that this file hasn't been validated at all (whether it exists,
  -- contains a key of the correct type, etc.)

deriving instance Show (VerificationKey keyrole)
  => Show (VerificationKeyOrFile keyrole)

deriving instance Eq (VerificationKey keyrole)
  => Eq (VerificationKeyOrFile keyrole)

-- | Read a verification key or verification key file and return a
-- verification key.
--
-- If a filepath is provided, the file can either be formatted as Bech32, hex,
-- or text envelope.
readVerificationKeyOrFile
  :: ( HasTextEnvelope (VerificationKey keyrole)
     , SerialiseAsBech32 (VerificationKey keyrole)
     )
  => AsType keyrole
  -> VerificationKeyOrFile keyrole
  -> IO (Either (FileError InputDecodeError) (VerificationKey keyrole))
readVerificationKeyOrFile asType verKeyOrFile =
  case verKeyOrFile of
    VerificationKeyValue vk -> pure (Right vk)
    VerificationKeyFilePath (VerificationKeyFile fp) ->
      readKeyFile
        (AsVerificationKey asType)
        (NE.fromList [InputFormatBech32, InputFormatHex, InputFormatTextEnvelope])
        fp

-- | Read a verification key or verification key file and return a
-- verification key.
--
-- If a filepath is provided, it will be interpreted as a text envelope
-- formatted file.
readVerificationKeyOrTextEnvFile
  :: HasTextEnvelope (VerificationKey keyrole)
  => AsType keyrole
  -> VerificationKeyOrFile keyrole
  -> IO (Either (FileError InputDecodeError) (VerificationKey keyrole))
readVerificationKeyOrTextEnvFile asType verKeyOrFile =
  case verKeyOrFile of
    VerificationKeyValue vk -> pure (Right vk)
    VerificationKeyFilePath (VerificationKeyFile fp) ->
      readKeyFileTextEnvelope (AsVerificationKey asType) fp

data PaymentVerifier
  = PaymentVerifierKey VerificationKeyTextOrFile
  | PaymentVerifierScriptFile ScriptFile
  deriving (Eq, Show)

data StakeVerifier
  = StakeVerifierKey (VerificationKeyOrFile StakeKey)
  | StakeVerifierScriptFile ScriptFile
  deriving (Eq, Show)

-- | Either an unvalidated text representation of a verification key or a path
-- to a verification key file.
data VerificationKeyTextOrFile
  = VktofVerificationKeyText !Text
  | VktofVerificationKeyFile !VerificationKeyFile
  deriving (Eq, Show)

-- | An error in deserialising a 'VerificationKeyTextOrFile' to a
-- 'VerificationKey'.
data VerificationKeyTextOrFileError
  = VerificationKeyTextError !InputDecodeError
  | VerificationKeyFileError !(FileError InputDecodeError)
  deriving Show

-- | Render an error message for a 'VerificationKeyTextOrFileError'.
renderVerificationKeyTextOrFileError :: VerificationKeyTextOrFileError -> Text
renderVerificationKeyTextOrFileError vkTextOrFileErr =
  case vkTextOrFileErr of
    VerificationKeyTextError err -> renderInputDecodeError err
    VerificationKeyFileError err -> Text.pack (displayError err)

-- | Deserialise a verification key from text or a verification key file given
-- that it is one of the provided types.
--
-- If a filepath is provided, the file can either be formatted as Bech32, hex,
-- or text envelope.
readVerificationKeyTextOrFileAnyOf
  :: forall b.
     [FromSomeType SerialiseAsBech32 b]
  -> [FromSomeType HasTextEnvelope b]
  -> VerificationKeyTextOrFile
  -> IO (Either VerificationKeyTextOrFileError b)
readVerificationKeyTextOrFileAnyOf bech32Types textEnvTypes verKeyTextOrFile =
  case verKeyTextOrFile of
    VktofVerificationKeyText vkText ->
      pure $ first VerificationKeyTextError $
        deserialiseInputAnyOf bech32Types textEnvTypes (Text.encodeUtf8 vkText)
    VktofVerificationKeyFile (VerificationKeyFile fp) ->
      first VerificationKeyFileError
        <$> readKeyFileAnyOf bech32Types textEnvTypes fp

-- | Verification key, verification key hash, or path to a verification key
-- file.
data VerificationKeyOrHashOrFile keyrole
  = VerificationKeyOrFile !(VerificationKeyOrFile keyrole)
  -- ^ Either a verification key or path to a verification key file.
  | VerificationKeyHash !(Hash keyrole)
  -- ^ A verification key hash.

deriving instance (Show (VerificationKeyOrFile keyrole), Show (Hash keyrole))
  => Show (VerificationKeyOrHashOrFile keyrole)

deriving instance (Eq (VerificationKeyOrFile keyrole), Eq (Hash keyrole))
  => Eq (VerificationKeyOrHashOrFile keyrole)

-- | Read a verification key or verification key hash or verification key file
-- and return a verification key hash.
--
-- If a filepath is provided, the file can either be formatted as Bech32, hex,
-- or text envelope.
readVerificationKeyOrHashOrFile
  :: (Key keyrole, SerialiseAsBech32 (VerificationKey keyrole))
  => AsType keyrole
  -> VerificationKeyOrHashOrFile keyrole
  -> IO (Either (FileError InputDecodeError) (Hash keyrole))
readVerificationKeyOrHashOrFile asType verKeyOrHashOrFile =
  case verKeyOrHashOrFile of
    VerificationKeyOrFile vkOrFile -> do
      eitherVk <- readVerificationKeyOrFile asType vkOrFile
      pure (verificationKeyHash <$> eitherVk)
    VerificationKeyHash vkHash -> pure (Right vkHash)

-- | Read a verification key or verification key hash or verification key file
-- and return a verification key hash.
--
-- If a filepath is provided, it will be interpreted as a text envelope
-- formatted file.
readVerificationKeyOrHashOrTextEnvFile
  :: Key keyrole
  => AsType keyrole
  -> VerificationKeyOrHashOrFile keyrole
  -> IO (Either (FileError InputDecodeError) (Hash keyrole))
readVerificationKeyOrHashOrTextEnvFile asType verKeyOrHashOrFile =
  case verKeyOrHashOrFile of
    VerificationKeyOrFile vkOrFile -> do
      eitherVk <- readVerificationKeyOrTextEnvFile asType vkOrFile
      pure (verificationKeyHash <$> eitherVk)
    VerificationKeyHash vkHash -> pure (Right vkHash)
