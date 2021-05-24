{-# LANGUAGE ScopedTypeVariables #-}

-- | Bech32 Serialisation
--
module Cardano.Api.SerialiseBech32
  ( SerialiseAsBech32(..)
  , serialiseToBech32
  , Bech32DecodeError(..)
  , deserialiseFromBech32
  , deserialiseAnyOfFromBech32
  ) where

import           Prelude

import           Data.ByteString (ByteString)
import           Data.Text (Text)

import qualified Data.List as List
import qualified Data.Set as Set
import           Data.Set (Set)

import           Control.Monad (guard)

import qualified Codec.Binary.Bech32 as Bech32

import           Cardano.Api.Error
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.SerialiseRaw
import           Cardano.Api.Utils


class (HasTypeProxy a, SerialiseAsRawBytes a) => SerialiseAsBech32 a where

    -- | The human readable prefix to use when encoding this value to Bech32.
    --
    bech32PrefixFor :: a -> Text

    -- | The set of human readable prefixes that can be used for this type.
    --
    bech32PrefixesPermitted :: AsType a -> [Text]


serialiseToBech32 :: SerialiseAsBech32 a => a -> Text
serialiseToBech32 a =
    Bech32.encodeLenient
      humanReadablePart
      (Bech32.dataPartFromBytes (serialiseToRawBytes a))
  where
    humanReadablePart =
      case Bech32.humanReadablePartFromText (bech32PrefixFor a) of
        Right p  -> p
        Left err -> error $ "serialiseToBech32: invalid prefix "
                         ++ show (bech32PrefixFor a)
                         ++ ", " ++ show err


deserialiseFromBech32 :: SerialiseAsBech32 a
                      => AsType a -> Text -> Either Bech32DecodeError a
deserialiseFromBech32 asType bech32Str = do
    (prefix, dataPart) <- Bech32.decodeLenient bech32Str
                            ?!. Bech32DecodingError

    let actualPrefix      = Bech32.humanReadablePartToText prefix
        permittedPrefixes = bech32PrefixesPermitted asType
    guard (actualPrefix `elem` permittedPrefixes)
      ?! Bech32UnexpectedPrefix actualPrefix (Set.fromList permittedPrefixes)

    payload <- Bech32.dataPartToBytes dataPart
                 ?! Bech32DataPartToBytesError (Bech32.dataPartToText dataPart)

    value <- deserialiseFromRawBytes asType payload
               ?! Bech32DeserialiseFromBytesError payload

    let expectedPrefix = bech32PrefixFor value
    guard (actualPrefix == expectedPrefix)
      ?! Bech32WrongPrefix actualPrefix expectedPrefix

    return value


deserialiseAnyOfFromBech32
  :: forall b.
     [FromSomeType SerialiseAsBech32 b]
  -> Text
  -> Either Bech32DecodeError b
deserialiseAnyOfFromBech32 types bech32Str = do
    (prefix, dataPart) <- Bech32.decodeLenient bech32Str
                            ?!. Bech32DecodingError

    let actualPrefix = Bech32.humanReadablePartToText prefix

    FromSomeType actualType fromType <-
      findForPrefix actualPrefix
        ?! Bech32UnexpectedPrefix actualPrefix permittedPrefixes

    payload <- Bech32.dataPartToBytes dataPart
                 ?! Bech32DataPartToBytesError (Bech32.dataPartToText dataPart)

    value <- deserialiseFromRawBytes actualType payload
               ?! Bech32DeserialiseFromBytesError payload

    let expectedPrefix = bech32PrefixFor value
    guard (actualPrefix == expectedPrefix)
      ?! Bech32WrongPrefix actualPrefix expectedPrefix

    return (fromType value)
  where
    findForPrefix
      :: Text
      -> Maybe (FromSomeType SerialiseAsBech32 b)
    findForPrefix prefix =
      List.find
        (\(FromSomeType t _) -> prefix `elem` bech32PrefixesPermitted t)
        types

    permittedPrefixes :: Set Text
    permittedPrefixes =
      Set.fromList $ concat
        [ bech32PrefixesPermitted ttoken
        | FromSomeType ttoken _f <- types
        ]


-- | Bech32 decoding error.
--
data Bech32DecodeError =

       -- | There was an error decoding the string as Bech32.
       Bech32DecodingError !Bech32.DecodingError

       -- | The human-readable prefix in the Bech32-encoded string is not one
       -- of the ones expected.
     | Bech32UnexpectedPrefix !Text !(Set Text)

       -- | There was an error in extracting a 'ByteString' from the data part of
       -- the Bech32-encoded string.
     | Bech32DataPartToBytesError !Text

       -- | There was an error in deserialising the bytes into a value of the
       -- expected type.
     | Bech32DeserialiseFromBytesError !ByteString

       -- | The human-readable prefix in the Bech32-encoded string does not
       -- correspond to the prefix that should be used for the payload value.
     | Bech32WrongPrefix !Text !Text

  deriving (Eq, Show)

instance Error Bech32DecodeError where
  displayError err = case err of
    Bech32DecodingError decErr -> show decErr -- TODO

    Bech32UnexpectedPrefix actual permitted ->
        "Unexpected Bech32 prefix: the actual prefix is " <> show actual
     <> ", but it was expected to be "
     <> List.intercalate " or " (map show (Set.toList permitted))

    Bech32DataPartToBytesError _dataPart ->
        "There was an error in extracting the bytes from the data part of the \
        \Bech32-encoded string."

    Bech32DeserialiseFromBytesError _bytes ->
        "There was an error in deserialising the data part of the \
        \Bech32-encoded string into a value of the expected type."

    Bech32WrongPrefix actual expected ->
        "Mismatch in the Bech32 prefix: the actual prefix is " <> show actual
     <> ", but the prefix for this payload value should be " <> show expected

