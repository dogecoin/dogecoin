{-# LANGUAGE ScopedTypeVariables #-}

-- | Raw binary serialisation
--
module Cardano.Api.SerialiseRaw
  ( SerialiseAsRawBytes(..)
  , serialiseToRawBytesHex
  , deserialiseFromRawBytesHex
  , serialiseToRawBytesHexText
  , UsingRawBytesHex(..)
  ) where

import           Prelude

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BSC
import           Data.String (IsString (..))
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text

import           Cardano.Api.HasTypeProxy


class HasTypeProxy a => SerialiseAsRawBytes a where

  serialiseToRawBytes :: a -> ByteString

  deserialiseFromRawBytes :: AsType a -> ByteString -> Maybe a

serialiseToRawBytesHex :: SerialiseAsRawBytes a => a -> ByteString
serialiseToRawBytesHex = Base16.encode . serialiseToRawBytes

serialiseToRawBytesHexText :: SerialiseAsRawBytes a => a -> Text
serialiseToRawBytesHexText = Text.decodeUtf8 . serialiseToRawBytesHex

deserialiseFromRawBytesHex :: SerialiseAsRawBytes a
                           => AsType a -> ByteString -> Maybe a
deserialiseFromRawBytesHex proxy hex =
    case Base16.decode hex of
      Right raw -> deserialiseFromRawBytes proxy raw
      Left _msg -> Nothing


-- | For use with @deriving via@, to provide 'Show' and\/or 'IsString' instances
-- using a hex encoding, based on the 'SerialiseAsRawBytes' instance.
--
-- > deriving (Show, IsString) via (UsingRawBytesHex Blah)
--
newtype UsingRawBytesHex a = UsingRawBytesHex a

instance SerialiseAsRawBytes a => Show (UsingRawBytesHex a) where
    show (UsingRawBytesHex x) = show (serialiseToRawBytesHex x)

instance SerialiseAsRawBytes a => IsString (UsingRawBytesHex a) where
    fromString str =
      case Base16.decode (BSC.pack str) of
        Right raw -> case deserialiseFromRawBytes ttoken raw of
          Just x  -> UsingRawBytesHex x
          Nothing -> error ("fromString: cannot deserialise " ++ show str)
        Left msg -> error ("fromString: invalid hex " ++ show str ++ ", " ++ msg)
      where
        ttoken :: AsType a
        ttoken = proxyToAsType Proxy

