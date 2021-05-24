{-# LANGUAGE GADTs #-}
module Cardano.CLI.Shelley.Run.Address.Info
  ( runAddressInfo
  , ShelleyAddressInfoError(..)
  ) where

import           Cardano.Api
import           Cardano.CLI.Shelley.Parsers (OutputFile (..))
import           Cardano.Prelude
import           Control.Monad.Trans.Except.Extra (left)
import           Data.Aeson (ToJSON (..), object, (.=))
import           Data.Aeson.Encode.Pretty (encodePretty)

import qualified Data.ByteString.Lazy.Char8 as LBS

newtype ShelleyAddressInfoError = ShelleyAddressInvalid Text
  deriving Show

instance Error ShelleyAddressInfoError where
  displayError (ShelleyAddressInvalid addrTxt) =
    "Invalid address: " <> show addrTxt

data AddressInfo = AddressInfo
  { aiType :: !Text
  , aiEra :: !Text
  , aiEncoding :: !Text
  , aiAddress :: !Text
  , aiBase16 :: !Text
  }

instance ToJSON AddressInfo where
  toJSON addrInfo =
    object
      [ "type" .= aiType addrInfo
      , "era" .= aiEra addrInfo
      , "encoding" .= aiEncoding addrInfo
      , "address" .= aiAddress addrInfo
      , "base16" .= aiBase16 addrInfo
      ]

runAddressInfo :: Text -> Maybe OutputFile -> ExceptT ShelleyAddressInfoError IO ()
runAddressInfo addrTxt mOutputFp = do
    addrInfo <- case (Left  <$> deserialiseAddress AsAddressAny addrTxt)
                 <|> (Right <$> deserialiseAddress AsStakeAddress addrTxt) of

      Nothing ->
        left $ ShelleyAddressInvalid addrTxt

      Just (Left (AddressByron payaddr)) ->
            pure $ AddressInfo
              { aiType = "payment"
              , aiEra = "byron"
              , aiEncoding = "base58"
              , aiAddress = addrTxt
              , aiBase16 = serialiseToRawBytesHexText payaddr
              }

      Just (Left (AddressShelley payaddr)) ->
            pure $ AddressInfo
              { aiType = "payment"
              , aiEra = "shelley"
              , aiEncoding = "bech32"
              , aiAddress = addrTxt
              , aiBase16 = serialiseToRawBytesHexText payaddr
              }

      Just (Right addr) ->
        pure $ AddressInfo
          { aiType = "stake"
          , aiEra = "shelley"
          , aiEncoding = "bech32"
          , aiAddress = addrTxt
          , aiBase16 = serialiseToRawBytesHexText addr
          }

    case mOutputFp of
      Just (OutputFile fpath) -> liftIO $ LBS.writeFile fpath $ encodePretty addrInfo
      Nothing -> liftIO $ LBS.putStrLn $ encodePretty addrInfo

