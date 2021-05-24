{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- | Stake pool off-chain metadata
--
module Cardano.Api.StakePoolMetadata (
    -- * Stake pool off-chain metadata
    StakePoolMetadata(..),
    validateAndHashStakePoolMetadata,
    StakePoolMetadataValidationError(..),

    -- * Data family instances
    AsType(..),
    Hash(..),
  ) where

import           Prelude

import           Data.Bifunctor (first)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Text (Text)
import qualified Data.Text as Text

import           Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson

import qualified Cardano.Crypto.Hash.Class as Crypto
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (StandardCrypto)
import qualified Shelley.Spec.Ledger.Keys as Shelley

import           Cardano.Api.Eras
import           Cardano.Api.Error
import           Cardano.Api.Hash
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.Key
import           Cardano.Api.KeysByron
import           Cardano.Api.KeysPraos
import           Cardano.Api.Script
import           Cardano.Api.SerialiseJSON
import           Cardano.Api.SerialiseRaw


-- ----------------------------------------------------------------------------
-- Stake pool metadata
--

-- | A representation of the required fields for off-chain stake pool metadata.
--
data StakePoolMetadata =
     StakePoolMetadata {

       -- | A name of up to 50 characters.
       stakePoolName :: !Text

       -- | A description of up to 255 characters.
     , stakePoolDescription :: !Text

       -- | A ticker of 3-5 characters, for a compact display of stake pools in
       -- a wallet.
     , stakePoolTicker :: !Text

       -- | A URL to a homepage with additional information about the pool.
       -- n.b. the spec does not specify a character limit for this field.
     , stakePoolHomepage :: !Text
     }
  deriving (Eq, Show)

newtype instance Hash StakePoolMetadata =
                 StakePoolMetadataHash (Shelley.Hash StandardCrypto ByteString)
    deriving (Eq, Show)

instance HasTypeProxy StakePoolMetadata where
    data AsType StakePoolMetadata = AsStakePoolMetadata
    proxyToAsType _ = AsStakePoolMetadata

instance SerialiseAsRawBytes (Hash StakePoolMetadata) where
    serialiseToRawBytes (StakePoolMetadataHash h) = Crypto.hashToBytes h

    deserialiseFromRawBytes (AsHash AsStakePoolMetadata) bs =
      StakePoolMetadataHash <$> Crypto.hashFromBytes bs

--TODO: instance ToJSON StakePoolMetadata where

instance FromJSON StakePoolMetadata where
    parseJSON =
        Aeson.withObject "StakePoolMetadata" $ \obj ->
          StakePoolMetadata
            <$> parseName obj
            <*> parseDescription obj
            <*> parseTicker obj
            <*> obj .: "homepage"

      where
        -- Parse and validate the stake pool metadata name from a JSON object.
        -- The name must be 50 characters or fewer.
        --
        parseName :: Aeson.Object -> Aeson.Parser Text
        parseName obj = do
          name <- obj .: "name"
          if Text.length name <= 50
            then pure name
            else fail $ "\"name\" must have at most 50 characters, but it has "
                     <> show (Text.length name)
                     <> " characters."

        -- Parse and validate the stake pool metadata description
        -- The description must be 255 characters or fewer.
        --
        parseDescription :: Aeson.Object -> Aeson.Parser Text
        parseDescription obj = do
          description <- obj .: "description"
          if Text.length description <= 255
            then pure description
            else fail $
                 "\"description\" must have at most 255 characters, but it has "
              <> show (Text.length description)
              <> " characters."

        -- | Parse and validate the stake pool ticker description
        -- The ticker must be 3 to 5 characters long.
        --
        parseTicker :: Aeson.Object -> Aeson.Parser Text
        parseTicker obj = do
          ticker <- obj .: "ticker"
          let tickerLen = Text.length ticker
          if tickerLen >= 3 && tickerLen <= 5
            then pure ticker
            else fail $
                 "\"ticker\" must have at least 3 and at most 5 "
              <> "characters, but it has "
              <> show (Text.length ticker)
              <> " characters."

-- | A stake pool metadata validation error.
data StakePoolMetadataValidationError
  = StakePoolMetadataJsonDecodeError !String
  | StakePoolMetadataInvalidLengthError
    -- ^ The length of the JSON-encoded stake pool metadata exceeds the
    -- maximum.
      !Int
      -- ^ Maximum byte length.
      !Int
      -- ^ Actual byte length.
  deriving Show

instance Error StakePoolMetadataValidationError where
    displayError (StakePoolMetadataJsonDecodeError errStr) = errStr
    displayError (StakePoolMetadataInvalidLengthError maxLen actualLen) =
         "Stake pool metadata must consist of at most "
      <> show maxLen
      <> " bytes, but it consists of "
      <> show actualLen
      <> " bytes."

-- | Decode and validate the provided JSON-encoded bytes as 'StakePoolMetadata'.
-- Return the decoded metadata and the hash of the original bytes.
--
validateAndHashStakePoolMetadata
  :: ByteString
  -> Either StakePoolMetadataValidationError
            (StakePoolMetadata, Hash StakePoolMetadata)
validateAndHashStakePoolMetadata bs
  | BS.length bs <= 512 = do
      md <- first StakePoolMetadataJsonDecodeError
                  (Aeson.eitherDecodeStrict' bs)
      let mdh = StakePoolMetadataHash (Crypto.hashWith id bs)
      return (md, mdh)
  | otherwise = Left $ StakePoolMetadataInvalidLengthError 512 (BS.length bs)

