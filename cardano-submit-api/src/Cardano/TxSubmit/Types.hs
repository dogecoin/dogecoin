{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Cardano.TxSubmit.Types
  ( TxSubmitApi
  , TxSubmitApiRecord (..)
  , TxSubmitWebApiError (..)
  , TxSubmitPort (..)
  , EnvSocketError(..)
  , TxCmdError(..)
  , RawCborDecodeError(..)
  , renderTxSubmitWebApiError
  , renderTxCmdError
  ) where

import           Cardano.Api (AnyCardanoEra, AnyConsensusMode (..), Error (..), TxId)
import           Cardano.Binary (DecoderError)
import           Cardano.TxSubmit.Util (textShow)
import           Data.Aeson (ToJSON (..), Value (..))
import           Data.ByteString.Char8 (ByteString)
import           Data.Either (Either (Right))
import           Data.Eq (Eq (..))
import           Data.Function (id, (.))
import           Data.Functor (Functor (fmap))
import           Data.Int (Int)
import           Data.Monoid (Monoid (mconcat), (<>))
import           Data.Text (Text)
import           Formatting (build, sformat)
import           GHC.Generics (Generic)
import           Network.HTTP.Media ((//))
import           Ouroboros.Consensus.Cardano.Block (EraMismatch (..))
import           Servant (Accept (..), JSON, MimeRender (..), MimeUnrender (..), PostAccepted,
                   ReqBody, (:>))
import           Servant.API.Generic (ToServantApi, (:-))
import           Text.Show (Show (..))

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.List as L

newtype TxSubmitPort = TxSubmitPort Int

-- | The errors that the raw CBOR transaction parsing\/decoding functions can return.
--
newtype RawCborDecodeError = RawCborDecodeError [DecoderError]
  deriving (Eq, Show)

instance Error RawCborDecodeError where
  displayError (RawCborDecodeError decodeErrors) = "RawCborDecodeError decode error: \n" <> L.intercalate "  \n" (fmap show decodeErrors)

-- | An error that can occur in the transaction submission web API.
data TxSubmitWebApiError
  = TxSubmitDecodeHex
  | TxSubmitEmpty
  | TxSubmitDecodeFail !DecoderError
  | TxSubmitBadTx !Text
  | TxSubmitFail TxCmdError

newtype EnvSocketError = CliEnvVarLookup Text deriving (Eq, Show)

data TxCmdError
  = TxCmdSocketEnvError EnvSocketError
  | TxCmdEraConsensusModeMismatch !AnyConsensusMode !AnyCardanoEra
  | TxCmdTxReadError !RawCborDecodeError
  | TxCmdTxSubmitError !Text
  | TxCmdTxSubmitErrorEraMismatch !EraMismatch

instance ToJSON TxSubmitWebApiError where
  toJSON = convertJson

convertJson :: TxSubmitWebApiError -> Value
convertJson = String . renderTxSubmitWebApiError

renderTxCmdError :: TxCmdError -> Text
renderTxCmdError (TxCmdSocketEnvError socketError) = "socket env error " <> textShow socketError
renderTxCmdError (TxCmdEraConsensusModeMismatch mode era) = "era consensus mode mismatch" <> textShow mode <> " " <> textShow era
renderTxCmdError (TxCmdTxReadError envelopeError) = "transaction read error " <> textShow envelopeError
renderTxCmdError (TxCmdTxSubmitError msg) = "transaction submit error " <> msg
renderTxCmdError (TxCmdTxSubmitErrorEraMismatch eraMismatch) = "transaction submit era mismatch" <> textShow eraMismatch

renderTxSubmitWebApiError :: TxSubmitWebApiError -> Text
renderTxSubmitWebApiError st =
  case st of
    TxSubmitDecodeHex -> "Provided data was hex encoded and this webapi expects raw binary"
    TxSubmitEmpty -> "Provided transaction has zero length"
    TxSubmitDecodeFail err -> sformat build err
    TxSubmitBadTx tt -> mconcat ["Transactions of type '", tt, "' not supported"]
    TxSubmitFail err -> renderTxCmdError err

-- | Servant API which provides access to tx submission webapi
type TxSubmitApi = "api" :> ToServantApi TxSubmitApiRecord

-- | A servant-generic record with all the methods of the API
newtype TxSubmitApiRecord route = TxSubmitApiRecord
  { _txSubmitPost :: route
        :- "submit"
        :> "tx"
        :> ReqBody '[CBORStream] ByteString
        :> PostAccepted '[JSON] TxId
  } deriving (Generic)

data CBORStream

instance Accept CBORStream where
  contentType _ = "application" // "cbor"

instance MimeRender CBORStream ByteString where
    mimeRender _ = LBS.fromStrict

instance MimeRender CBORStream LBS.ByteString where
    mimeRender _ = id

instance MimeUnrender CBORStream ByteString where
    mimeUnrender _ = Right . LBS.toStrict

instance MimeUnrender CBORStream LBS.ByteString where
    mimeUnrender _ = Right
