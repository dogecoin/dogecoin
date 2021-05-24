{-# LANGUAGE OverloadedStrings #-}
module Cardano.Api.TxSubmit.Types
  ( NodeApiEnv (..)
  , SocketPath (..)
  , TxSubmitStatus (..)
  , ApplyMempoolPayloadErr(..)
  , renderTxSubmitStatus
  , textShow
  ) where

import           Cardano.Api.TxSubmit.ErrorRender
import           Cardano.Binary (DecoderError)
import           Cardano.Chain.Byron.API (ApplyMempoolPayloadErr(..))
import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.UTxO as Utxo

import           Cardano.Prelude hiding ((%))

import           Data.Aeson (ToJSON (..), Value (..))
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text

import           Formatting (build, sformat, (%))


data NodeApiEnv = NodeApiEnv
  { naeConfig :: Genesis.Config
  , naeSocket :: SocketPath
  }

newtype SocketPath = SocketPath
  { unSocketPath :: FilePath
  }

data TxSubmitStatus
  = TxSubmitOk Utxo.TxId
  | TxSubmitDecodeHex
  | TxSubmitEmpty
  | TxSubmitDecodeFail DecoderError
  | TxSubmitBadTx Text
  | TxSubmitFail ApplyMempoolPayloadErr
  deriving Eq

instance ToJSON TxSubmitStatus where
  toJSON = convertJson

convertJson :: TxSubmitStatus -> Value
convertJson st =
    Aeson.object
      [ ( "status", String statusMsg )
      , ( "message", String (renderTxSubmitStatus st) )
      ]
  where
    statusMsg :: Text
    statusMsg =
      case st of
        TxSubmitOk{} -> "success"
        _other -> "fail"

renderTxSubmitStatus :: TxSubmitStatus -> Text
renderTxSubmitStatus st =
  case st of
    TxSubmitOk tx -> sformat ("Tx "% build %" submitted successfully") tx
    TxSubmitDecodeHex -> "Provided data was hex encoded and this webapi expects raw binary"
    TxSubmitEmpty -> "Provided transaction has zero length"
    TxSubmitDecodeFail err -> sformat build err
    TxSubmitBadTx tt -> mconcat ["Transactions of type '", tt, "' not supported"]
    TxSubmitFail err -> renderApplyMempoolPayloadErr err

textShow :: Show a => a -> Text
textShow = Text.pack . show
