{-# LANGUAGE NamedFieldPuns #-}

module Cardano.CLI.Mary.RenderValue
  ( RenderAdaAssetId (..)
  , RenderIndentation (..)
  , RenderPrettyValueOptions (..)
  , RenderValueOptions (..)
  , defaultRenderPrettyValueOptions
  , defaultRenderValueOptions
  , renderPrettyValue
  , renderValue
  ) where

import           Prelude

import qualified Data.ByteString as BS
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Cardano.Api (AssetId (..), AssetName (..), PolicyId (..), Quantity, Value,
                   serialiseToRawBytesHexText, valueToList)

-- | Whether the ADA asset ID should be rendered.
data RenderAdaAssetId
  = RenderAdaAssetId
    -- ^ Render the ADA asset ID.
  | RenderNoAdaAssetId
    -- ^ Do not render the ADA asset ID.
  deriving (Show, Eq)

-- | Options which detail how a representation of a 'Value' should be
-- rendered.
newtype RenderValueOptions = RenderValueOptions RenderAdaAssetId
  deriving Show

defaultRenderValueOptions :: RenderValueOptions
defaultRenderValueOptions = RenderValueOptions RenderAdaAssetId

-- | Render a textual representation of a 'Value'.
--
-- Note that this textual representation can be parsed by 'parseValue'.
renderValue :: RenderValueOptions -> Value -> Text
renderValue (RenderValueOptions renderAdaAssetId) v =
    Text.intercalate
      " + "
      (map (renderAssetIdQuantityPair renderAdaAssetId) vals)
  where
    vals :: [(AssetId, Quantity)]
    vals = valueToList v

-- | How a \"prettified\" representation of a 'Value' should be indented.
data RenderIndentation
  = IndentTab
    -- ^ Indent with a tab character (\'\\t\').
  | IndentSpaces !Int
    -- ^ Indent with a provided number of spaces.
  deriving Show

-- | Options which detail how a \"prettified\" representation of a 'Value'
-- should be rendered.
data RenderPrettyValueOptions = RenderPrettyValueOptions
  { rpvoIndentation :: !RenderIndentation
  , rpvoRenderAdaAssetId :: !RenderAdaAssetId
  } deriving Show

defaultRenderPrettyValueOptions :: RenderPrettyValueOptions
defaultRenderPrettyValueOptions =
  RenderPrettyValueOptions
    { rpvoIndentation = IndentSpaces 4
    , rpvoRenderAdaAssetId = RenderAdaAssetId
    }

-- | Render a \"prettified\" textual representation of a 'Value'.
renderPrettyValue :: RenderPrettyValueOptions -> Value -> Text
renderPrettyValue opts v =
    Text.intercalate
      ("\n" <> renderIndentation rpvoIndentation <> "+ ")
      (map (renderAssetIdQuantityPair rpvoRenderAdaAssetId) vals)
  where
    RenderPrettyValueOptions
      { rpvoIndentation
      , rpvoRenderAdaAssetId
      } = opts

    vals :: [(AssetId, Quantity)]
    vals = valueToList v

------------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------------

renderIndentation :: RenderIndentation -> Text
renderIndentation IndentTab = "\t"
renderIndentation (IndentSpaces n) = Text.replicate n " "

renderPolicyId :: PolicyId -> Text
renderPolicyId (PolicyId scriptHash) = serialiseToRawBytesHexText scriptHash

renderAssetId :: RenderAdaAssetId -> AssetId -> Text
renderAssetId RenderAdaAssetId AdaAssetId = "lovelace"
renderAssetId RenderNoAdaAssetId AdaAssetId = mempty
renderAssetId _ (AssetId polId (AssetName assetName))
  | BS.null assetName = renderPolicyId polId
  | otherwise = renderPolicyId polId <> "." <> Text.decodeUtf8 assetName

renderAssetIdQuantityPair :: RenderAdaAssetId -> (AssetId, Quantity) -> Text
renderAssetIdQuantityPair renderAdaAssetId (aId, quant) =
  Text.pack (show quant) <> " " <> renderAssetId renderAdaAssetId aId
