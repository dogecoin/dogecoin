module Cardano.CLI.Mary.ValueParser
  ( parseValue
  ) where

import           Prelude

import qualified Data.Char as Char
import           Data.Functor (void, ($>))
import           Data.List (foldl')
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Word (Word64)

import           Control.Applicative (some, (<|>))

import           Text.Parsec as Parsec (notFollowedBy, try, (<?>))
import           Text.Parsec.Char (alphaNum, char, digit, hexDigit, space, spaces, string)
import           Text.Parsec.Expr (Assoc (..), Operator (..), buildExpressionParser)
import           Text.Parsec.String (Parser)
import           Text.ParserCombinators.Parsec.Combinator (many1)

import           Cardano.Api

-- | Parse a 'Value' from its string representation.
parseValue :: Parser Value
parseValue = evalValueExpr <$> parseValueExpr

-- | Evaluate a 'ValueExpr' and construct a 'Value'.
evalValueExpr :: ValueExpr -> Value
evalValueExpr vExpr =
  case vExpr of
    ValueExprAdd x y -> evalValueExpr x <> evalValueExpr y
    ValueExprNegate x -> negateValue (evalValueExpr x)
    ValueExprLovelace quant -> valueFromList [(AdaAssetId, quant)]
    ValueExprMultiAsset polId aName quant ->
      valueFromList [(AssetId polId aName , quant)]


------------------------------------------------------------------------------
-- Expression parser
------------------------------------------------------------------------------

-- | Intermediate representation of a parsed multi-asset value.
data ValueExpr
  = ValueExprAdd !ValueExpr !ValueExpr
  | ValueExprNegate !ValueExpr
  | ValueExprLovelace !Quantity
  | ValueExprMultiAsset !PolicyId !AssetName !Quantity
  deriving (Eq, Ord, Show)

parseValueExpr :: Parser ValueExpr
parseValueExpr =
    buildExpressionParser operatorTable valueExprTerm
      <?> "multi-asset value expression"
  where
    operatorTable =
      [ [Prefix negateOp]
      , [Infix  plusOp AssocLeft]
      ]

-- | Parse either a 'ValueExprLovelace' or 'ValueExprMultiAsset'.
valueExprTerm :: Parser ValueExpr
valueExprTerm = do
    q <- try quantity <?> "quantity (word64)"
    aId <- try assetIdUnspecified <|> assetIdSpecified <?> "asset id"
    _ <- spaces
    pure $ case aId of
      AdaAssetId -> ValueExprLovelace q
      AssetId polId aName -> ValueExprMultiAsset polId aName q
  where
    -- Parse an asset ID which must be lead by one or more whitespace
    -- characters and may be trailed by whitespace characters.
    assetIdSpecified :: Parser AssetId
    assetIdSpecified = some space *> assetId

    -- Default for if an asset ID is not specified.
    assetIdUnspecified :: Parser AssetId
    assetIdUnspecified =
      spaces
        *> notFollowedBy alphaNum
        $> AdaAssetId

------------------------------------------------------------------------------
-- Primitive parsers
------------------------------------------------------------------------------

plusOp :: Parser (ValueExpr -> ValueExpr -> ValueExpr)
plusOp = (char '+' *> spaces) $> ValueExprAdd

negateOp :: Parser (ValueExpr -> ValueExpr)
negateOp = (char '-' *> spaces) $> ValueExprNegate

-- | Period (\".\") parser.
period :: Parser ()
period = void $ char '.'

-- | Word64 parser.
word64 :: Parser Integer
word64 = do
  i <- decimal
  if i > fromIntegral (maxBound :: Word64)
    then
      fail $
        "expecting word64, but the number exceeds the max bound: " <> show i
    else return i

decimal :: Parser Integer
decimal = do
    digits <- many1 digit
    return $! foldl' (\x d -> 10*x + toInteger (Char.digitToInt d)) 0 digits

-- | Asset name parser.
assetName :: Parser AssetName
assetName =
    toAssetName <$> some alphaNum
  where
    toAssetName = AssetName . Text.encodeUtf8 . Text.pack

-- | Policy ID parser.
policyId :: Parser PolicyId
policyId = do
  hexText <- many1 hexDigit
  case textToPolicyId hexText of
    Just p -> pure p
    Nothing ->
      fail $ "expecting a 56 hex-encoded policy ID, but found only "
          ++ show (length hexText) ++ " hex digits"
  where
    textToPolicyId =
        fmap PolicyId
      . deserialiseFromRawBytesHex AsScriptHash
      . Text.encodeUtf8
      . Text.pack

-- | Asset ID parser.
assetId :: Parser AssetId
assetId =
    try adaAssetId
      <|> nonAdaAssetId
      <?> "asset ID"
  where
    -- Parse the ADA asset ID.
    adaAssetId :: Parser AssetId
    adaAssetId = string "lovelace" $> AdaAssetId

    -- Parse a multi-asset ID.
    nonAdaAssetId :: Parser AssetId
    nonAdaAssetId = do
      polId <- policyId
      fullAssetId polId <|> assetIdNoAssetName polId

    -- Parse a fully specified multi-asset ID with both a policy ID and asset
    -- name.
    fullAssetId :: PolicyId -> Parser AssetId
    fullAssetId polId = do
      _ <- period
      aName <- assetName <?> "alphanumeric asset name"
      pure (AssetId polId aName)

    -- Parse a multi-asset ID that specifies a policy ID, but no asset name.
    assetIdNoAssetName :: PolicyId -> Parser AssetId
    assetIdNoAssetName polId = pure (AssetId polId "")

-- | Quantity (word64) parser.
quantity :: Parser Quantity
quantity = fmap Quantity word64
