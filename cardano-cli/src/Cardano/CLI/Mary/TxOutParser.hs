module Cardano.CLI.Mary.TxOutParser
  ( parseTxOutAnyEra
  ) where

import           Prelude

import           Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import           Data.Text (Text)
import qualified Data.Text as Text

import           Control.Applicative (some)
import           Text.Parsec (option, satisfy, (<?>))
import           Text.Parsec.Char (char, spaces)
import           Text.Parsec.String (Parser)

import           Cardano.Api (AddressAny (..), AsType (..), deserialiseAddress)
import           Cardano.CLI.Mary.ValueParser (parseValue)
import           Cardano.CLI.Types (TxOutAnyEra (..))


parseTxOutAnyEra :: Parser TxOutAnyEra
parseTxOutAnyEra = do
    addr <- parseAddressAny
    spaces
    -- Accept the old style of separating the address and value in a
    -- transaction output:
    option () (char '+' >> spaces)
    TxOutAnyEra addr <$> parseValue

parseAddressAny :: Parser AddressAny
parseAddressAny = do
  str <- plausibleAddressString <?> "address"
  case deserialiseAddress AsAddressAny str of
    Nothing -> fail "expecting valid address"
    Just addr -> pure addr

plausibleAddressString :: Parser Text
plausibleAddressString =
    Text.pack <$> some (satisfy isPlausibleAddressChar)
  where
    -- Covers both base58 and bech32 (with constrained prefixes)
    isPlausibleAddressChar c =
      isAsciiLower c
        || isAsciiUpper c
        || isDigit c
        || c == '_'
