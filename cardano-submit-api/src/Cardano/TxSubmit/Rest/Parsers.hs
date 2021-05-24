{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.TxSubmit.Rest.Parsers
  ( pWebserverConfig
  ) where

import           Cardano.TxSubmit.Rest.Types (WebserverConfig (..))
import           Control.Applicative (Applicative (pure), (<$>))
import           Data.Function (($))
import           Data.Semigroup ((<>))
import           Data.String (fromString)
import           Network.Wai.Handler.Warp (HostPreference, Port)
import           Options.Applicative (Parser, auto, help, long, metavar, option, showDefault,
                   strOption, value)

pWebserverConfig :: Port -> Parser WebserverConfig
pWebserverConfig defaultPort = do
  wcHost <- pHostPreferenceOption
  wcPort <- pPortOption defaultPort
  pure WebserverConfig
    { wcHost
    , wcPort
    }

pHostPreferenceOption :: Parser HostPreference
pHostPreferenceOption = fromString <$>
  strOption
    (  long "listen-address"
    <> metavar "HOST"
    <> help
        (   "Specification of which host to the bind API server to. "
        <>  "Can be an IPv[46] address, hostname, or '*'."
        )
    <> value "127.0.0.1" <> showDefault)

pPortOption :: Port -> Parser Port
pPortOption defaultPort = option auto $ long "port"
  <> metavar "INT"
  <> help "Port used for the API server."
  <> value defaultPort
  <> showDefault
