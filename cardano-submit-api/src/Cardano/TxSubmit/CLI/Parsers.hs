{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.TxSubmit.CLI.Parsers
  ( opts
  , pTxSubmitNodeParams
  , pConfigFile
  , pNetworkId
  , pProtocol
  , pSocketPath
  ) where

import           Cardano.Api (AnyConsensusModeParams (..), ConsensusModeParams (..),
                   EpochSlots (..), NetworkId (..), NetworkMagic (..))
import           Cardano.TxSubmit.CLI.Types (ConfigFile (..), SocketPath (..),
                   TxSubmitNodeParams (..))
import           Cardano.TxSubmit.Rest.Parsers (pWebserverConfig)
import           Control.Applicative (Alternative (..), Applicative (..), (<**>))
import           Data.Function ((.))
import           Data.Functor (Functor (fmap), (<$>))
import           Data.Semigroup (Semigroup ((<>)))
import           Data.Word (Word64)
import           Options.Applicative (Parser, ParserInfo)

import qualified Options.Applicative as Opt

opts :: ParserInfo TxSubmitNodeParams
opts = Opt.info (pTxSubmitNodeParams <**> Opt.helper)
  (   Opt.fullDesc
  <>  Opt.progDesc "Cardano transaction submission web API."
  )

pTxSubmitNodeParams :: Parser TxSubmitNodeParams
pTxSubmitNodeParams = TxSubmitNodeParams
  <$> pConfigFile
  <*> pProtocol
  <*> pNetworkId
  <*> pSocketPath
  <*> pWebserverConfig 8090

pConfigFile :: Parser ConfigFile
pConfigFile = ConfigFile <$> Opt.strOption
  (   Opt.long "config"
  <>  Opt.help "Path to the tx-submit web API configuration file"
  <>  Opt.completer (Opt.bashCompleter "file")
  <>  Opt.metavar "FILEPATH"
  )

-- TODO: This was ripped from `cardano-cli` because, unfortunately, it's not
-- exported. Once we export this parser from the appropriate module and update
-- our `cardano-cli` dependency, we should remove this and import the parser
-- from there.
pNetworkId :: Parser NetworkId
pNetworkId = pMainnet <|> fmap Testnet pTestnetMagic
  where
    pMainnet :: Parser NetworkId
    pMainnet = Opt.flag' Mainnet
      (   Opt.long "mainnet"
      <>  Opt.help "Use the mainnet magic id."
      )

    pTestnetMagic :: Parser NetworkMagic
    pTestnetMagic = NetworkMagic <$> Opt.option Opt.auto
      (   Opt.long "testnet-magic"
      <>  Opt.metavar "NATURAL"
      <>  Opt.help "Specify a testnet magic id."
      )


-- TODO: This was ripped from `cardano-cli` because, unfortunately, it's not
-- exported. Once we export this parser from the appropriate module and update
-- our `cardano-cli` dependency, we should remove this and import the parser
-- from there.
pProtocol :: Parser AnyConsensusModeParams
pProtocol =
      ( Opt.flag' ()
        (   Opt.long "shelley-mode"
        <>  Opt.help "For talking to a node running in Shelley-only mode."
        )
        *> pShelley
      )
  <|> ( Opt.flag' ()
        (   Opt.long "byron-mode"
        <>  Opt.help "For talking to a node running in Byron-only mode."
        )
        *> pByron
      )
  <|> ( Opt.flag' ()
        (   Opt.long "cardano-mode"
        <>  Opt.help "For talking to a node running in full Cardano mode (default)."
        )
        *> pCardano
      )
  <|> -- Default to the Cardano protocol.
      pure (AnyConsensusModeParams (CardanoModeParams (EpochSlots defaultByronEpochSlots)))
  where
    pByron :: Parser AnyConsensusModeParams
    pByron = AnyConsensusModeParams . ByronModeParams <$> pEpochSlots

    pShelley :: Parser AnyConsensusModeParams
    pShelley = pure (AnyConsensusModeParams ShelleyModeParams)

    pCardano :: Parser AnyConsensusModeParams
    pCardano = AnyConsensusModeParams . CardanoModeParams <$> pEpochSlots

    pEpochSlots :: Parser EpochSlots
    pEpochSlots = EpochSlots <$> Opt.option Opt.auto
      (   Opt.long "epoch-slots"
      <>  Opt.metavar "NATURAL"
      <>  Opt.help "The number of slots per epoch for the Byron era."
      <>  Opt.value defaultByronEpochSlots -- Default to the mainnet value.
      <>  Opt.showDefault
      )

    defaultByronEpochSlots :: Word64
    defaultByronEpochSlots = 21600

pSocketPath :: Parser SocketPath
pSocketPath = SocketPath <$> Opt.strOption
  (   Opt.long "socket-path"
  <>  Opt.help "Path to a cardano-node socket"
  <>  Opt.completer (Opt.bashCompleter "file")
  <>  Opt.metavar "FILEPATH"
  )
