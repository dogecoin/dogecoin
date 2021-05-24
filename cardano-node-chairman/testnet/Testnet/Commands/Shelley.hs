module Testnet.Commands.Shelley
  ( ShelleyOptions(..)
  , cmdShelley
  , runShelleyOptions
  ) where


import           Data.Eq
import           Data.Function
import           Data.Int
import           Data.Maybe
import           Data.Semigroup
import           Options.Applicative
import           System.IO (IO)
import           Testnet.Run (runTestnet)
import           Testnet.Shelley
import           Text.Show

import qualified Options.Applicative as OA

data ShelleyOptions = ShelleyOptions
  { maybeTestnetMagic :: Maybe Int
  , testnetOptions :: TestnetOptions
  } deriving (Eq, Show)

optsTestnet :: Parser TestnetOptions
optsTestnet = TestnetOptions
  <$> OA.option auto
      (   OA.long "num-praos-nodes"
      <>  OA.help "Number of PRAOS nodes"
      <>  OA.metavar "COUNT"
      <>  OA.showDefault
      <>  OA.value (numPraosNodes defaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "num-pool-nodes"
      <>  OA.help "Number of pool nodes"
      <>  OA.metavar "COUNT"
      <>  OA.showDefault
      <>  OA.value (numPoolNodes defaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "active-slots-coeff"
      <>  OA.help "Active slots co-efficient"
      <>  OA.metavar "DOUBLE"
      <>  OA.showDefault
      <>  OA.value (activeSlotsCoeff defaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "security-param"
      <>  OA.help "Security param"
      <>  OA.metavar "INT"
      <>  OA.showDefault
      <>  OA.value (securityParam defaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "epoch-length"
      <>  OA.help "Epoch length"
      <>  OA.metavar "MILLISECONDS"
      <>  OA.showDefault
      <>  OA.value (epochLength defaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "slot-length"
      <>  OA.help "Slot length"
      <>  OA.metavar "MILLISECONDS"
      <>  OA.showDefault
      <>  OA.value (slotLength defaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "max-lovelace-supply"
      <>  OA.help "Max lovelace supply"
      <>  OA.metavar "INTEGER"
      <>  OA.showDefault
      <>  OA.value (maxLovelaceSupply defaultTestnetOptions)
      )

optsShelley :: Parser ShelleyOptions
optsShelley = ShelleyOptions
  <$> optional
      ( OA.option auto
        (   long "testnet-magic"
        <>  help "Testnet magic"
        <>  metavar "INT"
        )
      )
  <*> optsTestnet

runShelleyOptions :: ShelleyOptions -> IO ()
runShelleyOptions options = runTestnet (maybeTestnetMagic options) $
  Testnet.Shelley.testnet (testnetOptions options)

cmdShelley :: Mod CommandFields (IO ())
cmdShelley = command "shelley"  $ flip info idm $ runShelleyOptions <$> optsShelley
