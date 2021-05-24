module Testnet.Commands.Byron
  ( ByronOptions(..)
  , cmdByron
  , runByronOptions
  ) where

import           Data.Eq
import           Data.Function
import           Data.Int
import           Data.Maybe
import           Data.Semigroup
import           Options.Applicative
import           System.IO (IO)
import           Testnet.Byron
import           Testnet.Run (runTestnet)
import           Text.Show

import qualified Options.Applicative as OA

data ByronOptions = ByronOptions
  { maybeTestnetMagic :: Maybe Int
  , testnetOptions :: TestnetOptions
  } deriving (Eq, Show)

optsByron :: Parser ByronOptions
optsByron = ByronOptions
  <$> optional
      ( OA.option auto
        (   long "testnet-magic"
        <>  help "Testnet magic"
        <>  metavar "INT"
        )
      )
  <*> optsTestnet

optsTestnet :: Parser TestnetOptions
optsTestnet = TestnetOptions
  <$> OA.option auto
      (   OA.long "num-bft-nodes"
      <>  OA.help "Number of BFT nodes"
      <>  OA.metavar "COUNT"
      <>  OA.showDefault
      <>  OA.value (numBftNodes defaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "slot-duration"
      <>  OA.help "Slot duration"
      <>  OA.metavar "MILLISECONDS"
      <>  OA.showDefault
      <>  OA.value (slotDuration defaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "security-param"
      <>  OA.help "Security parameter"
      <>  OA.metavar "INT"
      <>  OA.showDefault
      <>  OA.value (securityParam defaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "n-poor-addresses"
      <>  OA.help "N poor addresses"
      <>  OA.metavar "INT"
      <>  OA.showDefault
      <>  OA.value (nPoorAddresses defaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "total-balance"
      <>  OA.help "Total Balance"
      <>  OA.metavar "INT"
      <>  OA.showDefault
      <>  OA.value (totalBalance defaultTestnetOptions)
      )

runByronOptions :: ByronOptions -> IO ()
runByronOptions opts = runTestnet (maybeTestnetMagic opts) (Testnet.Byron.testnet (testnetOptions opts))

cmdByron :: Mod CommandFields (IO ())
cmdByron = command "byron" $ flip info idm $ runByronOptions <$> optsByron
