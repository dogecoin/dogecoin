module Main where

import           Cardano.Chairman.Commands
import           Control.Monad
import           Data.Function
import           Data.Semigroup
import           Options.Applicative
import           System.IO (IO)

main :: IO ()
main = join
  . customExecParser
    ( prefs (showHelpOnEmpty <> showHelpOnError)
    )
  $ info (commands <**> helper)
    (  fullDesc
    <> progDesc "Chairman checks Cardano clusters for progress and consensus."
    <> header "Chairman sits in a room full of Shelley nodes, and checks \
              \if they are all behaving ..."
    )
