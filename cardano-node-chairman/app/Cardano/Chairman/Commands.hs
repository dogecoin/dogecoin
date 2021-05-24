module Cardano.Chairman.Commands where

import           Cardano.Chairman.Commands.Run
import           Cardano.Chairman.Commands.Version
import           Data.Function
import           Data.Monoid
import           Options.Applicative
import           System.IO (IO)

{- HLINT ignore "Monoid law, left identity" -}

commands :: Parser (IO ())
commands = subparser $ mempty
  <>  cmdRun
  <>  cmdVersion
