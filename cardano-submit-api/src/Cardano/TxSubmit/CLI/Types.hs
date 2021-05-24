module Cardano.TxSubmit.CLI.Types
  ( ConfigFile (..)
  , GenesisFile (..)
  , SocketPath (..)
  , TxSubmitNodeParams (..)
  ) where

import           Cardano.Api (AnyConsensusModeParams, NetworkId (..))
import           Cardano.TxSubmit.Rest.Types (WebserverConfig)
import           System.IO (FilePath)

-- | The product type of all command line arguments
data TxSubmitNodeParams = TxSubmitNodeParams
  { tspConfigFile :: !ConfigFile
  , tspProtocol :: !AnyConsensusModeParams
  , tspNetworkId :: !NetworkId
  , tspSocketPath :: !SocketPath
  , tspWebserverConfig :: !WebserverConfig
  }

newtype ConfigFile = ConfigFile
  { unConfigFile :: FilePath
  }

newtype GenesisFile = GenesisFile
  { unGenesisFile :: FilePath
  }

newtype SocketPath = SocketPath
  { unSocketPath :: FilePath
  }
