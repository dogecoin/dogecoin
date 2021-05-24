module Test.Process
  ( execCli
  , procCli
  , procNode
  , procChairman
  ) where

import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Function
import           Data.String
import           GHC.Stack (HasCallStack)
import           Hedgehog (MonadTest)
import           System.Process (CreateProcess)

import qualified GHC.Stack as GHC
import qualified Hedgehog.Extras.Test.Process as H

-- | Run cardano-cli, returning the stdout
execCli
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => [String]
  -> m String
execCli = GHC.withFrozenCallStack $ H.execFlex "cardano-cli" "CARDANO_CLI"

-- | Create a 'CreateProcess' describing how to start the cardano-cli process
-- and an argument list.
procCli
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => [String]
  -- ^ Arguments to the CLI command
  -> m CreateProcess
  -- ^ Captured stdout
procCli = GHC.withFrozenCallStack $ H.procFlex "cardano-cli" "CARDANO_CLI"

-- | Create a 'CreateProcess' describing how to start the cardano-node process
-- and an argument list.
procNode
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => [String]
  -- ^ Arguments to the CLI command
  -> m CreateProcess
  -- ^ Captured stdout
procNode = GHC.withFrozenCallStack $ H.procFlex "cardano-node" "CARDANO_NODE"

-- | Create a 'CreateProcess' describing how to start the cardano-node-chairman process
-- and an argument list.
procChairman
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => [String]
  -- ^ Arguments to the CLI command
  -> m CreateProcess
  -- ^ Captured stdout
procChairman = GHC.withFrozenCallStack $ H.procFlex "cardano-node-chairman" "CARDANO_NODE_CHAIRMAN" . ("run":)
