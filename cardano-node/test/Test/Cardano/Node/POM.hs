{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Node.POM
  ( tests
  ) where

import           Cardano.Prelude

import           Data.Time.Clock (secondsToDiffTime)

import           Cardano.Node.Configuration.POM
import           Cardano.Node.Types
import           Cardano.Tracing.Config (TraceOptions (..))
import           Ouroboros.Network.Block (MaxSlotNo (..), SlotNo (..))
import           Ouroboros.Network.NodeToNode (DiffusionMode (InitiatorAndResponderDiffusionMode))
import           Ouroboros.Consensus.Storage.LedgerDB.DiskPolicy (SnapshotInterval (..))

import           Hedgehog (Property, discover, withTests, (===))
import qualified Hedgehog
import           Hedgehog.Internal.Property (failWith)


-- This is a simple test to check that the POM technique is working as intended.
-- What is entered on the command line via the cli takes precedence and this is
-- tested in the property below.
-- See: https://medium.com/@jonathangfischoff/the-partial-options-monoid-pattern-31914a71fc67

prop_sanityCheck_POM :: Property
prop_sanityCheck_POM =
   withTests 1 . Hedgehog.property $ do
    let combinedPartials = defaultPartialNodeConfiguration
                             <> testPartialYamlConfig
                             <> testPartialCliConfig
        nc = makeNodeConfiguration combinedPartials
    case nc of
      Left err -> failWith Nothing $ "Partial Options Monoid sanity check failure: " <> err
      Right config -> config === expectedConfig

-- | Example partial configuration theoretically created from a
-- config yaml file.
testPartialYamlConfig :: PartialNodeConfiguration
testPartialYamlConfig =
  PartialNodeConfiguration
    { pncProtocolConfig = Last . Just
                        . NodeProtocolConfigurationShelley
                        $ NodeShelleyProtocolConfiguration
                            (GenesisFile "dummmy-genesis-file") Nothing
    , pncSocketPath = Last Nothing
    , pncDiffusionMode = Last Nothing
    , pncSnapshotInterval = mempty
    , pncTestEnableDevelopmentNetworkProtocols = Last Nothing
    , pncMaxConcurrencyBulkSync = Last Nothing
    , pncMaxConcurrencyDeadline = Last Nothing
    , pncLoggingSwitch = Last $ Just True
    , pncLogMetrics = Last $ Just True
    , pncTraceConfig = Last $ Just TracingOff
    , pncNodeIPv4Addr = mempty
    , pncNodeIPv6Addr = mempty
    , pncNodePortNumber = mempty
    , pncConfigFile = mempty
    , pncTopologyFile = mempty
    , pncDatabaseFile = mempty
    , pncProtocolFiles = mempty
    , pncValidateDB = mempty
    , pncShutdownIPC = mempty
    , pncShutdownOnSlotSynced = mempty
    }

-- | Example partial configuration theoretically created
-- from what was parsed on the command line.
testPartialCliConfig :: PartialNodeConfiguration
testPartialCliConfig =
  PartialNodeConfiguration
    { pncNodeIPv4Addr = mempty
    , pncNodeIPv6Addr = mempty
    , pncNodePortNumber = mempty
    , pncConfigFile   = mempty
    , pncTopologyFile = mempty
    , pncDatabaseFile = mempty
    , pncSocketPath   = mempty
    , pncDiffusionMode = mempty
    , pncSnapshotInterval = Last . Just . RequestedSnapshotInterval $ secondsToDiffTime 100
    , pncTestEnableDevelopmentNetworkProtocols = Last $ Just True
    , pncProtocolFiles = Last . Just $ ProtocolFilepaths Nothing Nothing Nothing Nothing Nothing Nothing
    , pncValidateDB = Last $ Just True
    , pncShutdownIPC = Last $ Just Nothing
    , pncShutdownOnSlotSynced = Last . Just . MaxSlotNo $ SlotNo 42
    , pncProtocolConfig = mempty
    , pncMaxConcurrencyBulkSync = mempty
    , pncMaxConcurrencyDeadline = mempty
    , pncLoggingSwitch = mempty
    , pncLogMetrics = mempty
    , pncTraceConfig = mempty
    }

-- | Expected final NodeConfiguration
expectedConfig :: NodeConfiguration
expectedConfig =
  NodeConfiguration
    { ncNodeIPv4Addr = Nothing
    , ncNodeIPv6Addr = Nothing
    , ncNodePortNumber = Nothing
    , ncConfigFile = ConfigYamlFilePath "configuration/cardano/mainnet-config.json"
    , ncTopologyFile = TopologyFile "configuration/cardano/mainnet-topology.json"
    , ncDatabaseFile = DbFile "mainnet/db/"
    , ncProtocolFiles = ProtocolFilepaths Nothing Nothing Nothing Nothing Nothing Nothing
    , ncValidateDB = True
    , ncShutdownIPC = Nothing
    , ncShutdownOnSlotSynced = MaxSlotNo $ SlotNo 42
    , ncProtocolConfig = NodeProtocolConfigurationShelley
                           $ NodeShelleyProtocolConfiguration
                             (GenesisFile "dummmy-genesis-file") Nothing
    , ncSocketPath = Nothing
    , ncDiffusionMode = InitiatorAndResponderDiffusionMode
    , ncSnapshotInterval = RequestedSnapshotInterval $ secondsToDiffTime 100
    , ncTestEnableDevelopmentNetworkProtocols = True
    , ncMaxConcurrencyBulkSync = Nothing
    , ncMaxConcurrencyDeadline = Nothing
    , ncLoggingSwitch = True
    , ncLogMetrics = True
    , ncTraceConfig = TracingOff
    }

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  Hedgehog.checkParallel $$discover
