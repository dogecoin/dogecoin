{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Node.Types
  ( -- * Configuration
    AdjustFilePaths(..)
  , ConfigError(..)
  , ConfigYamlFilePath(..)
  , DbFile(..)
  , GenesisFile(..)
  , ProtocolFilepaths (..)
  , GenesisHash(..)
  , MaxConcurrencyBulkSync(..)
  , MaxConcurrencyDeadline(..)
    -- * Node addresses
  , NodeAddress'(..)
  , NodeIPAddress
  , nodeAddressToSockAddr
  , NodeIPv4Address
  , NodeIPv6Address
  , NodeDnsAddress
  , nodeIPv4ToIPAddress
  , nodeIPv6ToIPAddress
  , nodeDnsAddressToDomainAddress
  , NodeHostIPAddress (..)
  , nodeHostIPAddressToSockAddr
  , NodeHostIPv4Address (..)
  , NodeHostIPv6Address (..)
  , nodeHostIPv4AddressToIPAddress
  , nodeHostIPv6AddressToIPAddress
  , NodeHostDnsAddress (..)
  , nodeHostDnsAddressToDomain
  , PortNumber
  , SocketPath(..)
  , TopologyFile(..)
  , NodeDiffusionMode (..)
    -- * Consensus protocol configuration
  , NodeByronProtocolConfiguration(..)
  , NodeHardForkProtocolConfiguration(..)
  , NodeProtocolConfiguration(..)
  , NodeShelleyProtocolConfiguration(..)
  , VRFPrivateKeyFilePermissionError(..)
  , protocolName
  , renderVRFPrivateKeyFilePermissionError
  ) where

import           Cardano.Prelude
import           Prelude (String, fail)

import           Data.Aeson
import           Data.IP (IP (..), IPv4, IPv6)
import qualified Data.IP as IP
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Network.DNS as DNS (Domain)
import           Network.Socket (PortNumber, SockAddr (..))

import qualified Cardano.Chain.Update as Byron
import           Cardano.Crypto (RequiresNetworkMagic (..))
import qualified Cardano.Crypto.Hash as Crypto
import           Cardano.Node.Protocol.Types (Protocol (..))
import           Cardano.Slotting.Slot (EpochNo)
import           Ouroboros.Network.PeerSelection.RootPeersDNS (DomainAddress (..))

--TODO: things will probably be clearer if we don't use these newtype wrappers and instead
-- use records with named fields in the CLI code.
import           Ouroboros.Network.NodeToNode (DiffusionMode (..))

-- | Errors for the cardano-config module.
newtype ConfigError = ConfigErrorFileNotFound FilePath
    deriving Show

-- | Filepath of the configuration yaml file. This file determines
-- all the configuration settings required for the cardano node
-- (logging, tracing, protocol, slot length etc)
newtype ConfigYamlFilePath = ConfigYamlFilePath
  { unConfigPath :: FilePath }
  deriving newtype (Eq, Show)

newtype DbFile = DbFile
  { unDB :: FilePath }
  deriving newtype (Eq, Show)

newtype GenesisFile = GenesisFile
  { unGenesisFile :: FilePath }
  deriving stock (Eq, Ord)
  deriving newtype (IsString, Show)

instance FromJSON GenesisFile where
  parseJSON (String genFp) = pure . GenesisFile $ Text.unpack genFp
  parseJSON invalid = fail $ "Parsing of GenesisFile failed due to type mismatch. "
                          <> "Encountered: " <> show invalid

newtype MaxConcurrencyBulkSync = MaxConcurrencyBulkSync
  { unMaxConcurrencyBulkSync :: Word }
  deriving stock (Eq, Ord)
  deriving newtype (FromJSON, Show)

newtype MaxConcurrencyDeadline = MaxConcurrencyDeadline
  { unMaxConcurrencyDeadline :: Word }
  deriving stock (Eq, Ord)
  deriving newtype (FromJSON, Show)


-- | IPv4 or IPv6 address with a port number.
data NodeAddress' addr = NodeAddress
  { naHostAddress :: !addr
  , naPort :: !PortNumber
  } deriving (Eq, Ord, Show, Functor)

type NodeIPAddress   = NodeAddress' NodeHostIPAddress
type NodeIPv4Address = NodeAddress' NodeHostIPv4Address
type NodeIPv6Address = NodeAddress' NodeHostIPv6Address
type NodeDnsAddress  = NodeAddress' NodeHostDnsAddress


instance FromJSON addr => FromJSON (NodeAddress' addr) where
  parseJSON = withObject "NodeAddress" $ \v -> do
    NodeAddress
      <$> v .: "addr"
      <*> ((fromIntegral :: Int -> PortNumber) <$> v .: "port")

instance ToJSON addr => ToJSON (NodeAddress' addr) where
  toJSON na =
    object
      [ "addr" .= toJSON (naHostAddress na)
      , "port" .= (fromIntegral (naPort na) :: Int)
      ]


nodeIPv4ToIPAddress :: NodeIPv4Address -> NodeIPAddress
nodeIPv4ToIPAddress = fmap nodeHostIPv4AddressToIPAddress

nodeIPv6ToIPAddress :: NodeIPv6Address -> NodeIPAddress
nodeIPv6ToIPAddress = fmap nodeHostIPv6AddressToIPAddress

nodeDnsAddressToDomainAddress :: NodeDnsAddress -> DomainAddress
nodeDnsAddressToDomainAddress NodeAddress { naHostAddress = NodeHostDnsAddress dns, naPort }
  = DomainAddress (Text.encodeUtf8 dns) naPort

nodeAddressToSockAddr :: NodeIPAddress -> SockAddr
nodeAddressToSockAddr (NodeAddress addr port) =
  case unNodeHostIPAddress addr of
    IP.IPv4 ipv4 -> SockAddrInet  port   (IP.toHostAddress ipv4)
    IP.IPv6 ipv6 -> SockAddrInet6 port 0 (IP.toHostAddress6 ipv6) 0

nodeHostIPAddressToSockAddr :: NodeIPAddress -> SockAddr
nodeHostIPAddressToSockAddr NodeAddress { naHostAddress = NodeHostIPAddress ip, naPort } =
    case ip of
      IPv4 ipv4 -> SockAddrInet  (fromIntegral naPort)   (IP.toHostAddress ipv4)
      IPv6 ipv6 -> SockAddrInet6 (fromIntegral naPort) 0 (IP.toHostAddress6 ipv6) 0


newtype NodeHostIPv4Address
  = NodeHostIPv4Address { unNodeHostIPv4Address :: IPv4 }
  deriving newtype Show
  deriving (Eq, Ord)

instance FromJSON NodeHostIPv4Address where
  parseJSON (String ipStr) =
    case readMaybe $ Text.unpack ipStr of
      Just ip -> pure $ NodeHostIPv4Address ip
      Nothing -> fail $ "Parsing of IPv4 failed: " <> Text.unpack ipStr
  parseJSON invalid = fail $ "Parsing of IPv4 failed due to type mismatch. "
                           <> "Encountered: " <> show invalid <> "\n"

instance ToJSON NodeHostIPv4Address where
  toJSON (NodeHostIPv4Address ip) = String (Text.pack $ show ip)


newtype NodeHostIPv6Address
  = NodeHostIPv6Address { unNodeHostIPv6Address :: IPv6 }
  deriving newtype Show
  deriving (Eq, Ord)

instance FromJSON NodeHostIPv6Address where
  parseJSON (String ipStr) =
    case readMaybe $ Text.unpack ipStr of
      Just ip -> pure $ NodeHostIPv6Address ip
      Nothing -> fail $ "Parsing of IPv6 failed: " <> Text.unpack ipStr
  parseJSON invalid = fail $ "Parsing of IPv6 failed due to type mismatch. "
                          <> "Encountered: " <> show invalid <> "\n"
instance ToJSON NodeHostIPv6Address where
  toJSON (NodeHostIPv6Address ip) = String (Text.pack $ show ip)


newtype NodeHostIPAddress
  = NodeHostIPAddress { unNodeHostIPAddress :: IP }
  deriving newtype Show
  deriving (Eq, Ord)

instance FromJSON NodeHostIPAddress where
  parseJSON (String ipStr) =
    case readMaybe $ Text.unpack ipStr of
      Just ip -> pure $ NodeHostIPAddress ip
      Nothing -> fail $ "Parsing of IP failed: " <> Text.unpack ipStr
  parseJSON invalid = fail $ "Parsing of IP failed due to type mismatch. "
                          <> "Encountered: " <> show invalid <> "\n"

instance ToJSON NodeHostIPAddress where
  toJSON (NodeHostIPAddress ip) = String (Text.pack $ show ip)


nodeHostIPv6AddressToIPAddress :: NodeHostIPv6Address -> NodeHostIPAddress
nodeHostIPv6AddressToIPAddress (NodeHostIPv6Address ip) = NodeHostIPAddress (IPv6 ip)

nodeHostIPv4AddressToIPAddress :: NodeHostIPv4Address -> NodeHostIPAddress
nodeHostIPv4AddressToIPAddress (NodeHostIPv4Address ip) = NodeHostIPAddress (IPv4 ip)


-- | Domain name.
--
newtype NodeHostDnsAddress
  = NodeHostDnsAddress { unNodeHostDnsAddress :: Text }
  deriving newtype Show
  deriving (Eq, Ord)

nodeHostDnsAddressToDomain :: NodeHostDnsAddress -> DNS.Domain
nodeHostDnsAddressToDomain = Text.encodeUtf8 . unNodeHostDnsAddress


-- | Newtype wrapper which provides 'FromJSON' instance for 'DiffusionMode'.
--
newtype NodeDiffusionMode
  = NodeDiffusionMode { getDiffusionMode :: DiffusionMode }
  deriving newtype Show

instance FromJSON NodeDiffusionMode where
    parseJSON (String str) =
      case str of
        "InitiatorOnly"
          -> pure $ NodeDiffusionMode InitiatorOnlyDiffusionMode
        "InitiatorAndResponder"
          -> pure $ NodeDiffusionMode InitiatorAndResponderDiffusionMode
        _ -> fail "Parsing NodeDiffusionMode failed: can be either 'InitiatorOnly' or 'InitiatorAndResponder'"
    parseJSON _ = fail "Parsing NodeDiffusionMode failed"

class AdjustFilePaths a where
  adjustFilePaths :: (FilePath -> FilePath) -> a -> a


data ProtocolFilepaths =
     ProtocolFilepaths {
       byronCertFile        :: !(Maybe FilePath)
     , byronKeyFile         :: !(Maybe FilePath)
     , shelleyKESFile       :: !(Maybe FilePath)
     , shelleyVRFFile       :: !(Maybe FilePath)
     , shelleyCertFile      :: !(Maybe FilePath)
     , shelleyBulkCredsFile :: !(Maybe FilePath)
     } deriving (Eq, Show)

newtype GenesisHash = GenesisHash (Crypto.Hash Crypto.Blake2b_256 ByteString)
  deriving newtype (Eq, Show, ToJSON, FromJSON)

data NodeProtocolConfiguration =
       NodeProtocolConfigurationByron   NodeByronProtocolConfiguration
     | NodeProtocolConfigurationShelley NodeShelleyProtocolConfiguration
     | NodeProtocolConfigurationCardano NodeByronProtocolConfiguration
                                        NodeShelleyProtocolConfiguration
                                        NodeHardForkProtocolConfiguration
  deriving (Eq, Show)

data NodeShelleyProtocolConfiguration =
     NodeShelleyProtocolConfiguration {
       npcShelleyGenesisFile     :: !GenesisFile
     , npcShelleyGenesisFileHash :: !(Maybe GenesisHash)
     }
  deriving (Eq, Show)

data NodeByronProtocolConfiguration =
     NodeByronProtocolConfiguration {
       npcByronGenesisFile         :: !GenesisFile
     , npcByronGenesisFileHash     :: !(Maybe GenesisHash)
     , npcByronReqNetworkMagic     :: !RequiresNetworkMagic
     , npcByronPbftSignatureThresh :: !(Maybe Double)

       --TODO: eliminate these two: it can be hard-coded
       -- | Update application name.
     , npcByronApplicationName     :: !Byron.ApplicationName

       -- | Application (ie software) version.
     , npcByronApplicationVersion  :: !Byron.NumSoftwareVersion

       --TODO: eliminate these: it can be done automatically in consensus
       -- | These declare the version of the protocol that the node is prepared
       -- to run. This is usually the version of the protocol in use on the
       -- chain now, but during protocol updates this version will be the one
       -- that we declare that we are ready to move to. This is the endorsement
       -- mechanism for determining when enough block producers are ready to
       -- move to the next version.
       --
     , npcByronSupportedProtocolVersionMajor :: !Word16
     , npcByronSupportedProtocolVersionMinor :: !Word16
     , npcByronSupportedProtocolVersionAlt   :: !Word8
     }
  deriving (Eq, Show)

-- | Configuration relating to a hard forks themselves, not the specific eras.
--
data NodeHardForkProtocolConfiguration =
     NodeHardForkProtocolConfiguration {

       -- | During the development and integration of new eras we wish to be
       -- able to test the hard fork transition into the new era, but we do not
       -- wish to generally have the node advertise that it understands the new
       -- era. Avoiding advertising new development eras until they are ready
       -- makes it practical to include new not-yet-ready eras into the main
       -- release version of the node without the danger that operators on the
       -- mainnet will prematurely advertise that their nodes are capable of
       -- crossing the next hard fork.
       --
       -- It should /always/ remain at the default of false for nodes running
       -- on the mainnet.
       --
       -- This flag should be set to true for nodes taking part in testnets for
       -- testing the new era.
       --
       npcTestEnableDevelopmentHardForkEras :: Bool

       -- | For testing purposes we support specifying that the hard fork
       -- happens at an exact epoch number (ie the first epoch of the new era).
       --
       -- Obviously if this is used, all the nodes in the test cluster must be
       -- configured the same, or they will disagree.
       --
     , npcTestShelleyHardForkAtEpoch :: Maybe EpochNo

       -- | For testing purposes we support specifying that the hard fork
       -- happens at a given major protocol version. For example this can be
       -- used to cause the Shelley hard fork to occur at the transition from
       -- protocol version 0 to version 1 (rather than the default of from 1 to
       -- 2) which can make the test setup simpler.
       --
       -- Obviously if this is used, all the nodes in the test cluster must be
       -- configured the same, or they will disagree.
       --
     , npcTestShelleyHardForkAtVersion :: Maybe Word

       -- | For testing purposes we support specifying that the hard fork
       -- happens at an exact epoch number (ie the first epoch of the new era).
       --
       -- Obviously if this is used, all the nodes in the test cluster must be
       -- configured the same, or they will disagree.
       --
     , npcTestAllegraHardForkAtEpoch :: Maybe EpochNo

       -- | For testing purposes we support specifying that the hard fork
       -- happens at a given major protocol version.
       --
       -- Obviously if this is used, all the nodes in the test cluster must be
       -- configured the same, or they will disagree.
       --
     , npcTestAllegraHardForkAtVersion :: Maybe Word

       -- | For testing purposes we support specifying that the hard fork
       -- happens at an exact epoch number (ie the first epoch of the new era).
       --
       -- Obviously if this is used, all the nodes in the test cluster must be
       -- configured the same, or they will disagree.
       --
     , npcTestMaryHardForkAtEpoch :: Maybe EpochNo

       -- | For testing purposes we support specifying that the hard fork
       -- happens at a given major protocol version.
       --
       -- Obviously if this is used, all the nodes in the test cluster must be
       -- configured the same, or they will disagree.
       --
       --
     , npcTestMaryHardForkAtVersion :: Maybe Word

       -- | For testing purposes we support specifying that the hard fork
       -- happens at an exact epoch number (ie the first epoch of the new era).
       --
       -- Obviously if this is used, all the nodes in the test cluster must be
       -- configured the same, or they will disagree.
       --
     , npcTestAlonzoHardForkAtEpoch :: Maybe EpochNo

       -- | For testing purposes we support specifying that the hard fork
       -- happens at a given major protocol version.
       --
       -- Obviously if this is used, all the nodes in the test cluster must be
       -- configured the same, or they will disagree.
       --
     , npcTestAlonzoHardForkAtVersion :: Maybe Word
     }
  deriving (Eq, Show)

newtype SocketPath = SocketPath
  { unSocketPath :: FilePath }
  deriving stock (Eq, Ord)
  deriving newtype (FromJSON, IsString, Show)

newtype TopologyFile = TopologyFile
  { unTopology :: FilePath }
  deriving newtype (Show, Eq)

instance AdjustFilePaths NodeProtocolConfiguration where

  adjustFilePaths f (NodeProtocolConfigurationByron pc) =
    NodeProtocolConfigurationByron (adjustFilePaths f pc)

  adjustFilePaths f (NodeProtocolConfigurationShelley pc) =
    NodeProtocolConfigurationShelley (adjustFilePaths f pc)

  adjustFilePaths f (NodeProtocolConfigurationCardano pcb pcs pch) =
    NodeProtocolConfigurationCardano (adjustFilePaths f pcb)
                                     (adjustFilePaths f pcs)
                                     pch

instance AdjustFilePaths NodeByronProtocolConfiguration where
  adjustFilePaths f x@NodeByronProtocolConfiguration {
                        npcByronGenesisFile
                      } =
    x { npcByronGenesisFile = adjustFilePaths f npcByronGenesisFile }

instance AdjustFilePaths NodeShelleyProtocolConfiguration where
  adjustFilePaths f x@NodeShelleyProtocolConfiguration {
                        npcShelleyGenesisFile
                      } =
    x { npcShelleyGenesisFile = adjustFilePaths f npcShelleyGenesisFile }

instance AdjustFilePaths SocketPath where
  adjustFilePaths f (SocketPath p) = SocketPath (f p)

instance AdjustFilePaths GenesisFile where
  adjustFilePaths f (GenesisFile p) = GenesisFile (f p)

instance AdjustFilePaths a => AdjustFilePaths (Maybe a) where
  adjustFilePaths f = fmap (adjustFilePaths f)


instance AdjustFilePaths (Last NodeProtocolConfiguration) where

  adjustFilePaths f (Last (Just npc)) =
    Last . Just $ adjustFilePaths f npc

  adjustFilePaths _ (Last Nothing) = Last Nothing

instance AdjustFilePaths (Last SocketPath) where
  adjustFilePaths f (Last (Just (SocketPath p))) = Last . Just $ SocketPath (f p)
  adjustFilePaths _ (Last Nothing) = Last Nothing

-- | A human readable name for the protocol
--
protocolName :: Protocol -> String
protocolName ByronProtocol   = "Byron"
protocolName ShelleyProtocol = "Shelley"
protocolName CardanoProtocol = "Byron; Shelley"


data VRFPrivateKeyFilePermissionError
  = OtherPermissionsExist FilePath
  | GroupPermissionsExist FilePath
  | GenericPermissionsExist FilePath
  deriving Show

renderVRFPrivateKeyFilePermissionError :: VRFPrivateKeyFilePermissionError -> Text
renderVRFPrivateKeyFilePermissionError err =
  case err of
    OtherPermissionsExist fp ->
      "VRF private key file at: " <> Text.pack fp
      <> " has \"other\" file permissions. Please remove all \"other\" file permissions."

    GroupPermissionsExist fp ->
      "VRF private key file at: " <> Text.pack fp
      <> "has \"group\" file permissions. Please remove all \"group\" file permissions."
    GenericPermissionsExist fp ->
      "VRF private key file at: " <> Text.pack fp
      <> "has \"generic\" file permissions. Please remove all \"generic\" file permissions."
