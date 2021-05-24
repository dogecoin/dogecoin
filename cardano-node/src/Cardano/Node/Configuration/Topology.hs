{-# LANGUAGE OverloadedStrings #-}

module Cardano.Node.Configuration.Topology
  ( TopologyError(..)
  , NetworkTopology(..)
  , NodeHostIPAddress(..)
  , NodeHostIPv4Address(..)
  , NodeHostIPv6Address(..)
  , NodeSetup(..)
  , RemoteAddress(..)
  , nodeAddressToSockAddr
  , readTopologyFile
  , remoteAddressToNodeAddress
  )
where

import           Cardano.Prelude
import           Prelude (String)

import qualified Control.Exception as Exception
import           Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as Text

import           Cardano.Node.Configuration.POM (NodeConfiguration (..))
import           Cardano.Node.Types

import           Ouroboros.Consensus.Util.Condense (Condense (..))


newtype TopologyError
  = NodeIdNotFoundInToplogyFile FilePath
  deriving Show

-- | Domain name with port number
--
data RemoteAddress = RemoteAddress
  { raAddress   :: !Text
  -- ^ Either a dns address or an ip address.
  , raPort      :: !PortNumber
  -- ^ Port number of the destination.
  , raValency :: !Int
  -- ^ If a DNS address is given valency governs
  -- to how many resolved IP addresses
  -- should we maintain active (hot) connection;
  -- if an IP address is given valency is used as
  -- a Boolean value, @0@ means to ignore the address;
  } deriving (Eq, Ord, Show)


-- | Parse 'raAddress' field as an IP address; if it parses and the valency is
-- non zero return corresponding NodeAddress.
--
remoteAddressToNodeAddress
  :: RemoteAddress
  -> Maybe (Either NodeIPAddress
                   (NodeDnsAddress, Int))
remoteAddressToNodeAddress (RemoteAddress _addrText _port 0) =
    Nothing
remoteAddressToNodeAddress (RemoteAddress addrText port valency) =
    case readMaybe (Text.unpack addrText) of
      Nothing   -> Just $ Right (NodeAddress (NodeHostDnsAddress addrText) port
                                , valency)
      Just addr -> Just $ Left  (NodeAddress (NodeHostIPAddress addr) port)


instance Condense RemoteAddress where
  condense (RemoteAddress addr port val) =
    Text.unpack addr ++ ":" ++ show port ++ " (" ++ show val ++ ")"

instance FromJSON RemoteAddress where
  parseJSON = withObject "RemoteAddress" $ \v ->
    RemoteAddress
      <$> v .: "addr"
      <*> ((fromIntegral :: Int -> PortNumber) <$> v .: "port")
      <*> v .: "valency"

instance ToJSON RemoteAddress where
  toJSON ra =
    object
      [ "addr" .= raAddress ra
      , "port" .= (fromIntegral (raPort ra) :: Int)
      , "valency" .= raValency ra
      ]

data NodeSetup = NodeSetup
  { nodeId :: !Word64
  , nodeIPv4Address :: !(Maybe NodeIPv4Address)
  , nodeIPv6Address :: !(Maybe NodeIPv6Address)
  , producers :: ![RemoteAddress]
  } deriving (Eq, Show)

instance FromJSON NodeSetup where
  parseJSON = withObject "NodeSetup" $ \o ->
                NodeSetup
                  <$> o .: "nodeId"
                  <*> o .: "nodeIPv4Address"
                  <*> o .: "nodeIPv6Address"
                  <*> o .: "producers"

instance ToJSON NodeSetup where
  toJSON ns =
    object
      [ "nodeId" .= nodeId ns
      , "nodeIPv4Address" .= nodeIPv4Address ns
      , "nodeIPv6Address" .= nodeIPv6Address ns
      , "producers" .= producers ns
      ]

data NetworkTopology = MockNodeTopology ![NodeSetup]
                     | RealNodeTopology ![RemoteAddress]
  deriving (Eq, Show)

instance FromJSON NetworkTopology where
  parseJSON = withObject "NetworkTopology" $ \o -> asum
                [ MockNodeTopology <$> o .: "MockProducers"
                , RealNodeTopology <$> o .: "Producers"
                ]

instance ToJSON NetworkTopology where
  toJSON top =
    case top of
      MockNodeTopology nss -> object [ "MockProducers" .= toJSON nss ]
      RealNodeTopology ras -> object [ "Producers" .= toJSON ras ]

-- | Read the `NetworkTopology` configuration from the specified file.
-- While running a real protocol, this gives your node its own address and
-- other remote peers it will attempt to connect to.
readTopologyFile :: NodeConfiguration -> IO (Either Text NetworkTopology)
readTopologyFile nc = do
  eBs <- Exception.try $ BS.readFile (unTopology $ ncTopologyFile nc)

  case eBs of
    Left e -> return . Left $ handler e
    Right bs -> return . first handlerJSON . eitherDecode $ LBS.fromStrict bs

 where
  handler :: IOException -> Text
  handler e = Text.pack $ "Cardano.Node.Configuration.Topology.readTopologyFile: "
                        ++ displayException e
  handlerJSON :: String -> Text
  handlerJSON err = "Is your topology file formatted correctly? \
                    \The port and valency fields should be numerical. " <> Text.pack err
