{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

module Cardano.Node.Run
  ( runNode
  , checkVRFFilePermissions
  ) where

import           Cardano.Prelude hiding (ByteString, atomically, take, trace)
import           Prelude (String)

import qualified Control.Concurrent.Async as Async
import           Control.Monad.Trans.Except.Extra (left)
import           Control.Tracer
import           Data.Text (breakOn, pack, take)
import qualified Data.Text as Text
import           Data.Time.Clock (getCurrentTime)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import           Data.Version (showVersion)
import           Network.HostName (getHostName)
import           Network.Socket (AddrInfo, Socket)
import           System.Directory (canonicalizePath, createDirectoryIfMissing, makeAbsolute)
import           System.Environment (lookupEnv)
#ifdef UNIX
import           System.Posix.Files
import           System.Posix.Types (FileMode)
#else
import           System.Win32.File
#endif

import           Cardano.BM.Data.LogItem (LOContent (..), LogObject (..), PrivacyAnnotation (..),
                   mkLOMeta)
import           Cardano.BM.Data.Tracer (ToLogObject (..), TracingVerbosity (..))
import           Cardano.BM.Data.Transformers (setHostname)
import           Cardano.BM.Trace
import           Paths_cardano_node (version)

import qualified Cardano.Crypto.Libsodium as Crypto

import           Cardano.Node.Configuration.Logging (LoggingLayer (..), Severity (..),
                   createLoggingLayer, nodeBasicInfo, shutdownLoggingLayer)
import           Cardano.Node.Configuration.POM (NodeConfiguration (..),
                   PartialNodeConfiguration (..), defaultPartialNodeConfiguration,
                   makeNodeConfiguration, parseNodeConfigurationFP)
import           Cardano.Node.Types
import           Cardano.Tracing.Config (TraceOptions (..), TraceSelection (..))
import           Cardano.Tracing.Constraints (TraceConstraints)
import           Cardano.Tracing.Metrics (HasKESInfo (..), HasKESMetricsData (..))

import qualified Ouroboros.Consensus.Config as Consensus
import           Ouroboros.Consensus.Config.SupportsNode (getNetworkMagic)
import           Ouroboros.Consensus.Node (DiffusionArguments (..), DiffusionTracers (..),
                   DnsSubscriptionTarget (..), IPSubscriptionTarget (..), RunNode, RunNodeArgs (..),
                   StdRunNodeArgs (..))
import qualified Ouroboros.Consensus.Node as Node (getChainDB, run)
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Network.Magic (NetworkMagic (..))
import           Ouroboros.Network.NodeToNode (AcceptedConnectionsLimit (..), DiffusionMode)

import qualified Cardano.Api.Protocol.Types as Protocol
import           Cardano.Node.Configuration.Socket (SocketOrSocketInfo (..),
                   gatherConfiguredSockets, getSocketOrSocketInfoAddr, renderSocketConfigError)
import           Cardano.Node.Configuration.Topology
import           Cardano.Node.Handlers.Shutdown
import           Cardano.Node.Protocol (mkConsensusProtocol, renderProtocolInstantiationError)
import           Cardano.Node.Protocol.Types
import           Cardano.Tracing.Kernel
import           Cardano.Tracing.Peer
import           Cardano.Tracing.Tracers

{- HLINT ignore "Use fewer imports" -}

runNode
  :: PartialNodeConfiguration
  -> IO ()
runNode cmdPc = do
    -- TODO: Remove sodiumInit: https://github.com/input-output-hk/cardano-base/issues/175
    Crypto.sodiumInit

    configYamlPc <- parseNodeConfigurationFP . getLast $ pncConfigFile cmdPc

    nc <- case makeNodeConfiguration $ defaultPartialNodeConfiguration <> configYamlPc <> cmdPc of
            Left err -> panic $ "Error in creating the NodeConfiguration: " <> Text.pack err
            Right nc' -> return nc'

    case shelleyVRFFile $ ncProtocolFiles nc of
      Just vrfFp -> do vrf <- runExceptT $ checkVRFFilePermissions vrfFp
                       case vrf of
                         Left err ->
                           putTextLn (renderVRFPrivateKeyFilePermissionError err) >> exitFailure
                         Right () ->
                           pure ()
      Nothing -> pure ()

    eitherSomeProtocol <- runExceptT $ mkConsensusProtocol nc

    p :: SomeConsensusProtocol <-
      case eitherSomeProtocol of
        Left err -> putTextLn (renderProtocolInstantiationError err) >> exitFailure
        Right p -> pure p

    eLoggingLayer <- runExceptT $ createLoggingLayer
                     (Text.pack (showVersion version))
                     nc
                     p

    loggingLayer <- case eLoggingLayer of
                      Left err  -> putTextLn (show err) >> exitFailure
                      Right res -> return res

    !trace <- setupTrace loggingLayer
    let tracer = contramap pack $ toLogObject trace

    logTracingVerbosity nc tracer

    let handleNodeWithTracers
          :: ( HasKESMetricsData blk
             , HasKESInfo blk
             , TraceConstraints blk
             , Protocol.Protocol IO blk
             )
          => Protocol.ProtocolInfoArgs IO blk
          -> IO ()
        handleNodeWithTracers runP = do
          -- This IORef contains node kernel structure which holds node kernel.
          -- Used for ledger queries and peer connection status.
          nodeKernelData <- mkNodeKernelData
          let ProtocolInfo { pInfoConfig = cfg } = Protocol.protocolInfo runP
          tracers <- mkTracers
                       (Consensus.configBlock cfg)
                       (ncTraceConfig nc)
                       trace
                       nodeKernelData
                       (llEKGDirect loggingLayer)
          Async.withAsync (handlePeersListSimple trace nodeKernelData)
              $ \_peerLogingThread ->
                -- We ignore peer loging thread if it dies, but it will be killed
                -- when 'handleSimpleNode' terminates.
                handleSimpleNode p runP trace tracers nc (setNodeKernel nodeKernelData)
                `finally`
                shutdownLoggingLayer loggingLayer

    case p of
      SomeConsensusProtocol _ runP -> handleNodeWithTracers runP

logTracingVerbosity :: NodeConfiguration -> Tracer IO String -> IO ()
logTracingVerbosity nc tracer =
  case ncTraceConfig nc of
    TracingOff -> return ()
    TracingOn traceConf ->
      case traceVerbosity traceConf of
        NormalVerbosity -> traceWith tracer "tracing verbosity = normal verbosity "
        MinimalVerbosity -> traceWith tracer "tracing verbosity = minimal verbosity "
        MaximalVerbosity -> traceWith tracer "tracing verbosity = maximal verbosity "

-- | Add the application name and unqualified hostname to the logging
-- layer basic trace.
--
-- If the @CARDANO_NODE_LOGGING_HOSTNAME@ environment variable is set,
-- it overrides the system hostname. This is useful when running a
-- local test cluster with all nodes on the same host.
setupTrace
  :: LoggingLayer
  -> IO (Trace IO Text)
setupTrace loggingLayer = do
    hn <- maybe hostname (pure . pack) =<< lookupEnv "CARDANO_NODE_LOGGING_HOSTNAME"
    return $
        setHostname hn $
        llAppendName loggingLayer "node" (llBasicTrace loggingLayer)
  where
    hostname = do
      hn0 <- pack <$> getHostName
      return $ take 8 $ fst $ breakOn "." hn0

handlePeersListSimple
  :: Trace IO Text
  -> NodeKernelData blk
  -> IO ()
handlePeersListSimple tr nodeKern = forever $ do
  getCurrentPeers nodeKern >>= tracePeers tr
  threadDelay 2000000 -- 2 seconds.

-- | Sets up a simple node, which will run the chain sync protocol and block
-- fetch protocol, and, if core, will also look at the mempool when trying to
-- create a new block.

handleSimpleNode
  :: forall blk
  . ( RunNode blk
    , Protocol.Protocol IO blk
    )
  => SomeConsensusProtocol
  -> Protocol.ProtocolInfoArgs IO blk
  -> Trace IO Text
  -> Tracers RemoteConnectionId LocalConnectionId blk
  -> NodeConfiguration
  -> (NodeKernel IO RemoteConnectionId LocalConnectionId blk -> IO ())
  -- ^ Called on the 'NodeKernel' after creating it, but before the network
  -- layer is initialised.  This implies this function must not block,
  -- otherwise the node won't actually start.
  -> IO ()
handleSimpleNode scp runP trace nodeTracers nc onKernel = do
  meta <- mkLOMeta Notice Public

  let pInfo = Protocol.protocolInfo runP
      tracer = toLogObject trace

  createTracers nc trace tracer

  (publicIPv4SocketOrAddr, publicIPv6SocketOrAddr, localSocketOrPath) <- do
    result <- runExceptT (gatherConfiguredSockets nc)
    case result of
      Right triplet -> return triplet
      Left error -> do
        traceNamedObject
          (appendName "error" trace)
          (meta, LogMessage (Text.pack (renderSocketConfigError error)))
        throwIO error

  dbPath <- canonDbPath nc

  eitherTopology <- readTopologyFile nc
  nt <- either (\err -> panic $ "Cardano.Node.Run.handleSimpleNode.readTopologyFile: " <> err) pure eitherTopology

  let diffusionTracers :: DiffusionTracers
      diffusionTracers = createDiffusionTracers nodeTracers

      (ipProducerAddrs, dnsProducerAddrs) = producerAddresses nt

      dnsProducers :: [DnsSubscriptionTarget]
      dnsProducers = uncurry dnsSubscriptionTarget `map` dnsProducerAddrs

      ipProducers :: IPSubscriptionTarget
      ipProducers = ipSubscriptionTargets ipProducerAddrs

      diffusionArguments :: DiffusionArguments
      diffusionArguments =
        createDiffusionArguments
          publicIPv4SocketOrAddr
          publicIPv6SocketOrAddr
          localSocketOrPath
          (ncDiffusionMode nc)
          ipProducers
          dnsProducers

  ipv4 <- traverse getSocketOrSocketInfoAddr publicIPv4SocketOrAddr
  ipv6 <- traverse getSocketOrSocketInfoAddr publicIPv6SocketOrAddr

  traceNamedObject
    (appendName "addresses" trace)
    (meta, LogMessage . Text.pack . show $ catMaybes [ipv4, ipv6])
  traceNamedObject
    (appendName "diffusion-mode" trace)
    (meta, LogMessage . Text.pack . show . ncDiffusionMode $ nc)
  traceNamedObject
    (appendName "dns-producers" trace)
    (meta, LogMessage . Text.pack . show $ dnsProducers)
  traceNamedObject
    (appendName "ip-producers" trace)
    (meta, LogMessage . Text.pack . show $ ipProducers)

  withShutdownHandling nc trace $ \sfds ->
   Node.run
     RunNodeArgs
       { rnTraceConsensus = consensusTracers nodeTracers
       , rnTraceNTN       = nodeToNodeTracers nodeTracers
       , rnTraceNTC       = nodeToClientTracers nodeTracers
       , rnProtocolInfo   = pInfo
       , rnNodeKernelHook = \registry nodeKernel -> do
           maybeSpawnOnSlotSyncedShutdownHandler nc sfds trace registry
             (Node.getChainDB nodeKernel)
           onKernel nodeKernel
       }
     StdRunNodeArgs
       { srnBfcMaxConcurrencyBulkSync   = unMaxConcurrencyBulkSync <$> ncMaxConcurrencyBulkSync nc
       , srnBfcMaxConcurrencyDeadline   = unMaxConcurrencyDeadline <$> ncMaxConcurrencyDeadline nc
       , srnChainDbValidateOverride     = ncValidateDB nc
       , srnSnapshotInterval            = ncSnapshotInterval nc
       , srnDatabasePath                = dbPath
       , srnDiffusionArguments          = diffusionArguments
       , srnDiffusionTracers            = diffusionTracers
       , srnEnableInDevelopmentVersions = ncTestEnableDevelopmentNetworkProtocols nc
       , srnTraceChainDB                = chainDBTracer nodeTracers
       }
 where
  createDiffusionTracers :: Tracers RemoteConnectionId LocalConnectionId blk
                         -> DiffusionTracers
  createDiffusionTracers nodeTracers' = DiffusionTracers
    { dtIpSubscriptionTracer = ipSubscriptionTracer nodeTracers'
    , dtDnsSubscriptionTracer = dnsSubscriptionTracer nodeTracers'
    , dtDnsResolverTracer = dnsResolverTracer nodeTracers'
    , dtErrorPolicyTracer = errorPolicyTracer nodeTracers'
    , dtLocalErrorPolicyTracer = localErrorPolicyTracer nodeTracers'
    , dtAcceptPolicyTracer = acceptPolicyTracer nodeTracers'
    , dtMuxTracer = muxTracer nodeTracers'
    , dtMuxLocalTracer = muxLocalTracer nodeTracers'
    , dtHandshakeTracer = handshakeTracer nodeTracers'
    , dtHandshakeLocalTracer = localHandshakeTracer nodeTracers'
    , dtDiffusionInitializationTracer = diffusionInitializationTracer nodeTracers'
    , dtLedgerPeersTracer = nullTracer -- TODO network team
    }

  createTracers
    :: NodeConfiguration
    -> Trace IO Text
    -> Tracer IO Text
    -> IO ()
  createTracers NodeConfiguration { ncValidateDB }
                tr tracer = do
    let ProtocolInfo{ pInfoConfig = cfg } = Protocol.protocolInfo runP

    meta <- mkLOMeta Notice Public
    traceNamedObject (appendName "networkMagic" tr)
                     (meta, LogMessage ("NetworkMagic " <> show (unNetworkMagic . getNetworkMagic $ Consensus.configBlock cfg)))

    startTime <- getCurrentTime
    traceNodeBasicInfo tr =<< nodeBasicInfo nc scp startTime
    traceCounter "nodeStartTime" tr (ceiling $ utcTimeToPOSIXSeconds startTime)

    when ncValidateDB $ traceWith tracer "Performing DB validation"

  traceNodeBasicInfo :: Trace IO Text -> [LogObject Text] -> IO ()
  traceNodeBasicInfo tr basicInfoItems =
    forM_ basicInfoItems $ \(LogObject nm mt content) ->
      traceNamedObject (appendName nm tr) (mt, content)

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

canonDbPath :: NodeConfiguration -> IO FilePath
canonDbPath NodeConfiguration{ncDatabaseFile = DbFile dbFp} = do
  fp <- canonicalizePath =<< makeAbsolute dbFp
  createDirectoryIfMissing True fp
  return fp


-- | Make sure the VRF private key file is readable only
-- by the current process owner the node is running under.
checkVRFFilePermissions :: FilePath -> ExceptT VRFPrivateKeyFilePermissionError IO ()
#ifdef UNIX
checkVRFFilePermissions vrfPrivKey = do
  fs <- liftIO $ getFileStatus vrfPrivKey
  let fm = fileMode fs
  -- Check the the VRF private key file does not give read/write/exec permissions to others.
  when (hasOtherPermissions fm)
       (left $ OtherPermissionsExist vrfPrivKey)
  -- Check the the VRF private key file does not give read/write/exec permissions to any group.
  when (hasGroupPermissions fm)
       (left $ GroupPermissionsExist vrfPrivKey)
 where
  hasPermission :: FileMode -> FileMode -> Bool
  hasPermission fModeA fModeB = fModeA `intersectFileModes` fModeB /= nullFileMode

  hasOtherPermissions :: FileMode -> Bool
  hasOtherPermissions fm' = fm' `hasPermission` otherModes

  hasGroupPermissions :: FileMode -> Bool
  hasGroupPermissions fm' = fm' `hasPermission` groupModes
#else
checkVRFFilePermissions vrfPrivKey = do
  attribs <- liftIO $ getFileAttributes vrfPrivKey
  -- https://docs.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-createfilea
  -- https://docs.microsoft.com/en-us/windows/win32/fileio/file-access-rights-constants
  -- https://docs.microsoft.com/en-us/windows/win32/secauthz/standard-access-rights
  -- https://docs.microsoft.com/en-us/windows/win32/secauthz/generic-access-rights
  -- https://docs.microsoft.com/en-us/windows/win32/secauthz/access-mask
  when (attribs `hasPermission` genericPermissions)
       (left $ GenericPermissionsExist vrfPrivKey)
 where
  genericPermissions = gENERIC_ALL .|. gENERIC_READ .|. gENERIC_WRITE .|. gENERIC_EXECUTE
  hasPermission fModeA fModeB = fModeA .&. fModeB /= gENERIC_NONE
#endif

createDiffusionArguments
  :: Maybe (SocketOrSocketInfo Socket AddrInfo)
   -- ^ Either a socket bound to IPv4 address provided by systemd or IPv4
   -- address to bind to for NodeToNode communication.
  -> Maybe (SocketOrSocketInfo Socket AddrInfo)
   -- ^ Either a socket bound to IPv6 address provided by systemd or IPv6
   -- address to bind to for NodeToNode communication.
  -> Maybe (SocketOrSocketInfo Socket SocketPath)
  -- ^ Either a SOCKET_UNIX socket provided by systemd or a path for
  -- NodeToClient communication.
  -> DiffusionMode
  -> IPSubscriptionTarget
  -> [DnsSubscriptionTarget]
  -> DiffusionArguments
createDiffusionArguments publicIPv4SocketsOrAddrs
                         publicIPv6SocketsOrAddrs
                         mLocalSocketOrPath
                         diffusionMode
                         ipProducers dnsProducers
                         =
  DiffusionArguments
    -- This is not elegant, but it will change once `coot/connection-manager` is
    -- merged into `ouroboros-networ`.
    { daIPv4Address = eitherSocketOrSocketInfo <$> publicIPv4SocketsOrAddrs
    , daIPv6Address = eitherSocketOrSocketInfo <$> publicIPv6SocketsOrAddrs
    , daLocalAddress = mLocalSocketOrPath >>= return . fmap unSocketPath
                                                     . eitherSocketOrSocketInfo
    , daIpProducers  = ipProducers
    , daDnsProducers = dnsProducers
    -- TODO: these limits are arbitrary at the moment;
    -- issue: https://github.com/input-output-hk/ouroboros-network/issues/1836
    , daAcceptedConnectionsLimit = AcceptedConnectionsLimit {
        acceptedConnectionsHardLimit = 512
      , acceptedConnectionsSoftLimit = 384
      , acceptedConnectionsDelay     = 5
      }
    , daDiffusionMode = diffusionMode
    }
  where
    eitherSocketOrSocketInfo :: SocketOrSocketInfo a b -> Either a b
    eitherSocketOrSocketInfo (ActualSocket a) = Left a
    eitherSocketOrSocketInfo (SocketInfo b)   = Right b

dnsSubscriptionTarget :: NodeDnsAddress -> Int -> DnsSubscriptionTarget
dnsSubscriptionTarget na valency =
  DnsSubscriptionTarget { dstDomain  = nodeHostDnsAddressToDomain (naHostAddress na)
                        , dstPort    = naPort na
                        , dstValency = valency
                        }

ipSubscriptionTargets :: [NodeIPAddress] -> IPSubscriptionTarget
ipSubscriptionTargets ipProdAddrs =
  let ips = nodeAddressToSockAddr <$> ipProdAddrs
  in IPSubscriptionTarget { ispIps = ips
                          , ispValency = length ips
                          }


producerAddresses
  :: NetworkTopology
  -> ( [NodeIPAddress]
     , [(NodeDnsAddress, Int)])
producerAddresses nt =
  case nt of
    RealNodeTopology producers' ->
        partitionEithers
      . mapMaybe remoteAddressToNodeAddress
      $ producers'
    MockNodeTopology nodeSetup ->
        partitionEithers
      . mapMaybe remoteAddressToNodeAddress
      . concatMap producers
      $ nodeSetup
