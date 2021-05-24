{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Tracing.Tracers
  ( Tracers (..)
  , TraceOptions
  , mkTracers
  , nullTracers
  , traceCounter
  ) where

import           Cardano.Prelude hiding (show)
import           Prelude (String, show)

import           GHC.Clock (getMonotonicTimeNSec)

import           Codec.CBOR.Read (DeserialiseFailure)
import           Data.Aeson (ToJSON (..), Value (..))
import qualified Data.HashMap.Strict as Map
import qualified Data.Map.Strict as SMap
import qualified Data.Text as Text
import           Data.Time (UTCTime)
import qualified System.Metrics.Counter as Counter
import qualified System.Metrics.Gauge as Gauge
import qualified System.Metrics.Label as Label
import qualified System.Remote.Monitoring as EKG

import           Network.Mux (MuxTrace, WithMuxBearer)
import qualified Network.Socket as Socket (SockAddr)

import           Control.Tracer
import           Control.Tracer.Transformers

import           Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))

import           Cardano.BM.Data.Aggregated (Measurable (..))
import           Cardano.BM.Data.Tracer (WithSeverity (..), annotateSeverity)
import           Cardano.BM.Data.Transformers
import           Cardano.BM.Internal.ElidingTracer
import           Cardano.BM.Trace (traceNamedObject)
import           Cardano.BM.Tracing

import           Ouroboros.Consensus.Block (BlockConfig, BlockProtocol, CannotForge, ConvertRawHash,
                   ForgeStateInfo, ForgeStateUpdateError, Header, realPointSlot)
import           Ouroboros.Consensus.BlockchainTime (SystemStart (..),
                   TraceBlockchainTimeEvent (..))
import           Ouroboros.Consensus.HeaderValidation (OtherHeaderEnvelopeError)
import           Ouroboros.Consensus.Ledger.Abstract (LedgerErr, LedgerState)
import           Ouroboros.Consensus.Ledger.Extended (ledgerState)
import           Ouroboros.Consensus.Ledger.Inspect (InspectLedger, LedgerEvent)
import           Ouroboros.Consensus.Ledger.Query (Query)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, GenTx, GenTxId, HasTxs,
                   LedgerSupportsMempool)
import           Ouroboros.Consensus.Ledger.SupportsProtocol (LedgerSupportsProtocol)
import           Ouroboros.Consensus.Mempool.API (MempoolSize (..), TraceEventMempool (..))
import qualified Ouroboros.Consensus.Network.NodeToClient as NodeToClient
import qualified Ouroboros.Consensus.Network.NodeToNode as NodeToNode
import qualified Ouroboros.Consensus.Node.Run as Consensus (RunNode)
import qualified Ouroboros.Consensus.Node.Tracers as Consensus
import           Ouroboros.Consensus.Protocol.Abstract (ValidationErr)
import qualified Ouroboros.Consensus.Shelley.Protocol.HotKey as HotKey

import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (BlockNo (..), HasHeader (..), Point, StandardHash,
                   blockNo, pointSlot, unBlockNo)
import           Ouroboros.Network.BlockFetch.ClientState (TraceLabelPeer (..))
import           Ouroboros.Network.BlockFetch.Decision (FetchDecision, FetchDecline (..))
import qualified Ouroboros.Network.NodeToClient as NtC
import qualified Ouroboros.Network.NodeToNode as NtN
import           Ouroboros.Network.Point (fromWithOrigin, withOrigin)
import           Ouroboros.Network.Protocol.LocalStateQuery.Type (ShowQuery)
import           Ouroboros.Network.Subscription

import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.OnDisk as LedgerDB

import           Cardano.Tracing.Config
import           Cardano.Tracing.Constraints (TraceConstraints)
import           Cardano.Tracing.ConvertTxId (ConvertTxId)
import           Cardano.Tracing.Kernel
import           Cardano.Tracing.Metrics
import           Cardano.Tracing.Queries

import           Cardano.Node.Configuration.Logging
-- For tracing instances
import           Cardano.Node.Protocol.Byron ()
import           Cardano.Node.Protocol.Shelley ()
import           Ouroboros.Consensus.MiniProtocol.BlockFetch.Server
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Server
import           Ouroboros.Network.TxSubmission.Inbound

import qualified Cardano.Node.STM as STM
import qualified Control.Concurrent.STM as STM
import qualified Ouroboros.Network.Diffusion as ND

import           Shelley.Spec.Ledger.OCert (KESPeriod (..))

{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Use record patterns" -}

data Tracers peer localPeer blk = Tracers
  { -- | Trace the ChainDB
    chainDBTracer :: Tracer IO (ChainDB.TraceEvent blk)
    -- | Consensus-specific tracers.
  , consensusTracers :: Consensus.Tracers IO peer localPeer blk
    -- | Tracers for the node-to-node protocols.
  , nodeToNodeTracers :: NodeToNode.Tracers IO peer blk DeserialiseFailure
    --, serialisedBlockTracer :: NodeToNode.SerialisedTracer IO peer blk (SerialisedBlockTrace)
    -- | Tracers for the node-to-client protocols
  , nodeToClientTracers :: NodeToClient.Tracers IO localPeer blk DeserialiseFailure
    -- | Trace the IP subscription manager
  , ipSubscriptionTracer :: Tracer IO (WithIPList (SubscriptionTrace Socket.SockAddr))
    -- | Trace the DNS subscription manager
  , dnsSubscriptionTracer :: Tracer IO (WithDomainName (SubscriptionTrace Socket.SockAddr))
    -- | Trace the DNS resolver
  , dnsResolverTracer :: Tracer IO (WithDomainName DnsTrace)
    -- | Trace error policy resolution
  , errorPolicyTracer :: Tracer IO (NtN.WithAddr Socket.SockAddr NtN.ErrorPolicyTrace)
    -- | Trace local error policy resolution
  , localErrorPolicyTracer :: Tracer IO (NtN.WithAddr NtC.LocalAddress NtN.ErrorPolicyTrace)
  , acceptPolicyTracer :: Tracer IO NtN.AcceptConnectionsPolicyTrace
    -- | Trace the Mux
  , muxTracer :: Tracer IO (WithMuxBearer peer MuxTrace)
  , muxLocalTracer :: Tracer IO (WithMuxBearer localPeer MuxTrace)
  , handshakeTracer :: Tracer IO NtN.HandshakeTr
  , localHandshakeTracer :: Tracer IO NtC.HandshakeTr
  , diffusionInitializationTracer :: Tracer IO ND.DiffusionInitializationTracer
  }

data ForgeTracers = ForgeTracers
  { ftForged :: Trace IO Text
  , ftForgeAboutToLead :: Trace IO Text
  , ftCouldNotForge :: Trace IO Text
  , ftAdopted :: Trace IO Text
  , ftDidntAdoptBlock :: Trace IO Text
  , ftForgedInvalid   :: Trace IO Text
  , ftTraceNodeNotLeader  :: Trace IO Text
  , ftTraceNodeCannotForge :: Trace IO Text
  , ftTraceForgeStateUpdateError :: Trace IO Text
  , ftTraceBlockFromFuture :: Trace IO Text
  , ftTraceSlotIsImmutable :: Trace IO Text
  , ftTraceNodeIsLeader :: Trace IO Text
  }

nullTracers :: Tracers peer localPeer blk
nullTracers = Tracers
  { chainDBTracer = nullTracer
  , consensusTracers = Consensus.nullTracers
  , nodeToClientTracers = NodeToClient.nullTracers
  , nodeToNodeTracers = NodeToNode.nullTracers
  , ipSubscriptionTracer = nullTracer
  , dnsSubscriptionTracer = nullTracer
  , dnsResolverTracer = nullTracer
  , errorPolicyTracer = nullTracer
  , localErrorPolicyTracer = nullTracer
  , acceptPolicyTracer = nullTracer
  , muxTracer = nullTracer
  , muxLocalTracer = nullTracer
  , handshakeTracer = nullTracer
  , localHandshakeTracer = nullTracer
  , diffusionInitializationTracer = nullTracer
  }


indexGCType :: ChainDB.TraceGCEvent a -> Int
indexGCType ChainDB.ScheduledGC{} = 1
indexGCType ChainDB.PerformedGC{} = 2

indexReplType :: ChainDB.TraceLedgerReplayEvent a -> Int
indexReplType LedgerDB.ReplayFromGenesis{} = 1
indexReplType LedgerDB.ReplayFromSnapshot{} = 2
indexReplType LedgerDB.ReplayedBlock{} = 3

instance ElidingTracer (WithSeverity (ChainDB.TraceEvent blk)) where
  -- equivalent by type and severity
  isEquivalent (WithSeverity s1 (ChainDB.TraceLedgerReplayEvent ev1))
               (WithSeverity s2 (ChainDB.TraceLedgerReplayEvent ev2)) =
                  s1 == s2 && indexReplType ev1 == indexReplType ev2
  isEquivalent (WithSeverity s1 (ChainDB.TraceGCEvent ev1))
               (WithSeverity s2 (ChainDB.TraceGCEvent ev2)) =
                  s1 == s2 && indexGCType ev1 == indexGCType ev2
  isEquivalent (WithSeverity _s1 (ChainDB.TraceAddBlockEvent _))
               (WithSeverity _s2 (ChainDB.TraceAddBlockEvent _)) = True
  isEquivalent (WithSeverity _s1 (ChainDB.TraceGCEvent _ev1))
               (WithSeverity _s2 (ChainDB.TraceAddBlockEvent _)) = True
  isEquivalent (WithSeverity _s1 (ChainDB.TraceAddBlockEvent _))
               (WithSeverity _s2 (ChainDB.TraceGCEvent _ev2)) = True
  isEquivalent (WithSeverity _s1 (ChainDB.TraceGCEvent _ev1))
               (WithSeverity _s2 (ChainDB.TraceCopyToImmutableDBEvent _)) = True
  isEquivalent (WithSeverity _s1 (ChainDB.TraceCopyToImmutableDBEvent _))
               (WithSeverity _s2 (ChainDB.TraceGCEvent _ev2)) = True
  isEquivalent (WithSeverity _s1 (ChainDB.TraceCopyToImmutableDBEvent _))
               (WithSeverity _s2 (ChainDB.TraceAddBlockEvent _)) = True
  isEquivalent (WithSeverity _s1 (ChainDB.TraceAddBlockEvent _))
               (WithSeverity _s2 (ChainDB.TraceCopyToImmutableDBEvent _)) = True
  isEquivalent (WithSeverity _s1 (ChainDB.TraceCopyToImmutableDBEvent _))
               (WithSeverity _s2 (ChainDB.TraceCopyToImmutableDBEvent _)) = True
  isEquivalent _ _ = False
  -- the types to be elided
  doelide (WithSeverity _ (ChainDB.TraceLedgerReplayEvent _)) = True
  doelide (WithSeverity _ (ChainDB.TraceGCEvent _)) = True
  doelide (WithSeverity _ (ChainDB.TraceAddBlockEvent (ChainDB.IgnoreBlockOlderThanK _))) = False
  doelide (WithSeverity _ (ChainDB.TraceAddBlockEvent (ChainDB.IgnoreInvalidBlock _ _))) = False
  doelide (WithSeverity _ (ChainDB.TraceAddBlockEvent (ChainDB.BlockInTheFuture _ _))) = False
  doelide (WithSeverity _ (ChainDB.TraceAddBlockEvent (ChainDB.StoreButDontChange _))) = False
  doelide (WithSeverity _ (ChainDB.TraceAddBlockEvent (ChainDB.TrySwitchToAFork _ _))) = False
  doelide (WithSeverity _ (ChainDB.TraceAddBlockEvent (ChainDB.SwitchedToAFork{}))) = False
  doelide (WithSeverity _ (ChainDB.TraceAddBlockEvent (ChainDB.AddBlockValidation (ChainDB.InvalidBlock _ _)))) = False
  doelide (WithSeverity _ (ChainDB.TraceAddBlockEvent (ChainDB.AddBlockValidation (ChainDB.InvalidCandidate _)))) = False
  doelide (WithSeverity _ (ChainDB.TraceAddBlockEvent (ChainDB.AddBlockValidation ChainDB.CandidateContainsFutureBlocksExceedingClockSkew{}))) = False
  doelide (WithSeverity _ (ChainDB.TraceAddBlockEvent (ChainDB.AddedToCurrentChain events _ _  _))) = null events
  doelide (WithSeverity _ (ChainDB.TraceAddBlockEvent _)) = True
  doelide (WithSeverity _ (ChainDB.TraceCopyToImmutableDBEvent _)) = True
  doelide _ = False
  conteliding _tverb _tr _ (Nothing, _count) = return (Nothing, 0)
  conteliding tverb tr ev@(WithSeverity _ (ChainDB.TraceAddBlockEvent ChainDB.AddedToCurrentChain{})) (_old, oldt) = do
      tnow <- fromIntegral <$> getMonotonicTimeNSec
      let deltat = tnow - oldt
      if deltat > 1250000000 -- report at most every 1250 ms
        then do
          traceWith (toLogObject' tverb tr) ev
          return (Just ev, tnow)
        else return (Just ev, oldt)
  conteliding _tverb _tr ev@(WithSeverity _ (ChainDB.TraceAddBlockEvent _)) (_old, count) =
      return (Just ev, count)
  conteliding _tverb _tr ev@(WithSeverity _ (ChainDB.TraceCopyToImmutableDBEvent _)) (_old, count) =
      return (Just ev, count)
  conteliding _tverb _tr ev@(WithSeverity _ (ChainDB.TraceGCEvent _)) (_old, count) =
      return (Just ev, count)
  conteliding _tverb tr ev@(WithSeverity _ (ChainDB.TraceLedgerReplayEvent (LedgerDB.ReplayedBlock pt [] replayTo))) (_old, count) = do
      let slotno = toInteger $ unSlotNo (realPointSlot pt)
          endslot = toInteger $ withOrigin 0 unSlotNo (pointSlot replayTo)
          startslot = if count == 0 then slotno else toInteger count
          progress :: Double = (fromInteger slotno * 100.0) / fromInteger (max slotno endslot)
      when (count > 0 && (slotno - startslot) `mod` 1000 == 0) $ do  -- report every 1000th slot
          meta <- mkLOMeta (getSeverityAnnotation ev) (getPrivacyAnnotation ev)
          traceNamedObject tr (meta, LogValue "block replay progress (%)" (PureD (fromInteger (round (progress * 10.0)) / 10.0)))
      return (Just ev, fromInteger startslot)
  conteliding _ _ _ _ = return (Nothing, 0)

instance (StandardHash header, Eq peer) => ElidingTracer
  (WithSeverity [TraceLabelPeer peer (FetchDecision [Point header])]) where
  -- equivalent by type and severity
  isEquivalent (WithSeverity s1 _peers1)
               (WithSeverity s2 _peers2) = s1 == s2
  -- the types to be elided
  doelide (WithSeverity _ peers) =
    let checkDecision :: TraceLabelPeer peer (Either FetchDecline result) -> Bool
        checkDecision (TraceLabelPeer _peer (Left FetchDeclineChainNotPlausible)) = True
        checkDecision (TraceLabelPeer _peer (Left (FetchDeclineConcurrencyLimit _ _))) = True
        checkDecision (TraceLabelPeer _peer (Left (FetchDeclinePeerBusy _ _ _))) = True
        checkDecision _ = False
    in any checkDecision peers
  conteliding _tverb _tr _ (Nothing, _count) = return (Nothing, 0)
  conteliding tverb tr ev (_old, count) = do
      when (count > 0 && count `mod` 1000 == 0) $  -- report every 1000th message
          traceWith (toLogObject' tverb tr) ev
      return (Just ev, count + 1)

-- | Tracers for all system components.
--
mkTracers
  :: forall peer localPeer blk.
     ( Consensus.RunNode blk
     , HasKESMetricsData blk
     , HasKESInfo blk
     , TraceConstraints blk
     , Show peer, Eq peer, ToObject peer
     , Show localPeer, ToObject localPeer
     )
  => BlockConfig blk
  -> TraceOptions
  -> Trace IO Text
  -> NodeKernelData blk
  -> Maybe EKGDirect
  -> IO (Tracers peer localPeer blk)
mkTracers blockConfig tOpts@(TracingOn trSel) tr nodeKern ekgDirect = do
  fStats <- mkForgingStats
  consensusTracers <- mkConsensusTracers ekgDirect trSel verb tr nodeKern fStats
  elidedChainDB <- newstate  -- for eliding messages in ChainDB tracer

  pure Tracers
    { chainDBTracer = tracerOnOff' (traceChainDB trSel) $
        annotateSeverity $ teeTraceChainTip
                             blockConfig
                             fStats
                             tOpts elidedChainDB
                             ekgDirect
                             (appendName "ChainDB" tr)
                             (appendName "metrics" tr)
    , consensusTracers = consensusTracers
    , nodeToClientTracers = nodeToClientTracers' trSel verb tr
    , nodeToNodeTracers = nodeToNodeTracers' trSel verb tr
    , ipSubscriptionTracer = tracerOnOff (traceIpSubscription trSel) verb "IpSubscription" tr
    , dnsSubscriptionTracer =  tracerOnOff (traceDnsSubscription trSel) verb "DnsSubscription" tr
    , dnsResolverTracer = tracerOnOff (traceDnsResolver trSel) verb "DnsResolver" tr
    , errorPolicyTracer = tracerOnOff (traceErrorPolicy trSel) verb "ErrorPolicy" tr
    , localErrorPolicyTracer = tracerOnOff (traceLocalErrorPolicy trSel) verb "LocalErrorPolicy" tr
    , acceptPolicyTracer = tracerOnOff (traceAcceptPolicy trSel) verb "AcceptPolicy" tr
    , muxTracer = tracerOnOff (traceMux trSel) verb "Mux" tr
    , muxLocalTracer = tracerOnOff (traceLocalMux trSel) verb "MuxLocal" tr
    , handshakeTracer = tracerOnOff (traceHandshake trSel) verb "Handshake" tr
    , localHandshakeTracer = tracerOnOff (traceLocalHandshake trSel) verb "LocalHandshake" tr
    , diffusionInitializationTracer = tracerOnOff (traceDiffusionInitialization trSel) verb "DiffusionInitializationTracer" tr
    }
 where
   verb :: TracingVerbosity
   verb = traceVerbosity trSel

mkTracers _ TracingOff _ _ _ =
  pure Tracers
    { chainDBTracer = nullTracer
    , consensusTracers = Consensus.Tracers
      { Consensus.chainSyncClientTracer = nullTracer
      , Consensus.chainSyncServerHeaderTracer = nullTracer
      , Consensus.chainSyncServerBlockTracer = nullTracer
      , Consensus.blockFetchDecisionTracer = nullTracer
      , Consensus.blockFetchClientTracer = nullTracer
      , Consensus.blockFetchServerTracer = nullTracer
      , Consensus.forgeStateInfoTracer = nullTracer
      , Consensus.txInboundTracer = nullTracer
      , Consensus.txOutboundTracer = nullTracer
      , Consensus.localTxSubmissionServerTracer = nullTracer
      , Consensus.mempoolTracer = nullTracer
      , Consensus.forgeTracer = nullTracer
      , Consensus.blockchainTimeTracer = nullTracer
      , Consensus.keepAliveClientTracer = nullTracer
      }
    , nodeToClientTracers = NodeToClient.Tracers
      { NodeToClient.tChainSyncTracer = nullTracer
      , NodeToClient.tTxSubmissionTracer = nullTracer
      , NodeToClient.tStateQueryTracer = nullTracer
      }
    , nodeToNodeTracers = NodeToNode.Tracers
      { NodeToNode.tChainSyncTracer = nullTracer
      , NodeToNode.tChainSyncSerialisedTracer = nullTracer
      , NodeToNode.tBlockFetchTracer = nullTracer
      , NodeToNode.tBlockFetchSerialisedTracer = nullTracer
      , NodeToNode.tTxSubmissionTracer = nullTracer
      , NodeToNode.tTxSubmission2Tracer = nullTracer
      }
    , ipSubscriptionTracer = nullTracer
    , dnsSubscriptionTracer= nullTracer
    , dnsResolverTracer = nullTracer
    , errorPolicyTracer = nullTracer
    , localErrorPolicyTracer = nullTracer
    , acceptPolicyTracer = nullTracer
    , muxTracer = nullTracer
    , muxLocalTracer = nullTracer
    , handshakeTracer = nullTracer
    , localHandshakeTracer = nullTracer
    , diffusionInitializationTracer = nullTracer
    }

--------------------------------------------------------------------------------
-- Chain DB Tracers
--------------------------------------------------------------------------------

teeTraceChainTip
  :: ( ConvertRawHash blk
     , LedgerSupportsProtocol blk
     , InspectLedger blk
     , ToObject (Header blk)
     , ToObject (LedgerEvent blk)
     )
  => BlockConfig blk
  -> ForgingStats
  -> TraceOptions
  -> MVar (Maybe (WithSeverity (ChainDB.TraceEvent blk)), Integer)
  -> Maybe EKGDirect
  -> Trace IO Text
  -> Trace IO Text
  -> Tracer IO (WithSeverity (ChainDB.TraceEvent blk))
teeTraceChainTip _ _ TracingOff _ _ _ _ = nullTracer
teeTraceChainTip blockConfig fStats (TracingOn trSel) elided ekgDirect trTrc trMet =
  Tracer $ \ev -> do
    traceWith (teeTraceChainTipElide (traceVerbosity trSel) elided trTrc) ev
    traceWith (ignoringSeverity (traceChainMetrics ekgDirect blockConfig fStats trMet)) ev

teeTraceChainTipElide
  :: ( ConvertRawHash blk
     , LedgerSupportsProtocol blk
     , InspectLedger blk
     , ToObject (Header blk)
     , ToObject (LedgerEvent blk)
     )
  => TracingVerbosity
  -> MVar (Maybe (WithSeverity (ChainDB.TraceEvent blk)), Integer)
  -> Trace IO Text
  -> Tracer IO (WithSeverity (ChainDB.TraceEvent blk))
teeTraceChainTipElide = elideToLogObject
{-# INLINE teeTraceChainTipElide #-}

ignoringSeverity :: Tracer IO a -> Tracer IO (WithSeverity a)
ignoringSeverity tr = Tracer $ \(WithSeverity _ ev) -> traceWith tr ev
{-# INLINE ignoringSeverity #-}

traceChainMetrics
  :: forall blk. ()
  => HasHeader (Header blk)
  => Maybe EKGDirect
  -> BlockConfig blk
  -> ForgingStats
  -> Trace IO Text
  -> Tracer IO (ChainDB.TraceEvent blk)
traceChainMetrics Nothing _ _ _ = nullTracer
traceChainMetrics (Just _ekgDirect) _blockConfig _fStats tr = do
  Tracer $ \ev ->
    fromMaybe (pure ()) $ doTrace <$> chainTipInformation ev
  where
    chainTipInformation :: ChainDB.TraceEvent blk -> Maybe ChainInformation
    chainTipInformation = \case
      ChainDB.TraceAddBlockEvent ev -> case ev of
        ChainDB.SwitchedToAFork _warnings newTipInfo _oldChain newChain ->
          Just $ chainInformation newTipInfo newChain 0
        ChainDB.AddedToCurrentChain _warnings newTipInfo _oldChain newChain ->
          Just $ chainInformation newTipInfo newChain 0
        _ -> Nothing
      _ -> Nothing
    doTrace :: ChainInformation -> IO ()

    doTrace
        ChainInformation { slots, blocks, density, epoch, slotInEpoch } = do
      -- TODO this is executed each time the newFhain changes. How cheap is it?
      meta <- mkLOMeta Critical Public

      traceD tr meta "density"     (fromRational density)
      traceI tr meta "slotNum"     slots
      traceI tr meta "blockNum"    blocks
      traceI tr meta "slotInEpoch" slotInEpoch
      traceI tr meta "epoch"       (unEpochNo epoch)

traceD :: Trace IO a -> LOMeta -> Text -> Double -> IO ()
traceD tr meta msg d = traceNamedObject tr (meta, LogValue msg (PureD d))

traceI :: Integral i => Trace IO a -> LOMeta -> Text -> i -> IO ()
traceI tr meta msg i = traceNamedObject tr (meta, LogValue msg (PureI (fromIntegral i)))

sendEKGDirectCounter :: EKGDirect -> Text -> IO ()
sendEKGDirectCounter ekgDirect name = do
  modifyMVar_ (ekgCounters ekgDirect) $ \registeredMap -> do
    case SMap.lookup name registeredMap of
      Just counter -> do
        Counter.inc counter
        pure registeredMap
      Nothing -> do
        counter <- EKG.getCounter name (ekgServer ekgDirect)
        Counter.inc counter
        pure $ SMap.insert name counter registeredMap

_sendEKGDirectInt :: Integral a => EKGDirect -> Text -> a -> IO ()
_sendEKGDirectInt ekgDirect name val = do
  modifyMVar_ (ekgGauges ekgDirect) $ \registeredMap -> do
    case SMap.lookup name registeredMap of
      Just gauge -> do
        Gauge.set gauge (fromIntegral val)
        pure registeredMap
      Nothing -> do
        gauge <- EKG.getGauge name (ekgServer ekgDirect)
        Gauge.set gauge (fromIntegral val)
        pure $ SMap.insert name gauge registeredMap

_sendEKGDirectDouble :: EKGDirect -> Text -> Double -> IO ()
_sendEKGDirectDouble ekgDirect name val = do
  modifyMVar_ (ekgLabels ekgDirect) $ \registeredMap -> do
    case SMap.lookup name registeredMap of
      Just label -> do
        Label.set label (Text.pack (show val))
        pure registeredMap
      Nothing -> do
        label <- EKG.getLabel name (ekgServer ekgDirect)
        Label.set label (Text.pack (show val))
        pure $ SMap.insert name label registeredMap

--------------------------------------------------------------------------------
-- Consensus Tracers
--------------------------------------------------------------------------------

isRollForward :: TraceChainSyncServerEvent blk -> Bool
isRollForward (TraceChainSyncRollForward _) = True
isRollForward _ = False

isTraceBlockFetchServerBlockCount :: TraceBlockFetchServerEvent blk -> Bool
isTraceBlockFetchServerBlockCount (TraceBlockFetchServerSendBlock _) = True

mkConsensusTracers
  :: forall blk peer localPeer.
     ( Show peer
     , Eq peer
     , LedgerQueries blk
     , ToJSON (GenTxId blk)
     , ToObject (ApplyTxErr blk)
     , ToObject (CannotForge blk)
     , ToObject (GenTx blk)
     , ToObject (LedgerErr (LedgerState blk))
     , ToObject (OtherHeaderEnvelopeError blk)
     , ToObject (ValidationErr (BlockProtocol blk))
     , ToObject (ForgeStateUpdateError blk)
     , ToObject peer
     , Consensus.RunNode blk
     , HasKESMetricsData blk
     , HasKESInfo blk
     , Show (Header blk)
     )
  => Maybe EKGDirect
  -> TraceSelection
  -> TracingVerbosity
  -> Trace IO Text
  -> NodeKernelData blk
  -> ForgingStats
  -> IO (Consensus.Tracers' peer localPeer blk (Tracer IO))
mkConsensusTracers mbEKGDirect trSel verb tr nodeKern fStats = do
  let trmet = appendName "metrics" tr

  elidedFetchDecision <- newstate  -- for eliding messages in FetchDecision tr
  forgeTracers <- mkForgeTracers
  meta <- mkLOMeta Critical Public

  tBlocksServed <- STM.newTVarIO @Int 0
  tSubmissionsCollected <- STM.newTVarIO 0
  tSubmissionsAccepted <- STM.newTVarIO 0
  tSubmissionsRejected <- STM.newTVarIO 0

  pure Consensus.Tracers
    { Consensus.chainSyncClientTracer = tracerOnOff (traceChainSyncClient trSel) verb "ChainSyncClient" tr
    , Consensus.chainSyncServerHeaderTracer =
      Tracer $ \ev -> do
        traceWith (annotateSeverity . toLogObject' verb $ appendName "ChainSyncHeaderServer" tr) ev
        traceServedCount mbEKGDirect ev
    , Consensus.chainSyncServerBlockTracer = tracerOnOff (traceChainSyncBlockServer trSel) verb "ChainSyncBlockServer" tr
    , Consensus.blockFetchDecisionTracer = tracerOnOff' (traceBlockFetchDecisions trSel) $
        annotateSeverity $ teeTraceBlockFetchDecision verb elidedFetchDecision tr
    , Consensus.blockFetchClientTracer = tracerOnOff (traceBlockFetchClient trSel) verb "BlockFetchClient" tr
    , Consensus.blockFetchServerTracer = tracerOnOff' (traceBlockFetchServer trSel) $
        Tracer $ \ev -> do
          traceWith (annotateSeverity . toLogObject' verb $ appendName "BlockFetchServer" tr) ev
          when (isTraceBlockFetchServerBlockCount ev) $
            traceI trmet meta "served.block.count" =<<
              STM.modifyReadTVarIO tBlocksServed (+1)
    , Consensus.forgeStateInfoTracer = tracerOnOff' (traceForgeStateInfo trSel) $
        forgeStateInfoTracer (Proxy @ blk) trSel tr
    , Consensus.txInboundTracer = tracerOnOff' (traceTxInbound trSel) $
        Tracer $ \ev -> do
          traceWith (annotateSeverity . toLogObject' verb $ appendName "TxInbound" tr) ev
          case ev of
            TraceLabelPeer _ (TraceTxSubmissionCollected collected) ->
              traceI trmet meta "submissions.submitted.count" =<<
                STM.modifyReadTVarIO tSubmissionsCollected (+ collected)

            TraceLabelPeer _ (TraceTxSubmissionProcessed processed) -> do
              traceI trmet meta "submissions.accepted.count" =<<
                STM.modifyReadTVarIO tSubmissionsAccepted (+ ptxcAccepted processed)
              traceI trmet meta "submissions.rejected.count" =<<
                STM.modifyReadTVarIO tSubmissionsRejected (+ ptxcRejected processed)

            TraceLabelPeer _ TraceTxInboundTerminated -> return ()
            TraceLabelPeer _ (TraceTxInboundCanRequestMoreTxs _) -> return ()
            TraceLabelPeer _ (TraceTxInboundCannotRequestMoreTxs _) -> return ()

    , Consensus.txOutboundTracer = tracerOnOff (traceTxOutbound trSel) verb "TxOutbound" tr
    , Consensus.localTxSubmissionServerTracer = tracerOnOff (traceLocalTxSubmissionServer trSel) verb "LocalTxSubmissionServer" tr
    , Consensus.mempoolTracer = tracerOnOff' (traceMempool trSel) $ mempoolTracer trSel tr fStats
    , Consensus.forgeTracer = tracerOnOff' (traceForge trSel) $
        Tracer $ \tlcev@Consensus.TraceLabelCreds{} -> do
          traceWith (annotateSeverity
                     $ traceLeadershipChecks forgeTracers nodeKern verb tr) tlcev
          traceWith (forgeTracer verb tr forgeTracers fStats) tlcev

    , Consensus.blockchainTimeTracer = tracerOnOff' (traceBlockchainTime trSel) $
        Tracer $ \ev ->
          traceWith (toLogObject tr) (readableTraceBlockchainTimeEvent ev)
    , Consensus.keepAliveClientTracer = tracerOnOff (traceKeepAliveClient trSel) verb "KeepAliveClient" tr
    }
 where
   mkForgeTracers :: IO ForgeTracers
   mkForgeTracers = do
     -- We probably don't want to pay the extra IO cost per-counter-increment. -- sk
     staticMeta <- mkLOMeta Critical Confidential
     let name :: LoggerName = "metrics.Forge"
     ForgeTracers
       <$> counting (liftCounting staticMeta name "forged" tr)
       <*> counting (liftCounting staticMeta name "forge-about-to-lead" tr)
       <*> counting (liftCounting staticMeta name "could-not-forge" tr)
       <*> counting (liftCounting staticMeta name "adopted" tr)
       <*> counting (liftCounting staticMeta name "didnt-adopt" tr)
       <*> counting (liftCounting staticMeta name "forged-invalid" tr)
       <*> counting (liftCounting staticMeta name "node-not-leader" tr)
       <*> counting (liftCounting staticMeta name "cannot-forge" tr)
       <*> counting (liftCounting staticMeta name "forge-state-update-error" tr)
       <*> counting (liftCounting staticMeta name "block-from-future" tr)
       <*> counting (liftCounting staticMeta name "slot-is-immutable" tr)
       <*> counting (liftCounting staticMeta name "node-is-leader" tr)

   traceServedCount :: Maybe EKGDirect -> TraceChainSyncServerEvent blk -> IO ()
   traceServedCount Nothing _ = pure ()
   traceServedCount (Just ekgDirect) ev =
     when (isRollForward ev) $
       sendEKGDirectCounter ekgDirect "cardano.node.metrics.served.header.counter.int"

traceLeadershipChecks ::
  forall blk
  . ( Consensus.RunNode blk
     , LedgerQueries blk
     )
  => ForgeTracers
  -> NodeKernelData blk
  -> TracingVerbosity
  -> Trace IO Text
  -> Tracer IO (WithSeverity (Consensus.TraceLabelCreds (Consensus.TraceForgeEvent blk)))
traceLeadershipChecks _ft nodeKern _tverb tr = Tracer $
  \(WithSeverity sev (Consensus.TraceLabelCreds creds event)) ->
    case event of
      Consensus.TraceStartLeadershipCheck slot -> do
        !query <- mapNodeKernelDataIO
                    (\nk ->
                       (,,)
                         <$> nkQueryLedger (ledgerUtxoSize . ledgerState) nk
                         <*> nkQueryLedger (ledgerDelegMapSize . ledgerState) nk
                         <*> nkQueryChain fragmentChainDensity nk)
                    nodeKern
        meta <- mkLOMeta sev Public
        fromSMaybe (pure ()) $
          query <&>
            \(utxoSize, delegMapSize, _) -> do
                traceCounter "utxoSize"     tr utxoSize
                traceCounter "delegMapSize" tr delegMapSize
        traceNamedObject (appendName "LeadershipCheck" tr)
          ( meta
          , LogStructured $ Map.fromList $
            [("kind", String "TraceStartLeadershipCheck")
            ,("credentials", String creds)
            ,("slot", toJSON $ unSlotNo slot)]
            ++ fromSMaybe []
               (query <&>
                 \(utxoSize, delegMapSize, chainDensity) ->
                   [ ("utxoSize",     toJSON utxoSize)
                   , ("delegMapSize", toJSON delegMapSize)
                   , ("chainDensity", toJSON (fromRational chainDensity :: Float))
                   ])
          )
      _ -> pure ()

teeForge ::
  forall blk
  . ( Consensus.RunNode blk
     , ToObject (CannotForge blk)
     , ToObject (LedgerErr (LedgerState blk))
     , ToObject (OtherHeaderEnvelopeError blk)
     , ToObject (ValidationErr (BlockProtocol blk))
     , ToObject (ForgeStateUpdateError blk)
     )
  => ForgeTracers
  -> TracingVerbosity
  -> Trace IO Text
  -> Tracer IO (WithSeverity (Consensus.TraceLabelCreds (Consensus.TraceForgeEvent blk)))
teeForge ft tverb tr = Tracer $
 \ev@(WithSeverity sev (Consensus.TraceLabelCreds _creds event)) -> do
  flip traceWith (WithSeverity sev event) $ fanning $ \(WithSeverity _ e) ->
    case e of
      Consensus.TraceStartLeadershipCheck{} -> teeForge' (ftForgeAboutToLead ft)
      Consensus.TraceSlotIsImmutable{} -> teeForge' (ftTraceSlotIsImmutable ft)
      Consensus.TraceBlockFromFuture{} -> teeForge' (ftTraceBlockFromFuture ft)
      Consensus.TraceBlockContext{} -> nullTracer
      Consensus.TraceNoLedgerState{} -> teeForge' (ftCouldNotForge ft)
      Consensus.TraceLedgerState{} -> nullTracer
      Consensus.TraceNoLedgerView{} -> teeForge' (ftCouldNotForge ft)
      Consensus.TraceLedgerView{} -> nullTracer
      Consensus.TraceForgeStateUpdateError{} -> teeForge' (ftTraceForgeStateUpdateError ft)
      Consensus.TraceNodeCannotForge {} -> teeForge' (ftTraceNodeCannotForge ft)
      Consensus.TraceNodeNotLeader{} -> teeForge' (ftTraceNodeNotLeader ft)
      Consensus.TraceNodeIsLeader{} -> teeForge' (ftTraceNodeIsLeader ft)
      Consensus.TraceForgedBlock{} -> teeForge' (ftForged ft)
      Consensus.TraceDidntAdoptBlock{} -> teeForge' (ftDidntAdoptBlock ft)
      Consensus.TraceForgedInvalidBlock{} -> teeForge' (ftForgedInvalid ft)
      Consensus.TraceAdoptedBlock{} -> teeForge' (ftAdopted ft)
  case event of
    Consensus.TraceStartLeadershipCheck _slot -> pure ()
    _ -> traceWith (toLogObject' tverb tr) ev

teeForge'
  :: Trace IO Text
  -> Tracer IO (WithSeverity (Consensus.TraceForgeEvent blk))
teeForge' tr =
  Tracer $ \(WithSeverity _ ev) -> do
    meta <- mkLOMeta Critical Confidential
    traceNamedObject (appendName "metrics" tr) . (meta,) $
      case ev of
        Consensus.TraceStartLeadershipCheck slot ->
          LogValue "aboutToLeadSlotLast" $ PureI $ fromIntegral $ unSlotNo slot
        Consensus.TraceSlotIsImmutable slot _tipPoint _tipBlkNo ->
          LogValue "slotIsImmutable" $ PureI $ fromIntegral $ unSlotNo slot
        Consensus.TraceBlockFromFuture slot _slotNo ->
          LogValue "blockFromFuture" $ PureI $ fromIntegral $ unSlotNo slot
        Consensus.TraceBlockContext slot _tipBlkNo _tipPoint ->
          LogValue "blockContext" $ PureI $ fromIntegral $ unSlotNo slot
        Consensus.TraceNoLedgerState slot _ ->
          LogValue "couldNotForgeSlotLast" $ PureI $ fromIntegral $ unSlotNo slot
        Consensus.TraceLedgerState slot _ ->
          LogValue "ledgerState" $ PureI $ fromIntegral $ unSlotNo slot
        Consensus.TraceNoLedgerView slot _ ->
          LogValue "couldNotForgeSlotLast" $ PureI $ fromIntegral $ unSlotNo slot
        Consensus.TraceLedgerView slot ->
          LogValue "ledgerView" $ PureI $ fromIntegral $ unSlotNo slot
        Consensus.TraceForgeStateUpdateError slot _reason ->
          LogValue "forgeStateUpdateError" $ PureI $ fromIntegral $ unSlotNo slot
        Consensus.TraceNodeCannotForge slot _reason ->
          LogValue "nodeCannotForge" $ PureI $ fromIntegral $ unSlotNo slot
        Consensus.TraceNodeNotLeader slot ->
          LogValue "nodeNotLeader" $ PureI $ fromIntegral $ unSlotNo slot
        Consensus.TraceNodeIsLeader slot ->
          LogValue "nodeIsLeader" $ PureI $ fromIntegral $ unSlotNo slot
        Consensus.TraceForgedBlock slot _ _ _ ->
          LogValue "forgedSlotLast" $ PureI $ fromIntegral $ unSlotNo slot
        Consensus.TraceDidntAdoptBlock slot _ ->
          LogValue "notAdoptedSlotLast" $ PureI $ fromIntegral $ unSlotNo slot
        Consensus.TraceForgedInvalidBlock slot _ _ ->
          LogValue "forgedInvalidSlotLast" $ PureI $ fromIntegral $ unSlotNo slot
        Consensus.TraceAdoptedBlock slot _ _ ->
          LogValue "adoptedSlotLast" $ PureI $ fromIntegral $ unSlotNo slot

forgeTracer
  :: forall blk.
     ( Consensus.RunNode blk
     , ToObject (CannotForge blk)
     , ToObject (LedgerErr (LedgerState blk))
     , ToObject (OtherHeaderEnvelopeError blk)
     , ToObject (ValidationErr (BlockProtocol blk))
     , ToObject (ForgeStateUpdateError blk)
     , HasKESInfo blk
     )
  => TracingVerbosity
  -> Trace IO Text
  -> ForgeTracers
  -> ForgingStats
  -> Tracer IO (Consensus.TraceLabelCreds (Consensus.TraceForgeEvent blk))
forgeTracer verb tr forgeTracers fStats =
  Tracer $ \tlcev@(Consensus.TraceLabelCreds _ ev) -> do
    -- Ignoring the credentials label for measurement and counters:
    traceWith (notifyBlockForging fStats tr) ev
    -- Consensus tracer -- here we track the label:
    traceWith (annotateSeverity
                 $ teeForge forgeTracers verb
                 $ appendName "Forge" tr) tlcev
    traceKESInfoIfKESExpired ev
 where
  traceKESInfoIfKESExpired ev =
    case ev of
      Consensus.TraceForgeStateUpdateError _ reason ->
        -- KES-key cannot be evolved, but anyway trace KES-values.
        case getKESInfo (Proxy @blk) reason of
          Nothing -> pure ()
          Just kesInfo -> do
            let logValues :: [LOContent a]
                logValues =
                  [ LogValue "operationalCertificateStartKESPeriod"
                      $ PureI . fromIntegral . unKESPeriod . HotKey.kesStartPeriod $ kesInfo
                  , LogValue "operationalCertificateExpiryKESPeriod"
                      $ PureI . fromIntegral . unKESPeriod . HotKey.kesEndPeriod $ kesInfo
                  , LogValue "currentKESPeriod"
                      $ PureI 0
                  , LogValue "remainingKESPeriods"
                      $ PureI 0
                  ]
            meta <- mkLOMeta Critical Confidential
            mapM_ (traceNamedObject (appendName "metrics" tr) . (meta,)) logValues
      _ -> pure ()

notifyBlockForging
  :: ForgingStats
  -> Trace IO Text
  -> Tracer IO (Consensus.TraceForgeEvent blk)
notifyBlockForging fStats tr = Tracer $ \case
  Consensus.TraceNodeCannotForge {} ->
    traceCounter "nodeCannotForge" tr
      =<< mapForgingCurrentThreadStats fStats
            (\fts -> (fts { ftsNodeCannotForgeNum = ftsNodeCannotForgeNum fts + 1 },
                       ftsNodeCannotForgeNum fts + 1))
  (Consensus.TraceNodeIsLeader (SlotNo slot')) -> do
    let slot = fromIntegral slot'
    traceCounter "nodeIsLeaderNum" tr
      =<< mapForgingCurrentThreadStats fStats
            (\fts -> (fts { ftsNodeIsLeaderNum = ftsNodeIsLeaderNum fts + 1
                          , ftsLastSlot = slot },
                      ftsNodeIsLeaderNum fts + 1))
  Consensus.TraceForgedBlock {} -> do
    traceCounter "blocksForgedNum" tr
      =<< mapForgingCurrentThreadStats fStats
            (\fts -> (fts { ftsBlocksForgedNum = ftsBlocksForgedNum fts + 1 },
                       ftsBlocksForgedNum fts + 1))

  Consensus.TraceNodeNotLeader (SlotNo slot') -> do
    -- Not is not a leader again, so now the number of blocks forged by this node
    -- should be equal to the number of slots when this node was a leader.
    let slot = fromIntegral slot'
    hasMissed <-
      mapForgingCurrentThreadStats fStats
        (\fts ->
          if ftsLastSlot fts == 0 || succ (ftsLastSlot fts) == slot then
            (fts { ftsLastSlot = slot }, False)
          else
            let missed = ftsSlotsMissedNum fts + (slot - ftsLastSlot fts)
            in (fts { ftsLastSlot = slot, ftsSlotsMissedNum = missed }, True))
    when hasMissed $ do
      x <- sum <$> threadStatsProjection fStats ftsSlotsMissedNum
      traceCounter "slotsMissedNum" tr x
  _ -> pure ()


--------------------------------------------------------------------------------
-- Mempool Tracers
--------------------------------------------------------------------------------

notifyTxsProcessed :: ForgingStats -> Trace IO Text -> Tracer IO (TraceEventMempool blk)
notifyTxsProcessed fStats tr = Tracer $ \case
  TraceMempoolRemoveTxs [] _ -> return ()
  TraceMempoolRemoveTxs txs _ -> do
    -- TraceMempoolRemoveTxs are previously valid transactions that are no longer valid because of
    -- changes in the ledger state. These transactions are already removed from the mempool,
    -- so we can treat them as completely processed.
    updatedTxProcessed <- mapForgingStatsTxsProcessed fStats (+ (length txs))
    traceCounter "txsProcessedNum" tr (fromIntegral updatedTxProcessed)
  -- The rest of the constructors.
  _ -> return ()


mempoolMetricsTraceTransformer :: Trace IO a -> Tracer IO (TraceEventMempool blk)
mempoolMetricsTraceTransformer tr = Tracer $ \mempoolEvent -> do
  let tr' = appendName "metrics" tr
      (_n, tot) = case mempoolEvent of
                    TraceMempoolAddedTx     _tx0 _ tot0 -> (1, tot0)
                    TraceMempoolRejectedTx  _tx0 _ tot0 -> (1, tot0)
                    TraceMempoolRemoveTxs   txs0   tot0 -> (length txs0, tot0)
                    TraceMempoolManuallyRemovedTxs txs0 txs1 tot0 -> ( length txs0 + length txs1, tot0)
      logValue1 :: LOContent a
      logValue1 = LogValue "txsInMempool" $ PureI $ fromIntegral (msNumTxs tot)
      logValue2 :: LOContent a
      logValue2 = LogValue "mempoolBytes" $ PureI $ fromIntegral (msNumBytes tot)
  meta <- mkLOMeta Critical Confidential
  traceNamedObject tr' (meta, logValue1)
  traceNamedObject tr' (meta, logValue2)

mempoolTracer
  :: ( ToJSON (GenTxId blk)
     , ToObject (ApplyTxErr blk)
     , ToObject (GenTx blk)
     , LedgerSupportsMempool blk
     )
  => TraceSelection
  -> Trace IO Text
  -> ForgingStats
  -> Tracer IO (TraceEventMempool blk)
mempoolTracer tc tracer fStats = Tracer $ \ev -> do
    traceWith (mempoolMetricsTraceTransformer tracer) ev
    traceWith (notifyTxsProcessed fStats tracer) ev
    let tr = appendName "Mempool" tracer
    traceWith (mpTracer tc tr) ev

mpTracer :: ( ToJSON (GenTxId blk)
            , ToObject (ApplyTxErr blk)
            , ToObject (GenTx blk)
            , LedgerSupportsMempool blk
            )
         => TraceSelection -> Trace IO Text -> Tracer IO (TraceEventMempool blk)
mpTracer tc tr = annotateSeverity $ toLogObject' (traceVerbosity tc) tr

--------------------------------------------------------------------------------
-- ForgeStateInfo Tracers
--------------------------------------------------------------------------------

forgeStateInfoMetricsTraceTransformer
  :: forall a blk. HasKESMetricsData blk
  => Proxy blk
  -> Trace IO a
  -> Tracer IO (Consensus.TraceLabelCreds (ForgeStateInfo blk))
forgeStateInfoMetricsTraceTransformer p tr = Tracer $
  \(Consensus.TraceLabelCreds _ forgeStateInfo) -> do
    case getKESMetricsData p forgeStateInfo of
      NoKESMetricsData -> pure ()
      TPraosKESMetricsData kesPeriodOfKey
                           (MaxKESEvolutions maxKesEvos)
                           (OperationalCertStartKESPeriod oCertStartKesPeriod) -> do
        let metricsTr = appendName "metrics" tr

            -- The KES period of the hot key is relative to the start KES
            -- period of the operational certificate.
            currentKesPeriod = oCertStartKesPeriod + kesPeriodOfKey

            oCertExpiryKesPeriod = oCertStartKesPeriod + fromIntegral maxKesEvos

            kesPeriodsUntilExpiry =
              max 0 (oCertExpiryKesPeriod - currentKesPeriod)

            logValues :: [LOContent a]
            logValues =
              [ LogValue "operationalCertificateStartKESPeriod"
                  $ PureI
                  $ fromIntegral oCertStartKesPeriod
              , LogValue "operationalCertificateExpiryKESPeriod"
                  $ PureI
                  $ fromIntegral oCertExpiryKesPeriod
              , LogValue "currentKESPeriod"
                  $ PureI
                  $ fromIntegral currentKesPeriod
              , LogValue "remainingKESPeriods"
                  $ PureI
                  $ fromIntegral kesPeriodsUntilExpiry
              ]

        meta <- mkLOMeta Critical Confidential
        mapM_ (traceNamedObject metricsTr . (meta,)) logValues

        -- Trace warning messages on the last 7 KES periods and, in the
        -- final and subsequent KES periods, trace alert messages.
        metaWarning <- mkLOMeta Warning Public
        metaAlert <- mkLOMeta Alert Public
        when (kesPeriodsUntilExpiry <= 7) $
          traceWith tr
            ( mempty
            , LogObject
                mempty
                (if kesPeriodsUntilExpiry <= 1 then metaAlert else metaWarning)
                (LogStructuredText mempty (expiryLogMessage kesPeriodsUntilExpiry))
            )
  where
    expiryLogMessage :: Word -> Text
    expiryLogMessage kesPeriodsUntilExpiry =
      "Operational key will expire in "
        <> (Text.pack . show) kesPeriodsUntilExpiry
        <> " KES periods."

forgeStateInfoTracer
  :: forall blk.
     ( HasKESMetricsData blk
     , Show (ForgeStateInfo blk)
     )
  => Proxy blk
  -> TraceSelection
  -> Trace IO Text
  -> Tracer IO (Consensus.TraceLabelCreds (ForgeStateInfo blk))
forgeStateInfoTracer p _ts tracer = Tracer $ \ev -> do
    let tr = appendName "Forge" tracer
    traceWith (forgeStateInfoMetricsTraceTransformer p tracer) ev
    traceWith (fsTracer tr) ev
  where
    fsTracer :: Trace IO Text -> Tracer IO (Consensus.TraceLabelCreds (ForgeStateInfo blk))
    fsTracer tr = showTracing $ contramap Text.pack $ toLogObject tr

--------------------------------------------------------------------------------
-- NodeToClient Tracers
--------------------------------------------------------------------------------

nodeToClientTracers'
  :: ( StandardHash blk
     , Show (ApplyTxErr blk)
     , Show (GenTx blk)
     , Show localPeer
     , ToObject localPeer
     , ShowQuery (Query blk)
     )
  => TraceSelection
  -> TracingVerbosity
  -> Trace IO Text
  -> NodeToClient.Tracers' localPeer blk DeserialiseFailure (Tracer IO)
nodeToClientTracers' trSel verb tr =
  NodeToClient.Tracers
  { NodeToClient.tChainSyncTracer =
    tracerOnOff (traceLocalChainSyncProtocol trSel) verb "LocalChainSyncProtocol" tr
  , NodeToClient.tTxSubmissionTracer =
    tracerOnOff (traceLocalTxSubmissionProtocol trSel) verb "LocalTxSubmissionProtocol" tr
  , NodeToClient.tStateQueryTracer =
    tracerOnOff (traceLocalStateQueryProtocol trSel) verb "LocalStateQueryProtocol" tr
  }

--------------------------------------------------------------------------------
-- NodeToNode Tracers
--------------------------------------------------------------------------------

nodeToNodeTracers'
  :: ( Consensus.RunNode blk
     , ConvertTxId blk
     , HasTxs blk
     , Show blk
     , Show (Header blk)
     , Show peer
     , ToObject peer
     )
  => TraceSelection
  -> TracingVerbosity
  -> Trace IO Text
  -> NodeToNode.Tracers' peer blk DeserialiseFailure (Tracer IO)
nodeToNodeTracers' trSel verb tr =
  NodeToNode.Tracers
  { NodeToNode.tChainSyncTracer = tracerOnOff (traceChainSyncProtocol trSel) verb "ChainSyncProtocol" tr
  , NodeToNode.tChainSyncSerialisedTracer = showOnOff (traceChainSyncProtocol trSel) "ChainSyncProtocolSerialised" tr
  , NodeToNode.tBlockFetchTracer = tracerOnOff (traceBlockFetchProtocol trSel) verb "BlockFetchProtocol" tr
  , NodeToNode.tBlockFetchSerialisedTracer = showOnOff (traceBlockFetchProtocolSerialised trSel) "BlockFetchProtocolSerialised" tr
  , NodeToNode.tTxSubmissionTracer = tracerOnOff (traceTxSubmissionProtocol trSel) verb "TxSubmissionProtocol" tr
  , NodeToNode.tTxSubmission2Tracer = tracerOnOff (traceTxSubmission2Protocol trSel) verb "TxSubmission2Protocol" tr
  }

teeTraceBlockFetchDecision
    :: ( Eq peer
       , HasHeader blk
       , Show peer
       , ToObject peer
       )
    => TracingVerbosity
    -> MVar (Maybe (WithSeverity [TraceLabelPeer peer (FetchDecision [Point (Header blk)])]),Integer)
    -> Trace IO Text
    -> Tracer IO (WithSeverity [TraceLabelPeer peer (FetchDecision [Point (Header blk)])])
teeTraceBlockFetchDecision verb eliding tr =
  Tracer $ \ev -> do
    traceWith (teeTraceBlockFetchDecision' meTr) ev
    traceWith (teeTraceBlockFetchDecisionElide verb eliding bfdTr) ev
 where
   meTr  = appendName "metrics" tr
   bfdTr = appendName "BlockFetchDecision" tr

teeTraceBlockFetchDecision'
    :: Trace IO Text
    -> Tracer IO (WithSeverity [TraceLabelPeer peer (FetchDecision [Point (Header blk)])])
teeTraceBlockFetchDecision' tr =
    Tracer $ \(WithSeverity _ peers) -> do
      meta <- mkLOMeta Info Confidential
      traceNamedObject tr (meta, LogValue "connectedPeers" . PureI $ fromIntegral $ length peers)

teeTraceBlockFetchDecisionElide
    :: ( Eq peer
       , HasHeader blk
       , Show peer
       , ToObject peer
       )
    => TracingVerbosity
    -> MVar (Maybe (WithSeverity [TraceLabelPeer peer (FetchDecision [Point (Header blk)])]),Integer)
    -> Trace IO Text
    -> Tracer IO (WithSeverity [TraceLabelPeer peer (FetchDecision [Point (Header blk)])])
teeTraceBlockFetchDecisionElide = elideToLogObject


-- | get information about a chain fragment

data ChainInformation = ChainInformation
  { slots :: Word64
  , blocks :: Word64
  , density :: Rational
    -- ^ the actual number of blocks created over the maximum expected number
    -- of blocks that could be created over the span of the last @k@ blocks.
  , epoch :: EpochNo
    -- ^ In which epoch is the tip of the current chain
  , slotInEpoch :: Word64
    -- ^ Relative slot number of the tip of the current chain within the
    -- epoch.
  , blocksUncoupledDelta :: Int64
    -- ^ The net change in number of blocks forged since last restart not on the
    -- current chain.
  }

chainInformation
  :: forall blk. HasHeader (Header blk)
  => ChainDB.NewTipInfo blk
  -> AF.AnchoredFragment (Header blk)
  -> Int64
  -> ChainInformation
chainInformation newTipInfo frag blocksUncoupledDelta = ChainInformation
    { slots = unSlotNo $ fromWithOrigin 0 (AF.headSlot frag)
    , blocks = unBlockNo $ fromWithOrigin (BlockNo 1) (AF.headBlockNo frag)
    , density = fragmentChainDensity frag
    , epoch = ChainDB.newTipEpoch newTipInfo
    , slotInEpoch = ChainDB.newTipSlotInEpoch newTipInfo
    , blocksUncoupledDelta = blocksUncoupledDelta
    }

fragmentChainDensity ::
  HasHeader (Header blk)
  => AF.AnchoredFragment (Header blk) -> Rational
fragmentChainDensity frag = calcDensity blockD slotD
  where
    calcDensity :: Word64 -> Word64 -> Rational
    calcDensity bl sl
      | sl > 0 = toRational bl / toRational sl
      | otherwise = 0
    slotN  = unSlotNo $ fromWithOrigin 0 (AF.headSlot frag)
    -- Slot of the tip - slot @k@ blocks back. Use 0 as the slot for genesis
    -- includes EBBs
    slotD   = slotN
            - unSlotNo (fromWithOrigin 0 (AF.lastSlot frag))
    -- Block numbers start at 1. We ignore the genesis EBB, which has block number 0.
    blockD = blockN - firstBlock
    blockN = unBlockNo $ fromWithOrigin (BlockNo 1) (AF.headBlockNo frag)
    firstBlock = case unBlockNo . blockNo <$> AF.last frag of
      -- Empty fragment, no blocks. We have that @blocks = 1 - 1 = 0@
      Left _  -> 1
      -- The oldest block is the genesis EBB with block number 0,
      -- don't let it contribute to the number of blocks
      Right 0 -> 1
      Right b -> b


--------------------------------------------------------------------------------
-- Trace Helpers
--------------------------------------------------------------------------------

readableTraceBlockchainTimeEvent :: TraceBlockchainTimeEvent UTCTime -> Text
readableTraceBlockchainTimeEvent ev = case ev of
    TraceStartTimeInTheFuture (SystemStart start) toWait ->
      "Waiting " <> (Text.pack . show) toWait <> " until genesis start time at " <> (Text.pack . show) start
    TraceCurrentSlotUnknown time _ ->
      "Too far from the chain tip to determine the current slot number for the time "
       <> (Text.pack . show) time
    TraceSystemClockMovedBack prevTime newTime ->
      "The system wall clock time moved backwards, but within our tolerance "
      <> "threshold. Previous 'current' time: " <> (Text.pack . show) prevTime
      <> ". New 'current' time: " <> (Text.pack . show) newTime

tracerOnOff :: Transformable Text IO a
            => OnOff b
            -> TracingVerbosity
            -> LoggerName
            -> Trace IO Text
            -> Tracer IO a
tracerOnOff (OnOff False) _ _ _ = nullTracer
tracerOnOff (OnOff True) verb name trcer = annotateSeverity
                                        $ toLogObject' verb
                                        $ appendName name trcer

tracerOnOff'
  :: OnOff b -> Tracer IO a -> Tracer IO a
tracerOnOff' (OnOff False) _ = nullTracer
tracerOnOff' (OnOff True) tr = tr

instance Show a => Show (WithSeverity a) where
  show (WithSeverity _sev a) = show a

showOnOff
  :: (Show a, HasSeverityAnnotation a)
  => OnOff b -> LoggerName -> Trace IO Text -> Tracer IO a
showOnOff (OnOff False) _ _ = nullTracer
showOnOff (OnOff True) name trcer = annotateSeverity
                                        $ showTracing
                                        $ withName name trcer

withName :: Text -> Trace IO Text -> Tracer IO String
withName name tr = contramap Text.pack $ toLogObject $ appendName name tr
