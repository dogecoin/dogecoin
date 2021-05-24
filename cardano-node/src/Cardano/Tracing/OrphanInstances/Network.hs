{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Tracing.OrphanInstances.Network () where

import           Cardano.Prelude hiding (show)
import           Prelude (String, show)

import           Control.Monad.Class.MonadTime (DiffTime, Time (..))
import           Data.Aeson (Value (..))
import qualified Data.IP as IP
import           Data.Text (pack)

import           Network.Mux (MuxTrace (..), WithMuxBearer (..))
import           Network.Socket (SockAddr (..))

import           Cardano.Tracing.ConvertTxId (ConvertTxId)
import           Cardano.Tracing.OrphanInstances.Common
import           Cardano.Tracing.Render

import           Ouroboros.Consensus.Block (ConvertRawHash (..), getHeader)
import           Ouroboros.Consensus.Ledger.SupportsMempool (GenTx, HasTxs (..), txForgetValidated,
                   txId)
import           Ouroboros.Consensus.Node.Run (RunNode, estimateBlockSize)
import           Ouroboros.Network.Block
import           Ouroboros.Network.BlockFetch.ClientState (TraceFetchClientState,
                   TraceLabelPeer (..))
import qualified Ouroboros.Network.BlockFetch.ClientState as BlockFetch
import           Ouroboros.Network.BlockFetch.Decision (FetchDecision, FetchDecline (..))
import           Ouroboros.Network.Codec (AnyMessageAndAgency (..), PeerHasAgency (..))
import           Ouroboros.Network.DeltaQ (GSV (..), PeerGSV (..))
import           Ouroboros.Network.KeepAlive (TraceKeepAliveClient (..))
import qualified Ouroboros.Network.NodeToClient as NtC
import           Ouroboros.Network.NodeToNode (ErrorPolicyTrace (..), TraceSendRecv (..),
                   WithAddr (..))
import qualified Ouroboros.Network.NodeToNode as NtN
import           Ouroboros.Network.Protocol.BlockFetch.Type (BlockFetch, Message (..))
import           Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)
import qualified Ouroboros.Network.Protocol.ChainSync.Type as ChainSync
import           Ouroboros.Network.Protocol.LocalStateQuery.Type (LocalStateQuery)
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as LocalStateQuery
import           Ouroboros.Network.Protocol.LocalTxSubmission.Type (LocalTxSubmission)
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Type as LocalTxSub
import           Ouroboros.Network.Protocol.Trans.Hello.Type (ClientHasAgency (..), Message (..),
                   ServerHasAgency (..))
import           Ouroboros.Network.Protocol.TxSubmission.Type (Message (..), TxSubmission)
import           Ouroboros.Network.Protocol.TxSubmission2.Type (TxSubmission2)
import           Ouroboros.Network.Snocket (LocalAddress (..))
import           Ouroboros.Network.Subscription (ConnectResult (..), DnsTrace (..),
                   SubscriberError (..), SubscriptionTrace (..), WithDomainName (..),
                   WithIPList (..))
import           Ouroboros.Network.TxSubmission.Inbound (ProcessedTxCount (..),
                   TraceTxSubmissionInbound (..))
import           Ouroboros.Network.TxSubmission.Outbound (TraceTxSubmissionOutbound (..))

import qualified Ouroboros.Network.Diffusion as ND

{- HLINT ignore "Use record patterns" -}

--
-- * instances of @HasPrivacyAnnotation@ and @HasSeverityAnnotation@
--
-- NOTE: this list is sorted by the unqualified name of the outermost type.

instance HasPrivacyAnnotation ND.DiffusionInitializationTracer
instance HasSeverityAnnotation ND.DiffusionInitializationTracer where
  getSeverityAnnotation _ = Info

instance HasPrivacyAnnotation NtC.HandshakeTr
instance HasSeverityAnnotation NtC.HandshakeTr where
  getSeverityAnnotation _ = Info


instance HasPrivacyAnnotation NtN.HandshakeTr
instance HasSeverityAnnotation NtN.HandshakeTr where
  getSeverityAnnotation _ = Info


instance HasPrivacyAnnotation NtN.AcceptConnectionsPolicyTrace
instance HasSeverityAnnotation NtN.AcceptConnectionsPolicyTrace where
  getSeverityAnnotation NtN.ServerTraceAcceptConnectionRateLimiting {} = Info
  getSeverityAnnotation NtN.ServerTraceAcceptConnectionHardLimit {} = Warning


instance HasPrivacyAnnotation (TraceFetchClientState header)
instance HasSeverityAnnotation (TraceFetchClientState header) where
  getSeverityAnnotation BlockFetch.AddedFetchRequest {} = Info
  getSeverityAnnotation BlockFetch.AcknowledgedFetchRequest {} = Info
  getSeverityAnnotation BlockFetch.StartedFetchBatch {} = Info
  getSeverityAnnotation BlockFetch.CompletedBlockFetch {} = Info
  getSeverityAnnotation BlockFetch.CompletedFetchBatch {} = Info
  getSeverityAnnotation BlockFetch.RejectedFetchBatch {} = Info
  getSeverityAnnotation BlockFetch.ClientTerminating {} = Notice


instance HasPrivacyAnnotation (TraceSendRecv a)
instance HasSeverityAnnotation (TraceSendRecv a) where
  getSeverityAnnotation _ = Debug


instance HasPrivacyAnnotation a => HasPrivacyAnnotation (TraceLabelPeer peer a)
instance HasSeverityAnnotation a => HasSeverityAnnotation (TraceLabelPeer peer a) where
  getSeverityAnnotation (TraceLabelPeer _p a) = getSeverityAnnotation a


instance HasPrivacyAnnotation [TraceLabelPeer peer (FetchDecision [Point header])]
instance HasSeverityAnnotation [TraceLabelPeer peer (FetchDecision [Point header])] where
  getSeverityAnnotation [] = Debug
  getSeverityAnnotation xs =
      maximum $ map (\(TraceLabelPeer _ a) -> fetchDecisionSeverity a) xs
    where
      fetchDecisionSeverity :: FetchDecision a -> Severity
      fetchDecisionSeverity fd =
        case fd of
          Left FetchDeclineChainNotPlausible     -> Debug
          Left FetchDeclineChainNoIntersection   -> Notice
          Left FetchDeclineAlreadyFetched        -> Debug
          Left FetchDeclineInFlightThisPeer      -> Debug
          Left FetchDeclineInFlightOtherPeer     -> Debug
          Left FetchDeclinePeerShutdown          -> Info
          Left FetchDeclinePeerSlow              -> Info
          Left FetchDeclineReqsInFlightLimit {}  -> Info
          Left FetchDeclineBytesInFlightLimit {} -> Info
          Left FetchDeclinePeerBusy {}           -> Info
          Left FetchDeclineConcurrencyLimit {}   -> Info
          Right _                                -> Info


instance HasPrivacyAnnotation (TraceTxSubmissionInbound txid tx)
instance HasSeverityAnnotation (TraceTxSubmissionInbound txid tx) where
  getSeverityAnnotation _ = Info


instance HasPrivacyAnnotation (TraceTxSubmissionOutbound txid tx)
instance HasSeverityAnnotation (TraceTxSubmissionOutbound txid tx) where
  getSeverityAnnotation _ = Info


instance HasPrivacyAnnotation (TraceKeepAliveClient remotePeer)
instance HasSeverityAnnotation (TraceKeepAliveClient remotePeer) where
  getSeverityAnnotation _ = Info


instance HasPrivacyAnnotation (WithAddr addr ErrorPolicyTrace)
instance HasSeverityAnnotation (WithAddr addr ErrorPolicyTrace) where
  getSeverityAnnotation (WithAddr _ ev) = case ev of
    ErrorPolicySuspendPeer {} -> Warning -- peer misbehaved
    ErrorPolicySuspendConsumer {} -> Notice -- peer temporarily not useful
    ErrorPolicyLocalNodeError {} -> Error
    ErrorPolicyResumePeer {} -> Debug
    ErrorPolicyKeepSuspended {} -> Debug
    ErrorPolicyResumeConsumer {} -> Debug
    ErrorPolicyResumeProducer {} -> Debug
    ErrorPolicyUnhandledApplicationException {} -> Error
    ErrorPolicyUnhandledConnectionException {} -> Error
    ErrorPolicyAcceptException {} -> Error


instance HasPrivacyAnnotation (WithDomainName DnsTrace)
instance HasSeverityAnnotation (WithDomainName DnsTrace) where
  getSeverityAnnotation (WithDomainName _ ev) = case ev of
    DnsTraceLookupException {} -> Error
    DnsTraceLookupAError {} -> Error
    DnsTraceLookupAAAAError {} -> Error
    DnsTraceLookupIPv6First -> Debug
    DnsTraceLookupIPv4First -> Debug
    DnsTraceLookupAResult {} -> Debug
    DnsTraceLookupAAAAResult {} -> Debug


instance HasPrivacyAnnotation (WithDomainName (SubscriptionTrace SockAddr))
instance HasSeverityAnnotation (WithDomainName (SubscriptionTrace SockAddr)) where
  getSeverityAnnotation (WithDomainName _ ev) = case ev of
    SubscriptionTraceConnectStart {} -> Notice
    SubscriptionTraceConnectEnd {} -> Notice
    SubscriptionTraceConnectException _ e ->
        case fromException $ SomeException e of
             Just (_::SubscriberError) -> Debug
             Nothing -> Error
    SubscriptionTraceSocketAllocationException {} -> Error
    SubscriptionTraceTryConnectToPeer {} -> Info
    SubscriptionTraceSkippingPeer {} -> Info
    SubscriptionTraceSubscriptionRunning -> Debug
    SubscriptionTraceSubscriptionWaiting {} -> Debug
    SubscriptionTraceSubscriptionFailed -> Warning
    SubscriptionTraceSubscriptionWaitingNewConnection {} -> Debug
    SubscriptionTraceStart {} -> Debug
    SubscriptionTraceRestart {} -> Debug
    SubscriptionTraceConnectionExist {} -> Info
    SubscriptionTraceUnsupportedRemoteAddr {} -> Warning
    SubscriptionTraceMissingLocalAddress -> Warning
    SubscriptionTraceApplicationException _ e ->
        case fromException $ SomeException e of
             Just (_::SubscriberError) -> Debug
             Nothing -> Error
    SubscriptionTraceAllocateSocket {} -> Debug
    SubscriptionTraceCloseSocket {} -> Debug


instance HasPrivacyAnnotation (WithIPList (SubscriptionTrace SockAddr))
instance HasSeverityAnnotation (WithIPList (SubscriptionTrace SockAddr)) where
  getSeverityAnnotation (WithIPList _ _ ev) = case ev of
    SubscriptionTraceConnectStart _ -> Info
    SubscriptionTraceConnectEnd _ connectResult -> case connectResult of
      ConnectSuccess -> Info
      ConnectSuccessLast -> Notice
      ConnectValencyExceeded -> Warning
    SubscriptionTraceConnectException _ e ->
        case fromException $ SomeException e of
             Just (_::SubscriberError) -> Debug
             Nothing -> Error
    SubscriptionTraceSocketAllocationException {} -> Error
    SubscriptionTraceTryConnectToPeer {} -> Info
    SubscriptionTraceSkippingPeer {} -> Info
    SubscriptionTraceSubscriptionRunning -> Debug
    SubscriptionTraceSubscriptionWaiting {} -> Debug
    SubscriptionTraceSubscriptionFailed -> Error
    SubscriptionTraceSubscriptionWaitingNewConnection {} -> Notice
    SubscriptionTraceStart {} -> Debug
    SubscriptionTraceRestart {} -> Info
    SubscriptionTraceConnectionExist {} -> Notice
    SubscriptionTraceUnsupportedRemoteAddr {} -> Error
    SubscriptionTraceMissingLocalAddress -> Warning
    SubscriptionTraceApplicationException _ e ->
        case fromException $ SomeException e of
             Just (_::SubscriberError) -> Debug
             Nothing -> Error
    SubscriptionTraceAllocateSocket {} -> Debug
    SubscriptionTraceCloseSocket {} -> Info


instance HasPrivacyAnnotation (Identity (SubscriptionTrace LocalAddress))
instance HasSeverityAnnotation (Identity (SubscriptionTrace LocalAddress)) where
  getSeverityAnnotation (Identity ev) = case ev of
    SubscriptionTraceConnectStart {} -> Notice
    SubscriptionTraceConnectEnd {} -> Notice
    SubscriptionTraceConnectException {} -> Error
    SubscriptionTraceSocketAllocationException {} -> Error
    SubscriptionTraceTryConnectToPeer {} -> Notice
    SubscriptionTraceSkippingPeer {} -> Info
    SubscriptionTraceSubscriptionRunning -> Notice
    SubscriptionTraceSubscriptionWaiting {} -> Debug
    SubscriptionTraceSubscriptionFailed -> Warning
    SubscriptionTraceSubscriptionWaitingNewConnection {} -> Debug
    SubscriptionTraceStart {} -> Notice
    SubscriptionTraceRestart {} -> Notice
    SubscriptionTraceConnectionExist {} -> Debug
    SubscriptionTraceUnsupportedRemoteAddr {} -> Warning
    SubscriptionTraceMissingLocalAddress -> Warning
    SubscriptionTraceApplicationException {} -> Error
    SubscriptionTraceAllocateSocket {} -> Debug
    SubscriptionTraceCloseSocket {} -> Debug


instance Transformable Text IO (Identity (SubscriptionTrace LocalAddress)) where
  trTransformer = trStructuredText
instance HasTextFormatter (Identity (SubscriptionTrace LocalAddress)) where
  formatText a _ = pack (show a)


instance ToObject (Identity (SubscriptionTrace LocalAddress)) where
  toObject _verb (Identity ev) =
    mkObject [ "kind" .= ("SubscriptionTrace" :: String)
             , "event" .= show ev
             ]


instance HasPrivacyAnnotation (WithMuxBearer peer MuxTrace)
instance HasSeverityAnnotation (WithMuxBearer peer MuxTrace) where
  getSeverityAnnotation (WithMuxBearer _ ev) = case ev of
    MuxTraceRecvHeaderStart -> Debug
    MuxTraceRecvHeaderEnd {} -> Debug
    MuxTraceRecvStart {} -> Debug
    MuxTraceRecvEnd {} -> Debug
    MuxTraceSendStart {} -> Debug
    MuxTraceSendEnd -> Debug
    MuxTraceState {} -> Info
    MuxTraceCleanExit {} -> Notice
    MuxTraceExceptionExit {} -> Notice
    MuxTraceChannelRecvStart {} -> Debug
    MuxTraceChannelRecvEnd {} -> Debug
    MuxTraceChannelSendStart {} -> Debug
    MuxTraceChannelSendEnd {} -> Debug
    MuxTraceHandshakeStart -> Debug
    MuxTraceHandshakeClientEnd {} -> Info
    MuxTraceHandshakeServerEnd -> Debug
    MuxTraceHandshakeClientError {} -> Error
    MuxTraceHandshakeServerError {} -> Error
    MuxTraceRecvDeltaQObservation {} -> Debug
    MuxTraceRecvDeltaQSample {} -> Debug
    MuxTraceSDUReadTimeoutException -> Notice
    MuxTraceSDUWriteTimeoutException -> Notice
    MuxTraceStartEagerly _ _ -> Debug
    MuxTraceStartOnDemand _ _ -> Debug
    MuxTraceStartedOnDemand _ _ -> Debug
    MuxTraceTerminating {} -> Debug
    MuxTraceShutdown -> Debug

--
-- | instances of @Transformable@
--
-- NOTE: this list is sorted by the unqualified name of the outermost type.

instance Transformable Text IO ND.DiffusionInitializationTracer where
  trTransformer = trStructuredText
instance HasTextFormatter ND.DiffusionInitializationTracer where
  formatText a _ = pack (show a)

instance Transformable Text IO NtN.HandshakeTr where
  trTransformer = trStructuredText
instance HasTextFormatter NtN.HandshakeTr where
  formatText a _ = pack (show a)


instance Transformable Text IO NtC.HandshakeTr where
  trTransformer = trStructuredText
instance HasTextFormatter NtC.HandshakeTr where
  formatText a _ = pack (show a)


instance Transformable Text IO NtN.AcceptConnectionsPolicyTrace where
  trTransformer = trStructuredText
instance HasTextFormatter NtN.AcceptConnectionsPolicyTrace where
  formatText a _ = pack (show a)


instance (StandardHash header, Show peer, ToObject peer)
      => Transformable Text IO [TraceLabelPeer peer (FetchDecision [Point header])] where
  trTransformer = trStructuredText
instance (StandardHash header, Show peer)
      => HasTextFormatter [TraceLabelPeer peer (FetchDecision [Point header])] where
  formatText a _ = pack (show a)


instance ( Show peer, ToObject peer, Show a, HasPrivacyAnnotation a
         , HasSeverityAnnotation a, ToObject a)
      => Transformable Text IO (TraceLabelPeer peer a) where
  trTransformer = trStructuredText
instance (Show peer, Show a)
      => HasTextFormatter (TraceLabelPeer peer a) where
  formatText a _ = pack (show a)


instance Transformable Text IO (TraceTxSubmissionInbound txid tx) where
  trTransformer = trStructuredText
instance HasTextFormatter (TraceTxSubmissionInbound txid tx) where
  formatText a _ = pack (show a)


instance (Show tx, Show txid)
      => Transformable Text IO (TraceTxSubmissionOutbound txid tx) where
  trTransformer = trStructuredText
instance (Show tx, Show txid)
      => HasTextFormatter (TraceTxSubmissionOutbound txid tx) where
  formatText a _ = pack (show a)


instance Show remotePeer => Transformable Text IO (TraceKeepAliveClient remotePeer) where
  trTransformer = trStructuredText
instance Show addr
      => HasTextFormatter (TraceKeepAliveClient addr) where
    formatText a _ = pack (show a)


instance Show addr => Transformable Text IO (WithAddr addr ErrorPolicyTrace) where
  trTransformer = trStructuredText
instance Show addr => HasTextFormatter (WithAddr addr ErrorPolicyTrace) where
  formatText a _ = pack (show a)


instance Transformable Text IO (WithDomainName (SubscriptionTrace SockAddr)) where
  trTransformer = trStructuredText
instance HasTextFormatter (WithDomainName (SubscriptionTrace SockAddr)) where
  formatText a _ = pack (show a)


instance Transformable Text IO (WithDomainName DnsTrace) where
  trTransformer = trStructuredText
instance HasTextFormatter (WithDomainName DnsTrace) where
  formatText a _ = pack (show a)


instance Transformable Text IO (WithIPList (SubscriptionTrace SockAddr)) where
  trTransformer = trStructuredText
instance HasTextFormatter (WithIPList (SubscriptionTrace SockAddr)) where
  formatText a _ = pack (show a)


instance (Show peer)
      => Transformable Text IO (WithMuxBearer peer MuxTrace) where
  trTransformer = trStructuredText
instance (Show peer)
      => HasTextFormatter (WithMuxBearer peer MuxTrace) where
  formatText (WithMuxBearer peer ev) = \_o ->
        "Bearer on " <> pack (show peer)
     <> " event: " <> pack (show ev)


--
-- | instances of @ToObject@
--
-- NOTE: this list is sorted by the unqualified name of the outermost type.

instance ( ConvertTxId blk
         , RunNode blk
         , HasTxs blk
         )
      => ToObject (AnyMessageAndAgency (BlockFetch blk (Point blk))) where
  toObject MinimalVerbosity (AnyMessageAndAgency stok (MsgBlock blk)) =
    mkObject [ "kind" .= String "MsgBlock"
             , "agency" .= String (pack $ show stok)
             , "blockHash" .= renderHeaderHash (Proxy @blk) (blockHash blk)
             , "blockSize" .= toJSON (estimateBlockSize (getHeader blk))
             ]

  toObject verb (AnyMessageAndAgency stok (MsgBlock blk)) =
    mkObject [ "kind" .= String "MsgBlock"
             , "agency" .= String (pack $ show stok)
             , "blockHash" .= renderHeaderHash (Proxy @blk) (blockHash blk)
             , "blockSize" .= toJSON (estimateBlockSize (getHeader blk))
             , "txIds" .= toJSON (presentTx <$> map txForgetValidated (extractTxs blk))
             ]
      where
        presentTx :: GenTx blk -> Value
        presentTx =  String . renderTxIdForVerbosity verb . txId

  toObject _v (AnyMessageAndAgency stok MsgRequestRange{}) =
    mkObject [ "kind" .= String "MsgRequestRange"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _v (AnyMessageAndAgency stok MsgStartBatch{}) =
    mkObject [ "kind" .= String "MsgStartBatch"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _v (AnyMessageAndAgency stok MsgNoBlocks{}) =
    mkObject [ "kind" .= String "MsgNoBlocks"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _v (AnyMessageAndAgency stok MsgBatchDone{}) =
    mkObject [ "kind" .= String "MsgBatchDone"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _v (AnyMessageAndAgency stok MsgClientDone{}) =
    mkObject [ "kind" .= String "MsgClientDone"
             , "agency" .= String (pack $ show stok)
             ]

instance (forall result. Show (query result))
      => ToObject (AnyMessageAndAgency (LocalStateQuery blk pt query)) where
  toObject _verb (AnyMessageAndAgency stok LocalStateQuery.MsgAcquire{}) =
    mkObject [ "kind" .= String "MsgAcquire"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _verb (AnyMessageAndAgency stok LocalStateQuery.MsgAcquired{}) =
    mkObject [ "kind" .= String "MsgAcquired"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _verb (AnyMessageAndAgency stok LocalStateQuery.MsgFailure{}) =
    mkObject [ "kind" .= String "MsgFailure"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _verb (AnyMessageAndAgency stok LocalStateQuery.MsgQuery{}) =
    mkObject [ "kind" .= String "MsgQuery"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _verb (AnyMessageAndAgency stok LocalStateQuery.MsgResult{}) =
    mkObject [ "kind" .= String "MsgResult"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _verb (AnyMessageAndAgency stok LocalStateQuery.MsgRelease{}) =
    mkObject [ "kind" .= String "MsgRelease"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _verb (AnyMessageAndAgency stok LocalStateQuery.MsgReAcquire{}) =
    mkObject [ "kind" .= String "MsgReAcquire"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _verb (AnyMessageAndAgency stok LocalStateQuery.MsgDone{}) =
    mkObject [ "kind" .= String "MsgDone"
             , "agency" .= String (pack $ show stok)
             ]

instance ToObject (AnyMessageAndAgency (LocalTxSubmission tx err)) where
  toObject _verb (AnyMessageAndAgency stok LocalTxSub.MsgSubmitTx{}) =
    mkObject [ "kind" .= String "MsgSubmitTx"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _verb (AnyMessageAndAgency stok LocalTxSub.MsgAcceptTx{}) =
    mkObject [ "kind" .= String "MsgAcceptTx"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _verb (AnyMessageAndAgency stok LocalTxSub.MsgRejectTx{}) =
    mkObject [ "kind" .= String "MsgRejectTx"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _verb (AnyMessageAndAgency stok LocalTxSub.MsgDone{}) =
    mkObject [ "kind" .= String "MsgDone"
             , "agency" .= String (pack $ show stok)
             ]

instance ToObject (AnyMessageAndAgency (ChainSync blk pt tip)) where
   toObject _verb (AnyMessageAndAgency stok ChainSync.MsgRequestNext{}) =
     mkObject [ "kind" .= String "MsgRequestNext"
              , "agency" .= String (pack $ show stok)
              ]
   toObject _verb (AnyMessageAndAgency stok ChainSync.MsgAwaitReply{}) =
     mkObject [ "kind" .= String "MsgAwaitReply"
              , "agency" .= String (pack $ show stok)
              ]
   toObject _verb (AnyMessageAndAgency stok ChainSync.MsgRollForward{}) =
     mkObject [ "kind" .= String "MsgRollForward"
              , "agency" .= String (pack $ show stok)
              ]
   toObject _verb (AnyMessageAndAgency stok ChainSync.MsgRollBackward{}) =
     mkObject [ "kind" .= String "MsgRollBackward"
              , "agency" .= String (pack $ show stok)
              ]
   toObject _verb (AnyMessageAndAgency stok ChainSync.MsgFindIntersect{}) =
     mkObject [ "kind" .= String "MsgFindIntersect"
              , "agency" .= String (pack $ show stok)
              ]
   toObject _verb (AnyMessageAndAgency stok ChainSync.MsgIntersectFound{}) =
     mkObject [ "kind" .= String "MsgIntersectFound"
              , "agency" .= String (pack $ show stok)
              ]
   toObject _verb (AnyMessageAndAgency stok ChainSync.MsgIntersectNotFound{}) =
     mkObject [ "kind" .= String "MsgIntersectNotFound"
              , "agency" .= String (pack $ show stok)
              ]
   toObject _verb (AnyMessageAndAgency stok ChainSync.MsgDone{}) =
     mkObject [ "kind" .= String "MsgDone"
              , "agency" .= String (pack $ show stok)
              ]

instance (Show txid, Show tx)
      => ToObject (AnyMessageAndAgency (TxSubmission txid tx)) where
  toObject _verb (AnyMessageAndAgency stok (MsgRequestTxs txids)) =
    mkObject
      [ "kind" .= String "MsgRequestTxs"
      , "agency" .= String (pack $ show stok)
      , "txIds" .= String (pack $ show txids)
      ]
  toObject _verb (AnyMessageAndAgency stok (MsgReplyTxs txs)) =
    mkObject
      [ "kind" .= String "MsgReplyTxs"
      , "agency" .= String (pack $ show stok)
      , "txs" .= String (pack $ show txs)
      ]
  toObject _verb (AnyMessageAndAgency stok (MsgRequestTxIds _ _ _)) =
    mkObject
      [ "kind" .= String "MsgRequestTxIds"
      , "agency" .= String (pack $ show stok)
      ]
  toObject _verb (AnyMessageAndAgency stok (MsgReplyTxIds _)) =
    mkObject
      [ "kind" .= String "MsgReplyTxIds"
      , "agency" .= String (pack $ show stok)
      ]
  toObject _verb (AnyMessageAndAgency stok MsgDone) =
    mkObject
      [ "kind" .= String "MsgDone"
      , "agency" .= String (pack $ show stok)
      ]

instance (Show txid, Show tx)
      => ToObject (AnyMessageAndAgency (TxSubmission2 txid tx)) where
  toObject _verb (AnyMessageAndAgency
                   -- we need this pattern match for GHC to recognise this
                   -- function as total.
                   stok@(ClientAgency TokHello)
                   MsgHello) =
    mkObject
      [ "kind" .= String "MsgHello"
      , "agency" .= String (pack $ show stok)
      ]
  toObject verb (AnyMessageAndAgency
                  (ClientAgency (TokClientTalk stok))
                  (MsgTalk msg)) =
    toObject verb (AnyMessageAndAgency (ClientAgency stok) msg)
  toObject verb (AnyMessageAndAgency
                  (ServerAgency (TokServerTalk stok))
                  (MsgTalk msg)) =
    toObject verb (AnyMessageAndAgency (ServerAgency stok) msg)


instance ToObject (FetchDecision [Point header]) where
  toObject _verb (Left decline) =
    mkObject [ "kind" .= String "FetchDecision declined"
             , "declined" .= String (pack (show decline))
             ]
  toObject _verb (Right results) =
    mkObject [ "kind" .= String "FetchDecision results"
             , "length" .= String (pack $ show $ length results)
             ]

instance ToObject ND.DiffusionInitializationTracer where
  toObject _verb (ND.RunServer sockAddr) = mkObject
    [ "kind" .= String "RunServer"
    , "socketAddress" .= String (pack (show sockAddr))
    ]

  toObject _verb (ND.RunLocalServer localAddress) = mkObject
    [ "kind" .= String "RunLocalServer"
    , "localAddress" .= String (pack (show localAddress))
    ]
  toObject _verb (ND.UsingSystemdSocket path) = mkObject
    [ "kind" .= String "UsingSystemdSocket"
    , "path" .= String (pack path)
    ]

  toObject _verb (ND.CreateSystemdSocketForSnocketPath path) = mkObject
    [ "kind" .= String "CreateSystemdSocketForSnocketPath"
    , "path" .= String (pack path)
    ]
  toObject _verb (ND.CreatedLocalSocket path) = mkObject
    [ "kind" .= String "CreatedLocalSocket"
    , "path" .= String (pack path)
    ]
  toObject _verb (ND.ConfiguringLocalSocket path socket) = mkObject
    [ "kind" .= String "ConfiguringLocalSocket"
    , "path" .= String (pack path)
    , "socket" .= String (pack (show socket))
    ]
  toObject _verb (ND.ListeningLocalSocket path socket) = mkObject
    [ "kind" .= String "ListeningLocalSocket"
    , "path" .= String (pack path)
    , "socket" .= String (pack (show socket))
    ]
  toObject _verb (ND.LocalSocketUp path fd) = mkObject
    [ "kind" .= String "LocalSocketUp"
    , "path" .= String (pack path)
    , "socket" .= String (pack (show fd))
    ]
  toObject _verb (ND.CreatingServerSocket socket) = mkObject
    [ "kind" .= String "CreatingServerSocket"
    , "socket" .= String (pack (show socket))
    ]
  toObject _verb (ND.ListeningServerSocket socket) = mkObject
    [ "kind" .= String "ListeningServerSocket"
    , "socket" .= String (pack (show socket))
    ]
  toObject _verb (ND.ServerSocketUp socket) = mkObject
    [ "kind" .= String "ServerSocketUp"
    , "socket" .= String (pack (show socket))
    ]
  toObject _verb (ND.ConfiguringServerSocket socket) = mkObject
    [ "kind" .= String "ConfiguringServerSocket"
    , "socket" .= String (pack (show socket))
    ]
  toObject _verb (ND.UnsupportedLocalSystemdSocket path) = mkObject
    [ "kind" .= String "UnsupportedLocalSystemdSocket"
    , "path" .= String (pack (show path))
    ]
  toObject _verb ND.UnsupportedReadySocketCase = mkObject
    [ "kind" .= String "UnsupportedReadySocketCase"
    ]
  toObject _verb (ND.DiffusionErrored exception) = mkObject
    [ "kind" .= String "DiffusionErrored"
    , "path" .= String (pack (show exception))
    ]

instance ToObject NtC.HandshakeTr where
  toObject _verb (WithMuxBearer b ev) =
    mkObject [ "kind" .= String "LocalHandshakeTrace"
             , "bearer" .= show b
             , "event" .= show ev ]


instance ToObject NtN.HandshakeTr where
  toObject _verb (WithMuxBearer b ev) =
    mkObject [ "kind" .= String "HandshakeTrace"
             , "bearer" .= show b
             , "event" .= show ev ]


instance ToObject NtN.AcceptConnectionsPolicyTrace where
  toObject _verb (NtN.ServerTraceAcceptConnectionRateLimiting delay numOfConnections) =
    mkObject [ "kind" .= String "ServerTraceAcceptConnectionRateLimiting"
             , "delay" .= show delay
             , "numberOfConnection" .= show numOfConnections
             ]
  toObject _verb (NtN.ServerTraceAcceptConnectionHardLimit softLimit) =
    mkObject [ "kind" .= String "ServerTraceAcceptConnectionHardLimit"
             , "softLimit" .= show softLimit
             ]


instance ConvertRawHash blk
      => ToObject (Point blk) where
  toObject _verb GenesisPoint =
    mkObject
      [ "kind" .= String "GenesisPoint" ]
  toObject verb (BlockPoint slot h) =
    mkObject
      [ "kind" .= String "BlockPoint"
      , "slot" .= toJSON (unSlotNo slot)
      , "headerHash" .= renderHeaderHashForVerbosity (Proxy @blk) verb h
      ]


instance ToObject SlotNo where
  toObject _verb slot =
    mkObject [ "kind" .= String "SlotNo"
             , "slot" .= toJSON (unSlotNo slot) ]


instance ConvertRawHash header => ToObject (TraceFetchClientState header) where
  toObject _verb BlockFetch.AddedFetchRequest {} =
    mkObject [ "kind" .= String "AddedFetchRequest" ]
  toObject _verb BlockFetch.AcknowledgedFetchRequest {} =
    mkObject [ "kind" .= String "AcknowledgedFetchRequest" ]
  toObject _verb (BlockFetch.CompletedBlockFetch pt _ _ _ _) =
    mkObject [ "kind"  .= String "CompletedBlockFetch"
             , "block" .= String
               (case pt of
                  GenesisPoint -> "Genesis"
                  BlockPoint _ h -> renderHeaderHash (Proxy @header) h)
             ]
  toObject _verb BlockFetch.CompletedFetchBatch {} =
    mkObject [ "kind" .= String "CompletedFetchBatch" ]
  toObject _verb BlockFetch.StartedFetchBatch {} =
    mkObject [ "kind" .= String "StartedFetchBatch" ]
  toObject _verb BlockFetch.RejectedFetchBatch {} =
    mkObject [ "kind" .= String "RejectedFetchBatch" ]
  toObject _verb BlockFetch.ClientTerminating {} =
    mkObject [ "kind" .= String "ClientTerminating" ]


instance (ToObject peer)
      => ToObject [TraceLabelPeer peer (FetchDecision [Point header])] where
  toObject MinimalVerbosity _ = emptyObject
  toObject _ [] = emptyObject
  toObject _ xs = mkObject
    [ "kind"  .= String "PeersFetch"
    , "peers" .= toJSON
      (foldl' (\acc x -> toObject MaximalVerbosity x : acc) [] xs) ]

instance (ToObject peer, ToObject a) => ToObject (TraceLabelPeer peer a) where
  toObject verb (TraceLabelPeer peerid a) =
    mkObject [ "peer" .= toObject verb peerid ] <> toObject verb a


instance ToObject (AnyMessageAndAgency ps)
      => ToObject (TraceSendRecv ps) where
  toObject verb (TraceSendMsg m) = mkObject
    [ "kind" .= String "Send" , "msg" .= toObject verb m ]
  toObject verb (TraceRecvMsg m) = mkObject
    [ "kind" .= String "Recv" , "msg" .= toObject verb m ]


instance ToObject (TraceTxSubmissionInbound txid tx) where
  toObject _verb (TraceTxSubmissionCollected count) =
    mkObject
      [ "kind" .= String "TraceTxSubmissionCollected"
      , "count" .= toJSON count
      ]
  toObject _verb (TraceTxSubmissionProcessed processed) =
    mkObject
      [ "kind" .= String "TraceTxSubmissionProcessed"
      , "accepted" .= toJSON (ptxcAccepted processed)
      , "rejected" .= toJSON (ptxcRejected processed)
      ]
  toObject _verb TraceTxInboundTerminated =
    mkObject
      [ "kind" .= String "TraceTxInboundTerminated"
      ]
  toObject _verb (TraceTxInboundCanRequestMoreTxs count) =
    mkObject
      [ "kind" .= String "TraceTxInboundCanRequestMoreTxs"
      , "count" .= toJSON count
      ]
  toObject _verb (TraceTxInboundCannotRequestMoreTxs count) =
    mkObject
      [ "kind" .= String "TraceTxInboundCannotRequestMoreTxs"
      , "count" .= toJSON count
      ]


instance (Show txid, Show tx)
      => ToObject (TraceTxSubmissionOutbound txid tx) where
  toObject MaximalVerbosity (TraceTxSubmissionOutboundRecvMsgRequestTxs txids) =
    mkObject
      [ "kind" .= String "TraceTxSubmissionOutboundRecvMsgRequestTxs"
      , "txIds" .= String (pack $ show txids)
      ]
  toObject _verb (TraceTxSubmissionOutboundRecvMsgRequestTxs _txids) =
    mkObject
      [ "kind" .= String "TraceTxSubmissionOutboundRecvMsgRequestTxs"
      ]
  toObject MaximalVerbosity (TraceTxSubmissionOutboundSendMsgReplyTxs txs) =
    mkObject
      [ "kind" .= String "TraceTxSubmissionOutboundSendMsgReplyTxs"
      , "txs" .= String (pack $ show txs)
      ]
  toObject _verb (TraceTxSubmissionOutboundSendMsgReplyTxs _txs) =
    mkObject
      [ "kind" .= String "TraceTxSubmissionOutboundSendMsgReplyTxs"
      ]
  toObject _verb (TraceControlMessage _msg) =
    mkObject
      [ "kind" .= String "TraceControlMessage"
      ]


instance Show remotePeer => ToObject (TraceKeepAliveClient remotePeer) where
  toObject _verb (AddSample peer rtt pgsv) =
    mkObject
      [ "kind" .= String "TraceKeepAliveClient AddSample"
      , "address" .= show peer
      , "rtt" .= rtt
      , "sampleTime" .= show (dTime $ sampleTime pgsv)
      , "outboundG" .= (realToFrac $ gGSV (outboundGSV pgsv) :: Double)
      , "inboundG" .= (realToFrac $ gGSV (inboundGSV pgsv) :: Double)
      ]
    where
      gGSV :: GSV -> DiffTime
      gGSV (GSV g _ _) = g

      dTime :: Time -> Double
      dTime (Time d) = realToFrac d

instance Show addr => ToObject (WithAddr addr ErrorPolicyTrace) where
  toObject _verb (WithAddr addr ev) =
    mkObject [ "kind" .= String "ErrorPolicyTrace"
             , "address" .= show addr
             , "event" .= show ev ]


instance ToObject (WithIPList (SubscriptionTrace SockAddr)) where
  toObject _verb (WithIPList localAddresses dests ev) =
    mkObject [ "kind" .= String "WithIPList SubscriptionTrace"
             , "localAddresses" .= show localAddresses
             , "dests" .= show dests
             , "event" .= show ev ]


instance ToObject (WithDomainName DnsTrace) where
  toObject _verb (WithDomainName dom ev) =
    mkObject [ "kind" .= String "DnsTrace"
             , "domain" .= show dom
             , "event" .= show ev ]


instance ToObject (WithDomainName (SubscriptionTrace SockAddr)) where
  toObject _verb (WithDomainName dom ev) =
    mkObject [ "kind" .= String "SubscriptionTrace"
             , "domain" .= show dom
             , "event" .= show ev ]


instance (Show peer) => ToObject (WithMuxBearer peer MuxTrace) where
  toObject _verb (WithMuxBearer b ev) =
    mkObject [ "kind" .= String "MuxTrace"
             , "bearer" .= show b
             , "event" .= show ev ]

instance ToObject NtN.RemoteAddress where
    toObject _verb (SockAddrInet port addr) =
        let ip = IP.fromHostAddress addr in
        mkObject [ "addr" .= show ip
                 , "port" .= show port
                 ]
    toObject _verb (SockAddrInet6 port _ addr _) =
        let ip = IP.fromHostAddress6 addr in
        mkObject [ "addr" .= show ip
                 , "port" .= show port
                 ]
    toObject _verb (SockAddrUnix path) =
        mkObject [ "path" .= show path ]


instance ToObject NtN.RemoteConnectionId where
    toObject verb (NtN.ConnectionId l r) =
        mkObject [ "local" .= toObject verb l
                 , "remote" .= toObject verb r
                 ]

instance ToObject LocalAddress where
    toObject _verb (LocalAddress path) =
        mkObject ["path" .= path]

instance ToObject NtC.LocalConnectionId where
    toObject verb (NtC.ConnectionId l r) =
        mkObject [ "local" .= toObject verb l
                 , "remote" .= toObject verb r
                 ]
