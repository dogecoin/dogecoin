{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Tracing.OrphanInstances.Consensus () where

import           Cardano.Prelude hiding (show)
import           Prelude (show)

import           Data.Aeson (Value (..))
import           Data.Text (pack)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Cardano.Tracing.OrphanInstances.Common
import           Cardano.Tracing.OrphanInstances.Network ()
import           Cardano.Tracing.Render (renderChainHash, renderChunkNo, renderHeaderHash,
                   renderHeaderHashForVerbosity, renderPoint, renderPointAsPhrase,
                   renderPointForVerbosity, renderRealPoint, renderRealPointAsPhrase,
                   renderTipBlockNo, renderTipForVerbosity, renderTipHash, renderWithOrigin)
import           Cardano.Slotting.Slot (fromWithOrigin)

import           Ouroboros.Consensus.Block (BlockProtocol, CannotForge, ConvertRawHash (..),
                   ForgeStateUpdateError, Header, RealPoint, getHeader, headerPoint, realPointHash,
                   realPointSlot, blockNo, blockPrevHash, pointHash)
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Inspect (InspectLedger, LedgerEvent (..), LedgerUpdate,
                   LedgerWarning)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, GenTx, GenTxId, HasTxId,
                   LedgerSupportsMempool, TxId, txForgetValidated, txId)
import           Ouroboros.Consensus.Ledger.SupportsProtocol (LedgerSupportsProtocol)
import           Ouroboros.Consensus.Mempool.API (MempoolSize (..), TraceEventMempool (..))
import           Ouroboros.Consensus.MiniProtocol.BlockFetch.Server (TraceBlockFetchServerEvent (..))
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client (TraceChainSyncClientEvent (..))
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Server (TraceChainSyncServerEvent (..))
import           Ouroboros.Consensus.MiniProtocol.LocalTxSubmission.Server
                   (TraceLocalTxSubmissionServerEvent (..))
import           Ouroboros.Consensus.Node.Run (RunNode, estimateBlockSize)
import           Ouroboros.Consensus.Node.Tracers (TraceForgeEvent (..))
import qualified Ouroboros.Consensus.Node.Tracers as Consensus
import           Ouroboros.Consensus.Protocol.Abstract
import qualified Ouroboros.Consensus.Protocol.BFT as BFT
import qualified Ouroboros.Consensus.Protocol.PBFT as PBFT
import qualified Ouroboros.Consensus.Storage.VolatileDB.Impl as VolDb

import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()

import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (BlockNo (..), ChainUpdate (..), SlotNo (..), StandardHash,
                   blockHash, pointSlot)
import           Ouroboros.Network.Point (withOrigin)

import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
-- TODO: 'TraceCacheEvent' should be exported by the 'Impl' module
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Types as ImmDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.OnDisk as LedgerDB


{- HLINT ignore "Use const" -}
{- HLINT ignore "Use record patterns" -}

instance ConvertRawHash blk => ConvertRawHash (Header blk) where
  toShortRawHash _ h = toShortRawHash (Proxy @blk) h
  fromShortRawHash _ bs = fromShortRawHash (Proxy @blk) bs
  hashSize _ = hashSize (Proxy @blk)

--
-- * instances of @HasPrivacyAnnotation@ and @HasSeverityAnnotation@
--
-- NOTE: this list is sorted by the unqualified name of the outermost type.

instance HasPrivacyAnnotation (ChainDB.TraceEvent blk)
instance HasSeverityAnnotation (ChainDB.TraceEvent blk) where
  getSeverityAnnotation (ChainDB.TraceAddBlockEvent ev) = case ev of
    ChainDB.IgnoreBlockOlderThanK {} -> Info
    ChainDB.IgnoreBlockAlreadyInVolatileDB {} -> Info
    ChainDB.IgnoreInvalidBlock {} -> Info
    ChainDB.AddedBlockToQueue {} -> Debug
    ChainDB.BlockInTheFuture {} -> Info
    ChainDB.AddedBlockToVolatileDB {} -> Debug
    ChainDB.TryAddToCurrentChain {} -> Debug
    ChainDB.TrySwitchToAFork {} -> Info
    ChainDB.StoreButDontChange {} -> Debug
    ChainDB.AddedToCurrentChain events _ _ _ ->
      maximumDef Notice (map getSeverityAnnotation events)
    ChainDB.SwitchedToAFork events _ _ _ ->
      maximumDef Notice (map getSeverityAnnotation events)
    ChainDB.AddBlockValidation ev' -> case ev' of
      ChainDB.InvalidBlock {} -> Error
      ChainDB.InvalidCandidate {} -> Error
      ChainDB.ValidCandidate {} -> Info
      ChainDB.CandidateContainsFutureBlocks{} -> Debug
      ChainDB.CandidateContainsFutureBlocksExceedingClockSkew{} -> Error
    ChainDB.ChainSelectionForFutureBlock{} -> Debug

  getSeverityAnnotation (ChainDB.TraceLedgerReplayEvent ev) = case ev of
    LedgerDB.ReplayFromGenesis {} -> Info
    LedgerDB.ReplayFromSnapshot {} -> Info
    LedgerDB.ReplayedBlock {} -> Info

  getSeverityAnnotation (ChainDB.TraceLedgerEvent ev) = case ev of
    LedgerDB.TookSnapshot {} -> Info
    LedgerDB.DeletedSnapshot {} -> Debug
    LedgerDB.InvalidSnapshot {} -> Error

  getSeverityAnnotation (ChainDB.TraceCopyToImmutableDBEvent ev) = case ev of
    ChainDB.CopiedBlockToImmutableDB {} -> Debug
    ChainDB.NoBlocksToCopyToImmutableDB -> Debug

  getSeverityAnnotation (ChainDB.TraceGCEvent ev) = case ev of
    ChainDB.PerformedGC {} -> Debug
    ChainDB.ScheduledGC {} -> Debug

  getSeverityAnnotation (ChainDB.TraceOpenEvent ev) = case ev of
    ChainDB.OpenedDB {} -> Info
    ChainDB.ClosedDB {} -> Info
    ChainDB.OpenedImmutableDB {} -> Info
    ChainDB.OpenedVolatileDB -> Info
    ChainDB.OpenedLgrDB -> Info

  getSeverityAnnotation (ChainDB.TraceFollowerEvent ev) = case ev of
    ChainDB.NewFollower {} -> Debug
    ChainDB.FollowerNoLongerInMem {} -> Debug
    ChainDB.FollowerSwitchToMem {} -> Debug
    ChainDB.FollowerNewImmIterator {} -> Debug
  getSeverityAnnotation (ChainDB.TraceInitChainSelEvent ev) = case ev of
    ChainDB.InitChainSelValidation {} -> Debug
  getSeverityAnnotation (ChainDB.TraceIteratorEvent ev) = case ev of
    ChainDB.StreamFromVolatileDB {} -> Debug
    _ -> Debug
  getSeverityAnnotation (ChainDB.TraceImmutableDBEvent _ev) = Debug
  getSeverityAnnotation (ChainDB.TraceVolatileDBEvent _ev) = Debug

instance HasSeverityAnnotation (LedgerEvent blk) where
  getSeverityAnnotation (LedgerUpdate _)  = Notice
  getSeverityAnnotation (LedgerWarning _) = Critical

instance HasPrivacyAnnotation (TraceBlockFetchServerEvent blk)
instance HasSeverityAnnotation (TraceBlockFetchServerEvent blk) where
  getSeverityAnnotation _ = Info


instance HasPrivacyAnnotation (TraceChainSyncClientEvent blk)
instance HasSeverityAnnotation (TraceChainSyncClientEvent blk) where
  getSeverityAnnotation (TraceDownloadedHeader _) = Info
  getSeverityAnnotation (TraceFoundIntersection _ _ _) = Info
  getSeverityAnnotation (TraceRolledBack _) = Notice
  getSeverityAnnotation (TraceException _) = Warning
  getSeverityAnnotation (TraceTermination _) = Notice


instance HasPrivacyAnnotation (TraceChainSyncServerEvent blk)
instance HasSeverityAnnotation (TraceChainSyncServerEvent blk) where
  getSeverityAnnotation _ = Info


instance HasPrivacyAnnotation (TraceEventMempool blk)
instance HasSeverityAnnotation (TraceEventMempool blk) where
  getSeverityAnnotation _ = Info

instance HasPrivacyAnnotation ()
instance HasSeverityAnnotation () where
  getSeverityAnnotation () = Info

instance HasPrivacyAnnotation (TraceForgeEvent blk)
instance HasSeverityAnnotation (TraceForgeEvent blk) where
  getSeverityAnnotation TraceStartLeadershipCheck {}   = Info
  getSeverityAnnotation TraceSlotIsImmutable {}        = Error
  getSeverityAnnotation TraceBlockFromFuture {}        = Error
  getSeverityAnnotation TraceBlockContext {}           = Debug
  getSeverityAnnotation TraceNoLedgerState {}          = Error
  getSeverityAnnotation TraceLedgerState {}            = Debug
  getSeverityAnnotation TraceNoLedgerView {}           = Error
  getSeverityAnnotation TraceLedgerView {}             = Debug
  getSeverityAnnotation TraceForgeStateUpdateError {}  = Error
  getSeverityAnnotation TraceNodeCannotForge {}        = Error
  getSeverityAnnotation TraceNodeNotLeader {}          = Info
  getSeverityAnnotation TraceNodeIsLeader {}           = Info
  getSeverityAnnotation TraceForgedBlock {}            = Info
  getSeverityAnnotation TraceDidntAdoptBlock {}        = Error
  getSeverityAnnotation TraceForgedInvalidBlock {}     = Error
  getSeverityAnnotation TraceAdoptedBlock {}           = Info


instance HasPrivacyAnnotation (TraceLocalTxSubmissionServerEvent blk)
instance HasSeverityAnnotation (TraceLocalTxSubmissionServerEvent blk) where
  getSeverityAnnotation _ = Info


--
-- | instances of @Transformable@
--
-- NOTE: this list is sorted by the unqualified name of the outermost type.

instance ( HasPrivacyAnnotation (ChainDB.TraceAddBlockEvent blk)
         , HasSeverityAnnotation (ChainDB.TraceAddBlockEvent blk)
         , LedgerSupportsProtocol blk
         , ToObject (ChainDB.TraceAddBlockEvent blk))
      => Transformable Text IO (ChainDB.TraceAddBlockEvent blk) where
  trTransformer = trStructuredText


instance (LedgerSupportsProtocol blk)
      => HasTextFormatter (ChainDB.TraceAddBlockEvent blk) where
  formatText _ = pack . show . toList


instance ConvertRawHash blk
      => Transformable Text IO (TraceBlockFetchServerEvent blk) where
  trTransformer = trStructuredText


instance HasTextFormatter (TraceBlockFetchServerEvent blk) where
  formatText _ = pack . show . toList


instance (ConvertRawHash blk, LedgerSupportsProtocol blk)
      => Transformable Text IO (TraceChainSyncClientEvent blk) where
  trTransformer = trStructured


instance ConvertRawHash blk
      => Transformable Text IO (TraceChainSyncServerEvent blk) where
  trTransformer = trStructured


instance ( ToObject (ApplyTxErr blk), Show (ApplyTxErr blk), ToObject (GenTx blk),
           ToJSON (GenTxId blk), LedgerSupportsMempool blk)
      => Transformable Text IO (TraceEventMempool blk) where
  trTransformer = trStructured


condenseT :: Condense a => a -> Text
condenseT = pack . condense

showT :: Show a => a -> Text
showT = pack . show


instance ( tx ~ GenTx blk
         , HasTxId tx
         , RunNode blk
         , Show (TxId tx)
         , ToObject (LedgerError blk)
         , ToObject (OtherHeaderEnvelopeError blk)
         , ToObject (ValidationErr (BlockProtocol blk))
         , ToObject (CannotForge blk)
         , ToObject (ForgeStateUpdateError blk)
         , LedgerSupportsMempool blk)
      => Transformable Text IO (TraceForgeEvent blk) where
  trTransformer = trStructuredText

instance ( tx ~ GenTx blk
         , ConvertRawHash blk
         , HasTxId tx
         , LedgerSupportsMempool blk
         , LedgerSupportsProtocol blk
         , LedgerSupportsMempool blk
         , Show (TxId tx)
         , Show (ForgeStateUpdateError blk)
         , Show (CannotForge blk)
         , LedgerSupportsMempool blk)
      => HasTextFormatter (TraceForgeEvent blk) where
  formatText = \case
    TraceStartLeadershipCheck slotNo -> const $
      "Checking for leadership in slot " <> showT (unSlotNo slotNo)
    TraceSlotIsImmutable slotNo immutableTipPoint immutableTipBlkNo -> const $
      "Couldn't forge block because current slot is immutable: "
        <> "immutable tip: " <> renderPointAsPhrase immutableTipPoint
        <> ", immutable tip block no: " <> showT (unBlockNo immutableTipBlkNo)
        <> ", current slot: " <> showT (unSlotNo slotNo)
    TraceBlockFromFuture currentSlot tipSlot -> const $
      "Couldn't forge block because current tip is in the future: "
        <> "current tip slot: " <> showT (unSlotNo tipSlot)
        <> ", current slot: " <> showT (unSlotNo currentSlot)
    TraceBlockContext currentSlot tipBlockNo tipPoint -> const $
      "New block will fit onto: "
        <> "tip: " <> renderPointAsPhrase tipPoint
        <> ", tip block no: " <> showT (unBlockNo tipBlockNo)
        <> ", current slot: " <> showT (unSlotNo currentSlot)
    TraceNoLedgerState slotNo pt -> const $
      "Could not obtain ledger state for point "
        <> renderPointAsPhrase pt
        <> ", current slot: "
        <> showT (unSlotNo slotNo)
    TraceLedgerState slotNo pt -> const $
      "Obtained a ledger state for point "
        <> renderPointAsPhrase pt
        <> ", current slot: "
        <> showT (unSlotNo slotNo)
    TraceNoLedgerView slotNo _ -> const $
      "Could not obtain ledger view for slot " <> showT (unSlotNo slotNo)
    TraceLedgerView slotNo -> const $
      "Obtained a ledger view for slot " <> showT (unSlotNo slotNo)
    TraceForgeStateUpdateError slotNo reason -> const $
      "Updating the forge state in slot "
        <> showT (unSlotNo slotNo)
        <> " failed because: "
        <> showT reason
    TraceNodeCannotForge slotNo reason -> const $
      "We are the leader in slot "
        <> showT (unSlotNo slotNo)
        <> ", but we cannot forge because: "
        <> showT reason
    TraceNodeNotLeader slotNo -> const $
      "Not leading slot " <> showT (unSlotNo slotNo)
    TraceNodeIsLeader slotNo -> const $
      "Leading slot " <> showT (unSlotNo slotNo)
    TraceForgedBlock slotNo _ _ _ -> const $
      "Forged block in slot " <> showT (unSlotNo slotNo)
    TraceDidntAdoptBlock slotNo _ -> const $
      "Didn't adopt forged block in slot " <> showT (unSlotNo slotNo)
    TraceForgedInvalidBlock slotNo _ reason -> const $
      "Forged invalid block in slot "
        <> showT (unSlotNo slotNo)
        <> ", reason: " <> showT reason
    TraceAdoptedBlock slotNo blk txs -> const $
      "Adopted block forged in slot "
        <> showT (unSlotNo slotNo)
        <> ": " <> renderHeaderHash (Proxy @blk) (blockHash blk)
        <> ", TxIds: " <> showT (map (txId . txForgetValidated) txs)


instance Transformable Text IO (TraceLocalTxSubmissionServerEvent blk) where
  trTransformer = trStructured

instance HasPrivacyAnnotation a => HasPrivacyAnnotation (Consensus.TraceLabelCreds a)
instance HasSeverityAnnotation a => HasSeverityAnnotation (Consensus.TraceLabelCreds a) where
  getSeverityAnnotation (Consensus.TraceLabelCreds _ a) = getSeverityAnnotation a

instance ToObject a => ToObject (Consensus.TraceLabelCreds a) where
  toObject verb (Consensus.TraceLabelCreds creds val) =
    mkObject [ "credentials" .= toJSON creds
             , "val"         .= toObject verb val
             ]

instance (HasPrivacyAnnotation a, HasSeverityAnnotation a, ToObject a)
      => Transformable Text IO (Consensus.TraceLabelCreds a) where
  trTransformer = trStructured

instance ( ConvertRawHash blk
         , LedgerSupportsProtocol blk
         , InspectLedger blk
         , ToObject (Header blk)
         , ToObject (LedgerEvent blk))
      => Transformable Text IO (ChainDB.TraceEvent blk) where
  trTransformer = trStructuredText

instance ( ConvertRawHash blk
         , LedgerSupportsProtocol blk
         , InspectLedger blk)
      => HasTextFormatter (ChainDB.TraceEvent blk) where
    formatText tev _obj = case tev of
      ChainDB.TraceAddBlockEvent ev -> case ev of
        ChainDB.IgnoreBlockOlderThanK pt ->
          "Ignoring block older than K: " <> renderRealPointAsPhrase pt
        ChainDB.IgnoreBlockAlreadyInVolatileDB pt ->
          "Ignoring block already in DB: " <> renderRealPointAsPhrase pt
        ChainDB.IgnoreInvalidBlock pt _reason ->
          "Ignoring previously seen invalid block: " <> renderRealPointAsPhrase pt
        ChainDB.AddedBlockToQueue pt sz ->
          "Block added to queue: " <> renderRealPointAsPhrase pt <> " queue size " <> condenseT sz
        ChainDB.BlockInTheFuture pt slot ->
          "Ignoring block from future: " <> renderRealPointAsPhrase pt <> ", slot " <> condenseT slot
        ChainDB.StoreButDontChange pt ->
          "Ignoring block: " <> renderRealPointAsPhrase pt
        ChainDB.TryAddToCurrentChain pt ->
          "Block fits onto the current chain: " <> renderRealPointAsPhrase pt
        ChainDB.TrySwitchToAFork pt _ ->
          "Block fits onto some fork: " <> renderRealPointAsPhrase pt
        ChainDB.AddedToCurrentChain es _ _ c ->
          "Chain extended, new tip: " <> renderPointAsPhrase (AF.headPoint c) <>
          Text.concat [ "\nEvent: " <> showT e | e <- es ]
        ChainDB.SwitchedToAFork es _ _ c ->
          "Switched to a fork, new tip: " <> renderPointAsPhrase (AF.headPoint c) <>
          Text.concat [ "\nEvent: " <> showT e | e <- es ]
        ChainDB.AddBlockValidation ev' -> case ev' of
          ChainDB.InvalidBlock err pt ->
            "Invalid block " <> renderRealPointAsPhrase pt <> ": " <> showT err
          ChainDB.InvalidCandidate c ->
            "Invalid candidate " <> renderPointAsPhrase (AF.headPoint c)
          ChainDB.ValidCandidate c ->
            "Valid candidate " <> renderPointAsPhrase (AF.headPoint c)
          ChainDB.CandidateContainsFutureBlocks c hdrs ->
            "Candidate contains blocks from near future:  " <>
            renderPointAsPhrase (AF.headPoint c) <> ", slots " <>
            Text.intercalate ", " (map (renderPoint . headerPoint) hdrs)
          ChainDB.CandidateContainsFutureBlocksExceedingClockSkew c hdrs ->
            "Candidate contains blocks from future exceeding clock skew limit: " <>
            renderPointAsPhrase (AF.headPoint c) <> ", slots " <>
            Text.intercalate ", " (map (renderPoint . headerPoint) hdrs)
        ChainDB.AddedBlockToVolatileDB pt _ _ ->
          "Chain added block " <> renderRealPointAsPhrase pt
        ChainDB.ChainSelectionForFutureBlock pt ->
          "Chain selection run for block previously from future: " <> renderRealPointAsPhrase pt
      ChainDB.TraceLedgerReplayEvent ev -> case ev of
        LedgerDB.ReplayFromGenesis _replayTo ->
          "Replaying ledger from genesis"
        LedgerDB.ReplayFromSnapshot snap tip' _replayTo ->
          "Replaying ledger from snapshot " <> showT snap <> " at " <>
            renderRealPointAsPhrase tip'
        LedgerDB.ReplayedBlock pt _ledgerEvents replayTo ->
          "Replayed block: slot " <> showT (realPointSlot pt) <> " of " <> showT (pointSlot replayTo)
      ChainDB.TraceLedgerEvent ev -> case ev of
        LedgerDB.TookSnapshot snap pt ->
          "Took ledger snapshot " <> showT snap <>
          " at " <> renderRealPointAsPhrase pt
        LedgerDB.DeletedSnapshot snap ->
          "Deleted old snapshot " <> showT snap
        LedgerDB.InvalidSnapshot snap failure ->
          "Invalid snapshot " <> showT snap <> showT failure
      ChainDB.TraceCopyToImmutableDBEvent ev -> case ev of
        ChainDB.CopiedBlockToImmutableDB pt ->
          "Copied block " <> renderPointAsPhrase pt <> " to the ImmutableDB"
        ChainDB.NoBlocksToCopyToImmutableDB ->
          "There are no blocks to copy to the ImmutableDB"
      ChainDB.TraceGCEvent ev -> case ev of
        ChainDB.PerformedGC slot ->
          "Performed a garbage collection for " <> condenseT slot
        ChainDB.ScheduledGC slot _difft ->
          "Scheduled a garbage collection for " <> condenseT slot
      ChainDB.TraceOpenEvent ev -> case ev of
        ChainDB.OpenedDB immTip tip' ->
          "Opened db with immutable tip at " <> renderPointAsPhrase immTip <>
          " and tip " <> renderPointAsPhrase tip'
        ChainDB.ClosedDB immTip tip' ->
          "Closed db with immutable tip at " <> renderPointAsPhrase immTip <>
          " and tip " <> renderPointAsPhrase tip'
        ChainDB.OpenedImmutableDB immTip chunk ->
          "Opened imm db with immutable tip at " <> renderPointAsPhrase immTip <>
          " and chunk " <> showT chunk
        ChainDB.OpenedVolatileDB ->  "Opened vol db"
        ChainDB.OpenedLgrDB ->  "Opened lgr db"
      ChainDB.TraceFollowerEvent ev -> case ev of
        ChainDB.NewFollower ->  "New follower was created"
        ChainDB.FollowerNoLongerInMem _ ->  "FollowerNoLongerInMem"
        ChainDB.FollowerSwitchToMem _ _ ->  "FollowerSwitchToMem"
        ChainDB.FollowerNewImmIterator _ _ ->  "FollowerNewImmIterator"
      ChainDB.TraceInitChainSelEvent ev -> case ev of
        ChainDB.InitChainSelValidation _ ->  "InitChainSelValidation"
      ChainDB.TraceIteratorEvent ev -> case ev of
        ChainDB.UnknownRangeRequested ev' ->
          case ev' of
            ChainDB.MissingBlock realPt ->
              "The block at the given point was not found in the ChainDB."
              <> renderRealPoint realPt
            ChainDB.ForkTooOld streamFrom ->
              "The requested range forks off too far in the past"
              <> showT streamFrom
        ChainDB.BlockMissingFromVolatileDB realPt ->
          "This block is no longer in the VolatileDB because it has been garbage\
           \ collected. It might now be in the ImmutableDB if it was part of the\
           \ current chain. Block: " <> renderRealPoint realPt
        ChainDB.StreamFromImmutableDB sFrom sTo ->
          "Stream only from the ImmutableDB. StreamFrom:" <> showT sFrom <>
          " StreamTo: " <> showT sTo
        ChainDB.StreamFromBoth sFrom sTo pts ->
          "Stream from both the VolatileDB and the ImmutableDB."
          <> " StreamFrom: " <> showT sFrom <> " StreamTo: " <> showT sTo
          <> " Points: " <> showT (map renderRealPoint pts)
        ChainDB.StreamFromVolatileDB sFrom sTo pts ->
          "Stream only from the VolatileDB."
          <> " StreamFrom: " <> showT sFrom <> " StreamTo: " <> showT sTo
          <> " Points: " <> showT (map renderRealPoint pts)
        ChainDB.BlockWasCopiedToImmutableDB pt ->
          "This block has been garbage collected from the VolatileDB is now\
          \ found and streamed from the ImmutableDB. Block: " <> renderRealPoint pt
        ChainDB.BlockGCedFromVolatileDB pt ->
          "This block no longer in the VolatileDB and isn't in the ImmutableDB\
          \ either; it wasn't part of the current chain. Block: " <> renderRealPoint pt
        ChainDB.SwitchBackToVolatileDB ->  "SwitchBackToVolatileDB"
      ChainDB.TraceImmutableDBEvent _ev ->  "TraceImmutableDBEvent"
      ChainDB.TraceVolatileDBEvent _ev ->  "TraceVolatileDBEvent"


--
-- | instances of @ToObject@
--
-- NOTE: this list is sorted by the unqualified name of the outermost type.

instance ToObject BFT.BftValidationErr where
  toObject _verb (BFT.BftInvalidSignature err) =
    mkObject
      [ "kind" .= String "BftInvalidSignature"
      , "error" .= String (pack err)
      ]


instance ToObject LedgerDB.DiskSnapshot where
  toObject MinimalVerbosity snap = toObject NormalVerbosity snap
  toObject NormalVerbosity _ = mkObject [ "kind" .= String "snapshot" ]
  toObject MaximalVerbosity snap =
    mkObject [ "kind" .= String "snapshot"
             , "snapshot" .= String (pack $ show snap) ]


instance ( StandardHash blk
         , ToObject (LedgerError blk)
         , ToObject (OtherHeaderEnvelopeError blk)
         , ToObject (ValidationErr (BlockProtocol blk)))
      => ToObject (ExtValidationError blk) where
  toObject verb (ExtValidationErrorLedger err) = toObject verb err
  toObject verb (ExtValidationErrorHeader err) = toObject verb err


instance ( StandardHash blk
         , ToObject (OtherHeaderEnvelopeError blk)
         )
      => ToObject (HeaderEnvelopeError blk) where
  toObject _verb (UnexpectedBlockNo expect act) =
    mkObject
      [ "kind" .= String "UnexpectedBlockNo"
      , "expected" .= condense expect
      , "actual" .= condense act
      ]
  toObject _verb (UnexpectedSlotNo expect act) =
    mkObject
      [ "kind" .= String "UnexpectedSlotNo"
      , "expected" .= condense expect
      , "actual" .= condense act
      ]
  toObject _verb (UnexpectedPrevHash expect act) =
    mkObject
      [ "kind" .= String "UnexpectedPrevHash"
      , "expected" .= String (pack $ show expect)
      , "actual" .= String (pack $ show act)
      ]
  toObject verb (OtherHeaderEnvelopeError err) =
    toObject verb err


instance ( StandardHash blk
         , ToObject (ValidationErr (BlockProtocol blk))
         , ToObject (OtherHeaderEnvelopeError blk)
         )
      => ToObject (HeaderError blk) where
  toObject verb (HeaderProtocolError err) =
    mkObject
      [ "kind" .= String "HeaderProtocolError"
      , "error" .= toObject verb err
      ]
  toObject verb (HeaderEnvelopeError err) =
    mkObject
      [ "kind" .= String "HeaderEnvelopeError"
      , "error" .= toObject verb err
      ]


instance ( ConvertRawHash blk
         , StandardHash blk
         , ToObject (LedgerError blk)
         , ToObject (OtherHeaderEnvelopeError blk)
         , ToObject (ValidationErr (BlockProtocol blk)))
      => ToObject (ChainDB.InvalidBlockReason blk) where
  toObject verb (ChainDB.ValidationError extvalerr) =
    mkObject
      [ "kind" .= String "ValidationError"
      , "error" .= toObject verb extvalerr
      ]
  toObject verb (ChainDB.InFutureExceedsClockSkew point) =
    mkObject
      [ "kind" .= String "InFutureExceedsClockSkew"
      , "point" .= toObject verb point
      ]


instance (Show (PBFT.PBftVerKeyHash c))
      => ToObject (PBFT.PBftValidationErr c) where
  toObject _verb (PBFT.PBftInvalidSignature text) =
    mkObject
      [ "kind" .= String "PBftInvalidSignature"
      , "error" .= String text
      ]
  toObject _verb (PBFT.PBftNotGenesisDelegate vkhash _ledgerView) =
    mkObject
      [ "kind" .= String "PBftNotGenesisDelegate"
      , "vk" .= String (pack $ show vkhash)
      ]
  toObject _verb (PBFT.PBftExceededSignThreshold vkhash numForged) =
    mkObject
      [ "kind" .= String "PBftExceededSignThreshold"
      , "vk" .= String (pack $ show vkhash)
      , "numForged" .= String (pack (show numForged))
      ]
  toObject _verb PBFT.PBftInvalidSlot =
    mkObject
      [ "kind" .= String "PBftInvalidSlot"
      ]


instance (Show (PBFT.PBftVerKeyHash c))
      => ToObject (PBFT.PBftCannotForge c) where
  toObject _verb (PBFT.PBftCannotForgeInvalidDelegation vkhash) =
    mkObject
      [ "kind" .= String "PBftCannotForgeInvalidDelegation"
      , "vk" .= String (pack $ show vkhash)
      ]
  toObject _verb (PBFT.PBftCannotForgeThresholdExceeded numForged) =
    mkObject
      [ "kind" .= String "PBftCannotForgeThresholdExceeded"
      , "numForged" .= numForged
      ]


instance ConvertRawHash blk
      => ToObject (RealPoint blk) where
  toObject verb p = mkObject
        [ "kind" .= String "Point"
        , "slot" .= unSlotNo (realPointSlot p)
        , "hash" .= renderHeaderHashForVerbosity (Proxy @blk) verb (realPointHash p) ]


instance (ToObject (LedgerUpdate blk), ToObject (LedgerWarning blk))
      => ToObject (LedgerEvent blk) where
  toObject verb = \case
    LedgerUpdate  update  -> toObject verb update
    LedgerWarning warning -> toObject verb warning


instance ( ConvertRawHash blk
         , LedgerSupportsProtocol blk
         , ToObject (Header blk)
         , ToObject (LedgerEvent blk))
      => ToObject (ChainDB.TraceEvent blk) where
  toObject verb (ChainDB.TraceAddBlockEvent ev) = case ev of
    ChainDB.IgnoreBlockOlderThanK pt ->
      mkObject [ "kind" .= String "TraceAddBlockEvent.IgnoreBlockOlderThanK"
               , "block" .= toObject verb pt ]
    ChainDB.IgnoreBlockAlreadyInVolatileDB pt ->
      mkObject [ "kind" .= String "TraceAddBlockEvent.IgnoreBlockAlreadyInVolatileDB"
               , "block" .= toObject verb pt ]
    ChainDB.IgnoreInvalidBlock pt reason ->
      mkObject [ "kind" .= String "TraceAddBlockEvent.IgnoreInvalidBlock"
               , "block" .= toObject verb pt
               , "reason" .= show reason ]
    ChainDB.AddedBlockToQueue pt sz ->
      mkObject [ "kind" .= String "TraceAddBlockEvent.AddedBlockToQueue"
               , "block" .= toObject verb pt
               , "queueSize" .= toJSON sz ]
    ChainDB.BlockInTheFuture pt slot ->
      mkObject [ "kind" .= String "TraceAddBlockEvent.BlockInTheFuture"
               , "block" .= toObject verb pt
               , "slot" .= toObject verb slot ]
    ChainDB.StoreButDontChange pt ->
      mkObject [ "kind" .= String "TraceAddBlockEvent.StoreButDontChange"
               , "block" .= toObject verb pt ]
    ChainDB.TryAddToCurrentChain pt ->
      mkObject [ "kind" .= String "TraceAddBlockEvent.TryAddToCurrentChain"
               , "block" .= toObject verb pt ]
    ChainDB.TrySwitchToAFork pt _ ->
      mkObject [ "kind" .= String "TraceAddBlockEvent.TrySwitchToAFork"
               , "block" .= toObject verb pt ]
    ChainDB.AddedToCurrentChain events _ base extended ->
      mkObject $
               [ "kind" .= String "TraceAddBlockEvent.AddedToCurrentChain"
               , "newtip" .= renderPointForVerbosity verb (AF.headPoint extended)
               , "chainLengthDelta" .= extended `chainLengthΔ` base
               ]
            ++ [ "headers" .= toJSON (toObject verb `map` addedHdrsNewChain base extended)
               | verb == MaximalVerbosity ]
            ++ [ "events" .= toJSON (map (toObject verb) events)
               | not (null events) ]
    ChainDB.SwitchedToAFork events _ old new ->
      mkObject $
               [ "kind" .= String "TraceAddBlockEvent.SwitchedToAFork"
               , "newtip" .= renderPointForVerbosity verb (AF.headPoint new)
               , "chainLengthDelta" .= new `chainLengthΔ` old
               ]
            ++ [ "headers" .= toJSON (toObject verb `map` addedHdrsNewChain old new)
               | verb == MaximalVerbosity ]
            ++ [ "events" .= toJSON (map (toObject verb) events)
               | not (null events) ]
    ChainDB.AddBlockValidation ev' -> case ev' of
      ChainDB.InvalidBlock err pt ->
        mkObject [ "kind" .= String "TraceAddBlockEvent.AddBlockValidation.InvalidBlock"
                 , "block" .= toObject verb pt
                 , "error" .= show err ]
      ChainDB.InvalidCandidate c ->
        mkObject [ "kind" .= String "TraceAddBlockEvent.AddBlockValidation.InvalidCandidate"
                 , "block" .= renderPointForVerbosity verb (AF.headPoint c) ]
      ChainDB.ValidCandidate c ->
        mkObject [ "kind" .= String "TraceAddBlockEvent.AddBlockValidation.ValidCandidate"
                 , "block" .= renderPointForVerbosity verb (AF.headPoint c) ]
      ChainDB.CandidateContainsFutureBlocks c hdrs ->
        mkObject [ "kind" .= String "TraceAddBlockEvent.AddBlockValidation.CandidateContainsFutureBlocks"
                 , "block"   .= renderPointForVerbosity verb (AF.headPoint c)
                 , "headers" .= map (renderPointForVerbosity verb . headerPoint) hdrs ]
      ChainDB.CandidateContainsFutureBlocksExceedingClockSkew c hdrs ->
        mkObject [ "kind" .= String "TraceAddBlockEvent.AddBlockValidation.CandidateContainsFutureBlocksExceedingClockSkew"
                 , "block"   .= renderPointForVerbosity verb (AF.headPoint c)
                 , "headers" .= map (renderPointForVerbosity verb . headerPoint) hdrs ]
    ChainDB.AddedBlockToVolatileDB pt (BlockNo bn) _ ->
      mkObject [ "kind" .= String "TraceAddBlockEvent.AddedBlockToVolatileDB"
               , "block" .= toObject verb pt
               , "blockNo" .= show bn ]
    ChainDB.ChainSelectionForFutureBlock pt ->
      mkObject [ "kind" .= String "TraceAddBlockEvent.ChainSelectionForFutureBlock"
               , "block" .= toObject verb pt ]
   where
     addedHdrsNewChain
       :: AF.AnchoredFragment (Header blk)
       -> AF.AnchoredFragment (Header blk)
       -> [Header blk]
     addedHdrsNewChain fro to_ =
       case AF.intersect fro to_ of
         Just (_, _, _, s2 :: AF.AnchoredFragment (Header blk)) ->
           AF.toOldestFirst s2
         Nothing -> [] -- No sense to do validation here.
     chainLengthΔ :: AF.AnchoredFragment (Header blk) -> AF.AnchoredFragment (Header blk) -> Int
     chainLengthΔ = on (-) (fromWithOrigin (-1) . fmap (fromIntegral . unBlockNo) . AF.headBlockNo)
  toObject MinimalVerbosity (ChainDB.TraceLedgerReplayEvent _ev) = emptyObject -- no output
  toObject verb (ChainDB.TraceLedgerReplayEvent ev) = case ev of
    LedgerDB.ReplayFromGenesis _replayTo ->
      mkObject [ "kind" .= String "TraceLedgerReplayEvent.ReplayFromGenesis" ]
    LedgerDB.ReplayFromSnapshot snap tip' _replayTo ->
      mkObject [ "kind" .= String "TraceLedgerReplayEvent.ReplayFromSnapshot"
               , "snapshot" .= toObject verb snap
               , "tip" .= show tip' ]
    LedgerDB.ReplayedBlock pt _ledgerEvents replayTo ->
      mkObject [ "kind" .= String "TraceLedgerReplayEvent.ReplayedBlock"
               , "slot" .= unSlotNo (realPointSlot pt)
               , "tip"  .= withOrigin 0 unSlotNo (pointSlot replayTo) ]

  toObject MinimalVerbosity (ChainDB.TraceLedgerEvent _ev) = emptyObject -- no output
  toObject verb (ChainDB.TraceLedgerEvent ev) = case ev of
    LedgerDB.TookSnapshot snap pt ->
      mkObject [ "kind" .= String "TraceLedgerEvent.TookSnapshot"
               , "snapshot" .= toObject verb snap
               , "tip" .= show pt ]
    LedgerDB.DeletedSnapshot snap ->
      mkObject [ "kind" .= String "TraceLedgerEvent.DeletedSnapshot"
               , "snapshot" .= toObject verb snap ]
    LedgerDB.InvalidSnapshot snap failure ->
      mkObject [ "kind" .= String "TraceLedgerEvent.InvalidSnapshot"
               , "snapshot" .= toObject verb snap
               , "failure" .= show failure ]

  toObject verb (ChainDB.TraceCopyToImmutableDBEvent ev) = case ev of
    ChainDB.CopiedBlockToImmutableDB pt ->
      mkObject [ "kind" .= String "TraceCopyToImmutableDBEvent.CopiedBlockToImmutableDB"
               , "slot" .= toObject verb pt ]
    ChainDB.NoBlocksToCopyToImmutableDB ->
      mkObject [ "kind" .= String "TraceCopyToImmutableDBEvent.NoBlocksToCopyToImmutableDB" ]

  toObject verb (ChainDB.TraceGCEvent ev) = case ev of
    ChainDB.PerformedGC slot ->
      mkObject [ "kind" .= String "TraceGCEvent.PerformedGC"
               , "slot" .= toObject verb slot ]
    ChainDB.ScheduledGC slot difft ->
      mkObject $ [ "kind" .= String "TraceGCEvent.ScheduledGC"
                 , "slot" .= toObject verb slot ] <>
                 [ "difft" .= String ((pack . show) difft) | verb >= MaximalVerbosity]

  toObject verb (ChainDB.TraceOpenEvent ev) = case ev of
    ChainDB.OpenedDB immTip tip' ->
      mkObject [ "kind" .= String "TraceOpenEvent.OpenedDB"
               , "immtip" .= toObject verb immTip
               , "tip" .= toObject verb tip' ]
    ChainDB.ClosedDB immTip tip' ->
      mkObject [ "kind" .= String "TraceOpenEvent.ClosedDB"
               , "immtip" .= toObject verb immTip
               , "tip" .= toObject verb tip' ]
    ChainDB.OpenedImmutableDB immTip epoch ->
      mkObject [ "kind" .= String "TraceOpenEvent.OpenedImmutableDB"
               , "immtip" .= toObject verb immTip
               , "epoch" .= String ((pack . show) epoch) ]
    ChainDB.OpenedVolatileDB ->
      mkObject [ "kind" .= String "TraceOpenEvent.OpenedVolatileDB" ]
    ChainDB.OpenedLgrDB ->
      mkObject [ "kind" .= String "TraceOpenEvent.OpenedLgrDB" ]

  toObject _verb (ChainDB.TraceFollowerEvent ev) = case ev of
    ChainDB.NewFollower ->
      mkObject [ "kind" .= String "TraceFollowerEvent.NewFollower" ]
    ChainDB.FollowerNoLongerInMem _ ->
      mkObject [ "kind" .= String "TraceFollowerEvent.FollowerNoLongerInMem" ]
    ChainDB.FollowerSwitchToMem _ _ ->
      mkObject [ "kind" .= String "TraceFollowerEvent.FollowerSwitchToMem" ]
    ChainDB.FollowerNewImmIterator _ _ ->
      mkObject [ "kind" .= String "TraceFollowerEvent.FollowerNewImmIterator" ]
  toObject verb (ChainDB.TraceInitChainSelEvent ev) = case ev of
    ChainDB.InitChainSelValidation ev' -> case ev' of
      ChainDB.InvalidBlock err pt ->
         mkObject [ "kind" .= String "TraceInitChainSelEvent.InvalidBlock"
                  , "block" .= toObject verb pt
                  , "error" .= show err ]
      ChainDB.InvalidCandidate c ->
        mkObject [ "kind" .= String "TraceInitChainSelEvent.InvalidCandidate"
                 , "block" .= renderPointForVerbosity verb (AF.headPoint c) ]
      ChainDB.ValidCandidate c ->
        mkObject [ "kind" .= String "TraceInitChainSelEvent.ValidCandidate"
                 , "block" .= renderPointForVerbosity verb (AF.headPoint c) ]
      ChainDB.CandidateContainsFutureBlocks c hdrs ->
        mkObject [ "kind" .= String "TraceInitChainSelEvent.CandidateContainsFutureBlocks"
                 , "block"   .= renderPointForVerbosity verb (AF.headPoint c)
                 , "headers" .= map (renderPointForVerbosity verb . headerPoint) hdrs ]
      ChainDB.CandidateContainsFutureBlocksExceedingClockSkew c hdrs ->
        mkObject [ "kind" .= String "TraceInitChainSelEvent.CandidateContainsFutureBlocksExceedingClockSkew"
                 , "block"   .= renderPointForVerbosity verb (AF.headPoint c)
                 , "headers" .= map (renderPointForVerbosity verb . headerPoint) hdrs ]
  toObject _verb (ChainDB.TraceIteratorEvent ev) = case ev of
    ChainDB.UnknownRangeRequested unkRange ->
      mkObject [ "kind" .= String "TraceIteratorEvent.UnknownRangeRequested"
               , "range" .= String (showT unkRange)
               ]
    ChainDB.StreamFromVolatileDB streamFrom streamTo realPt ->
      mkObject [ "kind" .= String "TraceIteratorEvent.StreamFromVolatileDB"
               , "from" .= String (showT streamFrom)
               , "to" .= String (showT streamTo)
               , "point" .= String (Text.pack . show $ map renderRealPoint realPt)
               ]
    ChainDB.StreamFromImmutableDB streamFrom streamTo ->
      mkObject [ "kind" .= String "TraceIteratorEvent.StreamFromImmutableDB"
               , "from" .= String (showT streamFrom)
               , "to" .= String (showT streamTo)
               ]
    ChainDB.StreamFromBoth streamFrom streamTo realPt ->
      mkObject [ "kind" .= String "TraceIteratorEvent.StreamFromBoth"
               , "from" .= String (showT streamFrom)
               , "to" .= String (showT streamTo)
               , "point" .= String (Text.pack . show $ map renderRealPoint realPt)
               ]
    ChainDB.BlockMissingFromVolatileDB realPt ->
      mkObject [ "kind" .= String "TraceIteratorEvent.BlockMissingFromVolatileDB"
               , "point" .= String (renderRealPoint realPt)
               ]
    ChainDB.BlockWasCopiedToImmutableDB realPt ->
      mkObject [ "kind" .= String "TraceIteratorEvent.BlockWasCopiedToImmutableDB"
               , "point" .= String (renderRealPoint realPt)
               ]
    ChainDB.BlockGCedFromVolatileDB realPt ->
      mkObject [ "kind" .= String "TraceIteratorEvent.BlockGCedFromVolatileDB"
               , "point" .= String (renderRealPoint realPt)
               ]
    ChainDB.SwitchBackToVolatileDB ->
      mkObject ["kind" .= String "TraceIteratorEvent.SwitchBackToVolatileDB"
               ]
  toObject verb (ChainDB.TraceImmutableDBEvent ev) = case ev of
    ImmDB.NoValidLastLocation -> mkObject [ "kind" .= String "TraceImmutableDBEvent.NoValidLastLocation" ]
    ImmDB.ValidatedLastLocation chunkNo immTip ->
      mkObject [ "kind" .= String "TraceImmutableDBEvent.ValidatedLastLocation"
               , "chunkNo" .= String (renderChunkNo chunkNo)
               , "immTip" .= String (renderTipHash immTip)
               , "blockNo" .= String (renderTipBlockNo immTip)
               ]
    ImmDB.ValidatingChunk chunkNo ->
      mkObject [ "kind" .= String "TraceImmutableDBEvent.ValidatingChunk"
               , "chunkNo" .= String (renderChunkNo chunkNo)
               ]
    ImmDB.MissingChunkFile chunkNo ->
      mkObject [ "kind" .= String "TraceImmutableDBEvent.MissingChunkFile"
               , "chunkNo" .= String (renderChunkNo chunkNo)
               ]
    ImmDB.InvalidChunkFile chunkNo (ImmDB.ChunkErrRead readIncErr) ->
      mkObject [ "kind" .= String "TraceImmutableDBEvent.InvalidChunkFile.ChunkErrRead"
               , "chunkNo" .= String (renderChunkNo chunkNo)
               , "error" .= String (showT readIncErr)
               ]
    ImmDB.InvalidChunkFile chunkNo (ImmDB.ChunkErrHashMismatch hashPrevBlock prevHashOfBlock) ->
      mkObject [ "kind" .= String "TraceImmutableDBEvent.InvalidChunkFile.ChunkErrHashMismatch"
               , "chunkNo" .= String (renderChunkNo chunkNo)
               , "hashPrevBlock" .= String (Text.decodeLatin1 . toRawHash (Proxy @blk) $ hashPrevBlock)
               , "prevHashOfBlock" .= String (renderChainHash (Text.decodeLatin1 . toRawHash (Proxy @blk)) prevHashOfBlock)
               ]
    ImmDB.InvalidChunkFile chunkNo (ImmDB.ChunkErrCorrupt pt) ->
      mkObject [ "kind" .= String "TraceImmutableDBEvent.InvalidChunkFile.ChunkErrCorrupt"
               , "chunkNo" .= String (renderChunkNo chunkNo)
               , "block" .= String (renderPointForVerbosity verb pt)
               ]
    ImmDB.ChunkFileDoesntFit expectPrevHash actualPrevHash ->
      mkObject [ "kind" .= String "TraceImmutableDBEvent.ChunkFileDoesntFit"
               , "expectedPrevHash" .= String (renderChainHash (Text.decodeLatin1 . toRawHash (Proxy @blk)) expectPrevHash)
               , "actualPrevHash" .= String (renderChainHash (Text.decodeLatin1 . toRawHash (Proxy @blk)) actualPrevHash)
               ]
    ImmDB.MissingPrimaryIndex chunkNo ->
      mkObject [ "kind" .= String "TraceImmutableDBEvent.MissingPrimaryIndex"
               , "chunkNo" .= String (renderChunkNo chunkNo)
               ]
    ImmDB.MissingSecondaryIndex chunkNo ->
      mkObject [ "kind" .= String "TraceImmutableDBEvent.MissingSecondaryIndex"
               , "chunkNo" .= String (renderChunkNo chunkNo)
               ]
    ImmDB.InvalidPrimaryIndex chunkNo ->
      mkObject [ "kind" .= String "TraceImmutableDBEvent.InvalidPrimaryIndex"
               , "chunkNo" .= String (renderChunkNo chunkNo)
               ]
    ImmDB.InvalidSecondaryIndex chunkNo ->
      mkObject [ "kind" .= String "TraceImmutableDBEvent.InvalidSecondaryIndex"
               , "chunkNo" .= String (renderChunkNo chunkNo)
               ]
    ImmDB.RewritePrimaryIndex chunkNo ->
      mkObject [ "kind" .= String "TraceImmutableDBEvent.RewritePrimaryIndex"
               , "chunkNo" .= String (renderChunkNo chunkNo)
               ]
    ImmDB.RewriteSecondaryIndex chunkNo ->
      mkObject [ "kind" .= String "TraceImmutableDBEvent.RewriteSecondaryIndex"
               , "chunkNo" .= String (renderChunkNo chunkNo)
               ]
    ImmDB.Migrating txt ->
      mkObject [ "kind" .= String "TraceImmutableDBEvent.Migrating"
               , "info" .= String txt
               ]
    ImmDB.DeletingAfter immTipWithInfo ->
      mkObject [ "kind" .= String "TraceImmutableDBEvent.DeletingAfter"
               , "immTipHash" .= String (renderWithOrigin renderTipHash immTipWithInfo)
               , "immTipBlockNo" .= String (renderWithOrigin renderTipBlockNo immTipWithInfo)
               ]
    ImmDB.DBAlreadyClosed -> mkObject [ "kind" .= String "TraceImmutableDBEvent.DBAlreadyClosed" ]
    ImmDB.DBClosed -> mkObject [ "kind" .= String "TraceImmutableDBEvent.DBClosed" ]
    ImmDB.TraceCacheEvent cacheEv ->
      case cacheEv of
        ImmDB.TraceCurrentChunkHit chunkNo nbPastChunksInCache ->
          mkObject [ "kind" .= String "TraceImmDbEvent.TraceCacheEvent.TraceCurrentChunkHit"
                   , "chunkNo" .= String (renderChunkNo chunkNo)
                   , "noPastChunks" .= String (showT nbPastChunksInCache)
                   ]
        ImmDB.TracePastChunkHit chunkNo nbPastChunksInCache ->
          mkObject [ "kind" .= String "TraceImmDbEvent.TraceCacheEvent.TracePastChunkHit"
                   , "chunkNo" .= String (renderChunkNo chunkNo)
                   , "noPastChunks" .= String (showT nbPastChunksInCache)
                   ]
        ImmDB.TracePastChunkMiss chunkNo nbPastChunksInCache ->
          mkObject [ "kind" .= String "TraceImmDbEvent.TraceCacheEvent.TracePastChunkMiss"
                   , "chunkNo" .= String (renderChunkNo chunkNo)
                   , "noPastChunks" .= String (showT nbPastChunksInCache)
                   ]
        ImmDB.TracePastChunkEvict chunkNo nbPastChunksInCache ->
          mkObject [ "kind" .= String "TraceImmDbEvent.TraceCacheEvent.TracePastChunkEvict"
                   , "chunkNo" .= String (renderChunkNo chunkNo)
                   , "noPastChunks" .= String (showT nbPastChunksInCache)
                   ]
        ImmDB.TracePastChunksExpired chunkNos nbPastChunksInCache ->
          mkObject [ "kind" .= String "TraceImmDbEvent.TraceCacheEvent.TracePastChunksExpired"
                   , "chunkNos" .= String (Text.pack . show $ map renderChunkNo chunkNos)
                   , "noPastChunks" .= String (showT nbPastChunksInCache)
                   ]
  toObject _verb (ChainDB.TraceVolatileDBEvent ev) = case ev of
    VolDb.DBAlreadyClosed -> mkObject [ "kind" .= String "TraceVolatileDbEvent.DBAlreadyClosed"]
    VolDb.DBAlreadyOpen -> mkObject [ "kind" .= String "TraceVolatileDbEvent.DBAlreadyOpen"]
    VolDb.BlockAlreadyHere blockId ->
      mkObject [ "kind" .= String "TraceVolatileDbEvent.BlockAlreadyHere"
               , "blockId" .= String (showT blockId)
               ]
    VolDb.TruncateCurrentFile fsPath ->
      mkObject [ "kind" .= String "TraceVolatileDbEvent.TruncateCurrentFile"
               , "file" .= String (showT fsPath)
               ]
    VolDb.Truncate pErr fsPath blockOffset ->
      mkObject [ "kind" .= String "TraceVolatileDbEvent.Truncate"
               , "parserError" .= String (showT pErr)
               , "file" .= String (showT fsPath)
               , "blockOffset" .= String (showT blockOffset)
               ]
    VolDb.InvalidFileNames fsPaths ->
      mkObject [ "kind" .= String "TraceVolatileDBEvent.InvalidFileNames"
               , "files" .= String (Text.pack . show $ map show fsPaths)
               ]

instance ConvertRawHash blk => ToObject (TraceBlockFetchServerEvent blk) where
  toObject _verb (TraceBlockFetchServerSendBlock blk) =
    mkObject [ "kind"  .= String "TraceBlockFetchServerSendBlock"
             , "block" .= String (renderChainHash @blk (renderHeaderHash (Proxy @blk)) $ pointHash blk)
             ]


instance (ConvertRawHash blk, LedgerSupportsProtocol blk)
      => ToObject (TraceChainSyncClientEvent blk) where
  toObject verb ev = case ev of
    TraceDownloadedHeader pt ->
      mkObject [ "kind" .= String "ChainSyncClientEvent.TraceDownloadedHeader"
               , "block" .= toObject verb (headerPoint pt) ]
    TraceRolledBack tip ->
      mkObject [ "kind" .= String "ChainSyncClientEvent.TraceRolledBack"
               , "tip" .= toObject verb tip ]
    TraceException exc ->
      mkObject [ "kind" .= String "ChainSyncClientEvent.TraceException"
               , "exception" .= String (pack $ show exc) ]
    TraceFoundIntersection _ _ _ ->
      mkObject [ "kind" .= String "ChainSyncClientEvent.TraceFoundIntersection" ]
    TraceTermination _ ->
      mkObject [ "kind" .= String "ChainSyncClientEvent.TraceTermination" ]


instance ConvertRawHash blk
      => ToObject (TraceChainSyncServerEvent blk) where
  toObject verb ev = case ev of
    TraceChainSyncServerRead tip (AddBlock hdr) ->
      mkObject [ "kind" .= String "ChainSyncServerEvent.TraceChainSyncServerRead.AddBlock"
               , "tip" .= String (renderTipForVerbosity verb tip)
               , "addedBlock" .= String (renderPointForVerbosity verb hdr)
               ]
    TraceChainSyncServerRead tip (RollBack pt) ->
      mkObject [ "kind" .= String "ChainSyncServerEvent.TraceChainSyncServerRead.RollBack"
               , "tip" .= String (renderTipForVerbosity verb tip)
               , "rolledBackBlock" .= String (renderPointForVerbosity verb pt)
               ]
    TraceChainSyncServerReadBlocked tip (AddBlock hdr) ->
      mkObject [ "kind" .= String "ChainSyncServerEvent.TraceChainSyncServerReadBlocked.AddBlock"
               , "tip" .= String (renderTipForVerbosity verb tip)
               , "addedBlock" .= String (renderPointForVerbosity verb hdr)
               ]
    TraceChainSyncServerReadBlocked tip (RollBack pt) ->
      mkObject [ "kind" .= String "ChainSyncServerEvent.TraceChainSyncServerReadBlocked.RollBack"
               , "tip" .= String (renderTipForVerbosity verb tip)
               , "rolledBackBlock" .= String (renderPointForVerbosity verb pt)
               ]

    TraceChainSyncRollForward point ->
      mkObject [ "kind" .= String "ChainSyncServerEvent.TraceChainSyncRollForward"
               , "point" .= toObject verb point
               ]
    TraceChainSyncRollBackward point ->
      mkObject [ "kind" .= String "ChainSyncServerEvent.TraceChainSyncRollBackward"
               , "point" .= toObject verb point
               ]

instance ( Show (ApplyTxErr blk), ToObject (ApplyTxErr blk), ToObject (GenTx blk),
           ToJSON (GenTxId blk), LedgerSupportsMempool blk
         ) => ToObject (TraceEventMempool blk) where
  toObject verb (TraceMempoolAddedTx tx _mpSzBefore mpSzAfter) =
    mkObject
      [ "kind" .= String "TraceMempoolAddedTx"
      , "tx" .= toObject verb (txForgetValidated tx)
      , "mempoolSize" .= toObject verb mpSzAfter
      ]
  toObject verb (TraceMempoolRejectedTx tx txApplyErr mpSz) =
    mkObject
      [ "kind" .= String "TraceMempoolRejectedTx"
      , "err" .= toObject verb txApplyErr
      , "tx" .= toObject verb tx
      , "mempoolSize" .= toObject verb mpSz
      ]
  toObject verb (TraceMempoolRemoveTxs txs mpSz) =
    mkObject
      [ "kind" .= String "TraceMempoolRemoveTxs"
      , "txs" .= map (toObject verb . txForgetValidated) txs
      , "mempoolSize" .= toObject verb mpSz
      ]
  toObject verb (TraceMempoolManuallyRemovedTxs txs0 txs1 mpSz) =
    mkObject
      [ "kind" .= String "TraceMempoolManuallyRemovedTxs"
      , "txsRemoved" .= txs0
      , "txsInvalidated" .= map (toObject verb . txForgetValidated) txs1
      , "mempoolSize" .= toObject verb mpSz
      ]

instance ToObject MempoolSize where
  toObject _verb MempoolSize{msNumTxs, msNumBytes} =
    mkObject
      [ "numTxs" .= msNumTxs
      , "bytes" .= msNumBytes
      ]

instance HasTextFormatter () where
  formatText _ = pack . show . toList

-- ForgeStateInfo default value = ()
instance Transformable Text IO () where
  trTransformer = trStructuredText

instance ( tx ~ GenTx blk
         , ConvertRawHash blk
         , HasTxId tx
         , RunNode blk
         , Show (TxId tx)
         , ToObject (LedgerError blk)
         , ToObject (OtherHeaderEnvelopeError blk)
         , ToObject (ValidationErr (BlockProtocol blk))
         , ToObject (CannotForge blk)
         , ToObject (ForgeStateUpdateError blk))
      => ToObject (TraceForgeEvent blk) where
  toObject _verb (TraceStartLeadershipCheck slotNo) =
    mkObject
      [ "kind" .= String "TraceStartLeadershipCheck"
      , "slot" .= toJSON (unSlotNo slotNo)
      ]
  toObject verb (TraceSlotIsImmutable slotNo tipPoint tipBlkNo) =
    mkObject
      [ "kind" .= String "TraceSlotIsImmutable"
      , "slot" .= toJSON (unSlotNo slotNo)
      , "tip" .= renderPointForVerbosity verb tipPoint
      , "tipBlockNo" .= toJSON (unBlockNo tipBlkNo)
      ]
  toObject _verb (TraceBlockFromFuture currentSlot tip) =
    mkObject
      [ "kind" .= String "TraceBlockFromFuture"
      , "current slot" .= toJSON (unSlotNo currentSlot)
      , "tip" .= toJSON (unSlotNo tip)
      ]
  toObject verb (TraceBlockContext currentSlot tipBlkNo tipPoint) =
    mkObject
      [ "kind" .= String "TraceBlockContext"
      , "current slot" .= toJSON (unSlotNo currentSlot)
      , "tip" .= renderPointForVerbosity verb tipPoint
      , "tipBlockNo" .= toJSON (unBlockNo tipBlkNo)
      ]
  toObject _verb (TraceNoLedgerState slotNo _pt) =
    mkObject
      [ "kind" .= String "TraceNoLedgerState"
      , "slot" .= toJSON (unSlotNo slotNo)
      ]
  toObject _verb (TraceLedgerState slotNo _pt) =
    mkObject
      [ "kind" .= String "TraceLedgerState"
      , "slot" .= toJSON (unSlotNo slotNo)
      ]
  toObject _verb (TraceNoLedgerView slotNo _) =
    mkObject
      [ "kind" .= String "TraceNoLedgerView"
      , "slot" .= toJSON (unSlotNo slotNo)
      ]
  toObject _verb (TraceLedgerView slotNo) =
    mkObject
      [ "kind" .= String "TraceLedgerView"
      , "slot" .= toJSON (unSlotNo slotNo)
      ]
  toObject verb (TraceForgeStateUpdateError slotNo reason) =
    mkObject
      [ "kind" .= String "TraceForgeStateUpdateError"
      , "slot" .= toJSON (unSlotNo slotNo)
      , "reason" .= toObject verb reason
      ]
  toObject verb (TraceNodeCannotForge slotNo reason) =
    mkObject
      [ "kind" .= String "TraceNodeCannotForge"
      , "slot" .= toJSON (unSlotNo slotNo)
      , "reason" .= toObject verb reason
      ]
  toObject _verb (TraceNodeNotLeader slotNo) =
    mkObject
      [ "kind" .= String "TraceNodeNotLeader"
      , "slot" .= toJSON (unSlotNo slotNo)
      ]
  toObject _verb (TraceNodeIsLeader slotNo) =
    mkObject
      [ "kind" .= String "TraceNodeIsLeader"
      , "slot" .= toJSON (unSlotNo slotNo)
      ]
  toObject _verb (TraceForgedBlock slotNo _ blk _) =
    mkObject
      [ "kind"      .= String "TraceForgedBlock"
      , "slot"      .= toJSON (unSlotNo slotNo)
      , "block"     .= String (renderHeaderHash (Proxy @blk) $ blockHash blk)
      , "blockNo"   .= toJSON (unBlockNo $ blockNo blk)
      , "blockPrev" .= String (renderChainHash @blk (renderHeaderHash (Proxy @blk)) $ blockPrevHash blk)
      ]
  toObject _verb (TraceDidntAdoptBlock slotNo _) =
    mkObject
      [ "kind" .= String "TraceDidntAdoptBlock"
      , "slot" .= toJSON (unSlotNo slotNo)
      ]
  toObject verb (TraceForgedInvalidBlock slotNo _ reason) =
    mkObject
      [ "kind" .= String "TraceForgedInvalidBlock"
      , "slot" .= toJSON (unSlotNo slotNo)
      , "reason" .= toObject verb reason
      ]
  toObject MaximalVerbosity (TraceAdoptedBlock slotNo blk txs) =
    mkObject
      [ "kind" .= String "TraceAdoptedBlock"
      , "slot" .= toJSON (unSlotNo slotNo)
      , "blockHash" .= renderHeaderHashForVerbosity
          (Proxy @blk)
          MaximalVerbosity
          (blockHash blk)
      , "blockSize" .= toJSON (estimateBlockSize (getHeader blk))
      , "txIds" .= toJSON (map (show . txId . txForgetValidated) txs)
      ]
  toObject verb (TraceAdoptedBlock slotNo blk _txs) =
    mkObject
      [ "kind" .= String "TraceAdoptedBlock"
      , "slot" .= toJSON (unSlotNo slotNo)
      , "blockHash" .= renderHeaderHashForVerbosity
          (Proxy @blk)
          verb
          (blockHash blk)
      , "blockSize" .= toJSON (estimateBlockSize (getHeader blk))
      ]


instance ToObject (TraceLocalTxSubmissionServerEvent blk) where
  toObject _verb _ =
    mkObject [ "kind" .= String "TraceLocalTxSubmissionServerEvent" ]
