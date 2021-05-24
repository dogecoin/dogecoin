{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Tracing.Render
  ( renderBlockOrEBB
  , renderChunkNo
  , renderHeaderHash
  , renderHeaderHashForVerbosity
  , renderChainHash
  , renderTipBlockNo
  , renderTipHash
  , renderPoint
  , renderPointAsPhrase
  , renderPointForVerbosity
  , renderRealPoint
  , renderRealPointAsPhrase
  , renderSlotNo
  , renderTip
  , renderTipForVerbosity
  , renderTxId
  , renderTxIdForVerbosity
  , renderWithOrigin
  ) where

import           Cardano.Prelude
import           Prelude (id)

import qualified Data.ByteString.Base16 as B16
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Cardano.BM.Tracing (TracingVerbosity (..))
import           Cardano.Slotting.Slot (EpochNo (..), SlotNo (..), WithOrigin (..))
import           Cardano.Tracing.ConvertTxId (ConvertTxId (..))
import           Ouroboros.Consensus.Block (BlockNo (..), ConvertRawHash (..), RealPoint (..))
import           Ouroboros.Consensus.Block.Abstract (Point (..))
import           Ouroboros.Consensus.Ledger.SupportsMempool (GenTx, TxId)
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal (ChunkNo (..))
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Types (BlockOrEBB (..))
import qualified Ouroboros.Consensus.Storage.ImmutableDB.API as ImmDB
import           Ouroboros.Network.Block (ChainHash (..), HeaderHash, StandardHash, Tip, getTipPoint)

renderBlockOrEBB :: BlockOrEBB -> Text
renderBlockOrEBB (Block slotNo) = "Block at " <> renderSlotNo slotNo
renderBlockOrEBB (EBB epochNo) = "Epoch boundary block at " <> renderEpochNo epochNo

renderChunkNo :: ChunkNo -> Text
renderChunkNo = Text.pack . show . unChunkNo

renderEpochNo :: EpochNo -> Text
renderEpochNo = Text.pack . show . unEpochNo

renderTipBlockNo :: ImmDB.Tip blk -> Text
renderTipBlockNo = Text.pack . show . unBlockNo . ImmDB.tipBlockNo

renderTipHash :: StandardHash blk => ImmDB.Tip blk -> Text
renderTipHash tInfo = Text.pack . show $ ImmDB.tipHash tInfo

renderTxIdForVerbosity
  :: ConvertTxId blk
  => TracingVerbosity
  -> TxId (GenTx blk)
  -> Text
renderTxIdForVerbosity verb = trimHashTextForVerbosity verb . renderTxId

renderTxId :: ConvertTxId blk => TxId (GenTx blk) -> Text
renderTxId = Text.decodeLatin1 . B16.encode . txIdToRawBytes

renderWithOrigin :: (a -> Text) -> WithOrigin a -> Text
renderWithOrigin _ Origin = "origin"
renderWithOrigin render (At a) = render a

renderSlotNo :: SlotNo -> Text
renderSlotNo = Text.pack . show . unSlotNo

renderRealPoint
  :: forall blk.
     ConvertRawHash blk
  => RealPoint blk
  -> Text
renderRealPoint (RealPoint slotNo headerHash) =
  renderHeaderHash (Proxy @blk) headerHash
    <> "@"
    <> renderSlotNo slotNo

-- | Render a short phrase describing a 'RealPoint'.
-- e.g. "62292d753b2ee7e903095bc5f10b03cf4209f456ea08f55308e0aaab4350dda4 at
-- slot 39920"
renderRealPointAsPhrase
  :: forall blk.
     ConvertRawHash blk
  => RealPoint blk
  -> Text
renderRealPointAsPhrase (RealPoint slotNo headerHash) =
  renderHeaderHash (Proxy @blk) headerHash
    <> " at slot "
    <> renderSlotNo slotNo

renderPointForVerbosity
  :: forall blk.
     ConvertRawHash blk
  => TracingVerbosity
  -> Point blk
  -> Text
renderPointForVerbosity verb point =
  case point of
    GenesisPoint -> "genesis (origin)"
    BlockPoint slot h ->
      renderHeaderHashForVerbosity (Proxy @blk) verb h
        <> "@"
        <> renderSlotNo slot

renderPoint :: ConvertRawHash blk => Point blk -> Text
renderPoint = renderPointForVerbosity MaximalVerbosity

-- | Render a short phrase describing a 'Point'.
-- e.g. "62292d753b2ee7e903095bc5f10b03cf4209f456ea08f55308e0aaab4350dda4 at
-- slot 39920" or "genesis (origin)" in the case of a genesis point.
renderPointAsPhrase :: forall blk. ConvertRawHash blk => Point blk -> Text
renderPointAsPhrase point =
  case point of
    GenesisPoint -> "genesis (origin)"
    BlockPoint slot h ->
      renderHeaderHash (Proxy @blk) h
        <> " at slot "
        <> renderSlotNo slot

renderTipForVerbosity
  :: ConvertRawHash blk
  => TracingVerbosity
  -> Tip blk
  -> Text
renderTipForVerbosity verb = renderPointForVerbosity verb . getTipPoint

renderTip :: ConvertRawHash blk => Tip blk -> Text
renderTip = renderTipForVerbosity MaximalVerbosity

renderHeaderHashForVerbosity
  :: ConvertRawHash blk
  => proxy blk
  -> TracingVerbosity
  -> HeaderHash blk
  -> Text
renderHeaderHashForVerbosity p verb =
  trimHashTextForVerbosity verb . renderHeaderHash p

-- | Hex encode and render a 'HeaderHash' as text.
renderHeaderHash :: ConvertRawHash blk => proxy blk -> HeaderHash blk -> Text
renderHeaderHash p = Text.decodeLatin1 . B16.encode . toRawHash p

renderChainHash :: (HeaderHash blk -> Text) -> ChainHash blk -> Text
renderChainHash _ GenesisHash = "GenesisHash"
renderChainHash p (BlockHash hash) = p hash

trimHashTextForVerbosity :: TracingVerbosity -> Text -> Text
trimHashTextForVerbosity verb =
  case verb of
    MinimalVerbosity -> Text.take 7
    NormalVerbosity -> id
    MaximalVerbosity -> id
