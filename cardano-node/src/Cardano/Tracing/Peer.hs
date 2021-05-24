{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracing.Peer
  ( Peer (..)
  , getCurrentPeers
  , ppPeer
  , tracePeers
  ) where

import           Cardano.Prelude hiding (atomically)
import           Prelude (String)

import qualified Control.Monad.Class.MonadSTM.Strict as STM

import           Data.Aeson (ToJSON (..), Value (..), toJSON, (.=))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Text.Printf (printf)
import           NoThunks.Class (NoThunks, AllowThunk (..))

import           Cardano.BM.Data.LogItem (LOContent (..))
import           Cardano.BM.Data.Tracer (emptyObject, mkObject)
import           Cardano.BM.Trace (traceNamedObject)
import           Cardano.BM.Tracing

import           Ouroboros.Consensus.Block (Header)
import           Ouroboros.Consensus.Node (remoteAddress)
import           Ouroboros.Consensus.Util.Orphans ()

import qualified Ouroboros.Network.AnchoredFragment as Net
import           Ouroboros.Network.Block (unSlotNo)
import qualified Ouroboros.Network.Block as Net
import qualified Ouroboros.Network.BlockFetch.ClientRegistry as Net
import           Ouroboros.Network.BlockFetch.ClientState (PeerFetchInFlight (..),
                     PeerFetchStatus (..), readFetchClientState)

import           Cardano.Tracing.Kernel

data Peer blk =
  Peer
  !RemoteConnectionId
  !(Net.AnchoredFragment (Header blk))
  !(PeerFetchStatus (Header blk))
  !(PeerFetchInFlight (Header blk))
  deriving (Generic)
  deriving NoThunks via AllowThunk (Peer blk)

instance NFData (Peer blk) where
    rnf _ = ()

ppPeer :: Peer blk -> Text
ppPeer (Peer cid _af status inflight) =
  Text.pack $ printf "%-15s %-8s %s" (ppCid cid) (ppStatus status) (ppInFlight inflight)

ppCid :: RemoteConnectionId -> String
ppCid = takeWhile (/= ':') . show . remoteAddress

ppInFlight :: PeerFetchInFlight header -> String
ppInFlight f = printf
 "%5s  %3d  %5d  %6d"
 (ppMaxSlotNo $ peerFetchMaxSlotNo f)
 (peerFetchReqsInFlight f)
 (Set.size $ peerFetchBlocksInFlight f)
 (peerFetchBytesInFlight f)

ppMaxSlotNo :: Net.MaxSlotNo -> String
ppMaxSlotNo Net.NoMaxSlotNo = "???"
ppMaxSlotNo (Net.MaxSlotNo x) = show (unSlotNo x)

ppStatus :: PeerFetchStatus header -> String
ppStatus PeerFetchStatusShutdown = "shutdown"
ppStatus PeerFetchStatusAberrant = "aberrant"
ppStatus PeerFetchStatusBusy     = "fetching"
ppStatus PeerFetchStatusReady {} = "ready"

getCurrentPeers
  :: NodeKernelData blk
  -> IO [Peer blk]
getCurrentPeers nkd = mapNodeKernelDataIO extractPeers nkd
                      <&> fromSMaybe mempty
 where
  tuple3pop :: (a, b, c) -> (a, b)
  tuple3pop (a, b, _) = (a, b)

  getCandidates
    :: STM.StrictTVar IO (Map peer (STM.StrictTVar IO (Net.AnchoredFragment (Header blk))))
    -> STM.STM IO (Map peer (Net.AnchoredFragment (Header blk)))
  getCandidates var = STM.readTVar var >>= traverse STM.readTVar

  extractPeers :: NodeKernel IO RemoteConnectionId LocalConnectionId blk
                -> IO [Peer blk]
  extractPeers kernel = do
    peerStates <- fmap tuple3pop <$> (   STM.atomically
                                       . (>>= traverse readFetchClientState)
                                       . Net.readFetchClientsStateVars
                                       . getFetchClientRegistry $ kernel
                                     )
    candidates <- STM.atomically . getCandidates . getNodeCandidates $ kernel

    let peers = flip Map.mapMaybeWithKey candidates $ \cid af ->
                  maybe Nothing
                        (\(status, inflight) -> Just $ Peer cid af status inflight)
                        $ Map.lookup cid peerStates
    pure . Map.elems $ peers

-- | Trace peers list, it will be forwarded to an external process
--   (for example, to RTView service).
tracePeers
  :: Trace IO Text
  -> [Peer blk]
  -> IO ()
tracePeers tr peers = do
  let tr' = appendName "metrics" tr
  let tr'' = appendName "peersFromNodeKernel" tr'
  meta <- mkLOMeta Notice Public
  traceNamedObject tr'' (meta, LogStructured $ toObject MaximalVerbosity peers)

-- | Instances for converting [Peer blk] to Object.

instance ToObject [Peer blk] where
  toObject MinimalVerbosity _ = emptyObject
  toObject _ [] = emptyObject
  toObject verb xs = mkObject
    [ "kind"  .= String "NodeKernelPeers"
    , "peers" .= toJSON
      (foldl' (\acc x -> toObject verb x : acc) [] xs)
    ]

instance ToObject (Peer blk) where
  toObject _verb (Peer cid _af status inflight) =
    mkObject [ "peerAddress"   .= String (Text.pack . show . remoteAddress $ cid)
             , "peerStatus"    .= String (Text.pack . ppStatus $ status)
             , "peerSlotNo"    .= String (Text.pack . ppMaxSlotNo . peerFetchMaxSlotNo $ inflight)
             , "peerReqsInF"   .= String (show . peerFetchReqsInFlight $ inflight)
             , "peerBlocksInF" .= String (show . Set.size . peerFetchBlocksInFlight $ inflight)
             , "peerBytesInF"  .= String (show . peerFetchBytesInFlight $ inflight)
             ]
