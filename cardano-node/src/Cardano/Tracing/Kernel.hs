{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
module Cardano.Tracing.Kernel
  ( NodeKernelData (..)
  , mkNodeKernelData
  , setNodeKernel
  , mapNodeKernelDataIO
  , nkQueryLedger
  , nkQueryChain
  -- * Re-exports
  , NodeKernel (..)
  , LocalConnectionId
  , RemoteConnectionId
  , StrictMaybe(..)
  , fromSMaybe
  ) where

import           Cardano.Prelude

import           Data.IORef (IORef, newIORef, readIORef, writeIORef)
import           Shelley.Spec.Ledger.BaseTypes (StrictMaybe (..), fromSMaybe)

import           Ouroboros.Consensus.Block (Header)
import           Ouroboros.Consensus.Ledger.Abstract (IsLedger, LedgerState)
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState)
import           Ouroboros.Consensus.Node (NodeKernel (..))
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import           Ouroboros.Consensus.Util.Orphans ()

import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.NodeToClient (LocalConnectionId)
import           Ouroboros.Network.NodeToNode (RemoteConnectionId)


newtype NodeKernelData blk =
  NodeKernelData
  { unNodeKernelData :: IORef (StrictMaybe (NodeKernel IO RemoteConnectionId LocalConnectionId blk))
  }

mkNodeKernelData :: IO (NodeKernelData blk)
mkNodeKernelData = NodeKernelData <$> newIORef SNothing

setNodeKernel :: NodeKernelData blk
              -> NodeKernel IO RemoteConnectionId LocalConnectionId blk
              -> IO ()
setNodeKernel (NodeKernelData ref) nodeKern =
  writeIORef ref $ SJust nodeKern

mapNodeKernelDataIO ::
  (NodeKernel IO RemoteConnectionId LocalConnectionId blk -> IO a)
  -> NodeKernelData blk
  -> IO (StrictMaybe a)
mapNodeKernelDataIO f (NodeKernelData ref) =
  readIORef ref >>= traverse f

nkQueryLedger ::
     IsLedger (LedgerState blk)
  => (ExtLedgerState blk -> a)
  -> NodeKernel IO RemoteConnectionId LocalConnectionId blk
  -> IO a
nkQueryLedger f NodeKernel{getChainDB} =
  f <$> atomically (ChainDB.getCurrentLedger getChainDB)

nkQueryChain ::
     (AF.AnchoredFragment (Header blk) -> a)
  -> NodeKernel IO RemoteConnectionId LocalConnectionId blk
  -> IO a
nkQueryChain f NodeKernel{getChainDB} =
  f <$> atomically (ChainDB.getCurrentChain getChainDB)
