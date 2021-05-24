{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Tracing.ConvertTxId
  ( ConvertTxId (..)
  ) where

import           Cardano.Prelude hiding (All)

import           Data.SOP.Strict

import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Crypto.Hashing as Byron.Crypto
import qualified Cardano.Ledger.SafeHash as Ledger
import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import           Ouroboros.Consensus.Byron.Ledger.Mempool (TxId (..))
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import           Ouroboros.Consensus.Shelley.Ledger.Mempool (TxId (..))
import           Ouroboros.Consensus.TypeFamilyWrappers
import qualified Shelley.Spec.Ledger.TxBody as Shelley

-- | Convert a transaction ID to raw bytes.
class ConvertTxId blk where
  txIdToRawBytes :: TxId (GenTx blk) -> ByteString

instance ConvertTxId ByronBlock where
  txIdToRawBytes (ByronTxId txId) = Byron.Crypto.abstractHashToBytes txId
  txIdToRawBytes (ByronDlgId dlgId) = Byron.Crypto.abstractHashToBytes dlgId
  txIdToRawBytes (ByronUpdateProposalId upId) =
    Byron.Crypto.abstractHashToBytes upId
  txIdToRawBytes (ByronUpdateVoteId voteId) =
    Byron.Crypto.abstractHashToBytes voteId

instance ConvertTxId (ShelleyBlock c) where
  txIdToRawBytes (ShelleyTxId txId) =
    Crypto.hashToBytes . Ledger.extractHash . Shelley._unTxId $ txId

instance All ConvertTxId xs
      => ConvertTxId (HardForkBlock xs) where
  txIdToRawBytes =
    hcollapse
      . hcmap (Proxy @ ConvertTxId) (K . txIdToRawBytes . unwrapGenTxId)
      . getOneEraGenTxId
      . getHardForkGenTxId
