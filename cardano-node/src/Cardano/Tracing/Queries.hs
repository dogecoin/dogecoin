{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.Tracing.Queries
  (LedgerQueries(..))
where

import           Prelude (Int, (.))

import qualified Data.Map.Strict as Map

import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Embed.Unary

import qualified Cardano.Chain.Block as Byron
import qualified Cardano.Chain.UTxO as Byron
import qualified Ouroboros.Consensus.Byron.Ledger.Block as Byron
import qualified Ouroboros.Consensus.Byron.Ledger.Ledger as Byron

import qualified Ouroboros.Consensus.Shelley.Ledger as Shelley
import qualified Shelley.Spec.Ledger.LedgerState as Shelley
import qualified Shelley.Spec.Ledger.UTxO as Shelley

import qualified Ouroboros.Consensus.Cardano as Cardano
import qualified Ouroboros.Consensus.Cardano.Block as Cardano


class LedgerQueries blk where
  ledgerUtxoSize     :: LedgerState blk -> Int
  ledgerDelegMapSize :: LedgerState blk -> Int

instance LedgerQueries Byron.ByronBlock where
  ledgerUtxoSize = Map.size . Byron.unUTxO . Byron.cvsUtxo . Byron.byronLedgerState
  ledgerDelegMapSize _ = 0

instance LedgerQueries (Shelley.ShelleyBlock era) where
  ledgerUtxoSize =
      (\(Shelley.UTxO xs)-> Map.size xs)
    . Shelley._utxo
    . Shelley._utxoState
    . Shelley.esLState
    . Shelley.nesEs
    . Shelley.shelleyLedgerState
  ledgerDelegMapSize =
      Map.size
    . Shelley._delegations
    . Shelley._dstate
    . Shelley._delegationState
    . Shelley.esLState
    . Shelley.nesEs
    . Shelley.shelleyLedgerState

instance (LedgerQueries x, NoHardForks x)
      => LedgerQueries (HardForkBlock '[x]) where
  ledgerUtxoSize = ledgerUtxoSize . project
  ledgerDelegMapSize = ledgerDelegMapSize . project

instance LedgerQueries (Cardano.CardanoBlock c) where
  ledgerUtxoSize = \case
    Cardano.LedgerStateByron   ledgerByron   -> ledgerUtxoSize ledgerByron
    Cardano.LedgerStateShelley ledgerShelley -> ledgerUtxoSize ledgerShelley
    Cardano.LedgerStateAllegra ledgerAllegra -> ledgerUtxoSize ledgerAllegra
    Cardano.LedgerStateMary    ledgerMary    -> ledgerUtxoSize ledgerMary
    Cardano.LedgerStateAlonzo  ledgerAlonzo  -> ledgerUtxoSize ledgerAlonzo
  ledgerDelegMapSize = \case
    Cardano.LedgerStateByron   ledgerByron   -> ledgerDelegMapSize ledgerByron
    Cardano.LedgerStateShelley ledgerShelley -> ledgerDelegMapSize ledgerShelley
    Cardano.LedgerStateAllegra ledgerAllegra -> ledgerDelegMapSize ledgerAllegra
    Cardano.LedgerStateMary    ledgerMary    -> ledgerDelegMapSize ledgerMary
    Cardano.LedgerStateAlonzo  ledgerAlonzo  -> ledgerDelegMapSize ledgerAlonzo
