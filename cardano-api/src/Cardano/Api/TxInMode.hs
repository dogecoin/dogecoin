{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Transactions in the context of a consensus mode, and other types used in
-- the transaction submission protocol.
--
module Cardano.Api.TxInMode (

    -- * Transaction in a consensus mode
    TxInMode(..),
    toConsensusGenTx,

    -- * Transaction validation errors
    TxValidationError(..),
    TxValidationErrorInMode(..),
    fromConsensusApplyTxErr,
  ) where

import           Prelude

import           Data.SOP.Strict (NS (S, Z))

import qualified Ouroboros.Consensus.Byron.Ledger as Consensus
import qualified Ouroboros.Consensus.Cardano.Block as Consensus
import qualified Ouroboros.Consensus.HardFork.Combinator as Consensus
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)
import qualified Ouroboros.Consensus.HardFork.Combinator.Degenerate as Consensus
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as Consensus
import qualified Ouroboros.Consensus.Shelley.Ledger as Consensus

import           Cardano.Api.Eras
import           Cardano.Api.Modes
import           Cardano.Api.Tx


-- ----------------------------------------------------------------------------
-- Transactions in the context of a consensus mode
--

-- | A 'Tx' in one of the eras supported by a given protocol mode.
--
-- For multi-era modes such as the 'CardanoMode' this type is a sum of the
-- different transaction types for all the eras. It is used in the
-- LocalTxSubmission protocol.
--
data TxInMode mode where

     -- | Everything we consider a normal transaction.
     --
     TxInMode :: Tx era -> EraInMode era mode -> TxInMode mode

     -- | Byron has various things we can post to the chain which are not
     -- actually transactions. This covers: update proposals, votes and
     -- delegation certs.
     --
     TxInByronSpecial :: Consensus.GenTx Consensus.ByronBlock
                      -> EraInMode ByronEra mode -> TxInMode mode

deriving instance Show (TxInMode mode)


toConsensusGenTx :: ConsensusBlockForMode mode ~ block
                 => TxInMode mode
                 -> Consensus.GenTx block
toConsensusGenTx (TxInMode (ByronTx tx) ByronEraInByronMode) =
    Consensus.HardForkGenTx (Consensus.OneEraGenTx (Z tx'))
  where
    tx' = Consensus.ByronTx (Consensus.byronIdTx tx) tx

toConsensusGenTx (TxInMode (ByronTx tx) ByronEraInCardanoMode) =
    Consensus.HardForkGenTx (Consensus.OneEraGenTx (Z tx'))
  where
    tx' = Consensus.ByronTx (Consensus.byronIdTx tx) tx
    --TODO: add the above as mkByronTx to the consensus code,
    -- matching mkShelleyTx below

toConsensusGenTx (TxInByronSpecial gtx ByronEraInByronMode) =
    Consensus.HardForkGenTx (Consensus.OneEraGenTx (Z gtx))

toConsensusGenTx (TxInByronSpecial gtx ByronEraInCardanoMode) =
    Consensus.HardForkGenTx (Consensus.OneEraGenTx (Z gtx))

toConsensusGenTx (TxInMode (ShelleyTx _ tx) ShelleyEraInShelleyMode) =
    Consensus.HardForkGenTx (Consensus.OneEraGenTx (Z tx'))
  where
    tx' = Consensus.mkShelleyTx tx

toConsensusGenTx (TxInMode (ShelleyTx _ tx) ShelleyEraInCardanoMode) =
    Consensus.HardForkGenTx (Consensus.OneEraGenTx (S (Z tx')))
  where
    tx' = Consensus.mkShelleyTx tx

toConsensusGenTx (TxInMode (ShelleyTx _ tx) AllegraEraInCardanoMode) =
    Consensus.HardForkGenTx (Consensus.OneEraGenTx (S (S (Z tx'))))
  where
    tx' = Consensus.mkShelleyTx tx

toConsensusGenTx (TxInMode (ShelleyTx _ tx) MaryEraInCardanoMode) =
    Consensus.HardForkGenTx (Consensus.OneEraGenTx (S (S (S (Z tx')))))
  where
    tx' = Consensus.mkShelleyTx tx

toConsensusGenTx (TxInMode (ShelleyTx _ _tx) AlonzoEraInCardanoMode) =
    Consensus.HardForkGenTx (Consensus.OneEraGenTx (S (S (S (S (Z tx'))))))
  where
    tx' = error "toConsensusGenTx: Alonzo not implemented yet"
    -- Consensus needs to expose a function that can make create Alonzo txs



-- ----------------------------------------------------------------------------
-- Transaction validation errors in the context of eras and consensus modes
--

-- | The transaction validations errors that can occur from trying to submit a
-- transaction to a local node. The errors are specific to an era.
--
data TxValidationError era where

     ByronTxValidationError
       :: Consensus.ApplyTxErr Consensus.ByronBlock
       -> TxValidationError ByronEra

     ShelleyTxValidationError
       :: ShelleyBasedEra era
       -> Consensus.ApplyTxErr (Consensus.ShelleyBlock (ShelleyLedgerEra era))
       -> TxValidationError era

-- The GADT in the ShelleyTxValidationError case requires a custom instance
instance Show (TxValidationError era) where
    showsPrec p (ByronTxValidationError err) =
      showParen (p >= 11)
        ( showString "ByronTxValidationError "
        . showsPrec 11 err
        )

    showsPrec p (ShelleyTxValidationError ShelleyBasedEraShelley err) =
      showParen (p >= 11)
        ( showString "ShelleyTxValidationError ShelleyBasedEraShelley "
        . showsPrec 11 err
        )

    showsPrec p (ShelleyTxValidationError ShelleyBasedEraAllegra err) =
      showParen (p >= 11)
        ( showString "ShelleyTxValidationError ShelleyBasedEraAllegra "
        . showsPrec 11 err
        )

    showsPrec p (ShelleyTxValidationError ShelleyBasedEraMary err) =
      showParen (p >= 11)
        ( showString "ShelleyTxValidationError ShelleyBasedEraMary "
        . showsPrec 11 err
        )

    showsPrec p (ShelleyTxValidationError ShelleyBasedEraAlonzo err) =
      showParen (p >= 11)
        ( showString "ShelleyTxValidationError ShelleyBasedEraAlonzo "
        . showsPrec 11 err
        )


-- | A 'TxValidationError' in one of the eras supported by a given protocol
-- mode.
--
-- This is used in the LocalStateQuery protocol.
--
data TxValidationErrorInMode mode where
     TxValidationErrorInMode :: TxValidationError era
                             -> EraInMode era mode
                             -> TxValidationErrorInMode mode

     TxValidationEraMismatch :: EraMismatch
                             -> TxValidationErrorInMode mode

deriving instance Show (TxValidationErrorInMode mode)


fromConsensusApplyTxErr :: ConsensusBlockForMode mode ~ block
                        => ConsensusMode mode
                        -> Consensus.ApplyTxErr block
                        -> TxValidationErrorInMode mode
fromConsensusApplyTxErr ByronMode (Consensus.DegenApplyTxErr err) =
    TxValidationErrorInMode
      (ByronTxValidationError err)
      ByronEraInByronMode

fromConsensusApplyTxErr ShelleyMode (Consensus.DegenApplyTxErr err) =
    TxValidationErrorInMode
      (ShelleyTxValidationError ShelleyBasedEraShelley err)
      ShelleyEraInShelleyMode

fromConsensusApplyTxErr CardanoMode (Consensus.ApplyTxErrByron err) =
    TxValidationErrorInMode
      (ByronTxValidationError err)
      ByronEraInCardanoMode

fromConsensusApplyTxErr CardanoMode (Consensus.ApplyTxErrShelley err) =
    TxValidationErrorInMode
      (ShelleyTxValidationError ShelleyBasedEraShelley err)
      ShelleyEraInCardanoMode

fromConsensusApplyTxErr CardanoMode (Consensus.ApplyTxErrAllegra err) =
    TxValidationErrorInMode
      (ShelleyTxValidationError ShelleyBasedEraAllegra err)
      AllegraEraInCardanoMode

fromConsensusApplyTxErr CardanoMode (Consensus.ApplyTxErrMary err) =
    TxValidationErrorInMode
      (ShelleyTxValidationError ShelleyBasedEraMary err)
      MaryEraInCardanoMode

fromConsensusApplyTxErr CardanoMode (Consensus.ApplyTxErrAlonzo err) =
    TxValidationErrorInMode
      (ShelleyTxValidationError ShelleyBasedEraAlonzo err)
      AlonzoEraInCardanoMode

fromConsensusApplyTxErr CardanoMode (Consensus.ApplyTxErrWrongEra err) =
    TxValidationEraMismatch err

