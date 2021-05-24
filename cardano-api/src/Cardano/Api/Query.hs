{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- The Shelley ledger uses promoted data kinds which we have to use, but we do
-- not export any from this API. We also use them unticked as nature intended.
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}


-- | Queries from local clients to the node.
--
module Cardano.Api.Query (

    -- * Queries
    QueryInMode(..),
    QueryInEra(..),
    QueryInShelleyBasedEra(..),
    UTxO(..),

    -- * Internal conversion functions
    toConsensusQuery,
    fromConsensusQueryResult,

    -- * Wrapper types used in queries
    SerialisedDebugLedgerState(..),
    ProtocolState(..),

    DebugLedgerState(..),

    EraHistory(..),

    SlotsInEpoch(..),
    SlotsToEpochEnd(..),

    slotToEpoch,
  ) where

import           Data.Aeson (ToJSON (..), object, (.=))
import           Data.Bifunctor (bimap)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe)
import           Data.SOP.Strict (SListI)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Typeable
import           Prelude

import           Ouroboros.Network.Protocol.LocalStateQuery.Client (Some (..))

import qualified Ouroboros.Consensus.HardFork.Combinator as Consensus
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)
import qualified Ouroboros.Consensus.HardFork.Combinator.AcrossEras as Consensus
import qualified Ouroboros.Consensus.HardFork.Combinator.Degenerate as Consensus
import qualified Ouroboros.Consensus.HardFork.History as History
import qualified Ouroboros.Consensus.HardFork.History.Qry as Qry

import qualified Ouroboros.Consensus.Ledger.Query as Consensus
import qualified Ouroboros.Consensus.Byron.Ledger as Consensus
import           Ouroboros.Consensus.Cardano.Block (StandardCrypto)
import qualified Ouroboros.Consensus.Cardano.Block as Consensus
import qualified Ouroboros.Consensus.Shelley.Ledger as Consensus
import           Ouroboros.Network.Block (Serialised)

import           Cardano.Binary
import qualified Cardano.Chain.Update.Validation.Interface as Byron.Update
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Era as Ledger

import qualified Shelley.Spec.Ledger.API as Shelley
import qualified Shelley.Spec.Ledger.LedgerState as Shelley
import qualified Shelley.Spec.Ledger.PParams as Shelley

import           Cardano.Api.Address
import           Cardano.Api.Block
import           Cardano.Api.Certificate
import           Cardano.Api.Eras
import           Cardano.Api.KeysShelley
import           Cardano.Api.Modes
import           Cardano.Api.NetworkId
import           Cardano.Api.Orphans ()
import           Cardano.Api.ProtocolParameters
import           Cardano.Api.TxBody
import           Cardano.Api.Value

import           Data.Word (Word64)

-- ----------------------------------------------------------------------------
-- Queries
--

data QueryInMode mode result where
  QueryCurrentEra
    :: ConsensusModeIsMultiEra mode
    -> QueryInMode mode AnyCardanoEra

  QueryInEra
    :: EraInMode era mode
    -> QueryInEra era result
    -> QueryInMode mode (Either EraMismatch result)

  QueryEraHistory
    :: ConsensusModeIsMultiEra mode
    -> QueryInMode mode (EraHistory mode)

--TODO: add support for these
--     QueryEraStart   :: ConsensusModeIsMultiEra mode
--                     -> EraInMode era mode
--                     -> QueryInMode mode (Maybe EraStart)

newtype SlotsInEpoch = SlotsInEpoch Word64

newtype SlotsToEpochEnd = SlotsToEpochEnd Word64

data EraHistory mode where
  EraHistory
    :: ConsensusBlockForMode mode ~ Consensus.HardForkBlock xs
    => ConsensusMode mode
    -> History.Interpreter xs
    -> EraHistory mode

slotToEpoch :: SlotNo -> EraHistory mode -> Either Qry.PastHorizonException (EpochNo, SlotsInEpoch, SlotsToEpochEnd)
slotToEpoch slotNo (EraHistory _ interpreter) = case Qry.interpretQuery interpreter (Qry.slotToEpoch slotNo) of
  Right (epochNumber, slotsInEpoch, slotsToEpochEnd) -> Right (epochNumber, SlotsInEpoch slotsInEpoch, SlotsToEpochEnd slotsToEpochEnd)
  Left e -> Left e

deriving instance Show (QueryInMode mode result)

data QueryInEra era result where
     QueryByronUpdateState :: QueryInEra ByronEra ByronUpdateState

     QueryInShelleyBasedEra :: ShelleyBasedEra era
                            -> QueryInShelleyBasedEra era result
                            -> QueryInEra era result

deriving instance Show (QueryInEra era result)


data QueryInShelleyBasedEra era result where
     QueryChainPoint
       :: QueryInShelleyBasedEra era ChainPoint

     QueryEpoch
       :: QueryInShelleyBasedEra era EpochNo

     QueryGenesisParameters
       :: QueryInShelleyBasedEra era GenesisParameters

     QueryProtocolParameters
       :: QueryInShelleyBasedEra era ProtocolParameters

     QueryProtocolParametersUpdate
       :: QueryInShelleyBasedEra era
            (Map (Hash GenesisKey) ProtocolParametersUpdate)

     QueryStakeDistribution
       :: QueryInShelleyBasedEra era (Map (Hash StakePoolKey) Rational)

     QueryUTxO
       :: Maybe (Set AddressAny)
       -> QueryInShelleyBasedEra era (UTxO era)

     QueryStakeAddresses
       :: Set StakeCredential
       -> NetworkId
       -> QueryInShelleyBasedEra era (Map StakeAddress Lovelace,
                                      Map StakeAddress PoolId)

     -- TODO: Need to update ledger-specs dependency to access RewardProvenance
     -- QueryPoolRanking
     --   :: QueryInShelleyBasedEra era RewardProvenance

     QueryDebugLedgerState
       :: QueryInShelleyBasedEra era (SerialisedDebugLedgerState era)

     QueryProtocolState
       :: QueryInShelleyBasedEra era (ProtocolState era)

deriving instance Show (QueryInShelleyBasedEra era result)

-- ----------------------------------------------------------------------------
-- Wrapper types used in queries
--

--TODO: provide appropriate instances for these types as needed, e.g. JSON

newtype ByronUpdateState = ByronUpdateState Byron.Update.State
  deriving Show

newtype UTxO era = UTxO (Map TxIn (TxOut era))

instance IsCardanoEra era => ToJSON (UTxO era) where
  toJSON (UTxO m) = toJSON m

newtype SerialisedDebugLedgerState era
  = SerialisedDebugLedgerState (Serialised (Shelley.NewEpochState (ShelleyLedgerEra era)))

data DebugLedgerState era where
  DebugLedgerState :: ShelleyLedgerEra era ~ ledgerera => Shelley.NewEpochState ledgerera -> DebugLedgerState era

instance (Typeable era, Shelley.TransLedgerState FromCBOR (ShelleyLedgerEra era)) => FromCBOR (DebugLedgerState era) where
  fromCBOR = DebugLedgerState <$> (fromCBOR :: Decoder s (Shelley.NewEpochState (ShelleyLedgerEra era)))

-- TODO: Shelley based era class!
instance ( IsShelleyBasedEra era
         , ShelleyLedgerEra era ~ ledgerera
         , Consensus.ShelleyBasedEra ledgerera
         , ToJSON (Core.PParams ledgerera)
         , ToJSON (Core.PParamsDelta ledgerera)
         , ToJSON (Core.TxOut ledgerera)) => ToJSON (DebugLedgerState era) where
  toJSON (DebugLedgerState newEpochS) = object [ "lastEpoch" .= Shelley.nesEL newEpochS
                                          , "blocksBefore" .= Shelley.nesBprev newEpochS
                                          , "blocksCurrent" .= Shelley.nesBcur newEpochS
                                          , "stateBefore" .= Shelley.nesEs newEpochS
                                          , "possibleRewardUpdate" .= Shelley.nesRu newEpochS
                                          , "stakeDistrib" .= Shelley.nesPd newEpochS
                                          ]

newtype ProtocolState era
  = ProtocolState (Serialised (Shelley.ChainDepState (Ledger.Crypto (ShelleyLedgerEra era))))

toShelleyAddrSet :: CardanoEra era
                 -> Set AddressAny
                 -> Set (Shelley.Addr Consensus.StandardCrypto)
toShelleyAddrSet era =
    Set.fromList
  . map toShelleyAddr
    -- Ignore any addresses that are not appropriate for the era,
    -- e.g. Shelley addresses in the Byron era, as these would not
    -- appear in the UTxO anyway.
  . mapMaybe (anyAddressInEra era)
  . Set.toList


fromUTxO
  :: ShelleyLedgerEra era ~ ledgerera
  => ShelleyBasedEra era
  -> Shelley.UTxO ledgerera
  -> UTxO era
fromUTxO eraConversion utxo =
  case eraConversion of
    ShelleyBasedEraShelley ->
      let Shelley.UTxO sUtxo = utxo
      in UTxO . Map.fromList . map (bimap fromShelleyTxIn fromShelleyTxOut) $ Map.toList sUtxo
    ShelleyBasedEraAllegra ->
      let Shelley.UTxO sUtxo = utxo
      in UTxO . Map.fromList . map (bimap fromShelleyTxIn (fromTxOut ShelleyBasedEraAllegra)) $ Map.toList sUtxo
    ShelleyBasedEraMary ->
      let Shelley.UTxO sUtxo = utxo
      in UTxO . Map.fromList . map (bimap fromShelleyTxIn (fromTxOut ShelleyBasedEraMary)) $ Map.toList sUtxo
    ShelleyBasedEraAlonzo ->
      let Shelley.UTxO sUtxo = utxo
      in UTxO . Map.fromList . map (bimap fromShelleyTxIn (fromTxOut ShelleyBasedEraAlonzo)) $ Map.toList sUtxo

fromShelleyPoolDistr :: Shelley.PoolDistr StandardCrypto
                     -> Map (Hash StakePoolKey) Rational
fromShelleyPoolDistr =
    --TODO: write an appropriate property to show it is safe to use
    -- Map.fromListAsc or to use Map.mapKeysMonotonic
    Map.fromList
  . map (bimap StakePoolKeyHash Shelley.individualPoolStake)
  . Map.toList
  . Shelley.unPoolDistr

fromShelleyDelegations :: Map (Shelley.Credential Shelley.Staking StandardCrypto)
                              (Shelley.KeyHash Shelley.StakePool StandardCrypto)
                       -> Map StakeCredential PoolId
fromShelleyDelegations =
    --TODO: write an appropriate property to show it is safe to use
    -- Map.fromListAsc or to use Map.mapKeysMonotonic
    -- In this case it may not be: the Ord instances for Shelley.Credential
    -- do not match the one for StakeCredential
    Map.fromList
  . map (bimap fromShelleyStakeCredential StakePoolKeyHash)
  . Map.toList

fromShelleyRewardAccounts :: Shelley.RewardAccounts Consensus.StandardCrypto
                          -> Map StakeCredential Lovelace
fromShelleyRewardAccounts =
    --TODO: write an appropriate property to show it is safe to use
    -- Map.fromListAsc or to use Map.mapKeysMonotonic
    Map.fromList
  . map (bimap fromShelleyStakeCredential fromShelleyLovelace)
  . Map.toList


-- ----------------------------------------------------------------------------
-- Conversions of queries into the consensus types.
--

toConsensusQuery :: forall mode block result.
                    ConsensusBlockForMode mode ~ block
                 => QueryInMode mode result
                 -> Some (Consensus.Query block)
toConsensusQuery (QueryCurrentEra CardanoModeIsMultiEra) =
    Some $ Consensus.BlockQuery $
      Consensus.QueryHardFork
        Consensus.GetCurrentEra

toConsensusQuery (QueryInEra ByronEraInByronMode QueryByronUpdateState) =
    Some $ Consensus.BlockQuery $
      Consensus.DegenQuery
        Consensus.GetUpdateInterfaceState

toConsensusQuery (QueryEraHistory CardanoModeIsMultiEra) =
    Some $ Consensus.BlockQuery $
      Consensus.QueryHardFork
        Consensus.GetInterpreter

toConsensusQuery (QueryInEra ByronEraInCardanoMode QueryByronUpdateState) =
    Some $ Consensus.BlockQuery $
      Consensus.QueryIfCurrentByron
        Consensus.GetUpdateInterfaceState

toConsensusQuery (QueryInEra erainmode (QueryInShelleyBasedEra era q)) =
    case erainmode of
      ByronEraInByronMode     -> case era of {}
      ShelleyEraInShelleyMode -> toConsensusQueryShelleyBased erainmode q
      ByronEraInCardanoMode   -> case era of {}
      ShelleyEraInCardanoMode -> toConsensusQueryShelleyBased erainmode q
      AllegraEraInCardanoMode -> toConsensusQueryShelleyBased erainmode q
      MaryEraInCardanoMode    -> toConsensusQueryShelleyBased erainmode q
      AlonzoEraInCardanoMode  -> error "toConsensusQuery: Alonzo not implemented yet"


toConsensusQueryShelleyBased
  :: forall era ledgerera mode block xs result.
     ConsensusBlockForEra era ~ Consensus.ShelleyBlock ledgerera
  => Ledger.Crypto ledgerera ~ Consensus.StandardCrypto
  => ConsensusBlockForMode mode ~ block
  => block ~ Consensus.HardForkBlock xs
  => EraInMode era mode
  -> QueryInShelleyBasedEra era result
  -> Some (Consensus.Query block)
toConsensusQueryShelleyBased erainmode QueryChainPoint =
    Some (consensusQueryInEraInMode erainmode Consensus.GetLedgerTip)

toConsensusQueryShelleyBased erainmode QueryEpoch =
    Some (consensusQueryInEraInMode erainmode Consensus.GetEpochNo)

toConsensusQueryShelleyBased erainmode QueryGenesisParameters =
    Some (consensusQueryInEraInMode erainmode Consensus.GetGenesisConfig)

toConsensusQueryShelleyBased erainmode QueryProtocolParameters =
    Some (consensusQueryInEraInMode erainmode Consensus.GetCurrentPParams)

toConsensusQueryShelleyBased erainmode QueryProtocolParametersUpdate =
    Some (consensusQueryInEraInMode erainmode Consensus.GetProposedPParamsUpdates)

toConsensusQueryShelleyBased erainmode QueryStakeDistribution =
    Some (consensusQueryInEraInMode erainmode Consensus.GetStakeDistribution)

toConsensusQueryShelleyBased erainmode (QueryUTxO Nothing) =
    Some (consensusQueryInEraInMode erainmode Consensus.GetUTxO)

toConsensusQueryShelleyBased erainmode (QueryUTxO (Just addrs)) =
    Some (consensusQueryInEraInMode erainmode (Consensus.GetFilteredUTxO addrs'))
  where
    addrs' :: Set (Shelley.Addr Consensus.StandardCrypto)
    addrs' = toShelleyAddrSet (eraInModeToEra erainmode) addrs

toConsensusQueryShelleyBased erainmode (QueryStakeAddresses creds _nId) =
    Some (consensusQueryInEraInMode erainmode
            (Consensus.GetFilteredDelegationsAndRewardAccounts creds'))
  where
    creds' :: Set (Shelley.Credential Shelley.Staking StandardCrypto)
    creds' = Set.map toShelleyStakeCredential creds

toConsensusQueryShelleyBased erainmode QueryDebugLedgerState =
    Some (consensusQueryInEraInMode erainmode (Consensus.GetCBOR Consensus.DebugNewEpochState))

toConsensusQueryShelleyBased erainmode QueryProtocolState =
    Some (consensusQueryInEraInMode erainmode (Consensus.GetCBOR Consensus.DebugChainDepState))

consensusQueryInEraInMode
  :: forall era mode erablock modeblock result result' xs.
     ConsensusBlockForEra era   ~ erablock
  => ConsensusBlockForMode mode ~ modeblock
  => modeblock ~ Consensus.HardForkBlock xs
  => Consensus.HardForkQueryResult xs result ~ result'
  => EraInMode era mode
  -> Consensus.BlockQuery erablock  result
  -> Consensus.Query modeblock result'
consensusQueryInEraInMode erainmode =
    Consensus.BlockQuery
  . case erainmode of
      ByronEraInByronMode     -> Consensus.DegenQuery
      ShelleyEraInShelleyMode -> Consensus.DegenQuery
      ByronEraInCardanoMode   -> Consensus.QueryIfCurrentByron
      ShelleyEraInCardanoMode -> Consensus.QueryIfCurrentShelley
      AllegraEraInCardanoMode -> Consensus.QueryIfCurrentAllegra
      MaryEraInCardanoMode    -> Consensus.QueryIfCurrentMary
      AlonzoEraInCardanoMode  ->
        error "consensusQueryInEraInMode: Alonzo not implemented yet"


-- ----------------------------------------------------------------------------
-- Conversions of query results from the consensus types.
--

fromConsensusQueryResult :: forall mode block result result'.
                            ConsensusBlockForMode mode ~ block
                         => QueryInMode mode result
                         -> Consensus.Query block result'
                         -> result'
                         -> result
fromConsensusQueryResult (QueryEraHistory CardanoModeIsMultiEra) q' r' =
    case q' of
      Consensus.BlockQuery (Consensus.QueryHardFork Consensus.GetInterpreter)
        -> EraHistory CardanoMode r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryCurrentEra CardanoModeIsMultiEra) q' r' =
    case q' of
      Consensus.BlockQuery (Consensus.QueryHardFork Consensus.GetCurrentEra)
        -> anyEraInModeToAnyEra (fromConsensusEraIndex CardanoMode r')
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryInEra ByronEraInByronMode
                                     QueryByronUpdateState) q' r' =
    case (q', r') of
      (Consensus.BlockQuery (Consensus.DegenQuery Consensus.GetUpdateInterfaceState),
       Consensus.DegenQueryResult r'')
        -> Right (ByronUpdateState r'')

fromConsensusQueryResult (QueryInEra ByronEraInCardanoMode
                                     QueryByronUpdateState) q' r' =
    case q' of
      Consensus.BlockQuery
        (Consensus.QueryIfCurrentByron Consensus.GetUpdateInterfaceState)
        -> bimap fromConsensusEraMismatch ByronUpdateState r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryInEra ByronEraInByronMode
                                     (QueryInShelleyBasedEra era _)) _ _ =
    case era of {}

fromConsensusQueryResult (QueryInEra ShelleyEraInShelleyMode
                                     (QueryInShelleyBasedEra _era q)) q' r' =
    case (q', r') of
      (Consensus.BlockQuery (Consensus.DegenQuery q''),
       Consensus.DegenQueryResult r'')
        -> Right (fromConsensusQueryResultShelleyBased
                    ShelleyBasedEraShelley q q'' r'')

fromConsensusQueryResult (QueryInEra ByronEraInCardanoMode
                                     (QueryInShelleyBasedEra era _)) _ _ =
    case era of {}

fromConsensusQueryResult (QueryInEra ShelleyEraInCardanoMode
                                     (QueryInShelleyBasedEra _era q)) q' r' =
    case q' of
      Consensus.BlockQuery (Consensus.QueryIfCurrentShelley q'')
        -> bimap fromConsensusEraMismatch
                 (fromConsensusQueryResultShelleyBased
                    ShelleyBasedEraShelley q q'')
                 r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryInEra AllegraEraInCardanoMode
                                     (QueryInShelleyBasedEra _era q)) q' r' =
    case q' of
      Consensus.BlockQuery (Consensus.QueryIfCurrentAllegra q'')
        -> bimap fromConsensusEraMismatch
                 (fromConsensusQueryResultShelleyBased
                    ShelleyBasedEraAllegra q q'')
                 r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryInEra MaryEraInCardanoMode
                                     (QueryInShelleyBasedEra _era q)) q' r' =
    case q' of
      Consensus.BlockQuery (Consensus.QueryIfCurrentMary q'')
        -> bimap fromConsensusEraMismatch
                 (fromConsensusQueryResultShelleyBased
                    ShelleyBasedEraMary q q'')
                 r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryInEra AlonzoEraInCardanoMode
                                     (QueryInShelleyBasedEra _ _)) _ _ =
    error "fromConsensusQueryResult: Alonzo not implemented yet"

fromConsensusQueryResultShelleyBased
  :: forall era ledgerera result result'.
     ShelleyLedgerEra era ~ ledgerera
  => Core.PParams ledgerera ~ Shelley.PParams ledgerera
  => Core.PParamsDelta ledgerera ~ Shelley.PParamsUpdate ledgerera
  => Consensus.ShelleyBasedEra ledgerera
  => Ledger.Crypto ledgerera ~ Consensus.StandardCrypto
  => ShelleyBasedEra era
  -> QueryInShelleyBasedEra era result
  -> Consensus.BlockQuery (Consensus.ShelleyBlock ledgerera) result'
  -> result'
  -> result
fromConsensusQueryResultShelleyBased _ QueryChainPoint q' point =
    case q' of
      Consensus.GetLedgerTip -> fromConsensusPoint point
      _                      -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased _ QueryEpoch q' epoch =
    case q' of
      Consensus.GetEpochNo -> epoch
      _                    -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased _ QueryGenesisParameters q' r' =
    case q' of
      Consensus.GetGenesisConfig -> fromShelleyGenesis
                                      (Consensus.getCompactGenesis r')
      _                          -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased _ QueryProtocolParameters q' r' =
    case q' of
      Consensus.GetCurrentPParams -> fromShelleyPParams r'
      _                           -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased _ QueryProtocolParametersUpdate q' r' =
    case q' of
      Consensus.GetProposedPParamsUpdates -> fromShelleyProposedPPUpdates r'
      _                                   -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased _ QueryStakeDistribution q' r' =
    case q' of
      Consensus.GetStakeDistribution -> fromShelleyPoolDistr r'
      _                              -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased shelleyBasedEra' (QueryUTxO Nothing) q' utxo' =
    case q' of
      Consensus.GetUTxO -> fromUTxO shelleyBasedEra' utxo'
      _                 -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased shelleyBasedEra' (QueryUTxO Just{}) q' utxo' =
    case q' of
      Consensus.GetFilteredUTxO{} -> fromUTxO shelleyBasedEra' utxo'
      _                           -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased _ (QueryStakeAddresses _ nId) q' r' =
    case q' of
      Consensus.GetFilteredDelegationsAndRewardAccounts{}
        -> let (delegs, rwaccs) = r'
           in ( Map.mapKeys (makeStakeAddress nId) $ fromShelleyRewardAccounts rwaccs
              , Map.mapKeys (makeStakeAddress nId) $ fromShelleyDelegations delegs
              )
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased _ QueryDebugLedgerState{} q' r' =
    case q' of
      Consensus.GetCBOR Consensus.DebugNewEpochState -> SerialisedDebugLedgerState r'
      _                                              -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased _ QueryProtocolState q' r' =
    case q' of
      Consensus.GetCBOR Consensus.DebugChainDepState -> ProtocolState r'
      _                                              -> fromConsensusQueryResultMismatch

-- | This should /only/ happen if we messed up the mapping in 'toConsensusQuery'
-- and 'fromConsensusQueryResult' so they are inconsistent with each other.
--
-- If we do encounter this error it means that 'toConsensusQuery' maps a
-- API query constructor to a certain consensus query constructor but that
-- 'fromConsensusQueryResult' apparently expects a different pairing.
--
-- For example, imagine if 'toConsensusQuery would (incorrectly) map
-- 'QueryChainPoint' to 'Consensus.GetEpochNo' but 'fromConsensusQueryResult'
-- (correctly) expected to find 'Consensus.GetLedgerTip'. This mismatch would
-- trigger this error.
--
-- Such mismatches should be preventable with an appropriate property test.
--
fromConsensusQueryResultMismatch :: a
fromConsensusQueryResultMismatch =
    error "fromConsensusQueryResult: internal query mismatch"


fromConsensusEraMismatch :: SListI xs
                         => Consensus.MismatchEraInfo xs -> EraMismatch
fromConsensusEraMismatch = Consensus.mkEraMismatch
