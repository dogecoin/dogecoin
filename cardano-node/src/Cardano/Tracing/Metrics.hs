
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Tracing.Metrics
  ( KESMetricsData (..)
  , MaxKESEvolutions (..)
  , OperationalCertStartKESPeriod (..)
  , HasKESMetricsData (..)
  , HasKESInfo (..)
  , ForgingStats (..)
  , ForgeThreadStats (..)
  , mapForgingCurrentThreadStats
  , mapForgingCurrentThreadStats_
  , mapForgingStatsTxsProcessed
  , mkForgingStats
  , threadStatsProjection
  ) where

import           Cardano.Prelude hiding (All, (:.:))

import           Cardano.Crypto.KES.Class (Period)
import           Control.Concurrent.STM
import           Data.IORef (IORef, atomicModifyIORef', newIORef)
import qualified Data.Map.Strict as Map
import           Data.SOP.Strict (All, hcmap, K (..), hcollapse)
import           Ouroboros.Consensus.Block (ForgeStateInfo, ForgeStateUpdateError)
import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.TypeFamilyWrappers (WrapForgeStateInfo (..), WrapForgeStateUpdateError (..))
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (OneEraForgeStateInfo (..), OneEraForgeStateUpdateError (..))
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import           Ouroboros.Consensus.Shelley.Node ()
import qualified Ouroboros.Consensus.Shelley.Protocol.HotKey as HotKey
import           Shelley.Spec.Ledger.OCert (KESPeriod (..))

-- | KES-related data to be traced as metrics.
data KESMetricsData
  = NoKESMetricsData
  -- ^ The current protocol does not support KES.
  | TPraosKESMetricsData
      !Period
      -- ^ The current KES period of the hot key, relative to the start KES
      -- period of the operational certificate.
      !MaxKESEvolutions
      -- ^ The configured max KES evolutions.
      !OperationalCertStartKESPeriod
      -- ^ The start KES period of the configured operational certificate.

-- | The maximum number of evolutions that a KES key can undergo before it is
-- considered expired.
newtype MaxKESEvolutions = MaxKESEvolutions Word64

-- | The start KES period of the configured operational certificate.
newtype OperationalCertStartKESPeriod = OperationalCertStartKESPeriod Period

class HasKESMetricsData blk where
  -- Because 'ForgeStateInfo' is a type family, we need a Proxy argument to
  -- disambiguate.
  getKESMetricsData :: Proxy blk -> ForgeStateInfo blk -> KESMetricsData

  -- Default to 'NoKESMetricsData'
  getKESMetricsData _ _ = NoKESMetricsData

instance HasKESMetricsData (ShelleyBlock c) where
  getKESMetricsData _ forgeStateInfo =
      TPraosKESMetricsData currKesPeriod maxKesEvos oCertStartKesPeriod
    where
      HotKey.KESInfo
        { kesStartPeriod = KESPeriod startKesPeriod
        , kesEvolution = currKesPeriod
        , kesEndPeriod = KESPeriod endKesPeriod
        } = forgeStateInfo

      maxKesEvos = MaxKESEvolutions $
          fromIntegral $ endKesPeriod - startKesPeriod

      oCertStartKesPeriod = OperationalCertStartKESPeriod startKesPeriod

instance HasKESMetricsData ByronBlock where

instance All HasKESMetricsData xs => HasKESMetricsData (HardForkBlock xs) where
  getKESMetricsData _ forgeStateInfo =
      case forgeStateInfo of
        CurrentEraLacksBlockForging _ -> NoKESMetricsData
        CurrentEraForgeStateUpdated currentEraForgeStateInfo ->
            hcollapse
          . hcmap (Proxy @HasKESMetricsData) getOne
          . getOneEraForgeStateInfo
          $ currentEraForgeStateInfo
    where
      getOne :: forall blk. HasKESMetricsData blk
             => WrapForgeStateInfo blk
             -> K KESMetricsData blk
      getOne = K . getKESMetricsData (Proxy @blk) . unwrapForgeStateInfo

class HasKESInfo blk where
  getKESInfo :: Proxy blk -> ForgeStateUpdateError blk -> Maybe HotKey.KESInfo
  getKESInfo _ _ = Nothing

instance HasKESInfo (ShelleyBlock era) where
  getKESInfo _ (HotKey.KESCouldNotEvolve ki _) = Just ki
  getKESInfo _ (HotKey.KESKeyAlreadyPoisoned ki _) = Just ki

instance HasKESInfo ByronBlock

instance All HasKESInfo xs => HasKESInfo (HardForkBlock xs) where
  getKESInfo _ =
      hcollapse
    . hcmap (Proxy @HasKESInfo) getOne
    . getOneEraForgeStateUpdateError
   where
    getOne :: forall blk. HasKESInfo blk
           => WrapForgeStateUpdateError blk
           -> K (Maybe HotKey.KESInfo) blk
    getOne = K . getKESInfo (Proxy @blk) . unwrapForgeStateUpdateError

-- | This structure stores counters of blockchain-related events,
--   per individual forge thread.
--   These counters are driven by traces.
data ForgingStats
  = ForgingStats
  { fsTxsProcessedNum :: !(IORef Int)
    -- ^ Transactions removed from mempool.
  , fsState           :: !(TVar (Map ThreadId (TVar ForgeThreadStats)))
  , fsBlocksUncoupled :: !(TVar Int64)
    -- ^ Blocks forged since last restart not on the current chain
  }

-- | Per-forging-thread statistics.
data ForgeThreadStats = ForgeThreadStats
  { ftsNodeCannotForgeNum        :: !Int
  , ftsNodeIsLeaderNum           :: !Int
  , ftsBlocksForgedNum           :: !Int
  , ftsSlotsMissedNum            :: !Int
    -- ^ Potentially missed slots.  Note that this is not the same as the number
    -- of missed blocks, since this includes all occurences of not reaching a
    -- leadership check decision, whether or not leadership was possible or not.
    --
    -- Also note that when the aggregate total for this metric is reported in the
    -- multi-pool case, it can be much larger than the actual number of slots
    -- occuring since node start, for it is a sum total for all threads.
  , ftsLastSlot                  :: !Int
  }

mkForgingStats :: IO ForgingStats
mkForgingStats =
  ForgingStats
    <$> newIORef 0
    <*> newTVarIO mempty
    <*> newTVarIO 0

mapForgingStatsTxsProcessed ::
     ForgingStats
  -> (Int -> Int)
  -> IO Int
mapForgingStatsTxsProcessed fs f =
  atomicModifyIORef' (fsTxsProcessedNum fs) $
    \txCount -> join (,) $ f txCount

mapForgingCurrentThreadStats ::
     ForgingStats
  -> (ForgeThreadStats -> (ForgeThreadStats, a))
  -> IO a
mapForgingCurrentThreadStats ForgingStats { fsState } f = do
  tid <- myThreadId
  allStats <- readTVarIO fsState
  varStats <- case Map.lookup tid allStats of
    Nothing -> do
      varStats <- newTVarIO $ ForgeThreadStats 0 0 0 0 0
      atomically $ modifyTVar fsState $ Map.insert tid varStats
      return varStats
    Just varStats ->
      return varStats
  atomically $ do
    stats <- readTVar varStats
    let !(!stats', x) = f stats
    writeTVar varStats stats'
    return x

mapForgingCurrentThreadStats_ ::
     ForgingStats
  -> (ForgeThreadStats -> ForgeThreadStats)
  -> IO ()
mapForgingCurrentThreadStats_ fs f =
  void $ mapForgingCurrentThreadStats fs ((, ()) . f)

threadStatsProjection ::
     ForgingStats
  -> (ForgeThreadStats -> a)
  -> IO [a]
threadStatsProjection fs f = atomically $ do
  allStats <- readTVar (fsState fs)
  mapM (fmap f . readTVar) $ Map.elems allStats
