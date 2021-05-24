{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Api.Shelley.Genesis
  ( ShelleyGenesis(..)
  , shelleyGenesisDefaults
  ) where

import           Cardano.Prelude

import qualified Data.Map.Strict as Map
import qualified Data.Time as Time

import           Cardano.Slotting.Slot (EpochSize (..))

import           Ouroboros.Consensus.Shelley.Node (ShelleyGenesis (..), emptyGenesisStaking)

import           Shelley.Spec.Ledger.BaseTypes as Ledger
import           Shelley.Spec.Ledger.PParams as Ledger (PParams' (..), emptyPParams)


-- | Some reasonable starting defaults for constructing a 'ShelleyGenesis'.
--
-- You must override at least the following fields for this to be useful:
--
-- * 'sgSystemStart' the time of the first block
-- * 'sgNetworkMagic' to a suitable testnet or mainnet network magic number.
-- * 'sgGenDelegs' to have some initial nodes
-- * 'sgInitialFunds' to have any money in the system
-- * 'sgMaxLovelaceSupply' must be at least the sum of the 'sgInitialFunds'
--   but more if you want to allow for rewards.
--
shelleyGenesisDefaults :: ShelleyGenesis crypto
shelleyGenesisDefaults =
  ShelleyGenesis
    {
      -- parameters for this specific chain
      sgSystemStart           = zeroTime
    , sgNetworkMagic          = 42
    , sgNetworkId             = Ledger.Testnet

      -- consensus protocol parameters
    , sgSlotLength            = 1.0 :: Time.NominalDiffTime -- 1s slots
    , sgActiveSlotsCoeff      = 1/20                -- 20s block times on average
    , sgSecurityParam         = k
    , sgEpochLength           = EpochSize (k * 10 * 20) -- 10k/f
    , sgSlotsPerKESPeriod     = 60 * 60 * 36        -- 1.5 days with 1s slots
    , sgMaxKESEvolutions      = 60                  -- 90 days
    , sgUpdateQuorum          = 5                   -- assuming 7 genesis keys

    -- ledger protocol parameters
    , sgProtocolParams        =
        Ledger.emptyPParams
        { Ledger._d =
              Ledger.truncateUnitInterval
            . realToFrac
            $ (1 :: Double)
        , Ledger._maxBHSize = 1100                  -- TODO: compute from crypto
        , Ledger._maxBBSize = 64 * 1024             -- max 64kb blocks
        , Ledger._maxTxSize = 16 * 1024             -- max 16kb txs
        , Ledger._eMax      = 18
        }

      -- genesis keys and initial funds
    , sgGenDelegs             = Map.empty
    , sgStaking               = emptyGenesisStaking
    , sgInitialFunds          = Map.empty
    , sgMaxLovelaceSupply     = 0
    }
  where
    k = 2160
    zeroTime = Time.UTCTime (Time.fromGregorian 1970 1 1) 0 -- tradition
