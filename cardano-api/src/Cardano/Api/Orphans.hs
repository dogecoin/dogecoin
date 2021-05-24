{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Cardano.Api.Orphans () where

import           Prelude

import           Control.Iterate.SetAlgebra (BiMap (..), Bimap)
import           Data.Aeson (ToJSON (..), object, (.=))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Types (ToJSONKey (..), toJSONKeyText)
import qualified Data.ByteString.Base16 as B16
import qualified Data.Map.Strict as Map
import           Data.Scientific
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Ledger.Coin as Shelley
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as Crypto
import qualified Cardano.Ledger.Mary.Value as Mary
import qualified Cardano.Ledger.SafeHash as SafeHash
import qualified Cardano.Ledger.Shelley.Constraints as Shelley
import           Cardano.Slotting.Slot (SlotNo (..))
import qualified Ouroboros.Consensus.Shelley.Eras as Consensus
import qualified Shelley.Spec.Ledger.API as Shelley
import           Shelley.Spec.Ledger.BaseTypes (StrictMaybe (..))
import qualified Shelley.Spec.Ledger.Delegation.Certificates as Shelley
import qualified Shelley.Spec.Ledger.EpochBoundary as ShelleyEpoch
import qualified Shelley.Spec.Ledger.LedgerState as ShelleyLedger
import           Shelley.Spec.Ledger.PParams (PParamsUpdate)
import qualified Shelley.Spec.Ledger.Rewards as Shelley
import qualified Shelley.Spec.Ledger.RewardUpdate as Shelley

-- Orphan instances involved in the JSON output of the API queries.
-- We will remove/replace these as we provide more API wrapper types

instance ToJSON (Mary.Value era) where
  toJSON (Mary.Value l ps) =
    object
      [ "lovelace" .= toJSON l
      , "policies" .= toJSON ps
      ]

instance ToJSONKey Mary.AssetName where
  toJSONKey = toJSONKeyText render
    where
      render = Text.decodeLatin1 . B16.encode . Mary.assetName

instance ToJSON (Mary.PolicyID era) where
  toJSON (Mary.PolicyID (Shelley.ScriptHash h)) = Aeson.String (hashToText h)

instance ToJSONKey (Mary.PolicyID era) where
  toJSONKey = toJSONKeyText render
    where
      render (Mary.PolicyID (Shelley.ScriptHash h)) = hashToText h

instance ToJSON Mary.AssetName where
  toJSON = Aeson.String . Text.decodeLatin1 . B16.encode . Mary.assetName

instance ToJSON Shelley.AccountState where
  toJSON (Shelley.AccountState tr rs) = object [ "treasury" .= tr
                                               , "reserves" .= rs
                                               ]

instance ( Consensus.ShelleyBasedEra era
         , ToJSON (Core.TxOut era)
         , ToJSON (Core.PParams era)
         , ToJSON (Core.PParamsDelta era)
         ) => ToJSON (Shelley.EpochState era) where
  toJSON eState = object [ "esAccountState" .= Shelley.esAccountState eState
                         , "esSnapshots" .= Shelley.esSnapshots eState
                         , "esLState" .= Shelley.esLState eState
                         , "esPrevPp" .= Shelley.esPrevPp eState
                         , "esPp" .= Shelley.esPp eState
                         , "esNonMyopic" .= Shelley.esNonMyopic eState
                         ]

instance ( Consensus.ShelleyBasedEra era
         , ToJSON (Core.TxOut era)
         , ToJSON (Core.PParamsDelta era)
         ) => ToJSON (Shelley.LedgerState era) where
  toJSON lState = object [ "utxoState" .= Shelley._utxoState lState
                         , "delegationState" .= Shelley._delegationState lState
                         ]

instance ( Consensus.ShelleyBasedEra era
         , ToJSON (Core.TxOut era)
         , ToJSON (Core.PParamsDelta era)
         ) => ToJSON (Shelley.UTxOState era) where
  toJSON utxoState = object [ "utxo" .= Shelley._utxo utxoState
                            , "deposited" .= Shelley._deposited utxoState
                            , "fees" .= Shelley._fees utxoState
                            , "ppups" .= Shelley._ppups utxoState
                            ]

instance ( ToJSON (Core.PParamsDelta era)
         , Shelley.UsesPParams era
         ) => ToJSON (Shelley.PPUPState era) where
  toJSON ppUpState = object [ "proposals" .= Shelley.proposals ppUpState
                            , "futureProposals" .= Shelley.futureProposals ppUpState
                            ]

instance ( ToJSON (Core.PParamsDelta era)
         , Shelley.UsesPParams era
         ) => ToJSON (Shelley.ProposedPPUpdates era) where
  toJSON (Shelley.ProposedPPUpdates ppUpdates) = toJSON $ Map.toList ppUpdates

instance ToJSON (PParamsUpdate era) where
  toJSON pp =
    Aeson.object $
        [ "minFeeA"               .= x | x <- mbfield (Shelley._minfeeA pp) ]
     ++ [ "minFeeB"               .= x | x <- mbfield (Shelley._minfeeB pp) ]
     ++ [ "maxBlockBodySize"      .= x | x <- mbfield (Shelley._maxBBSize pp) ]
     ++ [ "maxTxSize"             .= x | x <- mbfield (Shelley._maxTxSize pp) ]
     ++ [ "maxBlockHeaderSize"    .= x | x <- mbfield (Shelley._maxBHSize pp) ]
     ++ [ "keyDeposit"            .= x | x <- mbfield (Shelley._keyDeposit pp) ]
     ++ [ "poolDeposit"           .= x | x <- mbfield (Shelley._poolDeposit pp) ]
     ++ [ "eMax"                  .= x | x <- mbfield (Shelley._eMax pp) ]
     ++ [ "nOpt"                  .= x | x <- mbfield (Shelley._nOpt pp) ]
     ++ [ "a0" .= (fromRational x :: Scientific)
                                       | x <- mbfield (Shelley._a0 pp) ]
     ++ [ "rho"                   .= x | x <- mbfield (Shelley._rho pp) ]
     ++ [ "tau"                   .= x | x <- mbfield (Shelley._tau pp) ]
     ++ [ "decentralisationParam" .= x | x <- mbfield (Shelley._d pp) ]
     ++ [ "extraEntropy"          .= x | x <- mbfield (Shelley._extraEntropy pp) ]
     ++ [ "protocolVersion"       .= x | x <- mbfield (Shelley._protocolVersion pp) ]
     ++ [ "minUTxOValue"          .= x | x <- mbfield (Shelley._minUTxOValue pp) ]
     ++ [ "minPoolCost"           .= x | x <- mbfield (Shelley._minPoolCost pp) ]
    where
      mbfield SNothing  = []
      mbfield (SJust x) = [x]

instance Crypto.Crypto crypto => ToJSON (Shelley.DPState crypto) where
  toJSON dpState = object [ "dstate" .= Shelley._dstate dpState
                          , "pstate" .= Shelley._pstate dpState
                          ]

instance Crypto.Crypto crypto => ToJSON (Shelley.DState crypto) where
  toJSON dState = object [ "rewards" .= Shelley._rewards dState
                         , "delegations" .= ShelleyLedger._delegations dState
                         , "ptrs" .= Shelley._ptrs dState
                         , "fGenDelegs" .= Map.toList (Shelley._fGenDelegs dState)
                         , "genDelegs" .= Shelley._genDelegs dState
                         , "irwd" .= Shelley._irwd dState
                         ]

instance ToJSON (ShelleyLedger.FutureGenDeleg crypto) where
  toJSON fGenDeleg =
    object [ "fGenDelegSlot" .= ShelleyLedger.fGenDelegSlot fGenDeleg
           , "fGenDelegGenKeyHash" .= ShelleyLedger.fGenDelegGenKeyHash fGenDeleg
           ]

instance Crypto.Crypto crypto => ToJSON (Shelley.GenDelegs crypto) where
  toJSON (Shelley.GenDelegs delegs) = toJSON delegs

instance ToJSON (Shelley.InstantaneousRewards crypto) where
  toJSON iRwds = object [ "iRReserves" .= Shelley.iRReserves iRwds
                        , "iRTreasury" .= Shelley.iRTreasury iRwds
                        ]

instance ToJSON (Bimap Shelley.Ptr (Shelley.Credential Shelley.Staking crypto)) where
  toJSON (MkBiMap ptsStakeM stakePtrSetM) =
    object [ "stakedCreds" .= Map.toList ptsStakeM
           , "credPtrR" .= toJSON stakePtrSetM
           ]
instance ToJSON Shelley.Ptr where
  toJSON (Shelley.Ptr slotNo txIndex certIndex) =
    object [ "slot" .= unSlotNo slotNo
           , "txIndex" .= txIndex
           , "certIndex" .= certIndex
           ]


instance Crypto.Crypto crypto => ToJSON (Shelley.PState crypto) where
  toJSON pState = object [ "pParams pState" .= Shelley._pParams pState
                         , "fPParams pState" .= Shelley._fPParams pState
                         , "retiring pState" .= Shelley._retiring pState
                         ]

instance ( Consensus.ShelleyBasedEra era
         , ToJSON (Core.TxOut era)
         ) => ToJSON (Shelley.UTxO era) where
  toJSON (Shelley.UTxO utxo) = toJSON utxo

instance ( Consensus.ShelleyBasedEra era
         , ToJSON (Core.Value era)
         ) => ToJSON (Shelley.TxOut era) where
  toJSON (Shelley.TxOut addr amount) =
    object
      [ "address" .= addr
      , "amount" .= amount
      ]

instance Crypto.Crypto crypto => ToJSON (Shelley.TxIn crypto) where
  toJSON = toJSON . txInToText

instance Crypto.Crypto crypto => ToJSONKey (Shelley.TxIn crypto) where
  toJSONKey = toJSONKeyText txInToText

txInToText :: Crypto.Crypto crypto => Shelley.TxIn crypto -> Text
txInToText (Shelley.TxIn (Shelley.TxId txidHash) ix) =
  hashToText (SafeHash.extractHash txidHash)
    <> Text.pack "#"
    <> Text.pack (show ix)

hashToText :: Crypto.Hash crypto a -> Text
hashToText = Text.decodeLatin1 . Crypto.hashToBytesAsHex

instance Crypto.Crypto crypto => ToJSON (Shelley.NonMyopic crypto) where
  toJSON nonMy = object [ "likelihoodsNM" .= Shelley.likelihoodsNM nonMy
                        , "rewardPotNM" .= Shelley.rewardPotNM nonMy
                        ]

instance ToJSON Shelley.Likelihood where
  toJSON (Shelley.Likelihood llhd) =
    toJSON $ fmap (\(Shelley.LogWeight f) -> exp $ realToFrac f :: Double) llhd

instance Crypto.Crypto crypto => ToJSON (Shelley.SnapShots crypto) where
  toJSON ss = object [ "pstakeMark" .= Shelley._pstakeMark ss
                     , "pstakeSet" .= Shelley._pstakeSet ss
                     , "pstakeGo" .= Shelley._pstakeGo ss
                     , "feeSS" .= Shelley._feeSS ss
                     ]

instance Crypto.Crypto crypto => ToJSON (Shelley.SnapShot crypto) where
  toJSON ss = object [ "stake" .= Shelley._stake ss
                     , "delegations" .= ShelleyEpoch._delegations ss
                     , "poolParams" .= Shelley._poolParams ss
                     ]

instance ToJSON (Shelley.Stake crypto) where
  toJSON (Shelley.Stake s) = toJSON s

instance Crypto.Crypto crypto => ToJSON (Shelley.RewardUpdate crypto) where
  toJSON rUpdate = object [ "deltaT" .= Shelley.deltaT rUpdate
                          , "deltaR" .= Shelley.deltaR rUpdate
                          , "rs" .= Shelley.rs rUpdate
                          , "deltaF" .= Shelley.deltaF rUpdate
                          , "nonMyopic" .= Shelley.nonMyopic rUpdate
                          ]

instance Crypto.Crypto crypto => ToJSON (Shelley.PulsingRewUpdate crypto) where
  toJSON (Shelley.Pulsing _ _) = Aeson.Null
  toJSON (Shelley.Complete ru) = toJSON ru

instance ToJSON Shelley.DeltaCoin where
  toJSON (Shelley.DeltaCoin i) = toJSON i

instance Crypto.Crypto crypto => ToJSON (Shelley.PoolDistr crypto) where
  toJSON (Shelley.PoolDistr m) = toJSON m

instance ToJSON (Shelley.IndividualPoolStake crypto) where
  toJSON indivPoolStake =
    object [ "individualPoolStake" .= Shelley.individualPoolStake indivPoolStake
           , "individualPoolStakeVrf" .= Shelley.individualPoolStakeVrf indivPoolStake
           ]

instance ToJSON (Shelley.Reward crypto) where
  toJSON reward =
     object [ "rewardType" .= Shelley.rewardType reward
            , "rewardPool" .= Shelley.rewardPool reward
            , "rewardAmount" .= Shelley.rewardAmount reward
            ]

instance ToJSON Shelley.RewardType where
  toJSON Shelley.MemberReward = "MemberReward"
  toJSON Shelley.LeaderReward = "LeaderReward"

instance ToJSON (SafeHash.SafeHash c a) where
  toJSON = toJSON . SafeHash.extractHash

