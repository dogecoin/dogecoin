{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Node.Protocol.Cardano
  ( mkSomeConsensusProtocolCardano

    -- * Errors
  , CardanoProtocolInstantiationError(..)
  , renderCardanoProtocolInstantiationError
  ) where

import           Prelude

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT)
import qualified Data.Text as T

import qualified Cardano.Chain.Update as Byron

import           Ouroboros.Consensus.Cardano
import qualified Ouroboros.Consensus.Cardano as Consensus
import qualified Ouroboros.Consensus.Cardano.CanHardFork as Consensus
import           Ouroboros.Consensus.HardFork.Combinator.Condense ()

import           Ouroboros.Consensus.Cardano.Condense ()

import           Cardano.Api.Orphans ()
import           Cardano.Api.Protocol.Types
import           Cardano.Node.Types

import           Cardano.Tracing.OrphanInstances.Byron ()
import           Cardano.Tracing.OrphanInstances.Shelley ()

import           Cardano.Node.Protocol.Alonzo (AlonzoProtocolInstantiationError, readAlonzoGenesis,
                   renderAlonzoProtocolInstantiationError)
import qualified Cardano.Node.Protocol.Byron as Byron
import qualified Cardano.Node.Protocol.Shelley as Shelley

import           Cardano.Node.Protocol.Types

------------------------------------------------------------------------------
-- Real Cardano protocol
--

-- | Make 'SomeConsensusProtocol' using the Cardano instance.
--
-- The Cardano protocol instance is currently the sequential composition of
-- the Byron and Shelley protocols, and will likely be extended in future
-- with further sequentially composed protocol revisions.
--
-- The use of 'SomeConsensusProtocol' lets us handle multiple protocols in a
-- generic way.
--
-- This also serves a purpose as a sanity check that we have all the necessary
-- type class instances available.
--
mkSomeConsensusProtocolCardano
  :: NodeByronProtocolConfiguration
  -> NodeShelleyProtocolConfiguration
  -> NodeHardForkProtocolConfiguration
  -> Maybe ProtocolFilepaths
  -> ExceptT CardanoProtocolInstantiationError IO SomeConsensusProtocol
mkSomeConsensusProtocolCardano NodeByronProtocolConfiguration {
                             npcByronGenesisFile,
                             npcByronGenesisFileHash,
                             npcByronReqNetworkMagic,
                             npcByronPbftSignatureThresh,
                             npcByronApplicationName,
                             npcByronApplicationVersion,
                             npcByronSupportedProtocolVersionMajor,
                             npcByronSupportedProtocolVersionMinor,
                             npcByronSupportedProtocolVersionAlt
                           }
                           NodeShelleyProtocolConfiguration {
                             npcShelleyGenesisFile,
                             npcShelleyGenesisFileHash
                           }
                           NodeHardForkProtocolConfiguration {
                             npcTestEnableDevelopmentHardForkEras,
                             npcTestShelleyHardForkAtEpoch,
                             npcTestShelleyHardForkAtVersion,
                             npcTestAllegraHardForkAtEpoch,
                             npcTestAllegraHardForkAtVersion,
                             npcTestMaryHardForkAtEpoch,
                             npcTestMaryHardForkAtVersion,
                             npcTestAlonzoHardForkAtEpoch,
                             npcTestAlonzoHardForkAtVersion
                           }
                           files = do
    byronGenesis <-
      firstExceptT CardanoProtocolInstantiationErrorByron $
        Byron.readGenesis npcByronGenesisFile
                          npcByronGenesisFileHash
                          npcByronReqNetworkMagic

    byronLeaderCredentials <-
      firstExceptT CardanoProtocolInstantiationErrorByron $
        Byron.readLeaderCredentials byronGenesis files

    (shelleyGenesis, shelleyGenesisHash) <-
      firstExceptT CardanoProtocolInstantiationErrorShelley $
        Shelley.readGenesis npcShelleyGenesisFile
                            npcShelleyGenesisFileHash

    shelleyLeaderCredentials <-
      firstExceptT CardanoProtocolInstantiationErrorShelley $
        Shelley.readLeaderCredentials files

    -- We choose to include the Alonzo relevant fields in the Shelley genesis
    -- and therefore avoid creating a separate Alonzo genesis file
    let GenesisFile shelleyGenFile = npcShelleyGenesisFile
    alonzoGenesis <- firstExceptT CardanoProtocolInstantiationErrorAlonzo
                   $ readAlonzoGenesis shelleyGenFile

    --TODO: all these protocol versions below are confusing and unnecessary.
    -- It could and should all be automated and these config entries eliminated.
    return $!
      SomeConsensusProtocol CardanoBlockType $ ProtocolInfoArgsCardano
        Consensus.ProtocolParamsByron {
          byronGenesis = byronGenesis,
          byronPbftSignatureThreshold =
            PBftSignatureThreshold <$> npcByronPbftSignatureThresh,

          -- This is /not/ the Byron protocol version. It is the protocol
          -- version that this node will use in blocks it creates. It is used
          -- in the Byron update mechanism to signal that this block-producing
          -- node is ready to move to the new protocol. For example, when the
          -- protocol version (according to the ledger state) is 0, this setting
          -- should be 1 when we are ready to move. Similarly when the current
          -- protocol version is 1, this should be 2 to indicate we are ready
          -- to move into the Shelley era.
          byronProtocolVersion =
            Byron.ProtocolVersion
              npcByronSupportedProtocolVersionMajor
              npcByronSupportedProtocolVersionMinor
              npcByronSupportedProtocolVersionAlt,
          byronSoftwareVersion =
            Byron.SoftwareVersion
              npcByronApplicationName
              npcByronApplicationVersion,
          byronLeaderCredentials =
            byronLeaderCredentials
        }
        Consensus.ProtocolParamsShelleyBased {
          shelleyBasedGenesis = shelleyGenesis,
          shelleyBasedInitialNonce =
            Shelley.genesisHashToPraosNonce shelleyGenesisHash,
            shelleyBasedLeaderCredentials = shelleyLeaderCredentials
        }
        Consensus.ProtocolParamsShelley {
          -- This is /not/ the Shelley protocol version. It is the protocol
          -- version that this node will declare that it understands, when it
          -- is in the Shelley era. That is, it is the version of protocol
          -- /after/ Shelley, i.e. Allegra.
          shelleyProtVer =
            ProtVer 3 0
        }
        Consensus.ProtocolParamsAllegra {
          -- This is /not/ the Allegra protocol version. It is the protocol
          -- version that this node will declare that it understands, when it
          -- is in the Allegra era. That is, it is the version of protocol
          -- /after/ Allegra, i.e. Mary.
          allegraProtVer =
            ProtVer 4 0
        }
        Consensus.ProtocolParamsMary {
          -- This is /not/ the Mary protocol version. It is the protocol
          -- version that this node will declare that it understands, when it
          -- is in the Mary era. Since Mary is currently the last known
          -- protocol version then this is also the Mary protocol version.
          --
          -- During testing of the Alonzo era, we conditionally declare that we
          -- know about the Alonzo era. We do so only when a config option for
          -- testing development/unstable eras is used. This lets us include
          -- not-yet-ready eras in released node versions without mainnet nodes
          -- prematurely advertising that they could hard fork into the new era.
          maryProtVer =
            if npcTestEnableDevelopmentHardForkEras
              then ProtVer 5 0  -- Advertise we can support Alonzo
              else ProtVer 4 0  -- Otherwise only advertise we know about Mary.
        }
        Consensus.ProtocolParamsAlonzo {
          -- This is /not/ the Alonzo protocol version. It is the protocol
          -- version that this node will declare that it understands, when it
          -- is in the Alonzo era. Since Alonzo is currently the last known
          -- protocol version then this is also the Alonzo protocol version.
          alonzoProtVer = ProtVer 5 0
        }

        -- ProtocolParamsTransition specifies the parameters needed to transition between two eras
        -- The comments below also apply for the Shelley -> Allegra and Allegra -> Mary hard forks.
        -- Byron to Shelley hard fork parameters
        Consensus.ProtocolTransitionParamsShelleyBased {
          transitionTranslationContext = (),
          transitionTrigger =
            -- What will trigger the Byron -> Shelley hard fork?
            case npcTestShelleyHardForkAtEpoch of

               -- This specifies the major protocol version number update that will
               -- trigger us moving to the Shelley protocol.
               --
               -- Version 0 is Byron with Ouroboros classic
               -- Version 1 is Byron with Ouroboros Permissive BFT
               -- Version 2 is Shelley
               -- Version 3 is Allegra
               -- Version 4 is Mary
               -- Version 5 is Alonzo
               --
               -- But we also provide an override to allow for simpler test setups
               -- such as triggering at the 0 -> 1 transition .
               --
               Nothing -> Consensus.TriggerHardForkAtVersion
                            (maybe 2 fromIntegral npcTestShelleyHardForkAtVersion)

               -- Alternatively, for testing we can transition at a specific epoch.
               --
               Just epochNo -> Consensus.TriggerHardForkAtEpoch epochNo
        }
        -- Shelley to Allegra hard fork parameters
        Consensus.ProtocolTransitionParamsShelleyBased {
          transitionTranslationContext = (),
          transitionTrigger =
            case npcTestAllegraHardForkAtEpoch of
               Nothing -> Consensus.TriggerHardForkAtVersion
                            (maybe 3 fromIntegral npcTestAllegraHardForkAtVersion)
               Just epochNo -> Consensus.TriggerHardForkAtEpoch epochNo
        }
        -- Allegra to Mary hard fork parameters
        Consensus.ProtocolTransitionParamsShelleyBased {
          transitionTranslationContext = (),
          transitionTrigger =
            case npcTestMaryHardForkAtEpoch of
               Nothing -> Consensus.TriggerHardForkAtVersion
                            (maybe 4 fromIntegral npcTestMaryHardForkAtVersion)
               Just epochNo -> Consensus.TriggerHardForkAtEpoch epochNo
        }
        -- Mary to Alonzo hard fork parameters
        Consensus.ProtocolTransitionParamsShelleyBased {
          transitionTranslationContext = alonzoGenesis,
          transitionTrigger =
            case npcTestAlonzoHardForkAtEpoch of
               Nothing -> Consensus.TriggerHardForkAtVersion
                            (maybe 5 fromIntegral npcTestAlonzoHardForkAtVersion)
               Just epochNo -> Consensus.TriggerHardForkAtEpoch epochNo
        }


------------------------------------------------------------------------------
-- Errors
--

data CardanoProtocolInstantiationError =
       CardanoProtocolInstantiationErrorByron
         Byron.ByronProtocolInstantiationError

     | CardanoProtocolInstantiationErrorShelley
         Shelley.ShelleyProtocolInstantiationError
     | CardanoProtocolInstantiationErrorAlonzo
         AlonzoProtocolInstantiationError
  deriving Show

renderCardanoProtocolInstantiationError :: CardanoProtocolInstantiationError
                                        -> T.Text
renderCardanoProtocolInstantiationError
  (CardanoProtocolInstantiationErrorByron err) =
    Byron.renderByronProtocolInstantiationError err

renderCardanoProtocolInstantiationError
  (CardanoProtocolInstantiationErrorShelley err) =
    Shelley.renderShelleyProtocolInstantiationError err

renderCardanoProtocolInstantiationError
  (CardanoProtocolInstantiationErrorAlonzo err) =
    renderAlonzoProtocolInstantiationError err
