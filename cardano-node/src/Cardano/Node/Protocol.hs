{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Node.Protocol
  ( mkConsensusProtocol
  , SomeConsensusProtocol(..)
  , ProtocolInstantiationError(..)
  , renderProtocolInstantiationError
  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except.Extra (firstExceptT)

import           Cardano.Node.Configuration.POM (NodeConfiguration (..))
import           Cardano.Node.Types

import           Cardano.Node.Orphans ()
import           Cardano.Node.Protocol.Byron
import           Cardano.Node.Protocol.Cardano
import           Cardano.Node.Protocol.Shelley
import           Cardano.Node.Protocol.Types (SomeConsensusProtocol (..))
------------------------------------------------------------------------------
-- Conversions from configuration into specific protocols and their params
--

mkConsensusProtocol
  :: NodeConfiguration
  -> ExceptT ProtocolInstantiationError IO SomeConsensusProtocol
mkConsensusProtocol NodeConfiguration{ncProtocolConfig, ncProtocolFiles} =
    case ncProtocolConfig of

      NodeProtocolConfigurationByron config ->
        firstExceptT ByronProtocolInstantiationError $
          mkSomeConsensusProtocolByron config (Just ncProtocolFiles)

      NodeProtocolConfigurationShelley config ->
        firstExceptT ShelleyProtocolInstantiationError $
          mkSomeConsensusProtocolShelley config (Just ncProtocolFiles)

      NodeProtocolConfigurationCardano byronConfig
                                       shelleyConfig
                                       hardForkConfig ->
        firstExceptT CardanoProtocolInstantiationError $
          mkSomeConsensusProtocolCardano
            byronConfig
            shelleyConfig
            hardForkConfig
            (Just ncProtocolFiles)

------------------------------------------------------------------------------
-- Errors
--

data ProtocolInstantiationError =
    ByronProtocolInstantiationError   ByronProtocolInstantiationError
  | ShelleyProtocolInstantiationError ShelleyProtocolInstantiationError
  | CardanoProtocolInstantiationError CardanoProtocolInstantiationError
  deriving Show


renderProtocolInstantiationError :: ProtocolInstantiationError -> Text
renderProtocolInstantiationError pie =
  case pie of
    ByronProtocolInstantiationError bpie ->
      renderByronProtocolInstantiationError bpie

    ShelleyProtocolInstantiationError spie ->
      renderShelleyProtocolInstantiationError spie

    CardanoProtocolInstantiationError cpie ->
      renderCardanoProtocolInstantiationError cpie
