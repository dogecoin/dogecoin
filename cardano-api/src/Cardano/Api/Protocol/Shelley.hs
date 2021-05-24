-- | Node client support for the Shelley protocol
--
module Cardano.Api.Protocol.Shelley
  ( -- * Client support
    mkNodeClientProtocolShelley
  , mkSomeNodeClientProtocolShelley
  ) where


import           Ouroboros.Consensus.Shelley.ShelleyHFC

import           Ouroboros.Consensus.Shelley.Eras (StandardShelley)

import           Cardano.Api.Protocol.Types


mkNodeClientProtocolShelley :: ProtocolClientInfoArgs (ShelleyBlockHFC StandardShelley)
mkNodeClientProtocolShelley = ProtocolClientInfoArgsShelley


mkSomeNodeClientProtocolShelley :: SomeNodeClientProtocol
mkSomeNodeClientProtocolShelley =
    SomeNodeClientProtocol mkNodeClientProtocolShelley
