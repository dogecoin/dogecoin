-- | Node client support for the Byron protocol
--
module Cardano.Api.Protocol.Byron
  ( -- * Client support
    mkNodeClientProtocolByron
  , mkSomeNodeClientProtocolByron
  ) where

import           Cardano.Api.Protocol.Types (ProtocolClient(..),
                     ProtocolClientInfoArgs(ProtocolClientInfoArgsByron),
                     SomeNodeClientProtocol(..))
import           Cardano.Chain.Slotting (EpochSlots)
import           Ouroboros.Consensus.Cardano.ByronHFC

mkNodeClientProtocolByron :: EpochSlots
                          -> ProtocolClientInfoArgs ByronBlockHFC
mkNodeClientProtocolByron = ProtocolClientInfoArgsByron

mkSomeNodeClientProtocolByron :: EpochSlots
                              -> SomeNodeClientProtocol
mkSomeNodeClientProtocolByron epochSlots =
    SomeNodeClientProtocol
      (mkNodeClientProtocolByron epochSlots)
