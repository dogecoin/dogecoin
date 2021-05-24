-- | Node client support for the Cardano protocol
--
module Cardano.Api.Protocol.Cardano
  ( -- * Client support
    mkNodeClientProtocolCardano
  , mkSomeNodeClientProtocolCardano
  ) where

import           Cardano.Api.Protocol.Types (ProtocolClient(..),
                     ProtocolClientInfoArgs(ProtocolClientInfoArgsCardano),
                     SomeNodeClientProtocol (..))
import           Cardano.Chain.Slotting (EpochSlots)
import           Ouroboros.Consensus.Cardano.Block (CardanoBlock)
import           Ouroboros.Consensus.Shelley.Protocol (StandardCrypto)

mkNodeClientProtocolCardano :: EpochSlots
                            -> ProtocolClientInfoArgs (CardanoBlock StandardCrypto)
mkNodeClientProtocolCardano = ProtocolClientInfoArgsCardano

mkSomeNodeClientProtocolCardano :: EpochSlots
                                -> SomeNodeClientProtocol
mkSomeNodeClientProtocolCardano epochSlots =
    SomeNodeClientProtocol
      (mkNodeClientProtocolCardano epochSlots)
