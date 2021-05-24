
module Cardano.Api.ChainSync.ClientPipelined (
      -- * Pipelined protocol type for the client
      -- | The protocol states from the point of view of the client.
      ChainSyncClientPipelined (..)
    , ClientPipelinedStIdle (..)
    , ClientStNext (..)
    , ClientPipelinedStIntersect (..)
    , ChainSyncInstruction (..)

      -- * Implementation Helpers
      -- | It's generally idiomatic to use these functions to implement your
      -- pipelined client. It aids in deciding when to make pipelined requests
      -- vs process received responses.
    , PipelineDecision(..)
    , MkPipelineDecision(..)
    , runPipelineDecision
    , constantPipelineDecision
    , pipelineDecisionMax
    , pipelineDecisionMin
    , pipelineDecisionLowHighMark

      -- * Type level natural numbers
    , N (..)
    , Nat (..)
    , natToInt

      -- * Utilities
    , mapChainSyncClientPipelined
    ) where

import           Network.TypedProtocol.Pipelined (N (..), Nat (..), natToInt)
import           Ouroboros.Network.Protocol.ChainSync.ClientPipelined
import           Ouroboros.Network.Protocol.ChainSync.PipelineDecision
