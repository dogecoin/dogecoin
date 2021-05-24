{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.Protocol.Types
  ( BlockType(..)
  , Protocol(..)
  , ProtocolInfoArgs(..)
  , ProtocolClient(..)
  , ProtocolClientInfoArgs(..)
  , SomeNodeClientProtocol(..)
  ) where

import           Cardano.Prelude

import           Cardano.Chain.Slotting (EpochSlots)

import           Ouroboros.Consensus.Cardano
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.ByronHFC (ByronBlockHFC)
import           Ouroboros.Consensus.Cardano.Node
import           Ouroboros.Consensus.HardFork.Combinator.Embed.Unary
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolClientInfo (..), ProtocolInfo (..))
import           Ouroboros.Consensus.Node.Run (RunNode)
import           Ouroboros.Consensus.Shelley.ShelleyHFC (ShelleyBlockHFC)
import           Ouroboros.Consensus.Util.IOLike (IOLike)

class (RunNode blk, IOLike m) => Protocol m blk where
  data ProtocolInfoArgs m blk
  protocolInfo :: ProtocolInfoArgs m blk -> ProtocolInfo m blk

-- | Node client support for each consensus protocol.
--
-- This is like 'Protocol' but for clients of the node, so with less onerous
-- requirements than to run a node.
--
class (RunNode blk) => ProtocolClient blk where
  data ProtocolClientInfoArgs blk
  protocolClientInfo :: ProtocolClientInfoArgs blk -> ProtocolClientInfo blk


-- | Run PBFT against the Byron ledger
instance IOLike m => Protocol m ByronBlockHFC where
  data ProtocolInfoArgs m ByronBlockHFC = ProtocolInfoArgsByron ProtocolParamsByron
  protocolInfo (ProtocolInfoArgsByron params) = inject $ protocolInfoByron params

instance IOLike m => Protocol m (CardanoBlock StandardCrypto) where
  data ProtocolInfoArgs m (CardanoBlock StandardCrypto) =
         ProtocolInfoArgsCardano
           ProtocolParamsByron
          (ProtocolParamsShelleyBased StandardShelley)
           ProtocolParamsShelley
           ProtocolParamsAllegra
           ProtocolParamsMary
           ProtocolParamsAlonzo
          (ProtocolTransitionParamsShelleyBased StandardShelley)
          (ProtocolTransitionParamsShelleyBased StandardAllegra)
          (ProtocolTransitionParamsShelleyBased StandardMary)
          (ProtocolTransitionParamsShelleyBased StandardAlonzo)

  protocolInfo (ProtocolInfoArgsCardano
               paramsByron
               paramsShelleyBased
               paramsShelley
               paramsAllegra
               paramsMary
               paramsAlonzo
               paramsByronShelley
               paramsShelleyAllegra
               paramsAllegraMary
               paramsMaryAlonzo) =
    protocolInfoCardano
      paramsByron
      paramsShelleyBased
      paramsShelley
      paramsAllegra
      paramsMary
      paramsAlonzo
      paramsByronShelley
      paramsShelleyAllegra
      paramsAllegraMary
      paramsMaryAlonzo

instance ProtocolClient ByronBlockHFC where
  data ProtocolClientInfoArgs ByronBlockHFC =
    ProtocolClientInfoArgsByron EpochSlots
  protocolClientInfo (ProtocolClientInfoArgsByron epochSlots) =
    inject $ protocolClientInfoByron epochSlots

instance ProtocolClient (CardanoBlock StandardCrypto) where
  data ProtocolClientInfoArgs (CardanoBlock StandardCrypto) =
    ProtocolClientInfoArgsCardano EpochSlots
  protocolClientInfo (ProtocolClientInfoArgsCardano epochSlots) =
    protocolClientInfoCardano epochSlots

instance IOLike m => Protocol m (ShelleyBlockHFC StandardShelley) where
  data ProtocolInfoArgs m (ShelleyBlockHFC StandardShelley) = ProtocolInfoArgsShelley
    (ProtocolParamsShelleyBased StandardShelley)
    ProtocolParamsShelley
  protocolInfo (ProtocolInfoArgsShelley paramsShelleyBased paramsShelley) =
    inject $ protocolInfoShelley paramsShelleyBased paramsShelley

instance ProtocolClient (ShelleyBlockHFC StandardShelley) where
  data ProtocolClientInfoArgs (ShelleyBlockHFC StandardShelley) =
    ProtocolClientInfoArgsShelley
  protocolClientInfo ProtocolClientInfoArgsShelley =
    inject protocolClientInfoShelley

data BlockType blk where
  ByronBlockType :: BlockType ByronBlockHFC
  ShelleyBlockType :: BlockType (ShelleyBlockHFC StandardShelley)
  CardanoBlockType :: BlockType (CardanoBlock StandardCrypto)

deriving instance Eq (BlockType blk)
deriving instance Show (BlockType blk)

data SomeNodeClientProtocol where

     SomeNodeClientProtocol
       :: (RunNode blk, ProtocolClient blk)
       => ProtocolClientInfoArgs blk
       -> SomeNodeClientProtocol
