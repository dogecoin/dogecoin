-- | The 'NetworkId' type and related functions
--
module Cardano.Api.NetworkId (
    -- * Network types
    NetworkId(..),
    NetworkMagic(..),
    toNetworkMagic,
    mainnetNetworkMagic,

    -- * Internal conversion functions
    toByronProtocolMagicId,
    toByronNetworkMagic,
    toByronRequiresNetworkMagic,
    toShelleyNetwork,
    fromShelleyNetwork,
  ) where

import           Prelude

import           Ouroboros.Network.Magic (NetworkMagic (..))

import qualified Cardano.Crypto.ProtocolMagic as Byron
                   (ProtocolMagicId(..), RequiresNetworkMagic(..))
import qualified Cardano.Chain.Common as Byron (NetworkMagic(..))
import qualified Cardano.Chain.Genesis as Byron (mainnetProtocolMagicId)

import qualified Shelley.Spec.Ledger.BaseTypes as Shelley (Network(..))


-- ----------------------------------------------------------------------------
-- NetworkId type
--

data NetworkId = Mainnet
               | Testnet !NetworkMagic
  deriving (Eq, Show)

toNetworkMagic :: NetworkId -> NetworkMagic
toNetworkMagic (Testnet nm) = nm
toNetworkMagic Mainnet      = mainnetNetworkMagic

mainnetNetworkMagic :: NetworkMagic
mainnetNetworkMagic = NetworkMagic
                    . Byron.unProtocolMagicId
                    $ Byron.mainnetProtocolMagicId


-- ----------------------------------------------------------------------------
-- Byron conversion functions
--

toByronProtocolMagicId :: NetworkId -> Byron.ProtocolMagicId
toByronProtocolMagicId Mainnet = Byron.mainnetProtocolMagicId
toByronProtocolMagicId (Testnet (NetworkMagic pm)) = Byron.ProtocolMagicId pm

toByronNetworkMagic :: NetworkId -> Byron.NetworkMagic
toByronNetworkMagic Mainnet                     = Byron.NetworkMainOrStage
toByronNetworkMagic (Testnet (NetworkMagic nm)) = Byron.NetworkTestnet nm

toByronRequiresNetworkMagic :: NetworkId -> Byron.RequiresNetworkMagic
toByronRequiresNetworkMagic Mainnet   = Byron.RequiresNoMagic
toByronRequiresNetworkMagic Testnet{} = Byron.RequiresMagic


-- ----------------------------------------------------------------------------
-- Shelley conversion functions
--

toShelleyNetwork :: NetworkId -> Shelley.Network
toShelleyNetwork  Mainnet    = Shelley.Mainnet
toShelleyNetwork (Testnet _) = Shelley.Testnet

fromShelleyNetwork :: Shelley.Network -> NetworkMagic -> NetworkId
fromShelleyNetwork Shelley.Testnet nm = Testnet nm
fromShelleyNetwork Shelley.Mainnet nm
  | nm == mainnetNetworkMagic = Mainnet
  | otherwise = error "fromShelleyNetwork Mainnet: wrong mainnet network magic"

