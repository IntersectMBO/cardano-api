-- | The 'NetworkId' type and related functions
module Cardano.Api.Network.Internal.NetworkId
  ( -- * Network types
    NetworkId (..)
  , NetworkMagic (..)
  , fromNetworkMagic
  , toNetworkMagic
  , mainnetNetworkMagic

    -- * Internal conversion functions
  , toByronProtocolMagicId
  , toByronNetworkMagic
  , toByronRequiresNetworkMagic
  , toShelleyNetwork
  , fromShelleyNetwork
  )
where

import Cardano.Chain.Common qualified as Byron (NetworkMagic (..))
import Cardano.Chain.Genesis qualified as Byron (mainnetProtocolMagicId)
import Cardano.Crypto.ProtocolMagic qualified as Byron
  ( ProtocolMagicId (..)
  , RequiresNetworkMagic (..)
  )
import Cardano.Ledger.BaseTypes qualified as Shelley (Network (..))
import Ouroboros.Network.Magic (NetworkMagic (..))

-- ----------------------------------------------------------------------------
-- NetworkId type
--

data NetworkId
  = Mainnet
  | Testnet !NetworkMagic
  deriving (Eq, Show)

fromNetworkMagic :: NetworkMagic -> NetworkId
fromNetworkMagic nm =
  if nm == mainnetNetworkMagic
    then Mainnet
    else Testnet nm

toNetworkMagic :: NetworkId -> NetworkMagic
toNetworkMagic (Testnet nm) = nm
toNetworkMagic Mainnet = mainnetNetworkMagic

mainnetNetworkMagic :: NetworkMagic
mainnetNetworkMagic =
  NetworkMagic
    . Byron.unProtocolMagicId
    $ Byron.mainnetProtocolMagicId

-- ----------------------------------------------------------------------------
-- Byron conversion functions
--

toByronProtocolMagicId :: NetworkId -> Byron.ProtocolMagicId
toByronProtocolMagicId Mainnet = Byron.mainnetProtocolMagicId
toByronProtocolMagicId (Testnet (NetworkMagic pm)) = Byron.ProtocolMagicId pm

toByronNetworkMagic :: NetworkId -> Byron.NetworkMagic
toByronNetworkMagic Mainnet = Byron.NetworkMainOrStage
toByronNetworkMagic (Testnet (NetworkMagic nm)) = Byron.NetworkTestnet nm

toByronRequiresNetworkMagic :: NetworkId -> Byron.RequiresNetworkMagic
toByronRequiresNetworkMagic Mainnet = Byron.RequiresNoMagic
toByronRequiresNetworkMagic Testnet{} = Byron.RequiresMagic

-- ----------------------------------------------------------------------------
-- Shelley conversion functions
--

toShelleyNetwork :: NetworkId -> Shelley.Network
toShelleyNetwork Mainnet = Shelley.Mainnet
toShelleyNetwork (Testnet _) = Shelley.Testnet

fromShelleyNetwork :: Shelley.Network -> NetworkMagic -> NetworkId
fromShelleyNetwork Shelley.Testnet nm = Testnet nm
fromShelleyNetwork Shelley.Mainnet nm
  | nm == mainnetNetworkMagic = Mainnet
  | otherwise = error "fromShelleyNetwork Mainnet: wrong mainnet network magic"
