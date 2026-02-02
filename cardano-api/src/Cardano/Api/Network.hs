module Cardano.Api.Network
  ( -- * Network types
    NetworkId (..)
  , NetworkMagic (..)
  , fromNetworkMagic
  , toNetworkMagic
  , mainnetNetworkMagic

    -- * @network@ reexports
  , LedgerPeerSnapshot (..)
  , SomeLedgerPeerSnapshot (..)
  , LedgerPeersKind (..)
  , Target (..)
  , Serialised (..)
  , SubmitResult (..)

    -- * Internal conversion functions
  , toByronProtocolMagicId
  , toByronNetworkMagic
  , toByronRequiresNetworkMagic
  , toShelleyNetwork
  , fromShelleyNetwork
  )
where

import Cardano.Api.Network.Internal.NetworkId
import Cardano.Api.Network.Internal.Reexport
