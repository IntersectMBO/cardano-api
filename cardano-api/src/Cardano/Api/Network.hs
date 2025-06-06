module Cardano.Api.Network
  ( -- * Network types
    NetworkId (..)
  , NetworkMagic (..)
  , fromNetworkMagic
  , toNetworkMagic
  , mainnetNetworkMagic

    -- * @network@ reexports
  , LedgerPeerSnapshot (..)
  , Target (..)
  , Serialised (..)
  , SubmitResult (..)
  )
where

import Cardano.Api.Network.Internal.NetworkId
import Cardano.Api.Network.Internal.Reexport
