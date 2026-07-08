{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MonoLocalBinds #-}

module Cardano.Wasm.Api.Address
  ( AddressInfo (..)
  , inspectAddressImpl
  )
where

import Cardano.Api qualified as Api
import Cardano.Api.Ledger qualified as Ledger

import Data.Aeson (ToJSON (toJSON), (.=))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as Text

-- | Information about a valid address, as returned by 'inspectAddressImpl'.
newtype AddressInfo = AddressInfo
  { addressNetwork :: Ledger.Network
  -- ^ The network the address belongs to.
  }
  deriving (Show, Eq)

instance ToJSON AddressInfo where
  toJSON :: AddressInfo -> Aeson.Value
  toJSON (AddressInfo network) =
    Aeson.object
      [ "network"
          .= case network of
            Ledger.Mainnet -> "mainnet" :: Text
            Ledger.Testnet -> "testnet"
      ]

-- | Inspect an address given as a string. If the address is a well-formed
-- Shelley-era address (bech32, like @addr...@ or @addr_test...@), it returns
-- information about the address: currently, the network it belongs to.
-- Otherwise it returns 'Nothing' (serialised as @null@ in the JavaScript
-- API). It never throws.
--
-- Note that an address only encodes whether it belongs to mainnet or a
-- testnet: it is not possible to distinguish between different testnets
-- (like preprod and preview) from an address alone, because they only differ
-- in the network magic, which is not part of the address.
inspectAddressImpl :: String -> Maybe AddressInfo
inspectAddressImpl addrStr =
  case Api.deserialiseAddress (Api.AsAddress Api.AsShelleyAddr) (Text.pack addrStr) of
    Just (Api.ShelleyAddress network _ _) -> Just (AddressInfo network)
    Nothing -> Nothing
