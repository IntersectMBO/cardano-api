{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Parameters fixed in the genesis file: 'GenesisParameters'
module Cardano.Api.GenesisParameters
  ( -- * Protocol parameters fixed in the genesis file
    GenesisParameters (..)
  , EpochSize (..)

    -- * Internal conversion functions
  , fromShelleyGenesis
  )
where

import Cardano.Api.Eon.ShelleyBasedEra
import Cardano.Api.Eras
import Cardano.Api.NetworkId
import qualified Cardano.Api.ReexposeLedger as Ledger
import qualified Cardano.Ledger.Coin as L
import qualified Cardano.Ledger.Shelley.Genesis as Shelley
import Cardano.Slotting.Slot (EpochSize (..))
import Data.Time (NominalDiffTime, UTCTime)

-- ----------------------------------------------------------------------------
-- Genesis parameters
--
-- TODO: Conway era - remove GenesisParameters and use ledger types directly
data GenesisParameters era
  = GenesisParameters
  { protocolParamSystemStart :: UTCTime
  -- ^ The reference time the system started. The time of slot zero.
  -- The time epoch against which all Ouroboros time slots are measured.
  , protocolParamNetworkId :: NetworkId
  -- ^ The network identifier for this blockchain instance. This
  -- distinguishes the mainnet from testnets, and different testnets from
  -- each other.
  , protocolParamActiveSlotsCoefficient :: Rational
  -- ^ The Ouroboros Praos active slot coefficient, aka @f@.
  , protocolParamSecurity :: Int
  -- ^ The Ouroboros security parameters, aka @k@. This is the maximum
  -- number of blocks the node would ever be prepared to roll back by.
  --
  -- Clients of the node following the chain should be prepared to handle
  -- the node switching forks up to this long.
  , protocolParamEpochLength :: EpochSize
  -- ^ The number of Ouroboros time slots in an Ouroboros epoch.
  , protocolParamSlotLength :: NominalDiffTime
  -- ^ The time duration of a slot.
  , protocolParamSlotsPerKESPeriod :: Int
  -- ^ For Ouroboros Praos, the length of a KES period as a number of time
  -- slots. The KES keys get evolved once per KES period.
  , protocolParamMaxKESEvolutions :: Int
  -- ^ The maximum number of times a KES key can be evolved before it is
  -- no longer considered valid. This can be less than the maximum number
  -- of times given the KES key size. For example the mainnet KES key size
  -- would allow 64 evolutions, but the max KES evolutions param is 62.
  , protocolParamUpdateQuorum :: Int
  -- ^ In the Shelley era, prior to decentralised governance, this is the
  -- number of genesis key delegates that need to agree for an update
  -- proposal to be enacted.
  , protocolParamMaxLovelaceSupply :: L.Coin
  -- ^ The maximum supply for Lovelace. This determines the initial value
  -- of the reserves.
  , protocolInitialUpdateableProtocolParameters :: Ledger.PParams (ShelleyLedgerEra era)
  -- ^ The initial values of the updateable 'ProtocolParameters'.
  }

-- ----------------------------------------------------------------------------
-- Conversion functions
--

fromShelleyGenesis :: Shelley.ShelleyGenesis Ledger.StandardCrypto -> GenesisParameters ShelleyEra
fromShelleyGenesis
  sg@Shelley.ShelleyGenesis
    { Shelley.sgSystemStart
    , Shelley.sgNetworkMagic
    , Shelley.sgNetworkId
    , Shelley.sgActiveSlotsCoeff
    , Shelley.sgSecurityParam
    , Shelley.sgEpochLength
    , Shelley.sgSlotsPerKESPeriod
    , Shelley.sgMaxKESEvolutions
    , Shelley.sgSlotLength
    , Shelley.sgUpdateQuorum
    , Shelley.sgMaxLovelaceSupply
    , Shelley.sgGenDelegs = _ -- unused, might be of interest
    , Shelley.sgInitialFunds = _ -- unused, not retained by the node
    , Shelley.sgStaking = _ -- unused, not retained by the node
    } =
    GenesisParameters
      { protocolParamSystemStart = sgSystemStart
      , protocolParamNetworkId =
          fromShelleyNetwork
            sgNetworkId
            (NetworkMagic sgNetworkMagic)
      , protocolParamActiveSlotsCoefficient =
          Ledger.unboundRational
            sgActiveSlotsCoeff
      , protocolParamSecurity = fromIntegral sgSecurityParam
      , protocolParamEpochLength = sgEpochLength
      , protocolParamSlotLength = Shelley.fromNominalDiffTimeMicro sgSlotLength
      , protocolParamSlotsPerKESPeriod = fromIntegral sgSlotsPerKESPeriod
      , protocolParamMaxKESEvolutions = fromIntegral sgMaxKESEvolutions
      , protocolParamUpdateQuorum = fromIntegral sgUpdateQuorum
      , protocolParamMaxLovelaceSupply = L.Coin $ fromIntegral sgMaxLovelaceSupply
      , protocolInitialUpdateableProtocolParameters = Shelley.sgProtocolParams sg
      }
