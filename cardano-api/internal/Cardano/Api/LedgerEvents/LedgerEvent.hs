{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Api.LedgerEvents.LedgerEvent
  ( LedgerEvent(..)
  , AnyProposals(..)
  , AnyRatificationState(..)
  , DRepRegistrationKind (..)
  , MIRDistributionDetails(..)
  , PoolReapDetails(..)
  , convertRetiredPoolsMap
  ) where

import           Cardano.Api.Address (StakeCredential, fromShelleyStakeCredential)
import           Cardano.Api.Block (EpochNo)
import           Cardano.Api.Keys.Shelley (Hash (..), StakePoolKey)
import           Cardano.Api.Value (Lovelace, fromShelleyLovelace)

import           Cardano.Ledger.Alonzo.Plutus.TxInfo (PlutusDebug)
import qualified Cardano.Ledger.Coin as Ledger
import qualified Cardano.Ledger.Conway.Governance as Ledger
import qualified Cardano.Ledger.Core as Ledger.Core
import qualified Cardano.Ledger.Credential as Ledger
import           Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Keys as Ledger
import           Cardano.Ledger.Shelley.Rewards (Reward)
import qualified Cardano.Ledger.TxIn as Ledger

import           Data.List.NonEmpty (NonEmpty)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)


data AnyProposals
  = forall era. Ledger.Core.EraPParams era => AnyProposals (Ledger.Proposals era)

deriving instance Show AnyProposals

data AnyRatificationState
  = forall era.  Ledger.Core.EraPParams era => AnyRatificationState (Ledger.RatifyState era)

deriving instance Show AnyRatificationState

data DRepRegistrationKind = Registration | Unregistration
  deriving Show

data LedgerEvent
  = -- | The given pool is being registered for the first time on chain.
    PoolRegistration
  | -- | The given pool already exists and is being re-registered.
    PoolReRegistration
  | -- | Incremental rewards are being computed.
    IncrementalRewardsDistribution EpochNo (Map StakeCredential (Set (Reward StandardCrypto)))
  | -- | Reward distribution has completed.
    RewardsDistribution EpochNo (Map StakeCredential (Set (Reward StandardCrypto)))
  | -- | MIR are being distributed.
    MIRDistribution MIRDistributionDetails
  | -- | Pools have been reaped and deposits refunded.
    PoolReap PoolReapDetails
    -- | A number of succeeded Plutus script evaluations.
  | SuccessfulPlutusScript (NonEmpty PlutusDebug)
    -- | A number of failed Plutus script evaluations.
  | FailedPlutusScript (NonEmpty PlutusDebug)


  -- Only events available on the Conway Era.
  -- TODO: Update the above constructors to work in the conway era.
  -- See toLedgerEventConway
    -- | Newly submittted governance proposals in a single transaction.
  | NewGovernanceProposals (Ledger.TxId StandardCrypto) AnyProposals
    -- | The current state of governance matters at the epoch boundary.
    -- I.E the current constitution, committee, protocol parameters, etc.
  | EpochBoundaryRatificationState AnyRatificationState
  | DRepRegistration DRepRegistrationKind
  deriving Show


--------------------------------------------------------------------------------
-- Event details
--------------------------------------------------------------------------------

-- | Details of fund transfers due to MIR certificates.
--
--   Note that the transfers from reserves to treasury and treasury to reserves
--   are inverse; a transfer of 100 ADA in either direction will result in a net
--   movement of 0, but we include both directions for assistance in debugging.
data MIRDistributionDetails = MIRDistributionDetails
  { mirddReservePayouts :: Map StakeCredential Lovelace,
    mirddTreasuryPayouts :: Map StakeCredential Lovelace,
    mirddReservesToTreasury :: Lovelace,
    mirddTreasuryToReserves :: Lovelace
  } deriving Show

data PoolReapDetails = PoolReapDetails
  { prdEpochNo :: EpochNo,
    -- | Refunded deposits. The pools referenced are now retired, and the
    --   'StakeCredential' accounts are credited with the deposits.
    prdRefunded :: Map StakeCredential (Map (Hash StakePoolKey) Lovelace),
    -- | Unclaimed deposits. The 'StakeCredential' referenced in this map is not
    -- actively registered at the time of the pool reaping, and as such the
    -- funds are returned to the treasury.
    prdUnclaimed :: Map StakeCredential (Map (Hash StakePoolKey) Lovelace)
  } deriving Show

convertRetiredPoolsMap
  :: Map (Ledger.StakeCredential StandardCrypto) (Map (Ledger.KeyHash Ledger.StakePool StandardCrypto) Ledger.Coin)
  -> Map StakeCredential (Map (Hash StakePoolKey) Lovelace)
convertRetiredPoolsMap =
  Map.mapKeys fromShelleyStakeCredential
    . fmap (Map.mapKeys StakePoolKeyHash . fmap fromShelleyLovelace)
