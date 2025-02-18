{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Api.Internal.LedgerEvents.LedgerEvent
  ( LedgerEvent (..)
  , AnyProposals (..)
  , AnyRatificationState (..)
  , MIRDistributionDetails (..)
  , PoolReapDetails (..)
  , convertRetiredPoolsMap
  )
where

import Cardano.Api.Internal.Address (StakeCredential, fromShelleyStakeCredential)
import Cardano.Api.Internal.Block (EpochNo)
import Cardano.Api.Internal.Keys.Shelley (Hash (..), StakePoolKey)

import Cardano.Ledger.Coin qualified as L
import Cardano.Ledger.Coin qualified as Ledger
import Cardano.Ledger.Conway.Governance qualified as Ledger
import Cardano.Ledger.Core qualified as Ledger.Core
import Cardano.Ledger.Credential qualified as Ledger
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Keys qualified as Ledger
import Cardano.Ledger.Plutus.Evaluate (PlutusWithContext)
import Cardano.Ledger.Shelley.Rewards (Reward)
import Cardano.Ledger.TxIn qualified as Ledger

import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)

data AnyProposals
  = forall era. Ledger.Core.EraPParams era => AnyProposals (Ledger.Proposals era)

deriving instance Show AnyProposals

data AnyRatificationState
  = forall era. Ledger.Core.EraPParams era => AnyRatificationState (Ledger.RatifyState era)

deriving instance Show AnyRatificationState

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
  | -- | A number of succeeded Plutus script evaluations.
    SuccessfulPlutusScript (NonEmpty (PlutusWithContext StandardCrypto))
  | -- | A number of failed Plutus script evaluations.
    FailedPlutusScript (NonEmpty (PlutusWithContext StandardCrypto))
  | -- Only events available on the Conway Era.
    -- TODO: Update the above constructors to work in the conway era.
    -- See toLedgerEventConway

    -- | Newly submittted governance proposals in a single transaction.
    NewGovernanceProposals (Ledger.TxId StandardCrypto) AnyProposals
  | -- | Governance votes that were invalidated.
    RemovedGovernanceVotes
      (Ledger.TxId StandardCrypto)
      (Set (Ledger.Voter StandardCrypto, Ledger.GovActionId StandardCrypto))
      -- ^ Votes that were replaced in this tx.
      (Set (Ledger.Credential 'Ledger.DRepRole StandardCrypto))
      -- ^ Any votes from these DReps in this or in previous txs are removed
  | -- | The current state of governance matters at the epoch boundary.
    -- I.E the current constitution, committee, protocol parameters, etc.
    EpochBoundaryRatificationState AnyRatificationState
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
  { mirddReservePayouts :: Map StakeCredential L.Coin
  , mirddTreasuryPayouts :: Map StakeCredential L.Coin
  , mirddReservesToTreasury :: L.Coin
  , mirddTreasuryToReserves :: L.Coin
  }
  deriving Show

data PoolReapDetails = PoolReapDetails
  { prdEpochNo :: EpochNo
  , prdRefunded :: Map StakeCredential (Map (Hash StakePoolKey) L.Coin)
  -- ^ Refunded deposits. The pools referenced are now retired, and the
  --   'StakeCredential' accounts are credited with the deposits.
  , prdUnclaimed :: Map StakeCredential (Map (Hash StakePoolKey) L.Coin)
  -- ^ Unclaimed deposits. The 'StakeCredential' referenced in this map is not
  -- actively registered at the time of the pool reaping, and as such the
  -- funds are returned to the treasury.
  }
  deriving Show

convertRetiredPoolsMap
  :: Map
       (Ledger.StakeCredential StandardCrypto)
       (Map (Ledger.KeyHash Ledger.StakePool StandardCrypto) Ledger.Coin)
  -> Map StakeCredential (Map (Hash StakePoolKey) L.Coin)
convertRetiredPoolsMap =
  Map.mapKeys fromShelleyStakeCredential
    . fmap (Map.mapKeys StakePoolKeyHash)
