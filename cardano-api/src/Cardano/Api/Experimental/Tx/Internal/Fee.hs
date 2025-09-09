{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Api.Experimental.Tx.Internal.Fee
  ( estimateBalancedTxBody
  )
where

import Cardano.Api.Address
import Cardano.Api.Certificate.Internal
import Cardano.Api.Era.Internal.Eon.Convert
import Cardano.Api.Experimental.Era
import Cardano.Api.Ledger.Internal.Reexport qualified as L
import Cardano.Api.Plutus
import Cardano.Api.Tx.Internal.Body
import Cardano.Api.Tx.Internal.Fee qualified as Fee
import Cardano.Api.Value.Internal

import Cardano.Ledger.Alonzo.Core qualified as Ledger
import Cardano.Ledger.Credential as Ledger (Credential)

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import GHC.Stack

-- | Use when you do not have access to the UTxOs you intend to spend
estimateBalancedTxBody
  :: HasCallStack
  => Era era
  -> TxBodyContent BuildTx era
  -> L.PParams (LedgerEra era)
  -> Set PoolId
  -- ^ The set of registered stake pools, being
  --   unregistered in this transaction.
  -> Map StakeCredential L.Coin
  -- ^ A map of all deposits for stake credentials that are being
  --   unregistered in this transaction.
  -> Map (Ledger.Credential Ledger.DRepRole) L.Coin
  -- ^ A map of all deposits for DRep credentials that are being
  --   unregistered in this transaction.
  -> Map (Ledger.PlutusPurpose Ledger.AsIx (LedgerEra era)) ExecutionUnits
  -- ^ Plutus script execution units.
  -> Coin
  -- ^ Total potential collateral amount.
  -> Int
  -- ^ The number of key witnesses to be added to the transaction.
  -> Int
  -- ^ The number of Byron key witnesses to be added to the transaction.
  -> Int
  -- ^ The size of all reference scripts in bytes.
  -> AddressInEra era
  -- ^ Change address.
  -> Value
  -- ^ Total value of UTXOs being spent.
  -> Either (Fee.TxFeeEstimationError era) (Fee.BalancedTxBody era)
estimateBalancedTxBody
  w
  txbodycontent
  pparams
  poolids
  stakeDelegDeposits
  drepDelegDeposits
  exUnitsMap =
    obtainCommonConstraints w $
      Fee.estimateBalancedTxBody
        (convert w)
        txbodycontent
        pparams
        poolids
        stakeDelegDeposits
        drepDelegDeposits
        (Map.mapKeys (toScriptIndex (convert w)) exUnitsMap)
