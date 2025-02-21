-- |
-- This module provides an experimental library interface intended to replace the existing API.
-- It is subject to significant changes. Please, use it with caution.
module Cardano.Api.Experimental
  ( -- * Creating transactions

    -- |
    -- For details and an example of creating a transaction using the experimental API,
    -- see the "Cardano.Api.Internal.Experimental.Tx" documentation.

    -- * Contents

    -- ** Transaction-related
    UnsignedTx (..)
  , UnsignedTxError (..)
  , makeUnsignedTx
  , makeKeyWitness
  , signTx
  , convertTxBodyToUnsignedTx
  , EraCommonConstraints
  , obtainCommonConstraints
  , hashTxBody
  , evaluateTransactionExecutionUnitsShelley

    -- ** Era-related
  , BabbageEra
  , ConwayEra
  , Era (..)
  , IsEra (..)
  , Some (..)
  , LedgerEra
  , DeprecatedEra (..)
  , eraToSbe
  , babbageEraOnwardsToEra
  , eraToBabbageEraOnwards
  , sbeToEra
  )
where

import Cardano.Api.Internal.Experimental.Eras
import Cardano.Api.Internal.Experimental.Tx
import Cardano.Api.Internal.Fees (evaluateTransactionExecutionUnitsShelley)
