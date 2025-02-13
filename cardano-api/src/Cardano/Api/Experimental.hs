-- |
-- This module provides an experimental library interface that is intended
-- to replace the existing API. It is subject to dramatic changes, so use with caution.
module Cardano.Api.Experimental
  ( -- * Creating transactions

    -- |
    -- For information and an example on how to create a transaction using the new experimental API,
    -- check the documentation of the module "Cardano.Api.Internal.Experimental.Tx".

    -- * Contents

    -- ** Tx related
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

    -- ** Era related
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

import           Cardano.Api.Internal.Experimental.Eras
import           Cardano.Api.Internal.Experimental.Tx
import           Cardano.Api.Internal.Fees (evaluateTransactionExecutionUnitsShelley)
