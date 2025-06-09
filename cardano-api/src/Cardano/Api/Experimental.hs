-- | This module provides an experimental library interface intended to replace the existing API.
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
  , eraToBabbageEraOnwards
  , sbeToEra

    -- ** Witness related
  , AnyWitness (..)
  , PlutusScriptWitness (..)
  , TxScriptWitnessRequirements (..)
  , Witnessable (..)
  , WitnessableItem (..)

    -- ** Simple script related
  , SimpleScript (..)
  , SimpleScriptOrReferenceInput (..)

    -- ** Plutus related
  , PlutusScriptInEra (..)
  , PlutusScriptOrReferenceInput (..)
  , IndexedPlutusScriptWitness (..)
  , PlutusScriptPurpose (..)
  , PlutusScriptDatum (..)
  , NoScriptDatum (..)

    -- ** Internal
  , getAnyWitnessRedeemerPointerMap
  , toPlutusScriptPurpose

    -- ** Legacy
  , legacyWitnessConversion
  , toPlutusSLanguage
  )
where

import Cardano.Api.Internal.Experimental.Eras
import Cardano.Api.Internal.Experimental.Plutus.IndexedPlutusScriptWitness
import Cardano.Api.Internal.Experimental.Plutus.Script
import Cardano.Api.Internal.Experimental.Plutus.ScriptWitness
import Cardano.Api.Internal.Experimental.Plutus.Shim.LegacyScripts
import Cardano.Api.Internal.Experimental.Simple.Script
import Cardano.Api.Internal.Experimental.Tx
import Cardano.Api.Internal.Fees (evaluateTransactionExecutionUnitsShelley)
