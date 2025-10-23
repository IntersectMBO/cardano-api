-- | This module provides an experimental library interface intended to replace the existing API.
-- It is subject to significant changes. Please, use it with caution.
module Cardano.Api.Experimental
  ( -- * Creating transactions

    -- |
    -- For details and an example of creating a transaction using the experimental API,
    -- see the "Cardano.Api.Experimental.Tx" documentation.

    -- * Contents

    -- ** Transaction-related
    UnsignedTx (..)
  , UnsignedTxError (..)
  , SignedTx (..)
  , makeUnsignedTx
  , makeKeyWitness
  , signTx
  , convertTxBodyToUnsignedTx
  , EraCommonConstraints
  , obtainCommonConstraints
  , hashTxBody
  , evaluateTransactionExecutionUnitsShelley
  , Certificate (..)
  , AnchorDataFromCertificateError (..)
  , getAnchorDataFromCertificate
  , mkTxCertificates

    -- ** Transaction fee related
  , estimateBalancedTxBody

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

import Cardano.Api.Experimental.Era
import Cardano.Api.Experimental.Plutus.Internal.IndexedPlutusScriptWitness
import Cardano.Api.Experimental.Plutus.Internal.Script
import Cardano.Api.Experimental.Plutus.Internal.ScriptWitness
import Cardano.Api.Experimental.Plutus.Internal.Shim.LegacyScripts
import Cardano.Api.Experimental.Simple.Script
import Cardano.Api.Experimental.Tx
import Cardano.Api.Experimental.Tx.Internal.Certificate
import Cardano.Api.Experimental.Tx.Internal.Compatible
import Cardano.Api.Experimental.Tx.Internal.Fee
import Cardano.Api.Tx.Internal.Fee (evaluateTransactionExecutionUnitsShelley)
