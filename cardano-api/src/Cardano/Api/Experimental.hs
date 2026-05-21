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
  , SignedTx (..)
  , MakeUnsignedTxError (..)
  , makeUnsignedTx
  , makeKeyWitness
  , signTx
  , convertTxBodyToUnsignedTx
  , EraCommonConstraints
  , obtainConwayConstraints
  , obtainCommonConstraints
  , hashTxBody
  , evaluateTransactionExecutionUnitsShelley
  , AnchorDataFromCertificateError (..)
  , getAnchorDataFromCertificate
  , mkTxCertificates

    -- ** Transaction fee related
  , FeeCalculationError (..)
  , calcMinFeeRecursive
  , estimateBalancedTxBody
  , evaluateTransactionFee
  , collectTxBodyScriptWitnesses
  , substituteExecutionUnits

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

    -- ** AnyScript related
  , AnyScript (..)
  , deserialiseAnyPlutusScriptOfLanguage
  , deserialiseAnySimpleScript
  , hashAnyScript

    -- ** Simple script related
  , SimpleScript (..)
  , SimpleScriptOrReferenceInput (..)
  , deserialiseSimpleScript
  , hashSimpleScript

    -- ** Plutus related
  , PlutusScriptInEra (..)
  , PlutusScriptOrReferenceInput (..)
  , serialiseAnyPlutusScriptToTextEnvelope
  , deserialiseAnyPlutusScriptFromTextEnvelope
  , IndexedPlutusScriptWitness (..)
  , PlutusScriptPurpose (..)
  , PlutusScriptDatum (..)
  , NoScriptDatum

    -- ** Certificate related
  , Certificate (..)

    -- * Registering stake address and delegating
  , makeStakeAddressDelegationCertificate
  , makeStakeAddressRegistrationCertificate
  , makeStakeAddressUnregistrationCertificate

    -- * Registering stake pools
  , makeStakePoolRegistrationCertificate
  , makeStakePoolRetirementCertificate

    -- * Governance related certificates
  , makeCommitteeColdkeyResignationCertificate
  , makeCommitteeHotKeyAuthorizationCertificate
  , makeDrepRegistrationCertificate
  , makeDrepUnregistrationCertificate
  , makeDrepUpdateCertificate
  , makeStakeAddressAndDRepDelegationCertificate

    -- ** Validation
  , TxValidationResult (..)
  , QueryValidateTxError (..)
  , validateTx

    -- * Data family instances
  , AsType (..)

    -- ** Internal
  , anyScriptWitnessToAnyWitness
  , getAnyWitnessRedeemerPointerMap
  , toPlutusScriptPurpose

    -- ** Legacy
  , legacyWitnessConversion
  , toPlutusSLanguage
  )
where

import Cardano.Api.Experimental.AnyScript
import Cardano.Api.Experimental.Certificate
import Cardano.Api.Experimental.Era
import Cardano.Api.Experimental.Plutus.Internal.IndexedPlutusScriptWitness
import Cardano.Api.Experimental.Plutus.Internal.Script
import Cardano.Api.Experimental.Plutus.Internal.ScriptWitness
import Cardano.Api.Experimental.Plutus.Internal.Shim.LegacyScripts
import Cardano.Api.Experimental.Simple.Script
import Cardano.Api.Experimental.Tx
import Cardano.Api.Experimental.Tx.Internal.AnyWitness
import Cardano.Api.Experimental.Tx.Internal.Fee
import Cardano.Api.Tx.Internal.Fee (evaluateTransactionExecutionUnitsShelley)
