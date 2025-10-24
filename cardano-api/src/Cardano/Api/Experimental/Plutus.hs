module Cardano.Api.Experimental.Plutus
  ( -- * Plutus Script
    PlutusScriptInEra (..)

    -- * Legacy Scripts
  , legacyWitnessToScriptRequirements
  , legacyWitnessConversion
  , toPlutusSLanguage

    -- * Plutus Script Witness
  , PlutusScriptWitness (..)

    -- ** Constructing a plutus script witness.
  , PlutusScriptOrReferenceInput (..)
  , ScriptRedeemer
  , PlutusScriptPurpose (..)
  , PlutusScriptDatum (..)
  , NoScriptDatum (..)
  , mkPlutusScriptWitness
  , getPlutusScriptWitnessLanguage

    -- ** Constuct an indexed plutus script witness.
  , AnyIndexedPlutusScriptWitness (..)
  , IndexedPlutusScriptWitness (..)

    -- ** Witnessable things.
  , Witnessable (..)
  , WitnessableItem (..)

    -- ** Create the index for a witnessable thing.
  , toPlutusScriptPurpose
  , createIndexedPlutusScriptWitnesses
  , getAnyWitnessRedeemerPointerMap
  , obtainAlonzoScriptPurposeConstraints

    -- ** Low level functions
  , constructRedeeemerPointerMap
  )
where

import Cardano.Api.Experimental.Plutus.Internal.IndexedPlutusScriptWitness
import Cardano.Api.Experimental.Plutus.Internal.Script as X
import Cardano.Api.Experimental.Plutus.Internal.ScriptWitness
import Cardano.Api.Experimental.Plutus.Internal.Shim.LegacyScripts
