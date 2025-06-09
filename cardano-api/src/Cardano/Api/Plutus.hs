module Cardano.Api.Plutus
  ( -- * Languages
    SimpleScript'
  , PlutusScriptV1
  , PlutusScriptV2
  , PlutusScriptV3
  , ScriptLanguage (..)
  , PlutusScriptVersion (..)
  , AnyScriptLanguage (..)
  , AnyPlutusScriptVersion (..)
  , IsPlutusScriptLanguage (..)
  , IsScriptLanguage (..)
  , ToLedgerPlutusLanguage

    -- * Scripts in a specific language
  , Script (..)
  , PlutusScriptInEra (..)

    -- * Scripts in any language
  , ScriptInAnyLang (..)
  , toScriptInAnyLang
  , removePlutusScriptDoubleEncoding

    -- * Scripts in an era
  , ScriptInEra (..)
  , toScriptInEra
  , eraOfScriptInEra
  , HasScriptLanguageInEra (..)
  , ToAlonzoScript (..)

    -- * Reference scripts
  , ReferenceScript (..)
  , refScriptToShelleyScript

    -- * Use of a script in an era as a witness
  , WitCtxTxIn
  , WitCtxMint
  , WitCtxStake
  , WitCtx (..)
  , ScriptWitness (..)
  , getScriptWitnessReferenceInput
  , getScriptWitnessScript
  , getScriptWitnessReferenceInputOrScript
  , Witness (..)
  , KeyWitnessInCtx (..)
  , ScriptWitnessInCtx (..)
  , IsScriptWitnessInCtx (..)
  , ScriptDatum (..)
  , ScriptRedeemer

    -- ** Languages supported in each era
  , ScriptLanguageInEra (..)
  , scriptLanguageSupportedInEra
  , sbeToSimpleScriptLanguageInEra
  , languageOfScriptLanguageInEra
  , eraOfScriptLanguageInEra

    -- * The simple script language
  , SimpleScript (..)
  , SimpleScriptOrReferenceInput (..)

    -- * The Plutus script language
  , PlutusScript (..)
  , PlutusScriptOrReferenceInput (..)
  , examplePlutusScriptAlwaysSucceeds
  , examplePlutusScriptAlwaysFails

    -- * Script execution units
  , ExecutionUnits (..)

    -- * Script hashes
  , ScriptHash (..)
  , parseScriptHash
  , hashScript

    -- * Internal conversion functions
  , toShelleyScript
  , fromShelleyBasedScript
  , toShelleyMultiSig
  , fromShelleyMultiSig
  , toAllegraTimelock
  , fromAllegraTimelock
  , toAlonzoExUnits
  , fromAlonzoExUnits
  , toShelleyScriptHash
  , fromShelleyScriptHash
  , toPlutusData
  , fromPlutusData
  , toAlonzoData
  , fromAlonzoData
  , toAlonzoLanguage
  , fromAlonzoLanguage
  , fromShelleyScriptToReferenceScript
  , scriptInEraToRefScript
  , DebugPlutusFailure (..)
  , renderDebugPlutusFailure
  , collectPlutusScriptHashes

    -- * Script data
  , HashableScriptData (..)
  , hashScriptDataBytes
  , getOriginalScriptDataBytes
  , getScriptData
  , unsafeHashableScriptData
  , ScriptData (..)
  , parseScriptDataHash

    -- * Validating metadata
  , validateScriptData
  , ScriptDataRangeError (..)

    -- * Conversion to\/from JSON
  , ScriptDataJsonSchema (..)
  , scriptDataFromJson
  , scriptDataToJson
  , ScriptDataJsonError (..)
  , ScriptDataJsonSchemaError (..)
  , scriptDataFromJsonDetailedSchema
  , scriptDataToJsonDetailedSchema
  , ScriptBytesError (..)
  , ScriptDataJsonBytesError (..)
  , scriptDataJsonToHashable

    -- * Data family instances
  , AsType (..)
  , Hash (..)
  )
where

import Cardano.Api.Internal.Plutus
import Cardano.Api.Internal.Script
import Cardano.Api.Internal.ScriptData
