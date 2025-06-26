{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Api.Experimental.Tx.Internal.AnyWitness
  ( -- * Any witness (key, simple script, plutus script).
    AnyWitness (..)
  , getAnyWitnessScript
  , getAnyWitnessPlutusLanguage
  , getAnyWitnessScriptData
  )
where

import Cardano.Api.Era.Internal.Eon.AlonzoEraOnwards
import Cardano.Api.Era.Internal.Eon.ShelleyBasedEra
  ( ShelleyBasedEra (..)
  , ShelleyLedgerEra
  , forShelleyBasedEraInEon
  )
import Cardano.Api.Experimental.Plutus.Internal.Script
import Cardano.Api.Experimental.Plutus.Internal.ScriptWitness
import Cardano.Api.Experimental.Simple.Script
  ( SimpleScript (SimpleScript)
  , SimpleScriptOrReferenceInput (..)
  )
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Plutus.Internal.ScriptData

import Cardano.Ledger.Alonzo.Scripts qualified as L
import Cardano.Ledger.Babbage.Scripts qualified as L
import Cardano.Ledger.Conway.Scripts qualified as L
import Cardano.Ledger.Core qualified as L
import Cardano.Ledger.Plutus.Data qualified as L
import Cardano.Ledger.Plutus.Language qualified as L

import GHC.Exts

-- | Here we consider three types of witnesses in Cardano:
-- * key witnesses
-- * simple script witnesses
-- * Plutus script witnesses
--
-- Note that 'AnyKeyWitnessPlaceholder' does not contain the actual key witness. This is because
-- key witnesses are provided in the signing stage of the transaction. However we need this constuctor
-- to index the witnessable things correctly when plutus scripts are being used within the transaction.
-- AnyWitness is solely used to contruct the transaction body.
data AnyWitness era where
  AnyKeyWitnessPlaceholder :: AnyWitness era
  AnySimpleScriptWitness :: SimpleScriptOrReferenceInput era -> AnyWitness era
  AnyPlutusScriptWitness :: PlutusScriptWitness lang purpose era -> AnyWitness era

deriving instance Show (AnyWitness era)

getAnyWitnessPlutusLanguage :: AnyWitness era -> Maybe L.Language
getAnyWitnessPlutusLanguage AnyKeyWitnessPlaceholder = Nothing
getAnyWitnessPlutusLanguage (AnySimpleScriptWitness _) = Nothing
getAnyWitnessPlutusLanguage (AnyPlutusScriptWitness swit) = Just $ getPlutusScriptWitnessLanguage swit

getAnyWitnessSimpleScript
  :: AnyWitness (ShelleyLedgerEra era) -> Maybe (L.NativeScript (ShelleyLedgerEra era))
getAnyWitnessSimpleScript AnyKeyWitnessPlaceholder = Nothing
getAnyWitnessSimpleScript (AnySimpleScriptWitness simpleScriptOrRefInput) =
  case simpleScriptOrRefInput of
    SScript (SimpleScript simpleScript) -> Just simpleScript
    SReferenceScript{} -> Nothing
getAnyWitnessSimpleScript (AnyPlutusScriptWitness _) = Nothing

getAnyWitnessPlutusScript
  :: AlonzoEraOnwards era
  -> AnyWitness (ShelleyLedgerEra era)
  -> Maybe (L.PlutusScript (ShelleyLedgerEra era))
getAnyWitnessPlutusScript _ AnyKeyWitnessPlaceholder = Nothing
getAnyWitnessPlutusScript _ (AnySimpleScriptWitness _) = Nothing
getAnyWitnessPlutusScript
  eon
  ( AnyPlutusScriptWitness
      (PlutusScriptWitness l (PScript (PlutusScriptInEra plutusScriptRunnable)) _ _ _)
    ) = fromPlutusRunnable l eon plutusScriptRunnable
getAnyWitnessPlutusScript _ (AnyPlutusScriptWitness (PlutusScriptWitness _ (PReferenceScript{}) _ _ _)) =
  Nothing

-- | NB this does not include datums from inline datums existing at tx outputs!
getAnyWitnessScriptData
  :: AlonzoEraOnwards era -> AnyWitness (ShelleyLedgerEra era) -> L.TxDats (ShelleyLedgerEra era)
getAnyWitnessScriptData eon AnyKeyWitnessPlaceholder = alonzoEraOnwardsConstraints eon mempty
getAnyWitnessScriptData eon AnySimpleScriptWitness{} = alonzoEraOnwardsConstraints eon mempty
getAnyWitnessScriptData eon (AnyPlutusScriptWitness (PlutusScriptWitness l _ scriptDatum _ _)) =
  let alonzoSdat = toAlonzoDatum eon l scriptDatum
   in alonzoEraOnwardsConstraints eon $
        case alonzoSdat of
          Nothing -> alonzoEraOnwardsConstraints eon mempty
          Just d -> alonzoEraOnwardsConstraints eon $ L.TxDats $ fromList [(L.hashData d, d)]

getAnyWitnessScript
  :: ShelleyBasedEra era -> AnyWitness (ShelleyLedgerEra era) -> Maybe (L.Script (ShelleyLedgerEra era))
getAnyWitnessScript _ AnyKeyWitnessPlaceholder = Nothing
getAnyWitnessScript era ss@(AnySimpleScriptWitness{}) =
  case era of
    ShelleyBasedEraShelley -> getAnyWitnessSimpleScript ss
    ShelleyBasedEraAllegra -> getAnyWitnessSimpleScript ss
    ShelleyBasedEraMary -> getAnyWitnessSimpleScript ss
    ShelleyBasedEraAlonzo -> L.TimelockScript <$> getAnyWitnessSimpleScript ss
    ShelleyBasedEraBabbage -> L.TimelockScript <$> getAnyWitnessSimpleScript ss
    ShelleyBasedEraConway -> L.TimelockScript <$> getAnyWitnessSimpleScript ss
getAnyWitnessScript era ps@(AnyPlutusScriptWitness{}) =
  forShelleyBasedEraInEon era Nothing $ \aEon ->
    case aEon of
      AlonzoEraOnwardsAlonzo -> L.PlutusScript <$> getAnyWitnessPlutusScript aEon ps
      AlonzoEraOnwardsBabbage -> L.PlutusScript <$> getAnyWitnessPlutusScript aEon ps
      AlonzoEraOnwardsConway -> L.PlutusScript <$> getAnyWitnessPlutusScript aEon ps

-- It should be noted that 'PlutusRunnable' is constructed via deserialization. The deserialization
-- instance lives in ledger and will fail for an invalid script language/era pairing. Therefore
-- this function should never return 'Nothing'.
fromPlutusRunnable
  :: L.SLanguage lang
  -> AlonzoEraOnwards era
  -> L.PlutusRunnable lang
  -> Maybe (L.PlutusScript (ShelleyLedgerEra era))
fromPlutusRunnable L.SPlutusV1 eon runnable =
  case eon of
    AlonzoEraOnwardsAlonzo ->
      let plutusScript = L.plutusFromRunnable runnable
       in Just $ L.AlonzoPlutusV1 plutusScript
    AlonzoEraOnwardsBabbage ->
      let plutusScript = L.plutusFromRunnable runnable
       in Just $ L.BabbagePlutusV1 plutusScript
    AlonzoEraOnwardsConway ->
      let plutusScript = L.plutusFromRunnable runnable
       in Just $ L.ConwayPlutusV1 plutusScript
fromPlutusRunnable L.SPlutusV2 eon runnable =
  case eon of
    AlonzoEraOnwardsAlonzo -> Nothing
    AlonzoEraOnwardsBabbage ->
      let plutusScript = L.plutusFromRunnable runnable
       in Just $ L.BabbagePlutusV2 plutusScript
    AlonzoEraOnwardsConway ->
      let plutusScript = L.plutusFromRunnable runnable
       in Just $ L.ConwayPlutusV2 plutusScript
fromPlutusRunnable L.SPlutusV3 eon runnable =
  case eon of
    AlonzoEraOnwardsAlonzo -> Nothing
    AlonzoEraOnwardsBabbage -> Nothing
    AlonzoEraOnwardsConway ->
      let plutusScript = L.plutusFromRunnable runnable
       in Just $ L.ConwayPlutusV3 plutusScript

toAlonzoDatum
  :: AlonzoEraOnwards era
  -> L.SLanguage lang
  -> PlutusScriptDatum lang purpose
  -> Maybe (L.Data (ShelleyLedgerEra era))
toAlonzoDatum eon l d =
  let mHashableData = getPlutusDatum l d
   in case mHashableData of
        Just h -> Just $ alonzoEraOnwardsConstraints eon $ toAlonzoData h
        Nothing -> Nothing

getPlutusDatum
  :: L.SLanguage lang -> PlutusScriptDatum lang purpose -> Maybe HashableScriptData
getPlutusDatum L.SPlutusV1 (SpendingScriptDatum d) = Just d
getPlutusDatum L.SPlutusV2 (SpendingScriptDatum d) = Just d
getPlutusDatum L.SPlutusV3 (SpendingScriptDatum d) = d
getPlutusDatum _ InlineDatum = Nothing
getPlutusDatum _ NoScriptDatum = Nothing
