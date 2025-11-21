{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Api.Experimental.Tx.Internal.AnyWitness
  ( -- * Any witness (key, simple script, plutus script).
    AnyWitness (..)
  , getAnyWitnessScript
  , getAnyWitnessSimpleScript
  , getAnyWitnessPlutusLanguage
  , getAnyWitnessScriptData
  , getPlutusDatum
  )
where

import Cardano.Api.Experimental.Plutus.Internal.Script
import Cardano.Api.Experimental.Plutus.Internal.ScriptWitness
import Cardano.Api.Experimental.Simple.Script
  ( SimpleScript (SimpleScript)
  , SimpleScriptOrReferenceInput (..)
  )
import Cardano.Api.Internal.Orphans.Misc ()
import Cardano.Api.Ledger.Internal.Reexport qualified as L
import Cardano.Api.Plutus.Internal.ScriptData

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

instance Eq (AnyWitness era) where
  AnyKeyWitnessPlaceholder == AnyKeyWitnessPlaceholder = True
  (AnySimpleScriptWitness s1) == (AnySimpleScriptWitness s2) = s1 == s2
  (AnyPlutusScriptWitness (PlutusScriptWitness l1 s1 d1 r1 e1)) == (AnyPlutusScriptWitness (PlutusScriptWitness l2 s2 d2 r2 e2)) =
    case (l1, l2) of
      (L.SPlutusV1, L.SPlutusV1) -> case (d1, d2) of
        (InlineDatum, InlineDatum) -> s1 == s2 && r1 == r2 && e1 == e2
        (NoScriptDatum, NoScriptDatum) -> s1 == s2 && r1 == r2 && e1 == e2
        (SpendingScriptDatum d1', SpendingScriptDatum d2') -> s1 == s2 && r1 == r2 && e1 == e2 && d1' == d2'
        (_, _) -> False
      (L.SPlutusV2, L.SPlutusV2) -> case (d1, d2) of
        (InlineDatum, InlineDatum) -> s1 == s2 && r1 == r2 && e1 == e2
        (NoScriptDatum, NoScriptDatum) -> s1 == s2 && r1 == r2 && e1 == e2
        (SpendingScriptDatum d1', SpendingScriptDatum d2') -> s1 == s2 && r1 == r2 && e1 == e2 && d1' == d2'
        (_, _) -> False
      (L.SPlutusV3, L.SPlutusV3) -> case (d1, d2) of
        (InlineDatum, InlineDatum) -> s1 == s2 && r1 == r2 && e1 == e2
        (NoScriptDatum, NoScriptDatum) -> s1 == s2 && r1 == r2 && e1 == e2
        (SpendingScriptDatum d1', SpendingScriptDatum d2') -> s1 == s2 && r1 == r2 && e1 == e2 && d1' == d2'
        (_, _) -> False
      (L.SPlutusV4, L.SPlutusV4) -> case (d1, d2) of
        (InlineDatum, InlineDatum) -> s1 == s2 && r1 == r2 && e1 == e2
        (NoScriptDatum, NoScriptDatum) -> s1 == s2 && r1 == r2 && e1 == e2
        (SpendingScriptDatum d1', SpendingScriptDatum d2') -> s1 == s2 && r1 == r2 && e1 == e2 && d1' == d2'
        (_, _) -> False
      (_, _) -> False
  _ == _ = False

getAnyWitnessPlutusLanguage :: AnyWitness era -> Maybe L.Language
getAnyWitnessPlutusLanguage AnyKeyWitnessPlaceholder = Nothing
getAnyWitnessPlutusLanguage (AnySimpleScriptWitness _) = Nothing
getAnyWitnessPlutusLanguage (AnyPlutusScriptWitness swit) = Just $ getPlutusScriptWitnessLanguage swit

getAnyWitnessSimpleScript
  :: AnyWitness era -> Maybe (L.Script era)
getAnyWitnessSimpleScript AnyKeyWitnessPlaceholder = Nothing
getAnyWitnessSimpleScript (AnySimpleScriptWitness simpleScriptOrRefInput) =
  case simpleScriptOrRefInput of
    SScript (SimpleScript simpleScript) -> Just $ L.fromNativeScript simpleScript
    SReferenceScript{} -> Nothing
getAnyWitnessSimpleScript (AnyPlutusScriptWitness _) = Nothing

getAnyWitnessPlutusScript
  :: L.AlonzoEraScript era
  => AnyWitness era
  -> Maybe (L.Script era)
getAnyWitnessPlutusScript AnyKeyWitnessPlaceholder = Nothing
getAnyWitnessPlutusScript (AnySimpleScriptWitness _) = Nothing
getAnyWitnessPlutusScript
  ( AnyPlutusScriptWitness
      (PlutusScriptWitness l (PScript (PlutusScriptInEra plutusScriptRunnable)) _ _ _)
    ) = L.fromPlutusScript <$> fromPlutusRunnable l plutusScriptRunnable
getAnyWitnessPlutusScript (AnyPlutusScriptWitness (PlutusScriptWitness _ (PReferenceScript{}) _ _ _)) =
  Nothing

-- | NB this does not include datums from inline datums existing at tx outputs!
getAnyWitnessScriptData
  :: L.Era era => AnyWitness era -> L.TxDats era
getAnyWitnessScriptData AnyKeyWitnessPlaceholder = mempty
getAnyWitnessScriptData AnySimpleScriptWitness{} = mempty
getAnyWitnessScriptData (AnyPlutusScriptWitness (PlutusScriptWitness l _ scriptDatum _ _)) =
  let alonzoSdat = toAlonzoDatum l scriptDatum
   in case alonzoSdat of
        Nothing -> mempty
        Just d -> L.TxDats $ fromList [(L.hashData d, d)]

getAnyWitnessScript
  :: L.AlonzoEraScript era => AnyWitness era -> Maybe (L.Script era)
getAnyWitnessScript AnyKeyWitnessPlaceholder = Nothing
getAnyWitnessScript ss@(AnySimpleScriptWitness{}) = getAnyWitnessSimpleScript ss
getAnyWitnessScript ps@(AnyPlutusScriptWitness{}) = getAnyWitnessPlutusScript ps

-- It should be noted that 'PlutusRunnable' is constructed via deserialization. The deserialization
-- instance lives in ledger and will fail for an invalid script language/era pairing. Therefore
-- this function should never return 'Nothing'.
fromPlutusRunnable
  :: L.AlonzoEraScript era
  => L.SLanguage lang
  -> L.PlutusRunnable lang
  -> Maybe (L.PlutusScript era)
fromPlutusRunnable L.SPlutusV1 runnable =
  L.mkPlutusScript $ L.plutusFromRunnable runnable
fromPlutusRunnable L.SPlutusV2 runnable =
  L.mkPlutusScript $ L.plutusFromRunnable runnable
fromPlutusRunnable L.SPlutusV3 runnable =
  L.mkPlutusScript $ L.plutusFromRunnable runnable
fromPlutusRunnable L.SPlutusV4 runnable =
  L.mkPlutusScript $ L.plutusFromRunnable runnable

toAlonzoDatum
  :: L.Era era
  => L.SLanguage lang
  -> PlutusScriptDatum lang purpose
  -> Maybe (L.Data era)
toAlonzoDatum l d =
  let mHashableData = getPlutusDatum l d
   in case mHashableData of
        Just h -> Just $ toAlonzoData h
        Nothing -> Nothing

getPlutusDatum
  :: L.SLanguage lang -> PlutusScriptDatum lang purpose -> Maybe HashableScriptData
getPlutusDatum L.SPlutusV1 (SpendingScriptDatum d) = Just d
getPlutusDatum L.SPlutusV2 (SpendingScriptDatum d) = Just d
getPlutusDatum L.SPlutusV3 (SpendingScriptDatum d) = d
getPlutusDatum L.SPlutusV4 (SpendingScriptDatum _d) = error "dijkstra"
getPlutusDatum _ InlineDatum = Nothing
getPlutusDatum _ NoScriptDatum = Nothing
