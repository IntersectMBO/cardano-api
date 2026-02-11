{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Api.Experimental.Tx.Internal.AnyWitness
  ( -- * Any witness (key, simple script, plutus script).
    AnyWitness (..)
  , anyScriptWitnessToAnyWitness
  , getAnyWitnessScript
  , getAnyWitnessSimpleScript
  , getAnyWitnessPlutusLanguage
  , getAnyWitnessReferenceInput
  , getAnyWitnessScriptData
  , getPlutusDatum
  )
where

import Cardano.Api.Experimental.AnyScriptWitness
import Cardano.Api.Experimental.Plutus.Internal.ScriptWitness
import Cardano.Api.Experimental.Simple.Script
  ( SimpleScript (SimpleScript)
  , SimpleScriptOrReferenceInput (..)
  )
import Cardano.Api.Internal.Orphans.Misc ()
import Cardano.Api.Ledger.Internal.Reexport qualified as L
import Cardano.Api.Plutus.Internal.ScriptData
import Cardano.Api.Tx.Internal.TxIn

import Cardano.Ledger.Core qualified as L

import Data.Type.Equality

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
  AnyPlutusScriptWitness :: AnyPlutusScriptWitness lang purpose era -> AnyWitness era

deriving instance Show (AnyWitness era)

instance Eq (AnyWitness era) where
  AnyKeyWitnessPlaceholder == AnyKeyWitnessPlaceholder = True
  (AnySimpleScriptWitness s1) == (AnySimpleScriptWitness s2) = s1 == s2
  (AnyPlutusScriptWitness (AnyPlutusSpendingScriptWitness s1)) == (AnyPlutusScriptWitness (AnyPlutusSpendingScriptWitness s2)) =
    s1 == s2
  (AnyPlutusScriptWitness (AnyPlutusMintingScriptWitness s1)) == (AnyPlutusScriptWitness (AnyPlutusMintingScriptWitness s2)) =
    case langTypeEquality s1 s2 of
      Just Refl -> s1 == s2
      Nothing -> False
  (AnyPlutusScriptWitness (AnyPlutusWithdrawingScriptWitness s1)) == (AnyPlutusScriptWitness (AnyPlutusWithdrawingScriptWitness s2)) =
    case langTypeEquality s1 s2 of
      Just Refl -> s1 == s2
      Nothing -> False
  AnyPlutusScriptWitness (AnyPlutusCertifyingScriptWitness s1) == (AnyPlutusScriptWitness (AnyPlutusCertifyingScriptWitness s2)) =
    case langTypeEquality s1 s2 of
      Just Refl -> s1 == s2
      Nothing -> False
  AnyPlutusScriptWitness (AnyPlutusProposingScriptWitness s1) == (AnyPlutusScriptWitness (AnyPlutusProposingScriptWitness s2)) =
    case langTypeEquality s1 s2 of
      Just Refl -> s1 == s2
      Nothing -> False
  AnyPlutusScriptWitness (AnyPlutusVotingScriptWitness s1) == (AnyPlutusScriptWitness (AnyPlutusVotingScriptWitness s2)) =
    case langTypeEquality s1 s2 of
      Just Refl -> s1 == s2
      Nothing -> False
  _ == _ = False

getAnyWitnessPlutusLanguage :: AnyWitness era -> Maybe L.Language
getAnyWitnessPlutusLanguage AnyKeyWitnessPlaceholder = Nothing
getAnyWitnessPlutusLanguage (AnySimpleScriptWitness _) = Nothing
getAnyWitnessPlutusLanguage (AnyPlutusScriptWitness swit) = Just $ getAnyPlutusScriptWitnessLanguage swit

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
      s
    ) = getAnyPlutusWitnessPlutusScript s

getAnyWitnessReferenceInput :: AnyWitness era -> Maybe TxIn
getAnyWitnessReferenceInput AnyKeyWitnessPlaceholder = Nothing
getAnyWitnessReferenceInput (AnySimpleScriptWitness (SReferenceScript txIn)) = Just txIn
getAnyWitnessReferenceInput (AnySimpleScriptWitness (SScript _)) = Nothing
getAnyWitnessReferenceInput (AnyPlutusScriptWitness s) =
  getAnyPlutusScriptWitnessReferenceInput s

-- | NB this does not include datums from inline datums existing at tx outputs!
getAnyWitnessScriptData
  :: L.Era era => AnyWitness era -> L.TxDats era
getAnyWitnessScriptData AnyKeyWitnessPlaceholder = mempty
getAnyWitnessScriptData AnySimpleScriptWitness{} = mempty
getAnyWitnessScriptData (AnyPlutusScriptWitness s) = getAnyPlutusScriptData s

getAnyWitnessScript
  :: L.AlonzoEraScript era => AnyWitness era -> Maybe (L.Script era)
getAnyWitnessScript AnyKeyWitnessPlaceholder = Nothing
getAnyWitnessScript ss@(AnySimpleScriptWitness{}) = getAnyWitnessSimpleScript ss
getAnyWitnessScript ps@(AnyPlutusScriptWitness{}) = getAnyWitnessPlutusScript ps

getPlutusDatum
  :: L.SLanguage lang -> PlutusScriptDatum lang purpose -> Maybe HashableScriptData
getPlutusDatum L.SPlutusV1 (SpendingScriptDatum d) = Just d
getPlutusDatum L.SPlutusV2 (SpendingScriptDatum d) = Just d
getPlutusDatum L.SPlutusV3 (SpendingScriptDatum d) = d
getPlutusDatum L.SPlutusV4 (SpendingScriptDatum _d) = error "dijkstra"
getPlutusDatum _ InlineDatum = Nothing
getPlutusDatum _ NoScriptDatum = Nothing

anyScriptWitnessToAnyWitness
  :: AnyScriptWitness era
  -> AnyWitness era
anyScriptWitnessToAnyWitness (AnyScriptWitnessSimple s) = AnySimpleScriptWitness s
anyScriptWitnessToAnyWitness (AnyScriptWitnessPlutus sw) = AnyPlutusScriptWitness sw
