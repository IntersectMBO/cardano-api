{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Api.Internal.Experimental.Plutus.ScriptWitness
  ( PlutusScriptWitness (..)

    -- * Constructing a plutus script witness.
  , PlutusScriptOrReferenceInput (..)
  , ScriptRedeemer
  , PlutusScriptPurpose (..)
  , PlutusScriptDatum (..)
  , NoScriptDatum (..)
  , mkPlutusScriptWitness

    -- * Helpers
  , getPlutusScriptWitnessLanguage
  )
where

import Cardano.Api.Internal.Eon.AlonzoEraOnwards
import Cardano.Api.Internal.Eon.ShelleyBasedEra
import Cardano.Api.Internal.Experimental.Plutus.Script
import Cardano.Api.Internal.Script (ExecutionUnits)
import Cardano.Api.Internal.ScriptData
import Cardano.Api.Ledger qualified as L

import Cardano.Ledger.Plutus.Language qualified as L

{-
To construct a plutus script witness you need:
1. The plutus script or reference input
2. A redeemer
3. The thing being witnessed

This is true regardless of the plutus script version.

-}

type ScriptRedeemer = HashableScriptData

-- | This is a Plutus script witness. It possesses:
-- 1. The plutus script or reference input
-- 2. The script redeemer
-- 3. The execution units
-- 4. Potentially a script datum. See the PlutusScriptDatum type family for more details.
--
-- Note that Plutus script witnesses do not exist on their own. They must witness something
-- and a redeemer pointer must be constucted to point to the thing being witnessed.
-- See 'IndexedPlutusScriptWitness' for more details.
data PlutusScriptWitness (lang :: L.Language) (purpose :: PlutusScriptPurpose) era where
  PlutusScriptWitness
    :: L.SLanguage lang
    -> (PlutusScriptOrReferenceInput lang era)
    -> (PlutusScriptDatum lang purpose)
    -> ScriptRedeemer
    -> ExecutionUnits
    -> PlutusScriptWitness lang purpose era

deriving instance Show (PlutusScriptWitness lang purpose era)

getPlutusScriptWitnessLanguage :: PlutusScriptWitness lang purpose era -> L.Language
getPlutusScriptWitnessLanguage (PlutusScriptWitness l _ _ _ _) =
  case l of
    L.SPlutusV1 -> L.plutusLanguage l
    L.SPlutusV2 -> L.plutusLanguage l
    L.SPlutusV3 -> L.plutusLanguage l

-- | Every Plutus script has a purpose that indicates
-- what that script is witnessing.
data PlutusScriptPurpose
  = -- | Witnesses a transaction input
    SpendingScript
  | -- | Witnesses a minting policy
    MintingScript
  | -- | Witnesses a withdrawal
    WithdrawingScript
  | -- | Witnesses a certificate
    CertifyingScript
  | -- | Witnesses a proposal
    ProposingScript
  | -- | Witnesses a vote
    VotingScript

data NoScriptDatum = NoScriptDatumAllowed deriving Show

-- | The PlutusScriptDatum type family is used to determine if a script datum is allowed
-- for a given plutus script purpose and version. This change was proposed in CIP-69
-- https://github.com/cardano-foundation/CIPs/tree/master/CIP-0069
type family PlutusScriptDatumF (lang :: L.Language) (purpose :: PlutusScriptPurpose) where
  PlutusScriptDatumF L.PlutusV1 SpendingScript = HashableScriptData
  PlutusScriptDatumF L.PlutusV2 SpendingScript = HashableScriptData
  PlutusScriptDatumF L.PlutusV3 SpendingScript = Maybe HashableScriptData -- CIP-69
  PlutusScriptDatumF L.PlutusV1 MintingScript = NoScriptDatum
  PlutusScriptDatumF L.PlutusV2 MintingScript = NoScriptDatum
  PlutusScriptDatumF L.PlutusV3 MintingScript = NoScriptDatum
  PlutusScriptDatumF L.PlutusV1 WithdrawingScript = NoScriptDatum
  PlutusScriptDatumF L.PlutusV2 WithdrawingScript = NoScriptDatum
  PlutusScriptDatumF L.PlutusV3 WithdrawingScript = NoScriptDatum
  PlutusScriptDatumF L.PlutusV1 CertifyingScript = NoScriptDatum
  PlutusScriptDatumF L.PlutusV2 CertifyingScript = NoScriptDatum
  PlutusScriptDatumF L.PlutusV3 CertifyingScript = NoScriptDatum
  PlutusScriptDatumF L.PlutusV1 ProposingScript = NoScriptDatum
  PlutusScriptDatumF L.PlutusV2 ProposingScript = NoScriptDatum
  PlutusScriptDatumF L.PlutusV3 ProposingScript = NoScriptDatum
  PlutusScriptDatumF L.PlutusV1 VotingScript = NoScriptDatum
  PlutusScriptDatumF L.PlutusV2 VotingScript = NoScriptDatum
  PlutusScriptDatumF L.PlutusV3 VotingScript = NoScriptDatum

data PlutusScriptDatum (lang :: L.Language) (purpose :: PlutusScriptPurpose) where
  SpendingScriptDatum
    :: PlutusScriptDatumF lang SpendingScript -> PlutusScriptDatum lang SpendingScript
  InlineDatum :: PlutusScriptDatum lang purpose
  NoScriptDatum
    :: PlutusScriptDatum lang purpose

instance Show (PlutusScriptDatum lang purpose) where
  show = \case
    SpendingScriptDatum _d -> "Datum"
    InlineDatum -> "InlineDatum"
    NoScriptDatum -> "NoScriptDatum"

mkPlutusScriptWitness
  :: AlonzoEraOnwards era
  -> L.SLanguage plutuslang
  -> L.PlutusRunnable plutuslang
  -> PlutusScriptDatum plutuslang purpose
  -> ScriptRedeemer
  -> ExecutionUnits
  -> PlutusScriptWitness plutuslang purpose (ShelleyLedgerEra era)
mkPlutusScriptWitness _ l plutusScriptRunnable =
  PlutusScriptWitness
    l
    (PScript $ PlutusScriptInEra plutusScriptRunnable)
