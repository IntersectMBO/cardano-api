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

module Cardano.Api.Experimental.Plutus.Internal.ScriptWitness
  ( PlutusScriptWitness (..)

    -- * Constructing a plutus script witness.
  , PlutusScriptOrReferenceInput (..)
  , ScriptRedeemer
  , PlutusScriptPurpose (..)
  , PlutusScriptDatum (..)
  , NoScriptDatum (..)

    -- * Helpers
  , getSpendingPlutusWitnessData
  , getPlutusScriptRunnable
  , getPlutusScriptWitnessLanguage
  )
where

import Cardano.Api.Experimental.Plutus.Internal.Script
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Plutus.Internal.Script (ExecutionUnits)
import Cardano.Api.Plutus.Internal.ScriptData

import Cardano.Ledger.Plutus.Data qualified as Plutus
import Cardano.Ledger.Plutus.Language qualified as L

import GHC.IsList

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
    -> PlutusScriptOrReferenceInput lang era
    -> PlutusScriptDatum lang purpose
    -> ScriptRedeemer
    -> ExecutionUnits
    -> PlutusScriptWitness lang purpose era

deriving instance Show (PlutusScriptWitness lang purpose era)

instance Eq (PlutusScriptWitness L.PlutusV1 SpendingScript era) where
  (==) (PlutusScriptWitness s1 l1 d1 r1 eu1) (PlutusScriptWitness s2 l2 d2 r2 eu2) =
    l1 == l2 && d1 == d2 && r1 == r2 && eu1 == eu2 && s1 == s2

instance Eq (PlutusScriptWitness L.PlutusV2 SpendingScript era) where
  (==) (PlutusScriptWitness s1 l1 d1 r1 eu1) (PlutusScriptWitness s2 l2 d2 r2 eu2) =
    l1 == l2 && d1 == d2 && r1 == r2 && eu1 == eu2 && s1 == s2

instance Eq (PlutusScriptWitness L.PlutusV3 SpendingScript era) where
  (==) (PlutusScriptWitness s1 l1 d1 r1 eu1) (PlutusScriptWitness s2 l2 d2 r2 eu2) =
    l1 == l2 && d1 == d2 && r1 == r2 && eu1 == eu2 && s1 == s2

instance Eq (PlutusScriptWitness L.PlutusV4 SpendingScript era) where
  (==) (PlutusScriptWitness s1 l1 d1 r1 eu1) (PlutusScriptWitness s2 l2 d2 r2 eu2) =
    l1 == l2 && d1 == d2 && r1 == r2 && eu1 == eu2 && s1 == s2

instance Eq (PlutusScriptWitness lang MintingScript era) where
  (==) (PlutusScriptWitness s1 l1 d1 r1 eu1) (PlutusScriptWitness s2 l2 d2 r2 eu2) =
    l1 == l2 && d1 == d2 && r1 == r2 && eu1 == eu2 && s1 == s2

instance Eq (PlutusScriptWitness lang WithdrawingScript era) where
  (==) (PlutusScriptWitness s1 l1 d1 r1 eu1) (PlutusScriptWitness s2 l2 d2 r2 eu2) =
    l1 == l2 && d1 == d2 && r1 == r2 && eu1 == eu2 && s1 == s2

instance Eq (PlutusScriptWitness lang CertifyingScript era) where
  (==) (PlutusScriptWitness s1 l1 d1 r1 eu1) (PlutusScriptWitness s2 l2 d2 r2 eu2) =
    l1 == l2 && d1 == d2 && r1 == r2 && eu1 == eu2 && s1 == s2

instance Eq (PlutusScriptWitness lang ProposingScript era) where
  (==) (PlutusScriptWitness s1 l1 d1 r1 eu1) (PlutusScriptWitness s2 l2 d2 r2 eu2) =
    l1 == l2 && d1 == d2 && r1 == r2 && eu1 == eu2 && s1 == s2

instance Eq (PlutusScriptWitness lang VotingScript era) where
  (==) (PlutusScriptWitness s1 l1 d1 r1 eu1) (PlutusScriptWitness s2 l2 d2 r2 eu2) =
    l1 == l2 && d1 == d2 && r1 == r2 && eu1 == eu2 && s1 == s2

getPlutusScriptWitnessLanguage :: PlutusScriptWitness lang purpose era -> L.Language
getPlutusScriptWitnessLanguage (PlutusScriptWitness l _ _ _ _) =
  case l of
    L.SPlutusV1 -> L.plutusLanguage l
    L.SPlutusV2 -> L.plutusLanguage l
    L.SPlutusV3 -> L.plutusLanguage l
    L.SPlutusV4 -> L.plutusLanguage l

getSpendingPlutusWitnessData
  :: forall era lang
   . L.Era era
  => PlutusScriptWitness lang SpendingScript era
  -> L.TxDats era
getSpendingPlutusWitnessData (PlutusScriptWitness L.SPlutusV1 _ d _ _) =
  case d of
    SpendingScriptDatum sd ->
      let d' :: Plutus.Data era = toAlonzoData sd
       in L.TxDats $ fromList [(Plutus.hashData d', d')]
    InlineDatum -> mempty
    NoScriptDatum -> mempty
getSpendingPlutusWitnessData (PlutusScriptWitness L.SPlutusV2 _ d _ _) =
  case d of
    SpendingScriptDatum sd ->
      let d' :: Plutus.Data era = toAlonzoData sd
       in L.TxDats $ fromList [(Plutus.hashData d', d')]
    InlineDatum -> mempty
    NoScriptDatum -> mempty
getSpendingPlutusWitnessData (PlutusScriptWitness L.SPlutusV3 _ d _ _) =
  case d of
    SpendingScriptDatum mSd -> case mSd of
      Just sd ->
        let d' :: Plutus.Data era = toAlonzoData sd
         in L.TxDats $ fromList [(Plutus.hashData d', d')]
      Nothing -> mempty
    InlineDatum -> mempty
    NoScriptDatum -> mempty
getSpendingPlutusWitnessData (PlutusScriptWitness L.SPlutusV4 _ d _ _) =
  case d of
    SpendingScriptDatum mSd -> case mSd of
      Just sd ->
        let d' :: Plutus.Data era = toAlonzoData sd
         in L.TxDats $ fromList [(Plutus.hashData d', d')]
      Nothing -> mempty
    InlineDatum -> mempty
    NoScriptDatum -> mempty

getPlutusScriptRunnable :: PlutusScriptWitness lang purpose era -> Maybe (L.PlutusRunnable lang)
getPlutusScriptRunnable (PlutusScriptWitness _ (PScript (PlutusScriptInEra plutusScriptRunnable)) _ _ _) =
  Just plutusScriptRunnable
getPlutusScriptRunnable (PlutusScriptWitness _ PReferenceScript{} _ _ _) = Nothing

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

data NoScriptDatum = NoScriptDatumAllowed deriving (Show, Eq)

-- | The PlutusScriptDatum type family is used to determine if a script datum is allowed
-- for a given plutus script purpose and version. This change was proposed in CIP-69
-- https://github.com/cardano-foundation/CIPs/tree/master/CIP-0069
type family PlutusScriptDatumF (lang :: L.Language) (purpose :: PlutusScriptPurpose) where
  PlutusScriptDatumF L.PlutusV1 SpendingScript = HashableScriptData
  PlutusScriptDatumF L.PlutusV2 SpendingScript = HashableScriptData
  PlutusScriptDatumF L.PlutusV3 SpendingScript = Maybe HashableScriptData -- CIP-69
  PlutusScriptDatumF L.PlutusV4 SpendingScript = Maybe HashableScriptData -- CIP-69
  PlutusScriptDatumF L.PlutusV1 MintingScript = NoScriptDatum
  PlutusScriptDatumF L.PlutusV2 MintingScript = NoScriptDatum
  PlutusScriptDatumF L.PlutusV3 MintingScript = NoScriptDatum
  PlutusScriptDatumF L.PlutusV4 MintingScript = NoScriptDatum
  PlutusScriptDatumF L.PlutusV1 WithdrawingScript = NoScriptDatum
  PlutusScriptDatumF L.PlutusV2 WithdrawingScript = NoScriptDatum
  PlutusScriptDatumF L.PlutusV3 WithdrawingScript = NoScriptDatum
  PlutusScriptDatumF L.PlutusV4 WithdrawingScript = NoScriptDatum
  PlutusScriptDatumF L.PlutusV1 CertifyingScript = NoScriptDatum
  PlutusScriptDatumF L.PlutusV2 CertifyingScript = NoScriptDatum
  PlutusScriptDatumF L.PlutusV3 CertifyingScript = NoScriptDatum
  PlutusScriptDatumF L.PlutusV4 CertifyingScript = NoScriptDatum
  PlutusScriptDatumF L.PlutusV1 ProposingScript = NoScriptDatum
  PlutusScriptDatumF L.PlutusV2 ProposingScript = NoScriptDatum
  PlutusScriptDatumF L.PlutusV3 ProposingScript = NoScriptDatum
  PlutusScriptDatumF L.PlutusV4 ProposingScript = NoScriptDatum
  PlutusScriptDatumF L.PlutusV1 VotingScript = NoScriptDatum
  PlutusScriptDatumF L.PlutusV2 VotingScript = NoScriptDatum
  PlutusScriptDatumF L.PlutusV3 VotingScript = NoScriptDatum
  PlutusScriptDatumF L.PlutusV4 VotingScript = NoScriptDatum

data PlutusScriptDatum (lang :: L.Language) (purpose :: PlutusScriptPurpose) where
  SpendingScriptDatum
    :: PlutusScriptDatumF lang SpendingScript -> PlutusScriptDatum lang SpendingScript
  InlineDatum :: PlutusScriptDatum lang purpose
  NoScriptDatum
    :: PlutusScriptDatum lang purpose

instance Eq (PlutusScriptDatumF lang SpendingScript) => Eq (PlutusScriptDatum lang SpendingScript) where
  (==) (SpendingScriptDatum d1) (SpendingScriptDatum d2) = d1 == d2
  (==) InlineDatum InlineDatum = True
  (==) NoScriptDatum NoScriptDatum = True
  (==) _ _ = False

--
instance Eq (PlutusScriptDatum lang MintingScript) where
  (==) InlineDatum InlineDatum = True
  (==) NoScriptDatum NoScriptDatum = True
  (==) _ _ = False

instance Eq (PlutusScriptDatum lang WithdrawingScript) where
  (==) InlineDatum InlineDatum = True
  (==) NoScriptDatum NoScriptDatum = True
  (==) _ _ = False

instance Eq (PlutusScriptDatum lang CertifyingScript) where
  (==) InlineDatum InlineDatum = True
  (==) NoScriptDatum NoScriptDatum = True
  (==) _ _ = False

instance Eq (PlutusScriptDatum lang ProposingScript) where
  (==) InlineDatum InlineDatum = True
  (==) NoScriptDatum NoScriptDatum = True
  (==) _ _ = False

instance Eq (PlutusScriptDatum lang VotingScript) where
  (==) InlineDatum InlineDatum = True
  (==) NoScriptDatum NoScriptDatum = True
  (==) _ _ = False

instance Show (PlutusScriptDatum lang purpose) where
  show = \case
    SpendingScriptDatum _d -> "Datum"
    InlineDatum -> "InlineDatum"
    NoScriptDatum -> "NoScriptDatum"
