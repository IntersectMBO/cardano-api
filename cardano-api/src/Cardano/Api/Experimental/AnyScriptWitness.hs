{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Api.Experimental.AnyScriptWitness
  ( AnyScriptWitness (..)
  , AnyPlutusScriptWitness (..)
  , PlutusSpendingScriptWitness (..)
  , getAnyScriptWitnessReferenceInput
  , createPlutusSpendingScriptWitness
  , getAnyPlutusScriptData
  , getAnyPlutusScriptWitnessExecutionUnits
  , getAnyPlutusScriptWitnessRedeemer
  , getAnyPlutusScriptWitnessReferenceInput
  , getAnyPlutusWitnessPlutusScript
  , getAnyPlutusScriptWitnessLanguage
  , langTypeEquality
  , updatePlutusScriptWitnessExecutionUnits
  )
where

import Cardano.Api.Experimental.Plutus.Internal.Script
import Cardano.Api.Experimental.Plutus.Internal.ScriptWitness
import Cardano.Api.Experimental.Simple.Script
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Plutus.Internal.Script (ExecutionUnits)
import Cardano.Api.Tx.Internal.TxIn

import Cardano.Ledger.Plutus.Language qualified as L

import Data.Type.Equality
import Data.Typeable

data AnyScriptWitness era where
  AnyScriptWitnessSimple :: SimpleScriptOrReferenceInput era -> AnyScriptWitness era
  AnyScriptWitnessPlutus :: AnyPlutusScriptWitness lang purpose era -> AnyScriptWitness era

deriving instance Show (AnyScriptWitness era)

instance Eq (AnyScriptWitness era) where
  (AnyScriptWitnessSimple s1) == (AnyScriptWitnessSimple s2) = s1 == s2
  (AnyScriptWitnessPlutus (AnyPlutusSpendingScriptWitness s1)) == (AnyScriptWitnessPlutus (AnyPlutusSpendingScriptWitness s2)) = s1 == s2
  (AnyScriptWitnessPlutus (AnyPlutusMintingScriptWitness s1)) == (AnyScriptWitnessPlutus (AnyPlutusMintingScriptWitness s2)) =
    case langTypeEquality s1 s2 of
      Just Refl -> s1 == s2
      Nothing -> False
  (AnyScriptWitnessPlutus (AnyPlutusWithdrawingScriptWitness s1)) == (AnyScriptWitnessPlutus (AnyPlutusWithdrawingScriptWitness s2)) =
    case langTypeEquality s1 s2 of
      Just Refl -> s1 == s2
      Nothing -> False
  (AnyScriptWitnessPlutus (AnyPlutusCertifyingScriptWitness s1)) == (AnyScriptWitnessPlutus (AnyPlutusCertifyingScriptWitness s2)) =
    case langTypeEquality s1 s2 of
      Just Refl -> s1 == s2
      Nothing -> False
  (AnyScriptWitnessPlutus (AnyPlutusProposingScriptWitness s1)) == (AnyScriptWitnessPlutus (AnyPlutusProposingScriptWitness s2)) =
    case langTypeEquality s1 s2 of
      Just Refl -> s1 == s2
      Nothing -> False
  (AnyScriptWitnessPlutus (AnyPlutusVotingScriptWitness s1)) == (AnyScriptWitnessPlutus (AnyPlutusVotingScriptWitness s2)) =
    case langTypeEquality s1 s2 of
      Just Refl -> s1 == s2
      Nothing -> False
  _ == _ = False

langTypeEquality
  :: (Typeable langA, Typeable langB)
  => PlutusScriptWitness langA purpose era
  -> PlutusScriptWitness langB purpose era
  -> Maybe (langA :~: langB)
langTypeEquality _ _ = eqT

data PlutusSpendingScriptWitness era
  = PlutusSpendingScriptWitnessV1 (PlutusScriptWitness L.PlutusV1 SpendingScript era)
  | PlutusSpendingScriptWitnessV2 (PlutusScriptWitness L.PlutusV2 SpendingScript era)
  | PlutusSpendingScriptWitnessV3 (PlutusScriptWitness L.PlutusV3 SpendingScript era)
  | PlutusSpendingScriptWitnessV4 (PlutusScriptWitness L.PlutusV4 SpendingScript era)
  deriving (Show, Eq)

createPlutusSpendingScriptWitness
  :: L.SLanguage lang
  -> PlutusScriptWitness lang SpendingScript era
  -> PlutusSpendingScriptWitness era
createPlutusSpendingScriptWitness L.SPlutusV1 witness = PlutusSpendingScriptWitnessV1 witness
createPlutusSpendingScriptWitness L.SPlutusV2 witness = PlutusSpendingScriptWitnessV2 witness
createPlutusSpendingScriptWitness L.SPlutusV3 witness = PlutusSpendingScriptWitnessV3 witness
createPlutusSpendingScriptWitness L.SPlutusV4 witness = PlutusSpendingScriptWitnessV4 witness

data AnyPlutusScriptWitness lang purpose era where
  AnyPlutusSpendingScriptWitness
    :: PlutusSpendingScriptWitness era -> AnyPlutusScriptWitness lang SpendingScript era
  AnyPlutusMintingScriptWitness
    :: Typeable lang
    => PlutusScriptWitness lang MintingScript era -> AnyPlutusScriptWitness lang MintingScript era
  AnyPlutusWithdrawingScriptWitness
    :: Typeable lang
    => PlutusScriptWitness lang WithdrawingScript era -> AnyPlutusScriptWitness lang WithdrawingScript era
  AnyPlutusCertifyingScriptWitness
    :: Typeable lang
    => PlutusScriptWitness lang CertifyingScript era -> AnyPlutusScriptWitness lang CertifyingScript era
  AnyPlutusProposingScriptWitness
    :: Typeable lang
    => PlutusScriptWitness lang ProposingScript era -> AnyPlutusScriptWitness lang ProposingScript era
  AnyPlutusVotingScriptWitness
    :: Typeable lang
    => PlutusScriptWitness lang VotingScript era -> AnyPlutusScriptWitness lang VotingScript era

deriving instance Show (AnyPlutusScriptWitness lang purpose era)

deriving instance Eq (AnyPlutusScriptWitness lang purpose era)

getAnyPlutusScriptWitnessExecutionUnits
  :: AnyPlutusScriptWitness lang purpose era -> ExecutionUnits
getAnyPlutusScriptWitnessExecutionUnits (AnyPlutusSpendingScriptWitness s) =
  case s of
    PlutusSpendingScriptWitnessV1 (PlutusScriptWitness _ _ _ _ eu) -> eu
    PlutusSpendingScriptWitnessV2 (PlutusScriptWitness _ _ _ _ eu) -> eu
    PlutusSpendingScriptWitnessV3 (PlutusScriptWitness _ _ _ _ eu) -> eu
    PlutusSpendingScriptWitnessV4 (PlutusScriptWitness _ _ _ _ eu) -> eu
getAnyPlutusScriptWitnessExecutionUnits (AnyPlutusMintingScriptWitness (PlutusScriptWitness _ _ _ _ eu)) = eu
getAnyPlutusScriptWitnessExecutionUnits (AnyPlutusWithdrawingScriptWitness (PlutusScriptWitness _ _ _ _ eu)) = eu
getAnyPlutusScriptWitnessExecutionUnits (AnyPlutusCertifyingScriptWitness (PlutusScriptWitness _ _ _ _ eu)) = eu
getAnyPlutusScriptWitnessExecutionUnits (AnyPlutusProposingScriptWitness (PlutusScriptWitness _ _ _ _ eu)) = eu
getAnyPlutusScriptWitnessExecutionUnits (AnyPlutusVotingScriptWitness (PlutusScriptWitness _ _ _ _ eu)) = eu

getAnyPlutusScriptWitnessRedeemer
  :: AnyPlutusScriptWitness lang purpose era
  -> ScriptRedeemer
getAnyPlutusScriptWitnessRedeemer (AnyPlutusSpendingScriptWitness s) =
  case s of
    PlutusSpendingScriptWitnessV1 (PlutusScriptWitness _ _ _ redeemer _) -> redeemer
    PlutusSpendingScriptWitnessV2 (PlutusScriptWitness _ _ _ redeemer _) -> redeemer
    PlutusSpendingScriptWitnessV3 (PlutusScriptWitness _ _ _ redeemer _) -> redeemer
    PlutusSpendingScriptWitnessV4 (PlutusScriptWitness _ _ _ redeemer _) -> redeemer
getAnyPlutusScriptWitnessRedeemer (AnyPlutusMintingScriptWitness (PlutusScriptWitness _ _ _ redeemer _)) = redeemer
getAnyPlutusScriptWitnessRedeemer (AnyPlutusWithdrawingScriptWitness (PlutusScriptWitness _ _ _ redeemer _)) = redeemer
getAnyPlutusScriptWitnessRedeemer (AnyPlutusCertifyingScriptWitness (PlutusScriptWitness _ _ _ redeemer _)) = redeemer
getAnyPlutusScriptWitnessRedeemer (AnyPlutusProposingScriptWitness (PlutusScriptWitness _ _ _ redeemer _)) = redeemer
getAnyPlutusScriptWitnessRedeemer (AnyPlutusVotingScriptWitness (PlutusScriptWitness _ _ _ redeemer _)) = redeemer

updatePlutusScriptWitnessExecutionUnits
  :: ExecutionUnits -> AnyPlutusScriptWitness lang purpose era -> AnyPlutusScriptWitness lang purpose era
updatePlutusScriptWitnessExecutionUnits eu (AnyPlutusSpendingScriptWitness s) =
  case s of
    PlutusSpendingScriptWitnessV1 (PlutusScriptWitness lang script dat redeemer _) ->
      AnyPlutusSpendingScriptWitness
        (PlutusSpendingScriptWitnessV1 (PlutusScriptWitness lang script dat redeemer eu))
    PlutusSpendingScriptWitnessV2 (PlutusScriptWitness lang script dat redeemer _) ->
      AnyPlutusSpendingScriptWitness
        (PlutusSpendingScriptWitnessV2 (PlutusScriptWitness lang script dat redeemer eu))
    PlutusSpendingScriptWitnessV3 (PlutusScriptWitness lang script dat redeemer _) ->
      AnyPlutusSpendingScriptWitness
        (PlutusSpendingScriptWitnessV3 (PlutusScriptWitness lang script dat redeemer eu))
    PlutusSpendingScriptWitnessV4 (PlutusScriptWitness lang script dat redeemer _) ->
      AnyPlutusSpendingScriptWitness
        (PlutusSpendingScriptWitnessV4 (PlutusScriptWitness lang script dat redeemer eu))
updatePlutusScriptWitnessExecutionUnits eu (AnyPlutusMintingScriptWitness (PlutusScriptWitness lang script dat redeemer _)) =
  AnyPlutusMintingScriptWitness (PlutusScriptWitness lang script dat redeemer eu)
updatePlutusScriptWitnessExecutionUnits eu (AnyPlutusWithdrawingScriptWitness (PlutusScriptWitness lang script dat redeemer _)) =
  AnyPlutusWithdrawingScriptWitness (PlutusScriptWitness lang script dat redeemer eu)
updatePlutusScriptWitnessExecutionUnits eu (AnyPlutusCertifyingScriptWitness (PlutusScriptWitness lang script dat redeemer _)) =
  AnyPlutusCertifyingScriptWitness (PlutusScriptWitness lang script dat redeemer eu)
updatePlutusScriptWitnessExecutionUnits eu (AnyPlutusProposingScriptWitness (PlutusScriptWitness lang script dat redeemer _)) =
  AnyPlutusProposingScriptWitness (PlutusScriptWitness lang script dat redeemer eu)
updatePlutusScriptWitnessExecutionUnits eu (AnyPlutusVotingScriptWitness (PlutusScriptWitness lang script dat redeemer _)) =
  AnyPlutusVotingScriptWitness (PlutusScriptWitness lang script dat redeemer eu)

getAnyPlutusScriptWitnessLanguage
  :: AnyPlutusScriptWitness lang purpose era -> L.Language
getAnyPlutusScriptWitnessLanguage (AnyPlutusSpendingScriptWitness s) =
  case s of
    PlutusSpendingScriptWitnessV1 s' -> getPlutusScriptWitnessLanguage s'
    PlutusSpendingScriptWitnessV2 s' -> getPlutusScriptWitnessLanguage s'
    PlutusSpendingScriptWitnessV3 s' -> getPlutusScriptWitnessLanguage s'
    PlutusSpendingScriptWitnessV4 s' -> getPlutusScriptWitnessLanguage s'
getAnyPlutusScriptWitnessLanguage (AnyPlutusMintingScriptWitness s) = getPlutusScriptWitnessLanguage s
getAnyPlutusScriptWitnessLanguage (AnyPlutusWithdrawingScriptWitness s) = getPlutusScriptWitnessLanguage s
getAnyPlutusScriptWitnessLanguage (AnyPlutusCertifyingScriptWitness s) = getPlutusScriptWitnessLanguage s
getAnyPlutusScriptWitnessLanguage (AnyPlutusProposingScriptWitness s) = getPlutusScriptWitnessLanguage s
getAnyPlutusScriptWitnessLanguage (AnyPlutusVotingScriptWitness s) = getPlutusScriptWitnessLanguage s

getAnyScriptWitnessReferenceInput
  :: AnyScriptWitness era
  -> Maybe TxIn
getAnyScriptWitnessReferenceInput (AnyScriptWitnessSimple s) =
  case s of
    SReferenceScript txin -> Just txin
    SScript{} -> Nothing
getAnyScriptWitnessReferenceInput (AnyScriptWitnessPlutus psw) =
  getAnyPlutusScriptWitnessReferenceInput psw

getAnyPlutusScriptWitnessReferenceInput
  :: AnyPlutusScriptWitness lang purpose era
  -> Maybe TxIn
getAnyPlutusScriptWitnessReferenceInput (AnyPlutusSpendingScriptWitness s) =
  case s of
    PlutusSpendingScriptWitnessV1 (PlutusScriptWitness _ ((PReferenceScript txin)) _ _ _) -> Just txin
    PlutusSpendingScriptWitnessV1 (PlutusScriptWitness _ PScript{} _ _ _) -> Nothing
    PlutusSpendingScriptWitnessV2 (PlutusScriptWitness _ (PReferenceScript txin) _ _ _) -> Just txin
    PlutusSpendingScriptWitnessV2 (PlutusScriptWitness _ PScript{} _ _ _) -> Nothing
    PlutusSpendingScriptWitnessV3 (PlutusScriptWitness _ (PReferenceScript txin) _ _ _) -> Just txin
    PlutusSpendingScriptWitnessV3 (PlutusScriptWitness _ PScript{} _ _ _) -> Nothing
    PlutusSpendingScriptWitnessV4 (PlutusScriptWitness _ (PReferenceScript txin) _ _ _) -> Just txin
    PlutusSpendingScriptWitnessV4 (PlutusScriptWitness _ PScript{} _ _ _) -> Nothing
getAnyPlutusScriptWitnessReferenceInput psw =
  case psw of
    AnyPlutusMintingScriptWitness (PlutusScriptWitness _ (PReferenceScript txin) _ _ _) -> Just txin
    AnyPlutusMintingScriptWitness (PlutusScriptWitness _ PScript{} _ _ _) -> Nothing
    AnyPlutusWithdrawingScriptWitness (PlutusScriptWitness _ (PReferenceScript txin) _ _ _) -> Just txin
    AnyPlutusWithdrawingScriptWitness (PlutusScriptWitness _ PScript{} _ _ _) -> Nothing
    AnyPlutusCertifyingScriptWitness (PlutusScriptWitness _ (PReferenceScript txin) _ _ _) -> Just txin
    AnyPlutusCertifyingScriptWitness (PlutusScriptWitness _ PScript{} _ _ _) -> Nothing
    AnyPlutusProposingScriptWitness (PlutusScriptWitness _ (PReferenceScript txin) _ _ _) -> Just txin
    AnyPlutusProposingScriptWitness (PlutusScriptWitness _ PScript{} _ _ _) -> Nothing
    AnyPlutusVotingScriptWitness (PlutusScriptWitness _ (PReferenceScript txin) _ _ _) -> Just txin
    AnyPlutusVotingScriptWitness (PlutusScriptWitness _ PScript{} _ _ _) -> Nothing

getAnyPlutusScriptData
  :: L.Era era
  => AnyPlutusScriptWitness lang purpose era
  -> L.TxDats era
getAnyPlutusScriptData (AnyPlutusSpendingScriptWitness s) =
  case s of
    PlutusSpendingScriptWitnessV1 sw -> getSpendingPlutusWitnessData sw
    PlutusSpendingScriptWitnessV2 sw -> getSpendingPlutusWitnessData sw
    PlutusSpendingScriptWitnessV3 sw -> getSpendingPlutusWitnessData sw
    PlutusSpendingScriptWitnessV4 sw -> getSpendingPlutusWitnessData sw
getAnyPlutusScriptData AnyPlutusMintingScriptWitness{} = mempty
getAnyPlutusScriptData AnyPlutusWithdrawingScriptWitness{} = mempty
getAnyPlutusScriptData AnyPlutusCertifyingScriptWitness{} = mempty
getAnyPlutusScriptData AnyPlutusProposingScriptWitness{} = mempty
getAnyPlutusScriptData AnyPlutusVotingScriptWitness{} = mempty

getAnyPlutusWitnessPlutusScript
  :: L.AlonzoEraScript era
  => AnyPlutusScriptWitness lang purpose era
  -> Maybe (L.Script era)
getAnyPlutusWitnessPlutusScript (AnyPlutusSpendingScriptWitness (PlutusSpendingScriptWitnessV1 s)) =
  let plutusScriptRunnable = getPlutusScriptRunnable s
   in L.fromPlutusScript <$> (fromPlutusRunnable L.SPlutusV1 =<< plutusScriptRunnable)
getAnyPlutusWitnessPlutusScript (AnyPlutusSpendingScriptWitness (PlutusSpendingScriptWitnessV2 s)) =
  let plutusScriptRunnable = getPlutusScriptRunnable s
   in L.fromPlutusScript <$> (fromPlutusRunnable L.SPlutusV2 =<< plutusScriptRunnable)
getAnyPlutusWitnessPlutusScript (AnyPlutusSpendingScriptWitness (PlutusSpendingScriptWitnessV3 s)) =
  let plutusScriptRunnable = getPlutusScriptRunnable s
   in L.fromPlutusScript <$> (fromPlutusRunnable L.SPlutusV3 =<< plutusScriptRunnable)
getAnyPlutusWitnessPlutusScript (AnyPlutusSpendingScriptWitness (PlutusSpendingScriptWitnessV4 s)) =
  let plutusScriptRunnable = getPlutusScriptRunnable s
   in L.fromPlutusScript <$> (fromPlutusRunnable L.SPlutusV4 =<< plutusScriptRunnable)
getAnyPlutusWitnessPlutusScript (AnyPlutusMintingScriptWitness s@(PlutusScriptWitness l _ _ _ _)) =
  let plutusScriptRunnable = getPlutusScriptRunnable s
   in L.fromPlutusScript <$> (fromPlutusRunnable l =<< plutusScriptRunnable)
getAnyPlutusWitnessPlutusScript (AnyPlutusWithdrawingScriptWitness s@(PlutusScriptWitness l _ _ _ _)) =
  let plutusScriptRunnable = getPlutusScriptRunnable s
   in L.fromPlutusScript <$> (fromPlutusRunnable l =<< plutusScriptRunnable)
getAnyPlutusWitnessPlutusScript (AnyPlutusCertifyingScriptWitness s@(PlutusScriptWitness l _ _ _ _)) =
  let plutusScriptRunnable = getPlutusScriptRunnable s
   in L.fromPlutusScript <$> (fromPlutusRunnable l =<< plutusScriptRunnable)
getAnyPlutusWitnessPlutusScript (AnyPlutusProposingScriptWitness s@(PlutusScriptWitness l _ _ _ _)) =
  let plutusScriptRunnable = getPlutusScriptRunnable s
   in L.fromPlutusScript <$> (fromPlutusRunnable l =<< plutusScriptRunnable)
getAnyPlutusWitnessPlutusScript (AnyPlutusVotingScriptWitness s@(PlutusScriptWitness l _ _ _ _)) =
  let plutusScriptRunnable = getPlutusScriptRunnable s
   in L.fromPlutusScript <$> (fromPlutusRunnable l =<< plutusScriptRunnable)

-- It should be noted that 'PlutusRunnable' is constructed via deserialization. The deserialization
-- instance lives in ledger and will fail for an invalid script language/era pairing.
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
