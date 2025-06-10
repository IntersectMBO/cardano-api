{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Cardano.Api.Experimental.Plutus.Internal.Shim.LegacyScripts
  ( legacyWitnessToScriptRequirements
  , legacyWitnessConversion
  , toPlutusSLanguage
  )
where

import Cardano.Api.Era.Internal.Eon.AlonzoEraOnwards
import Cardano.Api.Era.Internal.Eon.ShelleyBasedEra
import Cardano.Api.Experimental.Plutus.Internal.IndexedPlutusScriptWitness
import Cardano.Api.Experimental.Plutus.Internal.Script
import Cardano.Api.Experimental.Plutus.Internal.ScriptWitness
import Cardano.Api.Experimental.Simple.Script
import Cardano.Api.Experimental.Tx.Internal.AnyWitness
import Cardano.Api.Experimental.Tx.Internal.TxScriptWitnessRequirements
import Cardano.Api.Plutus.Internal.Script
  ( ExecutionUnits
  , Witness
  )
import Cardano.Api.Plutus.Internal.Script qualified as Old
import Cardano.Api.Pretty
import Cardano.Api.Tx.Internal.BuildTxWith

import Cardano.Binary qualified as CBOR
import Cardano.Ledger.Alonzo.Scripts qualified as L
import Cardano.Ledger.BaseTypes (Version)
import Cardano.Ledger.Core qualified as L
import Cardano.Ledger.Plutus.Language qualified as L

import Data.Text qualified as Text

-- | This module is concerned with converting legacy api scripts and by extension
-- script witnesses to the new api.

-- Remember we don't care about simple script witnesses beyond the fact that they require key witnesses
-- and therefore contribute to the determination of the script witness index.
toAnyWitness
  :: AlonzoEraOnwards era
  -> (Witnessable thing (ShelleyLedgerEra era), BuildTxWith BuildTx (Witness witctx era))
  -> Either
       CBOR.DecoderError
       (Witnessable thing (ShelleyLedgerEra era), AnyWitness (ShelleyLedgerEra era))
toAnyWitness _ (witnessable, BuildTxWith (Old.KeyWitness _)) =
  return (witnessable, AnyKeyWitnessPlaceholder)
toAnyWitness eon (witnessable, BuildTxWith (Old.ScriptWitness _ oldSw@Old.SimpleScriptWitness{})) =
  convertToNewScriptWitness eon oldSw witnessable
toAnyWitness eon (witnessable, BuildTxWith (Old.ScriptWitness _ oldApiPlutusScriptWitness)) =
  convertToNewScriptWitness eon oldApiPlutusScriptWitness witnessable

type family ToPlutusScriptPurpose witnessable = (purpose :: PlutusScriptPurpose) | purpose -> witnessable where
  ToPlutusScriptPurpose TxInItem = SpendingScript
  ToPlutusScriptPurpose CertItem = MintingScript
  ToPlutusScriptPurpose MintItem = CertifyingScript
  ToPlutusScriptPurpose WithdrawalItem = WithdrawingScript
  ToPlutusScriptPurpose VoterItem = ProposingScript
  ToPlutusScriptPurpose ProposalItem = VotingScript

convertToNewScriptWitness
  :: AlonzoEraOnwards era
  -> Old.ScriptWitness witctx era
  -> Witnessable thing (ShelleyLedgerEra era)
  -> Either
       CBOR.DecoderError
       (Witnessable thing (ShelleyLedgerEra era), AnyWitness (ShelleyLedgerEra era))
convertToNewScriptWitness eon (Old.PlutusScriptWitness _ v scriptOrRefInput datum scriptRedeemer execUnits) witnessable = do
  let d = createPlutusScriptDatum witnessable v datum
  newScriptWitness <-
    obtainConstraints v $
      toNewPlutusScriptWitness
        eon
        v
        scriptOrRefInput
        scriptRedeemer
        execUnits
        d
  return (witnessable, newScriptWitness)
convertToNewScriptWitness eon (Old.SimpleScriptWitness _ scriptOrRefInput) witnessable =
  case scriptOrRefInput of
    Old.SScript simpleScript -> alonzoEraOnwardsConstraints eon $ do
      let timelock = convertTotimelock eon simpleScript
      return (witnessable, AnySimpleScriptWitness $ SScript $ SimpleScript timelock)
    Old.SReferenceScript txIn ->
      return (witnessable, AnySimpleScriptWitness $ SReferenceScript txIn)

convertTotimelock
  :: AlonzoEraOnwards era -> Old.SimpleScript -> L.NativeScript (ShelleyLedgerEra era)
convertTotimelock eon s = alonzoEraOnwardsConstraints eon $ Old.toAllegraTimelock s

createPlutusScriptDatum
  :: Witnessable thing era
  -> Old.PlutusScriptVersion lang
  -> Old.ScriptDatum witctx
  -> PlutusScriptDatum (Old.ToLedgerPlutusLanguage lang) SpendingScript
createPlutusScriptDatum missingContext plutusVersion oldDatum =
  case (missingContext, oldDatum) of
    (w@WitTxIn{}, d@Old.ScriptDatumForTxIn{}) -> toPlutusScriptDatum w plutusVersion d
    (WitTxIn{}, _) -> NoScriptDatum
    (WitMint{}, _) -> NoScriptDatum
    (WitWithdrawal{}, _) -> NoScriptDatum
    (WitProposal{}, _) -> NoScriptDatum
    (WitVote{}, _) -> NoScriptDatum
    (WitTxCert{}, _) -> NoScriptDatum

toPlutusScriptDatum
  :: Witnessable TxInItem era
  -> Old.PlutusScriptVersion lang
  -> Old.ScriptDatum Old.WitCtxTxIn
  -> PlutusScriptDatum (Old.ToLedgerPlutusLanguage lang) (ToPlutusScriptPurpose TxInItem)
-- ^ Encapsulates CIP-69: V3 spending script datums are optional
toPlutusScriptDatum WitTxIn{} Old.PlutusScriptV3 (Old.ScriptDatumForTxIn r) = SpendingScriptDatum r
-- \^ V2 and V1 spending script datums are required
toPlutusScriptDatum WitTxIn{} Old.PlutusScriptV2 (Old.ScriptDatumForTxIn (Just r)) = SpendingScriptDatum r
toPlutusScriptDatum WitTxIn{} Old.PlutusScriptV1 (Old.ScriptDatumForTxIn (Just r)) = SpendingScriptDatum r
-- \^ V2 and V3 scripts can have inline datums
toPlutusScriptDatum WitTxIn{} Old.PlutusScriptV3 Old.InlineScriptDatum = InlineDatum
toPlutusScriptDatum WitTxIn{} Old.PlutusScriptV2 Old.InlineScriptDatum = InlineDatum
-- \^ Everything else is not allowed. The old api does not prevent these invalid combinations.
-- The valid combinations are enforced in the PlutusScriptDatum type family within the
-- resultant PlutusScriptDatum GADT.
toPlutusScriptDatum WitTxIn{} Old.PlutusScriptV1 Old.InlineScriptDatum = NoScriptDatum
toPlutusScriptDatum WitTxIn{} Old.PlutusScriptV1 (Old.ScriptDatumForTxIn Nothing) = NoScriptDatum
toPlutusScriptDatum WitTxIn{} Old.PlutusScriptV2 (Old.ScriptDatumForTxIn Nothing) = NoScriptDatum

toNewPlutusScriptWitness
  :: forall era lang purpose
   . L.PlutusLanguage (Old.ToLedgerPlutusLanguage lang)
  => AlonzoEraOnwards era
  -> Old.PlutusScriptVersion lang
  -> Old.PlutusScriptOrReferenceInput lang
  -> ScriptRedeemer
  -> ExecutionUnits
  -> PlutusScriptDatum (Old.ToLedgerPlutusLanguage lang) purpose
  -> Either
       CBOR.DecoderError
       ( AnyWitness
           (ShelleyLedgerEra era)
       )
toNewPlutusScriptWitness eon l (Old.PScript (Old.PlutusScriptSerialised scriptShortBs)) scriptRedeemer execUnits datum = do
  let protocolVersion = getVersion eon
      plutusScript = L.Plutus $ L.PlutusBinary scriptShortBs

  case L.decodePlutusRunnable @(Old.ToLedgerPlutusLanguage lang) protocolVersion plutusScript of
    Left e ->
      Left $
        CBOR.DecoderErrorCustom "PlutusLedgerApi.Common.ScriptDecodeError" (Text.pack . show $ pretty e)
    Right plutusScriptRunnable ->
      return
        . AnyPlutusScriptWitness
        $ mkPlutusScriptWitness
          eon
          (toPlutusSLanguage l)
          plutusScriptRunnable
          datum
          scriptRedeemer
          execUnits
toNewPlutusScriptWitness _ l (Old.PReferenceScript refInput) scriptRedeemer execUnits datum =
  return . AnyPlutusScriptWitness $
    PlutusScriptWitness (toPlutusSLanguage l) (PReferenceScript refInput) datum scriptRedeemer execUnits

-- | When it comes to using plutus scripts we need to provide
-- the following to the tx:
--
-- 1. The redeemer pointer map
-- 2. The set of plutus languages in use
-- 3. The set of plutus scripts in use (present in the t)
-- 4. The datum map
legacyWitnessConversion
  :: AlonzoEraOnwards era
  -> [(Witnessable witnessable (ShelleyLedgerEra era), BuildTxWith BuildTx (Witness ctx era))]
  -> Either
       CBOR.DecoderError
       [(Witnessable witnessable (ShelleyLedgerEra era), AnyWitness (ShelleyLedgerEra era))]
legacyWitnessConversion eon = mapM (toAnyWitness eon)

legacyWitnessToScriptRequirements
  :: AlonzoEraOnwards era
  -> [(Witnessable witnessable (ShelleyLedgerEra era), BuildTxWith BuildTx (Witness ctx era))]
  -> Either CBOR.DecoderError (TxScriptWitnessRequirements (ShelleyLedgerEra era))
legacyWitnessToScriptRequirements eon wits = do
  r <- legacyWitnessConversion eon wits
  return $ getTxScriptWitnessesRequirements eon r

-- Misc helpers

getVersion :: forall era. AlonzoEraOnwards era -> Version
getVersion eon = alonzoEraOnwardsConstraints eon $ L.eraProtVerHigh @(ShelleyLedgerEra era)

obtainConstraints
  :: Old.PlutusScriptVersion lang
  -> (L.PlutusLanguage (Old.ToLedgerPlutusLanguage lang) => a)
  -> a
obtainConstraints v =
  case v of
    Old.PlutusScriptV1 -> id
    Old.PlutusScriptV2 -> id
    Old.PlutusScriptV3 -> id

toPlutusSLanguage
  :: Old.PlutusScriptVersion lang -> L.SLanguage (Old.ToLedgerPlutusLanguage lang)
toPlutusSLanguage = \case
  Old.PlutusScriptV1 -> L.SPlutusV1
  Old.PlutusScriptV2 -> L.SPlutusV2
  Old.PlutusScriptV3 -> L.SPlutusV3
