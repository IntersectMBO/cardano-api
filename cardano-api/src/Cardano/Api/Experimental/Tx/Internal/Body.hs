{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Api.Experimental.Tx.Internal.Body
  ( extractAllIndexedPlutusScriptWitnesses
  )
where

import Cardano.Api.Era
import Cardano.Api.Experimental.Era
import Cardano.Api.Experimental.Plutus
import Cardano.Api.Plutus.Internal.Script
import Cardano.Api.Tx.Internal.Body

import Cardano.Binary qualified as CBOR

extractAllIndexedPlutusScriptWitnesses
  :: forall era
   . Era era
  -> TxBodyContent BuildTx era
  -> Either
       CBOR.DecoderError
       [AnyIndexedPlutusScriptWitness (LedgerEra era)]
extractAllIndexedPlutusScriptWitnesses era b = obtainCommonConstraints era $ do
  let sbe = convert era
      aeon = convert era
      legacyTxInWits = extractWitnessableTxIns aeon b
      legacyCertWits = extractWitnessableCertificates aeon b
      legacyMintWits = extractWitnessableMints aeon b
      proposalWits
        :: [(Witnessable ProposalItem (ShelleyLedgerEra era), BuildTxWith BuildTx (Witness WitCtxStake era))] =
          caseShelleyToBabbageOrConwayEraOnwards
            (const [])
            (`extractWitnessableProposals` b)
            sbe
      legacyWithdrawalWits = extractWitnessableWithdrawals aeon b
      legacyVoteWits
        :: [(Witnessable VoterItem (ShelleyLedgerEra era), BuildTxWith BuildTx (Witness WitCtxStake era))] =
          caseShelleyToBabbageOrConwayEraOnwards
            (const [])
            (`extractWitnessableVotes` b)
            sbe

  txInWits <- legacyWitnessConversion aeon legacyTxInWits
  let indexedScriptTxInWits = alonzoEraOnwardsConstraints aeon $ createIndexedPlutusScriptWitnesses txInWits

  certWits <- legacyWitnessConversion aeon legacyCertWits
  let indexedCertScriptWits = alonzoEraOnwardsConstraints aeon $ createIndexedPlutusScriptWitnesses certWits

  mintWits <- legacyWitnessConversion aeon legacyMintWits
  let indexedMintScriptWits = alonzoEraOnwardsConstraints aeon $ createIndexedPlutusScriptWitnesses mintWits

  withdrawalWits <- legacyWitnessConversion aeon legacyWithdrawalWits
  let indexedWithdrawalScriptWits = alonzoEraOnwardsConstraints aeon $ createIndexedPlutusScriptWitnesses withdrawalWits

  proposalScriptWits <- legacyWitnessConversion aeon proposalWits
  let indexedProposalScriptWits = alonzoEraOnwardsConstraints aeon $ createIndexedPlutusScriptWitnesses proposalScriptWits

  voteWits <- legacyWitnessConversion aeon legacyVoteWits
  let indexedVoteScriptWits = alonzoEraOnwardsConstraints aeon $ createIndexedPlutusScriptWitnesses voteWits
  return $
    mconcat
      [ indexedScriptTxInWits
      , indexedMintScriptWits
      , indexedCertScriptWits
      , indexedWithdrawalScriptWits
      , indexedProposalScriptWits
      , indexedVoteScriptWits
      ]
