{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Rpc.Server.Internal.UtxoRpc.Type.Governance
  ( proposalProcedureToUtxoRpcProposal
  )
where

import Cardano.Api.Address
import Cardano.Api.Era
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Serialise.Raw
import Cardano.Api.Tx
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as U5c
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as UtxoRpc
import Cardano.Rpc.Server.Internal.Orphans ()
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.Certificate
  ( anchorToUtxoRpcAnchor
  , credentialToUtxoRpcStakeCredential
  , scriptHashToBytes
  )
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.ProtocolParameters
  ( pparamsUpdateToUtxoRpcPParams
  )

import Cardano.Ledger.BaseTypes qualified as L
import Cardano.Ledger.Conway.PParams qualified as L

import RIO

import Data.Map.Strict qualified as M
import Data.ProtoLens (defMessage)
import Network.GRPC.Spec

-- | Convert a ledger governance proposal procedure to the UTxO RPC
-- 'UtxoRpc.GovernanceActionProposal' message.
proposalProcedureToUtxoRpcProposal
  :: L.ConwayEraPParams era
  => L.ProposalProcedure era
  -> Proto UtxoRpc.GovernanceActionProposal
proposalProcedureToUtxoRpcProposal proposal =
  defMessage
    & U5c.deposit .~ inject (L.pProcDeposit proposal)
    & U5c.rewardAccount .~ serialiseToRawBytes (fromShelleyStakeAddr (L.pProcReturnAddr proposal))
    & U5c.govAction .~ govActionToUtxoRpcGovernanceAction (L.pProcGovAction proposal)
    & U5c.anchor .~ anchorToUtxoRpcAnchor (L.pProcAnchor proposal)

-- | Convert a ledger governance action to the UTxO RPC 'UtxoRpc.GovernanceAction'
-- message, dispatching on the action type to select the corresponding oneof field.
govActionToUtxoRpcGovernanceAction
  :: L.ConwayEraPParams era
  => L.GovAction era
  -> Proto UtxoRpc.GovernanceAction
govActionToUtxoRpcGovernanceAction = \case
  L.ParameterChange prevActionId pparamsUpdate guardrailsScriptHash ->
    defMessage
      & U5c.parameterChangeAction
        .~ ( defMessage
               & U5c.maybe'govActionId
                 .~ fmap govPurposeIdToUtxoRpcGovernanceActionId (L.strictMaybeToMaybe prevActionId)
               & U5c.protocolParamUpdate .~ pparamsUpdateToUtxoRpcPParams pparamsUpdate
               & U5c.policyHash .~ L.strictMaybe mempty scriptHashToBytes guardrailsScriptHash
           )
  L.HardForkInitiation prevActionId protocolVersion ->
    defMessage
      & U5c.hardForkInitiationAction
        .~ ( defMessage
               & U5c.maybe'govActionId
                 .~ fmap govPurposeIdToUtxoRpcGovernanceActionId (L.strictMaybeToMaybe prevActionId)
               & U5c.protocolVersion .~ inject protocolVersion
           )
  L.TreasuryWithdrawals withdrawals guardrailsScriptHash ->
    defMessage
      & U5c.treasuryWithdrawalsAction
        .~ ( defMessage
               & U5c.withdrawals
                 .~ ( M.toList withdrawals <&> \(accountAddress, coin) ->
                        defMessage
                          & U5c.rewardAccount
                            .~ serialiseToRawBytes (fromShelleyStakeAddr accountAddress)
                          & U5c.coin .~ inject coin
                    )
               & U5c.policyHash .~ L.strictMaybe mempty scriptHashToBytes guardrailsScriptHash
           )
  L.NoConfidence prevActionId ->
    defMessage
      & U5c.noConfidenceAction
        .~ ( defMessage
               & U5c.maybe'govActionId
                 .~ fmap govPurposeIdToUtxoRpcGovernanceActionId (L.strictMaybeToMaybe prevActionId)
           )
  L.UpdateCommittee prevActionId removedMembers addedMembers threshold ->
    defMessage
      & U5c.updateCommitteeAction
        .~ ( defMessage
               & U5c.maybe'govActionId
                 .~ fmap govPurposeIdToUtxoRpcGovernanceActionId (L.strictMaybeToMaybe prevActionId)
               & U5c.removeCommitteeCredentials
                 .~ map credentialToUtxoRpcStakeCredential (toList removedMembers)
               & U5c.newCommitteeCredentials
                 .~ ( M.toList addedMembers <&> \(coldCredential, expirationEpoch) ->
                        defMessage
                          & U5c.committeeColdCredential
                            .~ credentialToUtxoRpcStakeCredential coldCredential
                          & U5c.expiresEpoch .~ fromIntegral (L.unEpochNo expirationEpoch)
                    )
               & U5c.newCommitteeThreshold .~ inject (L.unboundRational threshold)
           )
  L.NewConstitution prevActionId constitution ->
    defMessage
      & U5c.newConstitutionAction
        .~ ( defMessage
               & U5c.maybe'govActionId
                 .~ fmap govPurposeIdToUtxoRpcGovernanceActionId (L.strictMaybeToMaybe prevActionId)
               & U5c.constitution
                 .~ ( defMessage
                        & U5c.anchor .~ anchorToUtxoRpcAnchor (L.constitutionAnchor constitution)
                        & U5c.hash
                          .~ L.strictMaybe
                            mempty
                            scriptHashToBytes
                            (L.constitutionGuardrailsScriptHash constitution)
                    )
           )
  L.InfoAction ->
    defMessage & U5c.infoAction .~ defMessage

-- | Convert a governance purpose id, which identifies the previous governance
-- action, to the UTxO RPC 'UtxoRpc.GovernanceActionId' message.
govPurposeIdToUtxoRpcGovernanceActionId
  :: L.GovPurposeId purpose -> Proto UtxoRpc.GovernanceActionId
govPurposeIdToUtxoRpcGovernanceActionId =
  govActionIdToUtxoRpcGovernanceActionId . L.unGovPurposeId

-- | Convert a ledger governance action id to the UTxO RPC
-- 'UtxoRpc.GovernanceActionId' message.
govActionIdToUtxoRpcGovernanceActionId :: L.GovActionId -> Proto UtxoRpc.GovernanceActionId
govActionIdToUtxoRpcGovernanceActionId govActionId =
  defMessage
    & U5c.transactionId .~ serialiseToRawBytes (fromShelleyTxId (L.gaidTxId govActionId))
    & U5c.governanceActionIndex .~ fromIntegral (L.unGovActionIx (L.gaidGovActionIx govActionId))
