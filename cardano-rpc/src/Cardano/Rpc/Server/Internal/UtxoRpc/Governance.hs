{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Rpc.Server.Internal.UtxoRpc.Governance
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
import Cardano.Rpc.Server.Internal.UtxoRpc.Certificate
  ( anchorToUtxoRpcAnchor
  , credentialToUtxoRpcStakeCredential
  )

import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.BaseTypes qualified as L
import Cardano.Ledger.Conway.PParams qualified as L
import Cardano.Ledger.Hashes qualified as L (ScriptHash (..))

import RIO

import Data.Map.Strict qualified as M
import Data.ProtoLens (defMessage)
import Network.GRPC.Spec

-- | Convert a ledger governance proposal procedure to the UTxO RPC
-- 'UtxoRpc.GovernanceActionProposal' message.
proposalProcedureToUtxoRpcProposal
  :: L.ConwayEraPParams era
  => L.ProposalProcedure era
  -> UtxoRpc.GovernanceActionProposal
proposalProcedureToUtxoRpcProposal proposal =
  defMessage
    & U5c.deposit .~ getProto (inject (L.pProcDeposit proposal))
    & U5c.rewardAccount .~ serialiseToRawBytes (fromShelleyStakeAddr (L.pProcReturnAddr proposal))
    & U5c.govAction .~ govActionToUtxoRpcGovernanceAction (L.pProcGovAction proposal)
    & U5c.anchor .~ anchorToUtxoRpcAnchor (L.pProcAnchor proposal)

-- | Convert a ledger governance action to the UTxO RPC 'UtxoRpc.GovernanceAction'
-- message, dispatching on the action type to select the corresponding oneof field.
govActionToUtxoRpcGovernanceAction
  :: L.ConwayEraPParams era
  => L.GovAction era
  -> UtxoRpc.GovernanceAction
govActionToUtxoRpcGovernanceAction = \case
  L.ParameterChange prevActionId pparamsUpdate guardrailsScriptHash ->
    defMessage
      & U5c.parameterChangeAction
        .~ ( defMessage
               & U5c.maybe'govActionId .~ prevGovActionIdToUtxoRpcGovernanceActionId prevActionId
               & U5c.protocolParamUpdate .~ pparamsUpdateToUtxoRpcPParams pparamsUpdate
               & U5c.policyHash .~ scriptHashToBytes guardrailsScriptHash
           )
  L.HardForkInitiation prevActionId protocolVersion ->
    defMessage
      & U5c.hardForkInitiationAction
        .~ ( defMessage
               & U5c.maybe'govActionId .~ prevGovActionIdToUtxoRpcGovernanceActionId prevActionId
               & U5c.protocolVersion
                 .~ ( defMessage
                        & U5c.major .~ L.getVersion (L.pvMajor protocolVersion)
                        & U5c.minor .~ fromIntegral (L.pvMinor protocolVersion)
                    )
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
                          & U5c.coin .~ getProto (inject coin)
                    )
               & U5c.policyHash .~ scriptHashToBytes guardrailsScriptHash
           )
  L.NoConfidence prevActionId ->
    defMessage
      & U5c.noConfidenceAction
        .~ ( defMessage
               & U5c.maybe'govActionId .~ prevGovActionIdToUtxoRpcGovernanceActionId prevActionId
           )
  L.UpdateCommittee prevActionId removedMembers addedMembers threshold ->
    defMessage
      & U5c.updateCommitteeAction
        .~ ( defMessage
               & U5c.maybe'govActionId .~ prevGovActionIdToUtxoRpcGovernanceActionId prevActionId
               & U5c.removeCommitteeCredentials
                 .~ map credentialToUtxoRpcStakeCredential (toList removedMembers)
               & U5c.newCommitteeCredentials
                 .~ ( M.toList addedMembers <&> \(coldCredential, expirationEpoch) ->
                        defMessage
                          & U5c.committeeColdCredential
                            .~ credentialToUtxoRpcStakeCredential coldCredential
                          & U5c.expiresEpoch .~ fromIntegral (L.unEpochNo expirationEpoch)
                    )
               & U5c.newCommitteeThreshold .~ getProto (inject (L.unboundRational threshold))
           )
  L.NewConstitution prevActionId constitution ->
    defMessage
      & U5c.newConstitutionAction
        .~ ( defMessage
               & U5c.maybe'govActionId .~ prevGovActionIdToUtxoRpcGovernanceActionId prevActionId
               & U5c.constitution
                 .~ ( defMessage
                        & U5c.anchor .~ anchorToUtxoRpcAnchor (L.constitutionAnchor constitution)
                        & U5c.hash .~ scriptHashToBytes (L.constitutionGuardrailsScriptHash constitution)
                    )
           )
  L.InfoAction ->
    defMessage & U5c.infoAction .~ defMessage

-- | Convert a ledger protocol parameter update to the UTxO RPC 'UtxoRpc.PParams'
-- message. Only the parameters present in the update are set, everything else
-- keeps the proto3 default. The protocol version is excluded: from Conway
-- onwards it can only change through a hard fork initiation action.
pparamsUpdateToUtxoRpcPParams
  :: L.ConwayEraPParams era
  => L.PParamsUpdate era
  -> UtxoRpc.PParams
pparamsUpdateToUtxoRpcPParams pparamsUpdate =
  appUpdates
    [ pparamsUpdate
        ^. L.ppuCoinsPerUTxOByteL <&> \value ->
          U5c.coinsPerUtxoByte .~ getProto (inject (L.fromCompact (L.unCoinPerByte value)))
    , pparamsUpdate ^. L.ppuMaxTxSizeL <&> \value -> U5c.maxTxSize .~ fromIntegral value
    , pparamsUpdate
        ^. L.ppuTxFeePerByteL <&> \value ->
          U5c.minFeeCoefficient .~ getProto (inject (L.fromCompact (L.unCoinPerByte value)))
    , pparamsUpdate ^. L.ppuTxFeeFixedL <&> \value -> U5c.minFeeConstant .~ getProto (inject value)
    , pparamsUpdate ^. L.ppuMaxBBSizeL <&> \value -> U5c.maxBlockBodySize .~ fromIntegral value
    , pparamsUpdate ^. L.ppuMaxBHSizeL <&> \value -> U5c.maxBlockHeaderSize .~ fromIntegral value
    , pparamsUpdate ^. L.ppuKeyDepositL <&> \value -> U5c.stakeKeyDeposit .~ getProto (inject value)
    , pparamsUpdate ^. L.ppuPoolDepositL <&> \value -> U5c.poolDeposit .~ getProto (inject value)
    , pparamsUpdate
        ^. L.ppuEMaxL <&> \value ->
          U5c.poolRetirementEpochBound .~ fromIntegral (L.unEpochInterval value)
    , pparamsUpdate ^. L.ppuNOptL <&> \value -> U5c.desiredNumberOfPools .~ fromIntegral value
    , pparamsUpdate
        ^. L.ppuA0L <&> \value ->
          U5c.poolInfluence .~ getProto (inject (L.unboundRational value))
    , pparamsUpdate
        ^. L.ppuRhoL <&> \value ->
          U5c.monetaryExpansion .~ getProto (inject (L.unboundRational value))
    , pparamsUpdate
        ^. L.ppuTauL <&> \value ->
          U5c.treasuryExpansion .~ getProto (inject (L.unboundRational value))
    , pparamsUpdate ^. L.ppuMinPoolCostL <&> \value -> U5c.minPoolCost .~ getProto (inject value)
    , pparamsUpdate ^. L.ppuMaxValSizeL <&> \value -> U5c.maxValueSize .~ fromIntegral value
    , pparamsUpdate
        ^. L.ppuCollateralPercentageL <&> \value ->
          U5c.collateralPercentage .~ fromIntegral value
    , pparamsUpdate
        ^. L.ppuMaxCollateralInputsL <&> \value ->
          U5c.maxCollateralInputs .~ fromIntegral value
    , pparamsUpdate
        ^. L.ppuCostModelsL <&> \value ->
          U5c.costModels .~ costModelsToUtxoRpcCostModels value
    , pparamsUpdate
        ^. L.ppuPricesL <&> \prices ->
          (U5c.prices . U5c.steps .~ getProto (inject (L.unboundRational (L.prSteps prices))))
            . (U5c.prices . U5c.memory .~ getProto (inject (L.unboundRational (L.prMem prices))))
    , pparamsUpdate
        ^. L.ppuMaxTxExUnitsL <&> \value ->
          U5c.maxExecutionUnitsPerTransaction .~ getProto (inject value)
    , pparamsUpdate
        ^. L.ppuMaxBlockExUnitsL <&> \value ->
          U5c.maxExecutionUnitsPerBlock .~ getProto (inject value)
    , pparamsUpdate
        ^. L.ppuMinFeeRefScriptCostPerByteL <&> \value ->
          U5c.minFeeScriptRefCostPerByte .~ getProto (inject (L.unboundRational value))
    , pparamsUpdate
        ^. L.ppuPoolVotingThresholdsL <&> \poolVotingThresholds ->
          U5c.poolVotingThresholds . U5c.thresholds
            .~ ( getProto . inject . L.unboundRational
                   -- order taken from https://github.com/cardano-foundation/CIPs/blob/acb4b2348c968003dfc370cd3769615bfca1f159/CIP-1694/README.md#requirements
                   <$> [ poolVotingThresholds ^. L.pvtMotionNoConfidenceL
                       , poolVotingThresholds ^. L.pvtCommitteeNormalL
                       , poolVotingThresholds ^. L.pvtCommitteeNoConfidenceL
                       , poolVotingThresholds ^. L.pvtHardForkInitiationL
                       , poolVotingThresholds ^. L.pvtPPSecurityGroupL
                       ]
               )
    , pparamsUpdate
        ^. L.ppuDRepVotingThresholdsL <&> \drepVotingThresholds ->
          U5c.drepVotingThresholds . U5c.thresholds
            .~ ( getProto . inject . L.unboundRational
                   -- order taken from https://github.com/cardano-foundation/CIPs/blob/acb4b2348c968003dfc370cd3769615bfca1f159/CIP-1694/README.md#requirements
                   <$> [ drepVotingThresholds ^. L.dvtMotionNoConfidenceL
                       , drepVotingThresholds ^. L.dvtCommitteeNormalL
                       , drepVotingThresholds ^. L.dvtCommitteeNoConfidenceL
                       , drepVotingThresholds ^. L.dvtUpdateToConstitutionL
                       , drepVotingThresholds ^. L.dvtHardForkInitiationL
                       , drepVotingThresholds ^. L.dvtPPNetworkGroupL
                       , drepVotingThresholds ^. L.dvtPPEconomicGroupL
                       , drepVotingThresholds ^. L.dvtPPTechnicalGroupL
                       , drepVotingThresholds ^. L.dvtPPGovGroupL
                       , drepVotingThresholds ^. L.dvtTreasuryWithdrawalL
                       ]
               )
    , pparamsUpdate
        ^. L.ppuCommitteeMinSizeL <&> \value ->
          U5c.minCommitteeSize .~ fromIntegral value
    , pparamsUpdate
        ^. L.ppuCommitteeMaxTermLengthL <&> \value ->
          U5c.committeeTermLimit .~ fromIntegral (L.unEpochInterval value)
    , pparamsUpdate
        ^. L.ppuGovActionLifetimeL <&> \value ->
          U5c.governanceActionValidityPeriod .~ fromIntegral (L.unEpochInterval value)
    , pparamsUpdate
        ^. L.ppuGovActionDepositL <&> \value ->
          U5c.governanceActionDeposit .~ getProto (inject value)
    , pparamsUpdate ^. L.ppuDRepDepositL <&> \value -> U5c.drepDeposit .~ getProto (inject value)
    , pparamsUpdate
        ^. L.ppuDRepActivityL <&> \value ->
          U5c.drepInactivityPeriod .~ fromIntegral (L.unEpochInterval value)
    ]
    defMessage
 where
  -- Apply the parameter updates that are present to the default message
  appUpdates
    :: [L.StrictMaybe (UtxoRpc.PParams -> UtxoRpc.PParams)]
    -> UtxoRpc.PParams
    -> UtxoRpc.PParams
  appUpdates updates message = foldl' (\acc -> L.strictMaybe acc ($ acc)) message updates

-- | Convert ledger cost models to the UTxO RPC 'UtxoRpc.CostModels' message.
-- Only the languages present in the ledger value are marked present on the
-- wire, so a parameter update touching a single language does not report the
-- other languages as updated to empty cost models.
-- Note that 'L.costModelsUnknown' (raw models for languages this node does
-- not recognise) cannot be represented in the proto and are dropped.
costModelsToUtxoRpcCostModels :: L.CostModels -> UtxoRpc.CostModels
costModelsToUtxoRpcCostModels costModels = do
  let costModelParams :: Map L.Language [Int64]
      costModelParams = L.getCostModelParams <$> L.costModelsValid costModels
      costModelFor :: L.Language -> Maybe UtxoRpc.CostModel
      costModelFor language =
        M.lookup language costModelParams <&> \values -> defMessage & U5c.values .~ values
  defMessage
    & U5c.maybe'plutusV1 .~ costModelFor L.PlutusV1
    & U5c.maybe'plutusV2 .~ costModelFor L.PlutusV2
    & U5c.maybe'plutusV3 .~ costModelFor L.PlutusV3
    & U5c.maybe'plutusV4 .~ costModelFor L.PlutusV4

-- | Convert an optional previous governance action id. The proto encodes an
-- absent previous action id as an unset field.
prevGovActionIdToUtxoRpcGovernanceActionId
  :: L.StrictMaybe (L.GovPurposeId purpose)
  -> Maybe UtxoRpc.GovernanceActionId
prevGovActionIdToUtxoRpcGovernanceActionId =
  L.strictMaybeToMaybe . fmap (govActionIdToUtxoRpcGovernanceActionId . L.unGovPurposeId)

-- | Convert a ledger governance action id to the UTxO RPC
-- 'UtxoRpc.GovernanceActionId' message.
govActionIdToUtxoRpcGovernanceActionId :: L.GovActionId -> UtxoRpc.GovernanceActionId
govActionIdToUtxoRpcGovernanceActionId govActionId =
  defMessage
    & U5c.transactionId .~ serialiseToRawBytes (fromShelleyTxId (L.gaidTxId govActionId))
    & U5c.governanceActionIndex .~ fromIntegral (L.unGovActionIx (L.gaidGovActionIx govActionId))

-- | Encode an optional guardrails script hash as the bare hash bytes.
-- An absent script hash encodes as empty bytes, the proto3 default.
scriptHashToBytes :: L.StrictMaybe L.ScriptHash -> ByteString
scriptHashToBytes = L.strictMaybe mempty (\(L.ScriptHash scriptHash) -> L.hashToBytes scriptHash)
