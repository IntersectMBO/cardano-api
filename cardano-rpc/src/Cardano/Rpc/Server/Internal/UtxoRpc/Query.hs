{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Rpc.Server.Internal.UtxoRpc.Query
  ( readParamsMethod
  )
where

import Cardano.Api
import Cardano.Api.Ledger qualified as L
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as UtxoRpc
import Cardano.Rpc.Server.Internal.Error
import Cardano.Rpc.Server.Internal.Monad
import Cardano.Rpc.Server.Internal.Orphans ()

import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.BaseTypes (WithOrigin (..))
import Cardano.Ledger.Binary.Version qualified as L
import Cardano.Ledger.Conway.Core qualified as L
import Cardano.Ledger.Conway.PParams qualified as L
import Cardano.Ledger.Plutus qualified as L

import RIO

import Data.ByteString.Short qualified as SBS
import Data.Map.Strict qualified as M
import Data.ProtoLens (defMessage)
import Network.GRPC.Spec

readParamsMethod
  :: MonadRpc e m
  => Proto UtxoRpc.ReadParamsRequest
  -> m (Proto UtxoRpc.ReadParamsResponse)
readParamsMethod _req = do
  -- TODO: implement field masks - they are ignored for now
  -- they need to be normalised beforehand, see: https://github.com/protocolbuffers/protobuf/blob/main/java/util/src/main/java/com/google/protobuf/util/FieldMaskTree.java#L76
  -- let fieldMask :: [Text] = req ^. #fieldMask . #paths
  nodeConnInfo <- grab
  AnyCardanoEra era <- liftIO . throwExceptT $ determineEra nodeConnInfo
  eon <- forEraInEon era (error "Minimum Conway era required") pure
  let sbe = convert eon

  let target = VolatileTip
  (pparams, chainPoint, blockNo) <- liftIO . (throwEither =<<) $ executeLocalStateQueryExpr nodeConnInfo target $ do
    pparams <- throwEither =<< throwEither =<< queryProtocolParameters sbe
    chainPoint <- throwEither =<< queryChainPoint
    blockNo <- throwEither =<< queryChainBlockNo
    pure (pparams, chainPoint, blockNo)

  let pparamsCostModels :: Map L.Language [Int64] =
        babbageEraOnwardsConstraints (convert eon) $
          L.getCostModelParams <$> pparams ^. L.ppCostModelsL . to L.costModelsValid
      poolVotingThresholds :: L.PoolVotingThresholds =
        conwayEraOnwardsConstraints eon $
          pparams ^. L.ppPoolVotingThresholdsL
      drepVotingThresholds :: L.DRepVotingThresholds =
        conwayEraOnwardsConstraints eon $
          pparams ^. L.ppDRepVotingThresholdsL

  let pparamsMsg =
        conwayEraOnwardsConstraints eon $
          defMessage
            & #coinsPerUtxoByte .~ pparams ^. L.ppCoinsPerUTxOByteL . to L.unCoinPerByte . to fromIntegral
            & #maxTxSize .~ pparams ^. L.ppMaxTxSizeL . to fromIntegral
            & #minFeeCoefficient .~ pparams ^. L.ppMinFeeBL . to fromIntegral
            & #minFeeConstant .~ pparams ^. L.ppMinFeeAL . to fromIntegral
            & #maxBlockBodySize .~ pparams ^. L.ppMaxBBSizeL . to fromIntegral
            & #maxBlockHeaderSize .~ pparams ^. L.ppMaxBHSizeL . to fromIntegral
            & #stakeKeyDeposit .~ pparams ^. L.ppKeyDepositL . to fromIntegral
            & #poolDeposit .~ pparams ^. L.ppPoolDepositL . to fromIntegral
            & #poolRetirementEpochBound .~ pparams ^. L.ppEMaxL . to L.unEpochInterval . to fromIntegral
            & #desiredNumberOfPools .~ pparams ^. L.ppNOptL . to fromIntegral
            & #poolInfluence .~ pparams ^. L.ppA0L . to L.unboundRational . to inject
            & #desiredNumberOfPools .~ pparams ^. L.ppNOptL . to fromIntegral
            & #monetaryExpansion .~ pparams ^. L.ppRhoL . to L.unboundRational . to inject
            & #minPoolCost .~ pparams ^. L.ppMinPoolCostL . to fromIntegral
            & #protocolVersion . #major .~ pparams ^. L.ppProtocolVersionL . to L.pvMajor . to L.getVersion
            & #protocolVersion . #minor .~ pparams ^. L.ppProtocolVersionL . to L.pvMinor . to fromIntegral
            & #maxValueSize .~ pparams ^. L.ppMaxValSizeL . to fromIntegral
            & #collateralPercentage .~ pparams ^. L.ppCollateralPercentageL . to fromIntegral
            & #maxCollateralInputs .~ pparams ^. L.ppMaxCollateralInputsL . to fromIntegral
            & #costModels . #plutusV1 . #values .~ (join . maybeToList) (M.lookup L.PlutusV1 pparamsCostModels)
            & #costModels . #plutusV2 . #values .~ (join . maybeToList) (M.lookup L.PlutusV2 pparamsCostModels)
            & #costModels . #plutusV3 . #values .~ (join . maybeToList) (M.lookup L.PlutusV3 pparamsCostModels)
            & #prices . #steps .~ pparams ^. L.ppPricesL . to L.prSteps . to L.unboundRational . to inject
            & #prices . #memory .~ pparams ^. L.ppPricesL . to L.prMem . to L.unboundRational . to inject
            & #maxExecutionUnitsPerTransaction .~ pparams ^. L.ppMaxTxExUnitsL . to inject
            & #maxExecutionUnitsPerBlock .~ pparams ^. L.ppMaxBlockExUnitsL . to inject
            & #minFeeScriptRefCostPerByte
              .~ pparams ^. L.ppMinFeeRefScriptCostPerByteL . to L.unboundRational . to inject
            & #poolVotingThresholds . #thresholds
              .~ ( inject . L.unboundRational
                     -- order taken from https://github.com/cardano-foundation/CIPs/blob/acb4b2348c968003dfc370cd3769615bfca1f159/CIP-1694/README.md#requirements
                     <$> [ poolVotingThresholds ^. L.pvtMotionNoConfidenceL
                         , poolVotingThresholds ^. L.pvtCommitteeNormalL
                         , poolVotingThresholds ^. L.pvtCommitteeNoConfidenceL
                         , poolVotingThresholds ^. L.pvtHardForkInitiationL
                         , poolVotingThresholds ^. L.pvtPPSecurityGroupL
                         ]
                 )
            & #drepVotingThresholds . #thresholds
              .~ ( inject . L.unboundRational
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
            & #minCommitteeSize .~ pparams ^. L.ppCommitteeMinSizeL . to fromIntegral
            & #committeeTermLimit
              .~ pparams ^. L.ppCommitteeMaxTermLengthL . to L.unEpochInterval . to fromIntegral
            & #governanceActionValidityPeriod
              .~ pparams ^. L.ppGovActionLifetimeL . to L.unEpochInterval . to fromIntegral
            & #governanceActionDeposit .~ pparams ^. L.ppGovActionDepositL . to fromIntegral
            & #drepDeposit .~ pparams ^. L.ppDRepDepositL . to fromIntegral
            & #drepInactivityPeriod .~ pparams ^. L.ppDRepActivityL . to L.unEpochInterval . to fromIntegral
  pure $
    defMessage
      & #ledgerTip .~ mkChainPointMsg chainPoint blockNo
      & #values . #cardano .~ pparamsMsg

mkChainPointMsg
  :: ChainPoint
  -> WithOrigin BlockNo
  -> Proto UtxoRpc.ChainPoint
mkChainPointMsg chainPoint blockNo = do
  let (slotNo, blockHash) = case chainPoint of
        ChainPointAtGenesis -> (0, mempty)
        ChainPoint (SlotNo slot) (HeaderHash hash) -> (slot, SBS.fromShort hash)
      blockHeight = case blockNo of
        Origin -> 0
        At (BlockNo h) -> h
  defMessage
    & #slot .~ slotNo
    & #hash .~ blockHash
    & #height .~ blockHeight

-- & #timestamp .~ timestamp -- not supported currently
