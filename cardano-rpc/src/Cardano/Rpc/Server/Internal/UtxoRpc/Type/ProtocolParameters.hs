{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Rpc.Server.Internal.UtxoRpc.Type.ProtocolParameters
  ( protocolParamsToUtxoRpcPParams
  , utxoRpcPParamsToProtocolParams
  )
where

import Cardano.Api.Era
import Cardano.Api.Experimental.Era
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Monad.Error
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as U5c
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as UtxoRpc
import Cardano.Rpc.Server.Internal.Orphans ()
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.BigInt
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.Rational

import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.BaseTypes qualified as L
import Cardano.Ledger.Conway.Core qualified as L
import Cardano.Ledger.Conway.PParams qualified as L
import Cardano.Ledger.Plutus qualified as L

import RIO hiding (toList)

import Control.Error.Util (note)
import Data.Default
import Data.Map.Strict qualified as M
import GHC.IsList
import Network.GRPC.Spec

protocolParamsToUtxoRpcPParams
  :: forall era
   . Era era
  -> L.PParams (LedgerEra era)
  -> Proto UtxoRpc.PParams
protocolParamsToUtxoRpcPParams era pparams = obtainCommonConstraints era $ do
  let pparamsCostModels :: Map L.Language [Int64] =
        L.getCostModelParams <$> pparams ^. L.ppCostModelsL . to L.costModelsValid
      poolVotingThresholds :: L.PoolVotingThresholds =
        pparams ^. L.ppPoolVotingThresholdsL
      drepVotingThresholds :: L.DRepVotingThresholds =
        pparams ^. L.ppDRepVotingThresholdsL
  def
    & U5c.coinsPerUtxoByte
      .~ pparams ^. L.ppCoinsPerUTxOByteL . to L.unCoinPerByte . to L.fromCompact . to inject
    & U5c.maxTxSize .~ pparams ^. L.ppMaxTxSizeL . to fromIntegral
    & U5c.minFeeCoefficient
      .~ pparams ^. L.ppTxFeePerByteL . to L.unCoinPerByte . to L.fromCompact . to inject
    & U5c.minFeeConstant .~ pparams ^. L.ppTxFeeFixedL . to inject
    & U5c.maxBlockBodySize .~ pparams ^. L.ppMaxBBSizeL . to fromIntegral
    & U5c.maxBlockHeaderSize .~ pparams ^. L.ppMaxBHSizeL . to fromIntegral
    & U5c.stakeKeyDeposit .~ pparams ^. L.ppKeyDepositL . to inject
    & U5c.poolDeposit .~ pparams ^. L.ppPoolDepositL . to inject
    & U5c.poolRetirementEpochBound .~ pparams ^. L.ppEMaxL . to L.unEpochInterval . to fromIntegral
    & U5c.desiredNumberOfPools .~ pparams ^. L.ppNOptL . to fromIntegral
    & U5c.poolInfluence .~ pparams ^. L.ppA0L . to L.unboundRational . to inject
    & U5c.monetaryExpansion .~ pparams ^. L.ppRhoL . to L.unboundRational . to inject
    & U5c.treasuryExpansion .~ pparams ^. L.ppTauL . to L.unboundRational . to inject
    & U5c.minPoolCost .~ pparams ^. L.ppMinPoolCostL . to inject
    & U5c.protocolVersion . U5c.major .~ pparams ^. L.ppProtocolVersionL . to L.pvMajor . to L.getVersion
    & U5c.protocolVersion . U5c.minor .~ pparams ^. L.ppProtocolVersionL . to L.pvMinor . to fromIntegral
    & U5c.maxValueSize .~ pparams ^. L.ppMaxValSizeL . to fromIntegral
    & U5c.collateralPercentage .~ pparams ^. L.ppCollateralPercentageL . to fromIntegral
    & U5c.maxCollateralInputs .~ pparams ^. L.ppMaxCollateralInputsL . to fromIntegral
    & U5c.costModels . U5c.plutusV1 . U5c.values
      .~ (join . maybeToList) (M.lookup L.PlutusV1 pparamsCostModels)
    & U5c.costModels . U5c.plutusV2 . U5c.values
      .~ (join . maybeToList) (M.lookup L.PlutusV2 pparamsCostModels)
    & U5c.costModels . U5c.plutusV3 . U5c.values
      .~ (join . maybeToList) (M.lookup L.PlutusV3 pparamsCostModels)
    & U5c.costModels . U5c.plutusV4 . U5c.values
      .~ (join . maybeToList) (M.lookup L.PlutusV4 pparamsCostModels)
    & U5c.prices . U5c.steps .~ pparams ^. L.ppPricesL . to L.prSteps . to L.unboundRational . to inject
    & U5c.prices . U5c.memory .~ pparams ^. L.ppPricesL . to L.prMem . to L.unboundRational . to inject
    & U5c.maxExecutionUnitsPerTransaction .~ pparams ^. L.ppMaxTxExUnitsL . to inject
    & U5c.maxExecutionUnitsPerBlock .~ pparams ^. L.ppMaxBlockExUnitsL . to inject
    & U5c.minFeeScriptRefCostPerByte
      .~ pparams ^. L.ppMinFeeRefScriptCostPerByteL . to L.unboundRational . to inject
    & U5c.poolVotingThresholds . U5c.thresholds
      .~ ( inject . L.unboundRational
             -- order taken from https://github.com/cardano-foundation/CIPs/blob/acb4b2348c968003dfc370cd3769615bfca1f159/CIP-1694/README.md#requirements
             <$> [ poolVotingThresholds ^. L.pvtMotionNoConfidenceL
                 , poolVotingThresholds ^. L.pvtCommitteeNormalL
                 , poolVotingThresholds ^. L.pvtCommitteeNoConfidenceL
                 , poolVotingThresholds ^. L.pvtHardForkInitiationL
                 , poolVotingThresholds ^. L.pvtPPSecurityGroupL
                 ]
         )
    & U5c.drepVotingThresholds . U5c.thresholds
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
    & U5c.minCommitteeSize .~ pparams ^. L.ppCommitteeMinSizeL . to fromIntegral
    & U5c.committeeTermLimit
      .~ pparams ^. L.ppCommitteeMaxTermLengthL . to L.unEpochInterval . to fromIntegral
    & U5c.governanceActionValidityPeriod
      .~ pparams ^. L.ppGovActionLifetimeL . to L.unEpochInterval . to fromIntegral
    & U5c.governanceActionDeposit .~ pparams ^. L.ppGovActionDepositL . to inject
    & U5c.drepDeposit .~ pparams ^. L.ppDRepDepositL . to inject
    & U5c.drepInactivityPeriod .~ pparams ^. L.ppDRepActivityL . to L.unEpochInterval . to fromIntegral

utxoRpcPParamsToProtocolParams
  :: Era era
  -> Proto UtxoRpc.PParams
  -> Either String (L.PParams (ShelleyLedgerEra era))
utxoRpcPParamsToProtocolParams era pp = conwayEraOnwardsConstraints (convert era) $ do
  def
    & appFuns
      [ \r -> do
          coinsPerUtxoByte <-
            pp ^. U5c.coinsPerUtxoByte . to utxoRpcBigIntToInteger ?! "Invalid coinsPerUtxoByte"
          compactCoinsPerUtxO <-
            note "Could not convert coinsPerUtxoByte to compact form." $ L.toCompact (L.Coin coinsPerUtxoByte)
          pure $ set L.ppCoinsPerUTxOByteL (L.CoinPerByte compactCoinsPerUtxO) r
      , pure . (L.ppMaxTxSizeL .~ pp ^. U5c.maxTxSize . to fromIntegral)
      , \r -> do
          minFeeCoeff <-
            pp ^. U5c.minFeeCoefficient . to utxoRpcBigIntToInteger ?! "Invalid minFeeCoefficient"
          minFeeCoeffCompact <-
            note "Could not convert minFeeCoefficient to compact form." $ L.toCompact (L.Coin minFeeCoeff)
          pure $ set L.ppTxFeePerByteL (L.CoinPerByte minFeeCoeffCompact) r
      , \r -> do
          minFeeConst <- pp ^. U5c.minFeeConstant . to utxoRpcBigIntToInteger ?! "Invalid minFeeConstant"
          pure $ set L.ppTxFeeFixedL (L.Coin minFeeConst) r
      , pure . (L.ppMaxBBSizeL .~ pp ^. U5c.maxBlockBodySize . to fromIntegral)
      , pure . (L.ppMaxBHSizeL .~ pp ^. U5c.maxBlockHeaderSize . to fromIntegral)
      , \r -> do
          stakeKeyDeposit <-
            pp ^. U5c.stakeKeyDeposit . to utxoRpcBigIntToInteger ?! "Invalid stakeKeyDeposit"
          pure $ set L.ppKeyDepositL (L.Coin stakeKeyDeposit) r
      , \r -> do
          poolDeposit <- pp ^. U5c.poolDeposit . to utxoRpcBigIntToInteger ?! "Invalid poolDeposit"
          pure $ set L.ppPoolDepositL (L.Coin poolDeposit) r
      , pure . (L.ppEMaxL .~ pp ^. U5c.poolRetirementEpochBound . to fromIntegral . to L.EpochInterval)
      , pure . (L.ppNOptL .~ pp ^. U5c.desiredNumberOfPools . to fromIntegral)
      , \r -> do
          poolInfluence <-
            pp
              ^. U5c.poolInfluence
                . to (L.boundRational <=< utxoRpcRationalNumberToRational)
                ?! "Invalid poolInfluence"
          pure $ set L.ppA0L poolInfluence r
      , \r -> do
          monetaryExpansion <-
            pp
              ^. U5c.monetaryExpansion
                . to (L.boundRational <=< utxoRpcRationalNumberToRational)
                ?! "Invalid monetaryExpansion"
          pure $ set L.ppRhoL monetaryExpansion r
      , \r -> do
          treasuryExpansion <-
            pp
              ^. U5c.treasuryExpansion
                . to (L.boundRational <=< utxoRpcRationalNumberToRational)
                ?! "Invalid treasuryExpansion"
          pure $ set L.ppTauL treasuryExpansion r
      , \r -> do
          minPoolCost <- pp ^. U5c.minPoolCost . to utxoRpcBigIntToInteger ?! "Invalid minPoolCost"
          pure $ set L.ppMinPoolCostL (L.Coin minPoolCost) r
      , \r -> do
          major <- L.mkVersion64 $ pp ^. U5c.protocolVersion . U5c.major . to fromIntegral
          pure $ set (L.ppProtocolVersionL . pvMajorL) major r
      , pure . (L.ppProtocolVersionL . pvMinorL .~ pp ^. U5c.protocolVersion . U5c.minor . to fromIntegral)
      , pure . (L.ppMaxValSizeL .~ pp ^. U5c.maxValueSize . to fromIntegral)
      , pure . (L.ppCollateralPercentageL .~ pp ^. U5c.collateralPercentage . to fromIntegral)
      , pure . (L.ppMaxCollateralInputsL .~ pp ^. U5c.maxCollateralInputs . to fromIntegral)
      , \r -> first show $ do
          cm1 <- L.mkCostModel L.PlutusV1 $ pp ^. U5c.costModels . U5c.plutusV1 . U5c.values
          cm2 <- L.mkCostModel L.PlutusV2 $ pp ^. U5c.costModels . U5c.plutusV2 . U5c.values
          cm3 <- L.mkCostModel L.PlutusV3 $ pp ^. U5c.costModels . U5c.plutusV3 . U5c.values
          cm4 <- L.mkCostModel L.PlutusV4 $ pp ^. U5c.costModels . U5c.plutusV4 . U5c.values
          -- do not add empty cost models
          let nonEmptyCostModels =
                fromList . flip mapMaybe [cm1, cm2, cm3, cm4] $ \cm ->
                  if not (null $ L.getCostModelParams cm)
                    then Just (L.getCostModelLanguage cm, cm)
                    else Nothing
          pure $
            r & L.ppCostModelsL .~ L.mkCostModels nonEmptyCostModels
      , \r -> do
          steps <-
            pp
              ^. U5c.prices
                . U5c.steps
                . to (L.boundRational <=< utxoRpcRationalNumberToRational)
                ?! "Invalid prices.steps"
          mem <-
            pp
              ^. U5c.prices
                . U5c.memory
                . to (L.boundRational <=< utxoRpcRationalNumberToRational)
                ?! "Invalid prices.mem"
          pure $
            r
              & L.ppPricesL . prStepsL .~ steps
              & L.ppPricesL . prMemL .~ mem
      , pure . (L.ppMaxTxExUnitsL .~ pp ^. U5c.maxExecutionUnitsPerTransaction . to inject)
      , pure . (L.ppMaxBlockExUnitsL .~ pp ^. U5c.maxExecutionUnitsPerBlock . to inject)
      , \r -> do
          minFeeScriptRefCostPerByte <-
            pp
              ^. U5c.minFeeScriptRefCostPerByte
                . to (L.boundRational <=< utxoRpcRationalNumberToRational)
                ?! "Invalid minFeeScriptRefCostPerByte"
          pure $ set L.ppMinFeeRefScriptCostPerByteL minFeeScriptRefCostPerByte r
      , \r -> do
          let thresholds = pp ^. U5c.poolVotingThresholds . U5c.thresholds
          when (length thresholds /= 5) $
            throwError $
              "Invalid number of thresholds: " <> show (length thresholds)
          [ motionNoConfidence
            , committeeNormal
            , committeeNoConfidence
            , hardForkInitiation
            , ppSecurityGroup
            ] <-
            traverse (uncurry ($))
              . zip
                [ (?! "Invalid value in poolVotingThresholds: motionNoConfidence")
                , (?! "Invalid value in poolVotingThresholds: committeeNormal")
                , (?! "Invalid value in poolVotingThresholds: committeeNoConfidence")
                , (?! "Invalid value in poolVotingThresholds: hardForkInitiation")
                , (?! "Invalid value in poolVotingThresholds: ppSecurityGroup")
                ]
              $ map (L.boundRational <=< utxoRpcRationalNumberToRational) thresholds
          pure $
            r
              & L.ppPoolVotingThresholdsL . L.pvtMotionNoConfidenceL .~ motionNoConfidence
              & L.ppPoolVotingThresholdsL . L.pvtCommitteeNormalL .~ committeeNormal
              & L.ppPoolVotingThresholdsL . L.pvtCommitteeNoConfidenceL .~ committeeNoConfidence
              & L.ppPoolVotingThresholdsL . L.pvtHardForkInitiationL .~ hardForkInitiation
              & L.ppPoolVotingThresholdsL . L.pvtPPSecurityGroupL .~ ppSecurityGroup
      , \r -> do
          let thresholds = pp ^. U5c.drepVotingThresholds . U5c.thresholds
          when (length thresholds /= 10) $
            throwError $
              "Invalid number of thresholds: " <> show (length thresholds)
          [ motionNoConfidence
            , committeeNormal
            , committeeNoConfidence
            , updateToConstitution
            , hardforkInitiation
            , ppNetworkGroup
            , ppEconomicGroup
            , ppTechnicalGroup
            , ppGovGroup
            , treasuryWithdrawal
            ] <-
            traverse (uncurry ($))
              . zip
                [ (?! "Invalid value in drepVotingThresholds: motionNoConfidence")
                , (?! "Invalid value in drepVotingThresholds: committeeNormal")
                , (?! "Invalid value in drepVotingThresholds: committeeNoConfidence")
                , (?! "Invalid value in drepVotingThresholds: updateToConstitution")
                , (?! "Invalid value in drepVotingThresholds: hardforkInitiation")
                , (?! "Invalid value in drepVotingThresholds: ppNetworkGroup")
                , (?! "Invalid value in drepVotingThresholds: ppEconomicGroup")
                , (?! "Invalid value in drepVotingThresholds: ppTechnicalGroup")
                , (?! "Invalid value in drepVotingThresholds: ppGovGroup")
                , (?! "Invalid value in drepVotingThresholds: treasuryWithdrawal")
                ]
              $ map (L.boundRational <=< utxoRpcRationalNumberToRational) thresholds
          pure $
            r
              & L.ppDRepVotingThresholdsL . L.dvtMotionNoConfidenceL .~ motionNoConfidence
              & L.ppDRepVotingThresholdsL . L.dvtCommitteeNormalL .~ committeeNormal
              & L.ppDRepVotingThresholdsL . L.dvtCommitteeNoConfidenceL .~ committeeNoConfidence
              & L.ppDRepVotingThresholdsL . L.dvtUpdateToConstitutionL .~ updateToConstitution
              & L.ppDRepVotingThresholdsL . L.dvtHardForkInitiationL .~ hardforkInitiation
              & L.ppDRepVotingThresholdsL . L.dvtPPNetworkGroupL .~ ppNetworkGroup
              & L.ppDRepVotingThresholdsL . L.dvtPPEconomicGroupL .~ ppEconomicGroup
              & L.ppDRepVotingThresholdsL . L.dvtPPTechnicalGroupL .~ ppTechnicalGroup
              & L.ppDRepVotingThresholdsL . L.dvtPPGovGroupL .~ ppGovGroup
              & L.ppDRepVotingThresholdsL . L.dvtTreasuryWithdrawalL .~ treasuryWithdrawal
      , pure . (L.ppCommitteeMinSizeL .~ pp ^. U5c.minCommitteeSize . to fromIntegral)
      , pure
          . (L.ppCommitteeMaxTermLengthL .~ pp ^. U5c.committeeTermLimit . to fromIntegral . to L.EpochInterval)
      , pure
          . ( L.ppGovActionLifetimeL
                .~ pp ^. U5c.governanceActionValidityPeriod . to fromIntegral . to L.EpochInterval
            )
      , \r -> do
          govActionDeposit <-
            pp ^. U5c.governanceActionDeposit . to utxoRpcBigIntToInteger ?! "Invalid governanceActionDeposit"
          pure $ set L.ppGovActionDepositL (L.Coin govActionDeposit) r
      , \r -> do
          drepDeposit <- pp ^. U5c.drepDeposit . to utxoRpcBigIntToInteger ?! "Invalid drepDeposit"
          pure $ set L.ppDRepDepositL (L.Coin drepDeposit) r
      , pure . (L.ppDRepActivityL .~ pp ^. U5c.drepInactivityPeriod . to fromIntegral . to L.EpochInterval)
      ]
 where
  -- Run a list of functions feeding the output of one to the next
  appFuns :: Monad m => [a -> m a] -> a -> m a
  appFuns fs a0 = foldr (=<<) (pure a0) fs

  pvMajorL :: Lens' L.ProtVer L.Version
  pvMajorL = lens L.pvMajor $ \p v -> p{L.pvMajor = v}

  pvMinorL :: Lens' L.ProtVer Natural
  pvMinorL = lens L.pvMinor $ \p v -> p{L.pvMinor = v}

  prStepsL :: Lens' L.Prices L.NonNegativeInterval
  prStepsL = lens L.prSteps $ \p v -> p{L.prSteps = v}

  prMemL :: Lens' L.Prices L.NonNegativeInterval
  prMemL = lens L.prMem $ \p v -> p{L.prMem = v}
