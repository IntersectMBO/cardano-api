{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Rpc.Server.Internal.UtxoRpc.Type.ProtocolParameters
  ( protocolParamsToUtxoRpcPParams
  , pparamsUpdateToUtxoRpcPParams
  , utxoRpcPParamsToProtocolParams
  , utxoRpcProtocolVersionToProtVer
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

import Cardano.Ledger.Alonzo.PParams qualified as L
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.BaseTypes qualified as L
import Cardano.Ledger.Conway.Core qualified as L
import Cardano.Ledger.Conway.PParams qualified as L
import Cardano.Ledger.HKD qualified as L
import Cardano.Ledger.Plutus qualified as L

import RIO hiding (toList)

import Control.Error.Util (note)
import Data.Default
import Data.Map.Strict qualified as M
import Data.ProtoLens (defMessage)
import GHC.IsList
import Network.GRPC.Spec

-- | Convert full ledger protocol parameters to the UTxO RPC 'UtxoRpc.PParams'
-- message. All parameters are set.
protocolParamsToUtxoRpcPParams
  :: forall era
   . Era era
  -> L.PParams (LedgerEra era)
  -> Proto UtxoRpc.PParams
protocolParamsToUtxoRpcPParams era pparams =
  obtainCommonConstraints era $
    def
      & appSetters (pparamsFieldSetters @Identity L.SJust (pparams ^. L.ppLensHKD))
      -- The protocol version is not in 'pparamsFieldSetters': from Conway onwards
      -- parameter updates cannot change it, so it has no HKD lens in these eras.
      & U5c.protocolVersion .~ inject (pparams ^. L.ppProtocolVersionL)

-- | Convert a ledger protocol parameter update to the UTxO RPC 'UtxoRpc.PParams'
-- message. Only the parameters present in the update are set, everything else
-- keeps the proto3 default. The protocol version is excluded: from Conway
-- onwards it can only change through a hard fork initiation action.
pparamsUpdateToUtxoRpcPParams
  :: L.ConwayEraPParams era
  => L.PParamsUpdate era
  -> Proto UtxoRpc.PParams
pparamsUpdateToUtxoRpcPParams pparamsUpdate =
  defMessage
    & appSetters (pparamsFieldSetters @L.StrictMaybe id (pparamsUpdate ^. L.ppuLensHKD))

-- | A setter for each UTxO RPC 'UtxoRpc.PParams' field, built from the higher
-- kinded protocol parameters representation shared between 'L.PParams'
-- (@f ~ 'Identity'@, all fields present) and 'L.PParamsUpdate'
-- (@f ~ 'L.StrictMaybe'@, only the updated fields present). Keeping a single
-- field table guarantees that the protocol parameters query and the parameter
-- change governance action cannot disagree on the wire mapping.
pparamsFieldSetters
  :: forall f era
   . (L.HKDFunctor f, L.ConwayEraPParams era)
  => (forall a. L.HKD f a -> L.StrictMaybe a)
  -- ^ Extract a field value: 'L.SJust' for 'Identity', 'id' for 'L.StrictMaybe'
  -> L.PParamsHKD f era
  -> [L.StrictMaybe (Proto UtxoRpc.PParams -> Proto UtxoRpc.PParams)]
pparamsFieldSetters extract pparams =
  [ field @L.CoinPerByte (L.hkdCoinsPerUTxOByteL @era @f) <&> \value ->
      U5c.coinsPerUtxoByte .~ inject (L.fromCompact (L.unCoinPerByte value))
  , field @Word32 (L.hkdMaxTxSizeL @era @f) <&> \value ->
      U5c.maxTxSize .~ fromIntegral value
  , field @L.CoinPerByte (L.hkdTxFeePerByteL @era @f) <&> \value ->
      U5c.minFeeCoefficient .~ inject (L.fromCompact (L.unCoinPerByte value))
  , field @(L.CompactForm L.Coin) (L.hkdTxFeeFixedCompactL @era @f) <&> \value ->
      U5c.minFeeConstant .~ inject (L.fromCompact value)
  , field @Word32 (L.hkdMaxBBSizeL @era @f) <&> \value ->
      U5c.maxBlockBodySize .~ fromIntegral value
  , field @Word16 (L.hkdMaxBHSizeL @era @f) <&> \value ->
      U5c.maxBlockHeaderSize .~ fromIntegral value
  , field @(L.CompactForm L.Coin) (L.hkdKeyDepositCompactL @era @f) <&> \value ->
      U5c.stakeKeyDeposit .~ inject (L.fromCompact value)
  , field @(L.CompactForm L.Coin) (L.hkdPoolDepositCompactL @era @f) <&> \value ->
      U5c.poolDeposit .~ inject (L.fromCompact value)
  , field @L.EpochInterval (L.hkdEMaxL @era @f) <&> \value ->
      U5c.poolRetirementEpochBound .~ fromIntegral (L.unEpochInterval value)
  , field @Word16 (L.hkdNOptL @era @f) <&> \value ->
      U5c.desiredNumberOfPools .~ fromIntegral value
  , field @L.NonNegativeInterval (L.hkdA0L @era @f) <&> \value ->
      U5c.poolInfluence .~ inject (L.unboundRational value)
  , field @L.UnitInterval (L.hkdRhoL @era @f) <&> \value ->
      U5c.monetaryExpansion .~ inject (L.unboundRational value)
  , field @L.UnitInterval (L.hkdTauL @era @f) <&> \value ->
      U5c.treasuryExpansion .~ inject (L.unboundRational value)
  , field @(L.CompactForm L.Coin) (L.hkdMinPoolCostCompactL @era @f) <&> \value ->
      U5c.minPoolCost .~ inject (L.fromCompact value)
  , field @Word32 (L.hkdMaxValSizeL @era @f) <&> \value ->
      U5c.maxValueSize .~ fromIntegral value
  , field @Word16 (L.hkdCollateralPercentageL @era @f) <&> \value ->
      U5c.collateralPercentage .~ fromIntegral value
  , field @Word16 (L.hkdMaxCollateralInputsL @era @f) <&> \value ->
      U5c.maxCollateralInputs .~ fromIntegral value
  , field @L.CostModels (L.hkdCostModelsL @era @f) <&> \value ->
      U5c.costModels .~ costModelsToUtxoRpcCostModels value
  , field @L.Prices (L.hkdPricesL @era @f) <&> \prices ->
      (U5c.prices . U5c.steps .~ inject (L.unboundRational (L.prSteps prices)))
        . (U5c.prices . U5c.memory .~ inject (L.unboundRational (L.prMem prices)))
  , field @L.ExUnits (L.hkdMaxTxExUnitsL @era @f) <&> \value ->
      U5c.maxExecutionUnitsPerTransaction .~ inject value
  , field @L.ExUnits (L.hkdMaxBlockExUnitsL @era @f) <&> \value ->
      U5c.maxExecutionUnitsPerBlock .~ inject value
  , field @L.NonNegativeInterval (L.hkdMinFeeRefScriptCostPerByteL @era @f) <&> \value ->
      U5c.minFeeScriptRefCostPerByte .~ inject (L.unboundRational value)
  , field @L.PoolVotingThresholds (L.hkdPoolVotingThresholdsL @era @f) <&> \poolVotingThresholds ->
      U5c.poolVotingThresholds . U5c.thresholds
        .~ ( inject . L.unboundRational
               -- order taken from https://github.com/cardano-foundation/CIPs/blob/acb4b2348c968003dfc370cd3769615bfca1f159/CIP-1694/README.md#requirements
               <$> [ poolVotingThresholds ^. L.pvtMotionNoConfidenceL
                   , poolVotingThresholds ^. L.pvtCommitteeNormalL
                   , poolVotingThresholds ^. L.pvtCommitteeNoConfidenceL
                   , poolVotingThresholds ^. L.pvtHardForkInitiationL
                   , poolVotingThresholds ^. L.pvtPPSecurityGroupL
                   ]
           )
  , field @L.DRepVotingThresholds (L.hkdDRepVotingThresholdsL @era @f) <&> \drepVotingThresholds ->
      U5c.drepVotingThresholds . U5c.thresholds
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
  , field @Word16 (L.hkdCommitteeMinSizeL @era @f) <&> \value ->
      U5c.minCommitteeSize .~ fromIntegral value
  , field @L.EpochInterval (L.hkdCommitteeMaxTermLengthL @era @f) <&> \value ->
      U5c.committeeTermLimit .~ fromIntegral (L.unEpochInterval value)
  , field @L.EpochInterval (L.hkdGovActionLifetimeL @era @f) <&> \value ->
      U5c.governanceActionValidityPeriod .~ fromIntegral (L.unEpochInterval value)
  , field @(L.CompactForm L.Coin) (L.hkdGovActionDepositCompactL @era @f) <&> \value ->
      U5c.governanceActionDeposit .~ inject (L.fromCompact value)
  , field @(L.CompactForm L.Coin) (L.hkdDRepDepositCompactL @era @f) <&> \value ->
      U5c.drepDeposit .~ inject (L.fromCompact value)
  , field @L.EpochInterval (L.hkdDRepActivityL @era @f) <&> \value ->
      U5c.drepInactivityPeriod .~ fromIntegral (L.unEpochInterval value)
  ]
 where
  field :: forall a. Lens' (L.PParamsHKD f era) (L.HKD f a) -> L.StrictMaybe a
  field fieldLens = extract @a (pparams ^. fieldLens)

-- | Apply the setters for the fields that are present to the message
appSetters
  :: [L.StrictMaybe (Proto UtxoRpc.PParams -> Proto UtxoRpc.PParams)]
  -> Proto UtxoRpc.PParams
  -> Proto UtxoRpc.PParams
appSetters setters message = foldl' (\acc -> L.strictMaybe acc ($ acc)) message setters

-- | Convert ledger cost models to the UTxO RPC 'UtxoRpc.CostModels' message.
-- Only the languages present in the ledger value are marked present on the
-- wire: a parameter update touching a single language does not report the
-- other languages as updated to empty cost models, and a parameters query
-- does not report cost models for languages the era does not have.
-- Note that 'L.costModelsUnknown' (raw models for languages this node does
-- not recognise) cannot be represented in the proto and are dropped.
costModelsToUtxoRpcCostModels :: L.CostModels -> Proto UtxoRpc.CostModels
costModelsToUtxoRpcCostModels costModels = do
  let costModelParams :: Map L.Language [Int64]
      costModelParams = L.getCostModelParams <$> L.costModelsValid costModels
      costModelFor :: L.Language -> Maybe (Proto UtxoRpc.CostModel)
      costModelFor language =
        M.lookup language costModelParams <&> \values -> defMessage & U5c.values .~ values
  defMessage
    & U5c.maybe'plutusV1 .~ costModelFor L.PlutusV1
    & U5c.maybe'plutusV2 .~ costModelFor L.PlutusV2
    & U5c.maybe'plutusV3 .~ costModelFor L.PlutusV3
    & U5c.maybe'plutusV4 .~ costModelFor L.PlutusV4

utxoRpcPParamsToProtocolParams
  :: Era era
  -> Proto UtxoRpc.PParams
  -> Either String (L.PParams (ShelleyLedgerEra era))
utxoRpcPParamsToProtocolParams era pp = obtainCommonConstraints era $ do
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
          protocolVersion <-
            pp ^. U5c.protocolVersion . to utxoRpcProtocolVersionToProtVer ?! "Invalid protocolVersion"
          pure $ set L.ppProtocolVersionL protocolVersion r
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

  prStepsL :: Lens' L.Prices L.NonNegativeInterval
  prStepsL = lens L.prSteps $ \p v -> p{L.prSteps = v}

  prMemL :: Lens' L.Prices L.NonNegativeInterval
  prMemL = lens L.prMem $ \p v -> p{L.prMem = v}

-- | Build a ledger protocol version from the proto 'U5c.ProtocolVersion'.
-- The conversion can fail, because the ledger major 'L.Version' is bounded,
-- while the proto major is a full @uint32@.
utxoRpcProtocolVersionToProtVer :: Proto U5c.ProtocolVersion -> Maybe L.ProtVer
utxoRpcProtocolVersionToProtVer protocolVersion = do
  major <- L.mkVersion $ protocolVersion ^. U5c.major
  pure
    L.ProtVer
      { L.pvMajor = major
      , L.pvMinor = fromIntegral $ protocolVersion ^. U5c.minor
      }
