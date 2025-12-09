{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Rpc.Server.Internal.UtxoRpc.Type
  ( utxoRpcPParamsToProtocolParams
  , utxoToUtxoRpcAnyUtxoData
  , anyUtxoDataUtxoRpcToUtxo
  , txOutToUtxoRpcTxOutput
  , utxoRpcTxOutputToTxOut
  , protocolParamsToUtxoRpcPParams
  , simpleScriptToUtxoRpcNativeScript
  , utxoRpcBigIntToInteger
  , mkChainPointMsg
  )
where

import Cardano.Api.Address
import Cardano.Api.Block
import Cardano.Api.Era
import Cardano.Api.Error
import Cardano.Api.Experimental.Era
import Cardano.Api.HasTypeProxy
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Monad.Error
import Cardano.Api.Plutus
import Cardano.Api.Serialise.Cbor
import Cardano.Api.Serialise.Raw
import Cardano.Api.Tx
import Cardano.Api.Value
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as U5c
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as UtxoRpc
import Cardano.Rpc.Server.Internal.Orphans ()

import Cardano.Binary qualified as CBOR
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.BaseTypes (WithOrigin (..))
import Cardano.Ledger.BaseTypes qualified as L
import Cardano.Ledger.Conway.Core qualified as L
import Cardano.Ledger.Conway.PParams qualified as L
import Cardano.Ledger.Plutus qualified as L

import RIO hiding (toList)

import Control.Error.Util (note)
import Data.ByteString.Short qualified as SBS
import Data.Default
import Data.Map.Strict qualified as M
import Data.ProtoLens (defMessage)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
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
    & U5c.minFeeCoefficient .~ pparams ^. L.ppTxFeeFixedL . to inject
    & U5c.minFeeConstant
      .~ pparams ^. L.ppTxFeePerByteL . to L.unCoinPerByte . to L.fromCompact . to inject
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
          pure $ set L.ppTxFeeFixedL (L.Coin minFeeCoeff) r
      , \r -> do
          minFeeConst <- pp ^. U5c.minFeeConstant . to utxoRpcBigIntToInteger ?! "Invalid minFeeConstant"
          minFeeConstCompact <-
            note "Could not convert minFeeConstant to compact form." $ L.toCompact (L.Coin minFeeConst)
          pure $ set L.ppTxFeePerByteL (L.CoinPerByte minFeeConstCompact) r
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
          poolInfluence <- pp ^. U5c.poolInfluence . to inject . to L.boundRational ?! "Invalid poolInfluence"
          pure $ set L.ppA0L poolInfluence r
      , \r -> do
          monetaryExpansion <-
            pp ^. U5c.monetaryExpansion . to inject . to L.boundRational ?! "Invalid monetaryExpansion"
          pure $ set L.ppRhoL monetaryExpansion r
      , \r -> do
          treasuryExpansion <-
            pp ^. U5c.treasuryExpansion . to inject . to L.boundRational ?! "Invalid treasuryExpansion"
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
          steps <- pp ^. U5c.prices . U5c.steps . to inject . to L.boundRational ?! "Invalid prices.steps"
          mem <- pp ^. U5c.prices . U5c.memory . to inject . to L.boundRational ?! "Invalid prices.mem"
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
                . to inject
                . to L.boundRational
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
              $ map (L.boundRational . inject) thresholds
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
            , ppEcomonicGroup
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
                , (?! "Invalid value in drepVotingThresholds: ppEcomonicGroup")
                , (?! "Invalid value in drepVotingThresholds: ppTechnicalGroup")
                , (?! "Invalid value in drepVotingThresholds: ppGovGroup")
                , (?! "Invalid value in drepVotingThresholds: treasuryWithdrawal")
                ]
              $ map (L.boundRational . inject) thresholds
          pure $
            r
              & L.ppDRepVotingThresholdsL . L.dvtMotionNoConfidenceL .~ motionNoConfidence
              & L.ppDRepVotingThresholdsL . L.dvtCommitteeNormalL .~ committeeNormal
              & L.ppDRepVotingThresholdsL . L.dvtCommitteeNoConfidenceL .~ committeeNoConfidence
              & L.ppDRepVotingThresholdsL . L.dvtUpdateToConstitutionL .~ updateToConstitution
              & L.ppDRepVotingThresholdsL . L.dvtHardForkInitiationL .~ hardforkInitiation
              & L.ppDRepVotingThresholdsL . L.dvtPPNetworkGroupL .~ ppNetworkGroup
              & L.ppDRepVotingThresholdsL . L.dvtPPEconomicGroupL .~ ppEcomonicGroup
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
    & U5c.slot .~ slotNo
    & U5c.hash .~ blockHash
    & U5c.height .~ blockHeight

simpleScriptToUtxoRpcNativeScript :: SimpleScript -> Proto UtxoRpc.NativeScript
simpleScriptToUtxoRpcNativeScript = \case
  RequireSignature paymentKeyHash ->
    defMessage & U5c.scriptPubkeyHash .~ serialiseToRawBytes paymentKeyHash
  RequireTimeBefore (SlotNo slotNo) ->
    defMessage & U5c.invalidHereafter .~ slotNo
  RequireTimeAfter (SlotNo slotNo) ->
    defMessage & U5c.invalidBefore .~ slotNo
  RequireAllOf scripts ->
    defMessage & U5c.scriptAll . U5c.items .~ map simpleScriptToUtxoRpcNativeScript scripts
  RequireAnyOf scripts ->
    defMessage & U5c.scriptAny . U5c.items .~ map simpleScriptToUtxoRpcNativeScript scripts
  RequireMOf k scripts -> do
    let nScriptsOf =
          defMessage
            & U5c.k .~ fromIntegral k
            & U5c.scripts .~ map simpleScriptToUtxoRpcNativeScript scripts
    defMessage & U5c.scriptNOfK .~ nScriptsOf

utxoRpcNativeScriptToSimpleScript
  :: HasCallStack
  => MonadThrow m
  => Proto UtxoRpc.NativeScript
  -> m SimpleScript
utxoRpcNativeScriptToSimpleScript scriptRpc
  | Just paymentKeyHash <- scriptRpc ^. U5c.maybe'scriptPubkeyHash =
      RequireSignature <$> liftEitherError (deserialiseFromRawBytes asType paymentKeyHash)
  | Just slotNo <- scriptRpc ^. U5c.maybe'invalidHereafter =
      pure . RequireTimeBefore $ SlotNo slotNo
  | Just slotNo <- scriptRpc ^. U5c.maybe'invalidBefore =
      pure . RequireTimeAfter $ SlotNo slotNo
  | Just scriptsRpc <- scriptRpc ^. U5c.maybe'scriptAll = do
      fmap RequireAllOf $
        mapM utxoRpcNativeScriptToSimpleScript $
          scriptsRpc ^. U5c.items
  | Just scriptsRpc <- scriptRpc ^. U5c.maybe'scriptAny = do
      fmap RequireAnyOf $
        mapM utxoRpcNativeScriptToSimpleScript $
          scriptsRpc ^. U5c.items
  | Just scriptsRpc <- scriptRpc ^. U5c.maybe'scriptNOfK = do
      fmap (RequireMOf . fromIntegral $ scriptsRpc ^. U5c.k) $
        mapM utxoRpcNativeScriptToSimpleScript $
          scriptsRpc ^. U5c.scripts
  | otherwise = throwM . stringException $ "Cannot decode UTxORPC NativeScript"

referenceScriptToUtxoRpcScript :: ReferenceScript era -> Proto UtxoRpc.Script
referenceScriptToUtxoRpcScript ReferenceScriptNone = defMessage
referenceScriptToUtxoRpcScript (ReferenceScript _ (ScriptInAnyLang _ script)) =
  case script of
    SimpleScript ss ->
      defMessage & U5c.native .~ simpleScriptToUtxoRpcNativeScript ss
    PlutusScript PlutusScriptV1 ps ->
      defMessage & U5c.plutusV1 .~ serialiseToRawBytes ps
    PlutusScript PlutusScriptV2 ps ->
      defMessage & U5c.plutusV2 .~ serialiseToRawBytes ps
    PlutusScript PlutusScriptV3 ps ->
      defMessage & U5c.plutusV3 .~ serialiseToRawBytes ps
    PlutusScript PlutusScriptV4 ps ->
      defMessage & U5c.plutusV4 .~ serialiseToRawBytes ps

utxoRpcScriptToReferenceScript
  :: forall era m
   . HasCallStack
  => MonadThrow m
  => IsEra era
  => Proto UtxoRpc.Script
  -> m (ReferenceScript era)
utxoRpcScriptToReferenceScript protoScript
  | Just script <- protoScript ^. U5c.maybe'native =
      ReferenceScript (convert $ useEra @era) . ScriptInAnyLang SimpleScriptLanguage . SimpleScript
        <$> utxoRpcNativeScriptToSimpleScript script
  | Just script <- protoScript ^. U5c.maybe'plutusV1 =
      ReferenceScript (convert $ useEra @era) . ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV1)
        <$> liftEitherError (deserialiseFromCBOR asType script)
  | Just script <- protoScript ^. U5c.maybe'plutusV2 =
      ReferenceScript (convert $ useEra @era) . ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV2)
        <$> liftEitherError (deserialiseFromCBOR asType script)
  | Just script <- protoScript ^. U5c.maybe'plutusV3 =
      ReferenceScript (convert $ useEra @era) . ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV3)
        <$> liftEitherError (deserialiseFromCBOR asType script)
  | Just script <- protoScript ^. U5c.maybe'plutusV4 =
      ReferenceScript (convert $ useEra @era) . ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV4)
        <$> liftEitherError (deserialiseFromCBOR asType script)
  | otherwise = pure ReferenceScriptNone

scriptDataToUtxoRpcPlutusData :: ScriptData -> Proto UtxoRpc.PlutusData
scriptDataToUtxoRpcPlutusData = \case
  ScriptDataBytes bs ->
    defMessage & U5c.boundedBytes .~ bs
  ScriptDataNumber int -> defMessage & U5c.bigInt .~ inject int
  ScriptDataList sds ->
    defMessage & U5c.array . U5c.items .~ map scriptDataToUtxoRpcPlutusData sds
  ScriptDataMap elements -> do
    let pairs =
          elements <&> \(k, v) ->
            defMessage
              & U5c.key .~ scriptDataToUtxoRpcPlutusData k
              & U5c.value .~ scriptDataToUtxoRpcPlutusData v
    defMessage & U5c.map . U5c.pairs .~ pairs
  ScriptDataConstructor tag args -> do
    -- Details of plutus tag serialisation:
    -- https://github.com/IntersectMBO/plutus/blob/fc78c36b545ee287ae8796a0c1a7d04cf31f4cee/plutus-core/plutus-core/src/PlutusCore/Data.hs#L72
    let constr =
          defMessage
            & ( if tag <= fromIntegral (maxBound @Word32)
                  then U5c.tag .~ fromIntegral tag
                  else (U5c.tag .~ 102) . (U5c.anyConstructor .~ fromIntegral @_ @Word64 tag)
              )
            & U5c.fields .~ map scriptDataToUtxoRpcPlutusData args
    defMessage & U5c.constr .~ constr

utxoToUtxoRpcAnyUtxoData :: forall era. IsEra era => UTxO era -> [Proto UtxoRpc.AnyUtxoData]
utxoToUtxoRpcAnyUtxoData utxo =
  toList utxo <&> \(txIn, txOut) -> do
    let era = useEra @era
        txOutCbor =
          obtainCommonConstraints era $
            CBOR.serialize' $
              toShelleyTxOut (convert era) txOut
    defMessage
      & U5c.nativeBytes .~ txOutCbor
      & U5c.txoRef .~ inject txIn
      & U5c.cardano .~ txOutToUtxoRpcTxOutput txOut

anyUtxoDataUtxoRpcToUtxo
  :: forall era m
   . HasCallStack
  => MonadThrow m
  => Era era
  -> [Proto UtxoRpc.AnyUtxoData]
  -> m (UTxO era)
anyUtxoDataUtxoRpcToUtxo era = fmap fromList . foldM f mempty
 where
  f
    :: [(TxIn, TxOut CtxUTxO era)]
    -> Proto UtxoRpc.AnyUtxoData
    -> m [(TxIn, TxOut CtxUTxO era)]
  f acc e = do
    txOut <- obtainCommonConstraints era $ utxoRpcTxOutputToTxOut $ e ^. U5c.cardano
    txIn <- txoRefUtxoRpcToTxIn $ e ^. U5c.txoRef
    pure $ (txIn, txOut) : acc

txoRefUtxoRpcToTxIn
  :: forall m
   . HasCallStack
  => MonadThrow m
  => Proto UtxoRpc.TxoRef
  -> m TxIn
txoRefUtxoRpcToTxIn txoRef = do
  txId' <-
    liftEitherError $
      deserialiseFromRawBytes asType $
        txoRef ^. U5c.hash
  pure $ TxIn txId' (TxIx . fromIntegral $ txoRef ^. U5c.index)

txOutToUtxoRpcTxOutput
  :: forall era
   . IsEra era
  => TxOut CtxUTxO era
  -> Proto UtxoRpc.TxOutput
txOutToUtxoRpcTxOutput (TxOut addressInEra txOutValue datum script) = do
  let multiAsset =
        fromList $
          toList (valueToPolicyAssets $ txOutValueToValue txOutValue) <&> \(pId, policyAssets) -> do
            let assets =
                  toList policyAssets <&> \(assetName, Quantity qty) -> do
                    defMessage
                      & U5c.name .~ serialiseToRawBytes assetName
                      -- we don't have access to info if the coin was minted in the transaction,
                      -- maybe we should add it later
                      -- & U5c.maybe'mintCoin .~ Nothing
                      & U5c.quantity .~ inject qty
            defMessage
              & U5c.policyId .~ serialiseToRawBytes pId
              & U5c.assets .~ assets
      datumRpc = case datum of
        TxOutDatumNone ->
          Nothing
        TxOutDatumHash _ scriptDataHash ->
          Just $
            defMessage
              & U5c.hash .~ serialiseToRawBytes scriptDataHash
              & U5c.maybe'payload .~ Nothing -- we don't have it
              & U5c.maybe'originalCbor .~ Nothing
        TxOutDatumInline _ hashableScriptData ->
          Just $
            defMessage
              & U5c.hash .~ serialiseToCBOR hashableScriptData
              & U5c.payload .~ scriptDataToUtxoRpcPlutusData (getScriptData hashableScriptData)
              & U5c.originalCbor .~ getOriginalScriptDataBytes hashableScriptData

  defMessage
    & U5c.address .~ T.encodeUtf8 (obtainCommonConstraints (useEra @era) $ serialiseAddress addressInEra)
    & U5c.coin .~ inject (L.unCoin (txOutValueToLovelace txOutValue))
    & U5c.assets .~ multiAsset
    & U5c.maybe'datum .~ datumRpc
    & U5c.script .~ referenceScriptToUtxoRpcScript script

utxoRpcTxOutputToTxOut
  :: forall era m
   . HasCallStack
  => MonadThrow m
  => IsEra era
  => Proto UtxoRpc.TxOutput
  -> m (TxOut CtxUTxO era)
utxoRpcTxOutputToTxOut txOutput = do
  let era = useEra @era
  addrUtf8 <- liftEitherError $ T.decodeUtf8' (txOutput ^. U5c.address)
  address <-
    maybe (throwM . stringException $ "Cannot decode address: " <> T.unpack addrUtf8) pure $
      obtainCommonConstraints era $
        deserialiseAddress asType addrUtf8
  datum <-
    case txOutput ^. U5c.maybe'datum of
      Just datumRpc ->
        case datumRpc ^. U5c.maybe'originalCbor of
          Just cbor ->
            liftEitherError $
              TxOutDatumInline (convert era)
                <$> deserialiseFromCBOR asType cbor
          Nothing ->
            liftEitherError $
              TxOutDatumHash (convert era)
                <$> deserialiseFromRawBytes asType (datumRpc ^. U5c.hash)
      Nothing -> pure TxOutDatumNone
  referenceScript <- utxoRpcScriptToReferenceScript (txOutput ^. U5c.script)
  coinValue <- lovelaceToValue . L.Coin <$> txOutput ^. U5c.coin . to utxoRpcBigIntToInteger
  multiAssetValue <- fmap (fromList @Value . join) . forM (txOutput ^. U5c.assets) $ \policyAssets -> do
    pId <-
      liftEitherError $ deserialiseFromRawBytes AsPolicyId (policyAssets ^. U5c.policyId)
    forM (policyAssets ^. U5c.assets) $ \asset -> do
      assetName <-
        liftEitherError $
          deserialiseFromRawBytes AsAssetName (asset ^. U5c.name)
      coin <- Quantity <$> asset ^. U5c.quantity . to utxoRpcBigIntToInteger
      pure (AssetId pId assetName, coin)
  pure $
    TxOut
      address
      ( obtainCommonConstraints era $
          TxOutValueShelleyBased (convert era) (toMaryValue $ coinValue <> multiAssetValue)
      )
      datum
      referenceScript

utxoRpcBigIntToInteger
  :: forall m
   . HasCallStack
  => MonadThrow m
  => Proto UtxoRpc.BigInt
  -> m Integer
utxoRpcBigIntToInteger bigInt
  | Just int <- bigInt ^. U5c.maybe'int = pure $ fromIntegral int
  | Just bytes <- bigInt ^. U5c.maybe'bigNInt = do
      n <- fmap fromIntegral . liftEitherError $ deserialiseFromRawBytes AsNatural bytes
      pure $ -n - 1
  | Just bytes <- bigInt ^. U5c.maybe'bigUInt =
      fmap fromIntegral . liftEitherError $ deserialiseFromRawBytes AsNatural bytes
  | otherwise = pure 0 -- assume default value
