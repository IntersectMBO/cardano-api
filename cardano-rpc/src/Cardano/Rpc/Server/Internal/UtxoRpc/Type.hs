{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
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
    & #coinsPerUtxoByte .~ pparams ^. L.ppCoinsPerUTxOByteL . to L.unCoinPerByte . to inject
    & #maxTxSize .~ pparams ^. L.ppMaxTxSizeL . to fromIntegral
    & #minFeeCoefficient .~ pparams ^. L.ppMinFeeBL . to inject
    & #minFeeConstant .~ pparams ^. L.ppMinFeeAL . to inject
    & #maxBlockBodySize .~ pparams ^. L.ppMaxBBSizeL . to fromIntegral
    & #maxBlockHeaderSize .~ pparams ^. L.ppMaxBHSizeL . to fromIntegral
    & #stakeKeyDeposit .~ pparams ^. L.ppKeyDepositL . to inject
    & #poolDeposit .~ pparams ^. L.ppPoolDepositL . to inject
    & #poolRetirementEpochBound .~ pparams ^. L.ppEMaxL . to L.unEpochInterval . to fromIntegral
    & #desiredNumberOfPools .~ pparams ^. L.ppNOptL . to fromIntegral
    & #poolInfluence .~ pparams ^. L.ppA0L . to L.unboundRational . to inject
    & #monetaryExpansion .~ pparams ^. L.ppRhoL . to L.unboundRational . to inject
    & #treasuryExpansion .~ pparams ^. L.ppTauL . to L.unboundRational . to inject
    & #minPoolCost .~ pparams ^. L.ppMinPoolCostL . to inject
    & #protocolVersion . #major .~ pparams ^. L.ppProtocolVersionL . to L.pvMajor . to L.getVersion
    & #protocolVersion . #minor .~ pparams ^. L.ppProtocolVersionL . to L.pvMinor . to fromIntegral
    & #maxValueSize .~ pparams ^. L.ppMaxValSizeL . to fromIntegral
    & #collateralPercentage .~ pparams ^. L.ppCollateralPercentageL . to fromIntegral
    & #maxCollateralInputs .~ pparams ^. L.ppMaxCollateralInputsL . to fromIntegral
    & #costModels . #plutusV1 . #values .~ (join . maybeToList) (M.lookup L.PlutusV1 pparamsCostModels)
    & #costModels . #plutusV2 . #values .~ (join . maybeToList) (M.lookup L.PlutusV2 pparamsCostModels)
    & #costModels . #plutusV3 . #values .~ (join . maybeToList) (M.lookup L.PlutusV3 pparamsCostModels)
    & #costModels . #plutusV4 . #values .~ (join . maybeToList) (M.lookup L.PlutusV4 pparamsCostModels)
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
    & #governanceActionDeposit .~ pparams ^. L.ppGovActionDepositL . to inject
    & #drepDeposit .~ pparams ^. L.ppDRepDepositL . to inject
    & #drepInactivityPeriod .~ pparams ^. L.ppDRepActivityL . to L.unEpochInterval . to fromIntegral

utxoRpcPParamsToProtocolParams
  :: Era era
  -> Proto UtxoRpc.PParams
  -> Either String (L.PParams (ShelleyLedgerEra era))
utxoRpcPParamsToProtocolParams era pp = conwayEraOnwardsConstraints (convert era) $ do
  def
    & appFuns
      [ \r -> do
          coinsPerUtxoByte <-
            pp ^. #coinsPerUtxoByte . to utxoRpcBigIntToInteger ?! "Invalid coinsPerUtxoByte"
          pure $ set L.ppCoinsPerUTxOByteL (L.CoinPerByte $ L.Coin coinsPerUtxoByte) r
      , pure . (L.ppMaxTxSizeL .~ pp ^. #maxTxSize . to fromIntegral)
      , \r -> do
          minFeeCoeff <- pp ^. #minFeeCoefficient . to utxoRpcBigIntToInteger ?! "Invalid minFeeCoefficient"
          pure $ set L.ppMinFeeBL (L.Coin minFeeCoeff) r
      , \r -> do
          minFeeConst <- pp ^. #minFeeConstant . to utxoRpcBigIntToInteger ?! "Invalid minFeeConstant"
          pure $ set L.ppMinFeeAL (L.Coin minFeeConst) r
      , pure . (L.ppMaxBBSizeL .~ pp ^. #maxBlockBodySize . to fromIntegral)
      , pure . (L.ppMaxBHSizeL .~ pp ^. #maxBlockHeaderSize . to fromIntegral)
      , \r -> do
          stakeKeyDeposit <- pp ^. #stakeKeyDeposit . to utxoRpcBigIntToInteger ?! "Invalid stakeKeyDeposit"
          pure $ set L.ppKeyDepositL (L.Coin stakeKeyDeposit) r
      , \r -> do
          poolDeposit <- pp ^. #poolDeposit . to utxoRpcBigIntToInteger ?! "Invalid poolDeposit"
          pure $ set L.ppPoolDepositL (L.Coin poolDeposit) r
      , pure . (L.ppEMaxL .~ pp ^. #poolRetirementEpochBound . to fromIntegral . to L.EpochInterval)
      , pure . (L.ppNOptL .~ pp ^. #desiredNumberOfPools . to fromIntegral)
      , \r -> do
          poolInfluence <- pp ^. #poolInfluence . to inject . to L.boundRational ?! "Invalid poolInfluence"
          pure $ set L.ppA0L poolInfluence r
      , \r -> do
          monetaryExpansion <-
            pp ^. #monetaryExpansion . to inject . to L.boundRational ?! "Invalid monetaryExpansion"
          pure $ set L.ppRhoL monetaryExpansion r
      , \r -> do
          treasuryExpansion <-
            pp ^. #treasuryExpansion . to inject . to L.boundRational ?! "Invalid treasuryExpansion"
          pure $ set L.ppTauL treasuryExpansion r
      , \r -> do
          minPoolCost <- pp ^. #minPoolCost . to utxoRpcBigIntToInteger ?! "Invalid minPoolCost"
          pure $ set L.ppMinPoolCostL (L.Coin minPoolCost) r
      , \r -> do
          major <- L.mkVersion64 $ pp ^. #protocolVersion . #major . to fromIntegral
          pure $ set (L.ppProtocolVersionL . pvMajorL) major r
      , pure . (L.ppProtocolVersionL . pvMinorL .~ pp ^. #protocolVersion . #minor . to fromIntegral)
      , pure . (L.ppMaxValSizeL .~ pp ^. #maxValueSize . to fromIntegral)
      , pure . (L.ppCollateralPercentageL .~ pp ^. #collateralPercentage . to fromIntegral)
      , pure . (L.ppMaxCollateralInputsL .~ pp ^. #maxCollateralInputs . to fromIntegral)
      , \r -> first show $ do
          cm1 <- L.mkCostModel L.PlutusV1 $ pp ^. #costModels . #plutusV1 . #values
          cm2 <- L.mkCostModel L.PlutusV2 $ pp ^. #costModels . #plutusV2 . #values
          cm3 <- L.mkCostModel L.PlutusV3 $ pp ^. #costModels . #plutusV3 . #values
          cm4 <- L.mkCostModel L.PlutusV4 $ pp ^. #costModels . #plutusV4 . #values
          -- do not add empty cost models
          let nonEmptyCostModels =
                fromList . flip mapMaybe [cm1, cm2, cm3, cm4] $ \cm ->
                  if not (null $ L.getCostModelParams cm)
                    then Just (L.getCostModelLanguage cm, cm)
                    else Nothing
          pure $
            r & L.ppCostModelsL .~ L.mkCostModels nonEmptyCostModels
      , \r -> do
          steps <- pp ^. #prices . #steps . to inject . to L.boundRational ?! "Invalid prices.steps"
          mem <- pp ^. #prices . #memory . to inject . to L.boundRational ?! "Invalid prices.mem"
          pure $
            r
              & L.ppPricesL . prStepsL .~ steps
              & L.ppPricesL . prMemL .~ mem
      , pure . (L.ppMaxTxExUnitsL .~ pp ^. #maxExecutionUnitsPerTransaction . to inject)
      , pure . (L.ppMaxBlockExUnitsL .~ pp ^. #maxExecutionUnitsPerBlock . to inject)
      , \r -> do
          minFeeScriptRefCostPerByte <-
            pp
              ^. #minFeeScriptRefCostPerByte . to inject . to L.boundRational ?! "Invalid minFeeScriptRefCostPerByte"
          pure $ set L.ppMinFeeRefScriptCostPerByteL minFeeScriptRefCostPerByte r
      , \r -> do
          let thresholds = pp ^. #poolVotingThresholds . #thresholds
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
          let thresholds = pp ^. #drepVotingThresholds . #thresholds
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
      , pure . (L.ppCommitteeMinSizeL .~ pp ^. #minCommitteeSize . to fromIntegral)
      , pure
          . (L.ppCommitteeMaxTermLengthL .~ pp ^. #committeeTermLimit . to fromIntegral . to L.EpochInterval)
      , pure
          . ( L.ppGovActionLifetimeL
                .~ pp ^. #governanceActionValidityPeriod . to fromIntegral . to L.EpochInterval
            )
      , \r -> do
          govActionDeposit <-
            pp ^. #governanceActionDeposit . to utxoRpcBigIntToInteger ?! "Invalid governanceActionDeposit"
          pure $ set L.ppGovActionDepositL (L.Coin govActionDeposit) r
      , \r -> do
          drepDeposit <- pp ^. #drepDeposit . to utxoRpcBigIntToInteger ?! "Invalid drepDeposit"
          pure $ set L.ppDRepDepositL (L.Coin drepDeposit) r
      , pure . (L.ppDRepActivityL .~ pp ^. #drepInactivityPeriod . to fromIntegral . to L.EpochInterval)
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
    & #slot .~ slotNo
    & #hash .~ blockHash
    & #height .~ blockHeight

simpleScriptToUtxoRpcNativeScript :: SimpleScript -> Proto UtxoRpc.NativeScript
simpleScriptToUtxoRpcNativeScript = \case
  RequireSignature paymentKeyHash ->
    defMessage & #scriptPubkeyHash .~ serialiseToRawBytes paymentKeyHash
  RequireTimeBefore (SlotNo slotNo) ->
    defMessage & #invalidHereafter .~ slotNo
  RequireTimeAfter (SlotNo slotNo) ->
    defMessage & #invalidBefore .~ slotNo
  RequireAllOf scripts ->
    defMessage & #scriptAll . #items .~ map simpleScriptToUtxoRpcNativeScript scripts
  RequireAnyOf scripts ->
    defMessage & #scriptAny . #items .~ map simpleScriptToUtxoRpcNativeScript scripts
  RequireMOf k scripts -> do
    let nScriptsOf =
          defMessage
            & #k .~ fromIntegral k
            & #scripts .~ map simpleScriptToUtxoRpcNativeScript scripts
    defMessage & #scriptNOfK .~ nScriptsOf

utxoRpcNativeScriptToSimpleScript
  :: HasCallStack
  => MonadThrow m
  => Proto UtxoRpc.NativeScript
  -> m SimpleScript
utxoRpcNativeScriptToSimpleScript scriptRpc
  | Just paymentKeyHash <- scriptRpc ^. #maybe'scriptPubkeyHash =
      RequireSignature <$> liftEitherError (deserialiseFromRawBytes asType paymentKeyHash)
  | Just slotNo <- scriptRpc ^. #maybe'invalidHereafter =
      pure . RequireTimeBefore $ SlotNo slotNo
  | Just slotNo <- scriptRpc ^. #maybe'invalidBefore =
      pure . RequireTimeAfter $ SlotNo slotNo
  | Just scriptsRpc <- scriptRpc ^. #maybe'scriptAll = do
      fmap RequireAllOf $
        mapM utxoRpcNativeScriptToSimpleScript $
          scriptsRpc ^. #items
  | Just scriptsRpc <- scriptRpc ^. #maybe'scriptAny = do
      fmap RequireAnyOf $
        mapM utxoRpcNativeScriptToSimpleScript $
          scriptsRpc ^. #items
  | Just scriptsRpc <- scriptRpc ^. #maybe'scriptNOfK = do
      fmap (RequireMOf . fromIntegral $ scriptsRpc ^. #k) $
        mapM utxoRpcNativeScriptToSimpleScript $
          scriptsRpc ^. #scripts
  | otherwise = throwM . stringException $ "Cannot decode UTxORPC NativeScript"

referenceScriptToUtxoRpcScript :: ReferenceScript era -> Proto UtxoRpc.Script
referenceScriptToUtxoRpcScript ReferenceScriptNone = defMessage
referenceScriptToUtxoRpcScript (ReferenceScript _ (ScriptInAnyLang _ script)) =
  case script of
    SimpleScript ss ->
      defMessage & #native .~ simpleScriptToUtxoRpcNativeScript ss
    PlutusScript PlutusScriptV1 ps ->
      defMessage & #plutusV1 .~ serialiseToRawBytes ps
    PlutusScript PlutusScriptV2 ps ->
      defMessage & #plutusV2 .~ serialiseToRawBytes ps
    PlutusScript PlutusScriptV3 ps ->
      defMessage & #plutusV3 .~ serialiseToRawBytes ps
    PlutusScript PlutusScriptV4 ps ->
      defMessage & #plutusV4 .~ serialiseToRawBytes ps

utxoRpcScriptToReferenceScript
  :: forall era m
   . HasCallStack
  => MonadThrow m
  => IsEra era
  => Proto UtxoRpc.Script
  -> m (ReferenceScript era)
utxoRpcScriptToReferenceScript protoScript
  | Just script <- protoScript ^. #maybe'native =
      ReferenceScript (convert $ useEra @era) . ScriptInAnyLang SimpleScriptLanguage . SimpleScript
        <$> utxoRpcNativeScriptToSimpleScript script
  | Just script <- protoScript ^. #maybe'plutusV1 =
      ReferenceScript (convert $ useEra @era) . ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV1)
        <$> liftEitherError (deserialiseFromCBOR asType script)
  | Just script <- protoScript ^. #maybe'plutusV2 =
      ReferenceScript (convert $ useEra @era) . ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV2)
        <$> liftEitherError (deserialiseFromCBOR asType script)
  | Just script <- protoScript ^. #maybe'plutusV3 =
      ReferenceScript (convert $ useEra @era) . ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV3)
        <$> liftEitherError (deserialiseFromCBOR asType script)
  | Just script <- protoScript ^. #maybe'plutusV4 =
      ReferenceScript (convert $ useEra @era) . ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV4)
        <$> liftEitherError (deserialiseFromCBOR asType script)
  | otherwise = pure ReferenceScriptNone

scriptDataToUtxoRpcPlutusData :: ScriptData -> Proto UtxoRpc.PlutusData
scriptDataToUtxoRpcPlutusData = \case
  ScriptDataBytes bs ->
    defMessage & #boundedBytes .~ bs
  ScriptDataNumber int -> defMessage & #bigInt .~ inject int
  ScriptDataList sds ->
    defMessage & #array . #items .~ map scriptDataToUtxoRpcPlutusData sds
  ScriptDataMap elements -> do
    let pairs =
          elements <&> \(k, v) ->
            defMessage
              & #key .~ scriptDataToUtxoRpcPlutusData k
              & #value .~ scriptDataToUtxoRpcPlutusData v
    defMessage & #map . #pairs .~ pairs
  ScriptDataConstructor tag args -> do
    -- Details of plutus tag serialisation:
    -- https://github.com/IntersectMBO/plutus/blob/fc78c36b545ee287ae8796a0c1a7d04cf31f4cee/plutus-core/plutus-core/src/PlutusCore/Data.hs#L72
    let constr =
          defMessage
            & ( if tag <= fromIntegral (maxBound @Word32)
                  then #tag .~ fromIntegral tag
                  else (#tag .~ 102) . (#anyConstructor .~ fromIntegral @_ @Word64 tag)
              )
            & #fields .~ map scriptDataToUtxoRpcPlutusData args
    defMessage & #constr .~ constr

utxoToUtxoRpcAnyUtxoData :: forall era. IsEra era => UTxO era -> [Proto UtxoRpc.AnyUtxoData]
utxoToUtxoRpcAnyUtxoData utxo =
  toList utxo <&> \(txIn, txOut) -> do
    let era = useEra @era
        txOutCbor =
          obtainCommonConstraints era $
            CBOR.serialize' $
              toShelleyTxOut (convert era) txOut
    defMessage
      & #nativeBytes .~ txOutCbor
      & #txoRef .~ inject txIn
      & #cardano .~ txOutToUtxoRpcTxOutput txOut

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
    txOut <- obtainCommonConstraints era $ utxoRpcTxOutputToTxOut $ e ^. #cardano
    txIn <- txoRefUtxoRpcToTxIn $ e ^. #txoRef
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
        txoRef ^. #hash
  pure $ TxIn txId' (TxIx . fromIntegral $ txoRef ^. #index)

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
                      & #name .~ serialiseToRawBytes assetName
                      -- we don't have access to info if the coin was minted in the transaction,
                      -- maybe we should add it later
                      -- & #maybe'mintCoin .~ Nothing
                      & #quantity .~ inject qty
            defMessage
              & #policyId .~ serialiseToRawBytes pId
              & #assets .~ assets
      datumRpc = case datum of
        TxOutDatumNone ->
          Nothing
        TxOutDatumHash _ scriptDataHash ->
          Just $
            defMessage
              & #hash .~ serialiseToRawBytes scriptDataHash
              & #maybe'payload .~ Nothing -- we don't have it
              & #maybe'originalCbor .~ Nothing
        TxOutDatumInline _ hashableScriptData ->
          Just $
            defMessage
              & #hash .~ serialiseToCBOR hashableScriptData
              & #payload .~ scriptDataToUtxoRpcPlutusData (getScriptData hashableScriptData)
              & #originalCbor .~ getOriginalScriptDataBytes hashableScriptData

  defMessage
    & #address .~ T.encodeUtf8 (obtainCommonConstraints (useEra @era) $ serialiseAddress addressInEra)
    & #coin .~ inject (L.unCoin (txOutValueToLovelace txOutValue))
    & #assets .~ multiAsset
    & #maybe'datum .~ datumRpc
    & #script .~ referenceScriptToUtxoRpcScript script

utxoRpcTxOutputToTxOut
  :: forall era m
   . HasCallStack
  => MonadThrow m
  => IsEra era
  => Proto UtxoRpc.TxOutput
  -> m (TxOut CtxUTxO era)
utxoRpcTxOutputToTxOut txOutput = do
  let era = useEra @era
  addrUtf8 <- liftEitherError $ T.decodeUtf8' (txOutput ^. #address)
  address <-
    maybe (throwM . stringException $ "Cannot decode address: " <> T.unpack addrUtf8) pure $
      obtainCommonConstraints era $
        deserialiseAddress asType addrUtf8
  datum <-
    case txOutput ^. #maybe'datum of
      Just datumRpc ->
        case datumRpc ^. #maybe'originalCbor of
          Just cbor ->
            liftEitherError $
              TxOutDatumInline (convert era)
                <$> deserialiseFromCBOR asType cbor
          Nothing ->
            liftEitherError $
              TxOutDatumHash (convert era)
                <$> deserialiseFromRawBytes asType (datumRpc ^. #hash)
      Nothing -> pure TxOutDatumNone
  referenceScript <- utxoRpcScriptToReferenceScript (txOutput ^. #script)
  coinValue <- lovelaceToValue . L.Coin <$> txOutput ^. #coin . to utxoRpcBigIntToInteger
  multiAssetValue <- fmap (fromList @Value . join) . forM (txOutput ^. #assets) $ \policyAssets -> do
    pId <-
      liftEitherError $ deserialiseFromRawBytes AsPolicyId (policyAssets ^. #policyId)
    forM (policyAssets ^. #assets) $ \asset -> do
      assetName <-
        liftEitherError $
          deserialiseFromRawBytes AsAssetName (asset ^. #name)
      coin <- Quantity <$> asset ^. #quantity . to utxoRpcBigIntToInteger
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
  | Just int <- bigInt ^. #maybe'int = pure $ fromIntegral int
  | Just bytes <- bigInt ^. #maybe'bigNInt = do
      n <- fmap fromIntegral . liftEitherError $ deserialiseFromRawBytes AsNatural bytes
      pure $ -n - 1
  | Just bytes <- bigInt ^. #maybe'bigUInt =
      fmap fromIntegral . liftEitherError $ deserialiseFromRawBytes AsNatural bytes
  | otherwise = pure 0 -- assume default value
