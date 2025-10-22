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
  , protocolParamsToUtxoRpcPParams
  , simpleScriptToUtxoRpcNativeScript
  , mkChainPointMsg
  )
where

import Cardano.Api (SerialiseAsCBOR (serialiseToCBOR), ToCBOR (..))
import Cardano.Api.Address
import Cardano.Api.Block
import Cardano.Api.Block (SlotNo (..))
import Cardano.Api.Era
import Cardano.Api.Error
import Cardano.Api.Experimental.Era
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Monad.Error
import Cardano.Api.Plutus
import Cardano.Api.Pretty
import Cardano.Api.Serialise.Raw
import Cardano.Api.Serialise.SerialiseUsing
import Cardano.Api.Tx
import Cardano.Api.Value
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as UtxoRpc
import Cardano.Rpc.Server.Internal.Orphans ()

import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.BaseTypes (WithOrigin (..))
import Cardano.Ledger.BaseTypes qualified as L
import Cardano.Ledger.Binary.Version qualified as L
import Cardano.Ledger.Conway.Core qualified as L
import Cardano.Ledger.Conway.PParams qualified as L
import Cardano.Ledger.Plutus qualified as L

import RIO hiding (toList)

import Data.ByteString.Short qualified as SBS
import Data.Default
import Data.ProtoLens (defMessage)
import Data.Text.Encoding qualified as T
import GHC.IsList
import Network.GRPC.Spec

protocolParamsToUtxoRpcPParams
  :: Era era
  -> L.PParams (ShelleyLedgerEra era)
  -> Proto UtxoRpc.PParams
protocolParamsToUtxoRpcPParams era = conwayEraOnwardsConstraints (convert era) inject

utxoRpcPParamsToProtocolParams
  :: Era era
  -> Proto UtxoRpc.PParams
  -> Either String (L.PParams (ShelleyLedgerEra era))
utxoRpcPParamsToProtocolParams era pp = conwayEraOnwardsConstraints (convert era) $ do
  def
    & appFuns
      [ pure
          . (L.ppCoinsPerUTxOByteL .~ pp ^. #coinsPerUtxoByte . to fromIntegral . to L.Coin . to L.CoinPerByte)
      , pure . (L.ppMaxTxSizeL .~ pp ^. #maxTxSize . to fromIntegral)
      , pure . (L.ppMinFeeBL .~ pp ^. #minFeeCoefficient . to fromIntegral)
      , pure . (L.ppMinFeeAL .~ pp ^. #minFeeConstant . to fromIntegral)
      , pure . (L.ppMaxBBSizeL .~ pp ^. #maxBlockBodySize . to fromIntegral)
      , pure . (L.ppMaxBHSizeL .~ pp ^. #maxBlockHeaderSize . to fromIntegral)
      , pure . (L.ppKeyDepositL .~ pp ^. #stakeKeyDeposit . to fromIntegral)
      , pure . (L.ppPoolDepositL .~ pp ^. #poolDeposit . to fromIntegral)
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
      , pure . (L.ppMinPoolCostL .~ pp ^. #minPoolCost . to fromIntegral)
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
          -- do not add empty cost models
          let nonEmptyCostModels =
                fromList . flip mapMaybe [cm1, cm2, cm3] $ \cm ->
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
      , pure . (L.ppGovActionDepositL .~ pp ^. #governanceActionDeposit . to fromIntegral)
      , pure . (L.ppDRepDepositL .~ pp ^. #drepDeposit . to fromIntegral)
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
    defMessage & #scriptPubkey .~ serialiseToRawBytes paymentKeyHash
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

scriptDataToUtxoRpcPlutusData :: ScriptData -> Proto UtxoRpc.PlutusData
scriptDataToUtxoRpcPlutusData = \case
  ScriptDataBytes bs ->
    defMessage & #boundedBytes .~ bs
  ScriptDataNumber int
    | int <= fromIntegral (maxBound @Int64)
        && int >= fromIntegral (minBound @Int64) ->
        defMessage & #bigInt . #int .~ fromIntegral int
    | int < 0 ->
        -- https://www.rfc-editor.org/rfc/rfc8949.html#name-bignums see 3.4.3 for negative integers
        defMessage & #bigInt . #bigNInt .~ serialiseToRawBytes (fromIntegral @_ @Natural (-1 - int))
    | otherwise ->
        defMessage & #bigInt . #bigUInt .~ serialiseToRawBytes (fromIntegral @_ @Natural int)
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

utxoToUtxoRpcAnyUtxoData :: forall era. IsCardanoEra era => UTxO era -> [Proto UtxoRpc.AnyUtxoData]
utxoToUtxoRpcAnyUtxoData utxo =
  toList utxo <&> \(txIn, TxOut addressInEra txOutValue datum script) -> do
    let multiAsset =
          fromList $
            toList (valueToPolicyAssets $ txOutValueToValue txOutValue) <&> \(pId, policyAssets) -> do
              let assets =
                    toList policyAssets <&> \(assetName, Quantity qty) -> do
                      defMessage
                        & #name .~ serialiseToRawBytes assetName
                        -- we don't have access to info if the coin was minted in the transaction,
                        -- maybe we should add it later
                        & #maybe'mintCoin .~ Nothing
                        & #outputCoin .~ fromIntegral qty
              defMessage
                & #policyId .~ serialiseToRawBytes pId
                & #assets .~ assets
        datumRpc = case datum of
          TxOutDatumNone ->
            defMessage
          TxOutDatumHash _ scriptDataHash ->
            defMessage
              & #hash .~ serialiseToRawBytes scriptDataHash
              & #maybe'payload .~ Nothing -- we don't have it
              & #originalCbor .~ mempty -- we don't have it
          TxOutDatumInline _ hashableScriptData ->
            defMessage
              & #hash .~ serialiseToRawBytes (hashScriptDataBytes hashableScriptData)
              & #payload .~ scriptDataToUtxoRpcPlutusData (getScriptData hashableScriptData)
              & #originalCbor .~ getOriginalScriptDataBytes hashableScriptData

        protoTxOut =
          defMessage
            -- TODO we don't have serialiseToRawBytes for AddressInEra, so perhaps this is wrong, because 'address'
            -- has type bytes, but we're putting text there
            & #address .~ T.encodeUtf8 (cardanoEraConstraints (cardanoEra @era) $ serialiseAddress addressInEra)
            & #coin .~ fromIntegral (L.unCoin (txOutValueToLovelace txOutValue))
            & #assets .~ multiAsset
            & #datum .~ datumRpc
            & #script .~ referenceScriptToUtxoRpcScript script
    defMessage
      & #nativeBytes .~ "" -- TODO where to get that from? run cbor serialisation of utxos list?
      & #txoRef .~ inject txIn
      & #cardano .~ protoTxOut
