{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Rpc.Server.Internal.Orphans where

import Cardano.Api (SerialiseAsCBOR (serialiseToCBOR), ToCBOR (..))
import Cardano.Api.Address
import Cardano.Api.Block (SlotNo (..))
import Cardano.Api.Era
import Cardano.Api.Error
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Plutus
import Cardano.Api.Pretty
import Cardano.Api.Serialise.Raw
import Cardano.Api.Serialise.SerialiseUsing
import Cardano.Api.Tx
import Cardano.Api.Value
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as UtxoRpc

import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.BaseTypes qualified as L
import Cardano.Ledger.Conway.PParams qualified as L
import Cardano.Ledger.Plutus qualified as L

import RIO hiding (toList)

import Data.ByteString qualified as B
import Data.Default
import Data.Map.Strict qualified as M
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Message (Message)
import Data.Ratio (denominator, numerator, (%))
import Data.Text.Encoding qualified as T
import GHC.IsList
import Network.GRPC.Spec

---------------
-- Conversion
---------------

-- It's easier to use 'Proto a' wrappers for RPC types, because it makes lens automatically available.

instance Inject (Proto UtxoRpc.RationalNumber) Rational where
  inject r = r ^. #numerator . to fromIntegral % r ^. #denominator . to fromIntegral

-- NB. this clips value in Integer -> Int64/Word64 conversion here
instance Inject Rational (Proto UtxoRpc.RationalNumber) where
  inject r =
    defMessage
      & #numerator .~ fromIntegral (numerator r)
      & #denominator .~ fromIntegral (denominator r)

instance Inject (Proto UtxoRpc.ExUnits) L.ExUnits where
  inject r =
    L.ExUnits
      { L.exUnitsMem = r ^. #memory . to fromIntegral
      , L.exUnitsSteps = r ^. #steps . to fromIntegral
      }

instance Inject L.ExUnits (Proto UtxoRpc.ExUnits) where
  inject L.ExUnits{L.exUnitsMem = mem, L.exUnitsSteps = steps} =
    defMessage
      & #memory .~ fromIntegral mem
      & #steps .~ fromIntegral steps

-- | Note that conversion is not total in the other direction
instance Inject TxIn (Proto UtxoRpc.TxoRef) where
  inject (TxIn txId' (TxIx txIx)) =
    defMessage
      & #hash .~ serialiseToRawBytes txId'
      & #index .~ fromIntegral txIx

instance Inject (ReferenceScript era) (Proto UtxoRpc.Script) where
  inject ReferenceScriptNone = defMessage
  inject (ReferenceScript _ (ScriptInAnyLang _ script)) =
    case script of
      SimpleScript ss ->
        defMessage & #native .~ inject ss
      PlutusScript PlutusScriptV1 ps ->
        defMessage & #plutusV1 .~ serialiseToRawBytes ps
      PlutusScript PlutusScriptV2 ps ->
        defMessage & #plutusV2 .~ serialiseToRawBytes ps
      PlutusScript PlutusScriptV3 ps ->
        defMessage & #plutusV3 .~ serialiseToRawBytes ps
      PlutusScript PlutusScriptV4 ps ->
        defMessage & #plutusV4 .~ serialiseToRawBytes ps

instance Inject SimpleScript (Proto UtxoRpc.NativeScript) where
  inject = \case
    RequireSignature paymentKeyHash ->
      defMessage & #scriptPubkey .~ serialiseToRawBytes paymentKeyHash
    RequireTimeBefore (SlotNo slotNo) ->
      defMessage & #invalidHereafter .~ slotNo
    RequireTimeAfter (SlotNo slotNo) ->
      defMessage & #invalidBefore .~ slotNo
    RequireAllOf scripts ->
      defMessage & #scriptAll . #items .~ map inject scripts
    RequireAnyOf scripts ->
      defMessage & #scriptAny . #items .~ map inject scripts
    RequireMOf k scripts -> do
      let nScriptsOf =
            defMessage
              & #k .~ fromIntegral k
              & #scripts .~ map inject scripts
      defMessage & #scriptNOfK .~ nScriptsOf

instance Inject ScriptData (Proto UtxoRpc.PlutusData) where
  inject = \case
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
      defMessage & #array . #items .~ map inject sds
    ScriptDataMap elements -> do
      let pairs =
            elements <&> \(k, v) ->
              defMessage
                & #key .~ inject k
                & #value .~ inject v
      defMessage & #map . #pairs .~ pairs
    ScriptDataConstructor tag args -> do
      let constr =
            defMessage
              & #tag .~ fromIntegral tag
              & #fields .~ map inject args
      defMessage & #constr .~ constr

instance IsCardanoEra era => Inject (UTxO era) [Proto UtxoRpc.AnyUtxoData] where
  inject utxo =
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
                & #payload .~ inject (getScriptData hashableScriptData)
                & #originalCbor .~ getOriginalScriptDataBytes hashableScriptData

          protoTxOut =
            defMessage
              -- TODO we don't have serialiseToRawBytes for AddressInEra, so perhaps this is wrong, because 'address'
              -- has type bytes, but we're putting text there
              & #address .~ T.encodeUtf8 (cardanoEraConstraints (cardanoEra @era) $ serialiseAddress addressInEra)
              & #coin .~ fromIntegral (L.unCoin (txOutValueToLovelace txOutValue))
              & #assets .~ multiAsset
              & #datum .~ datumRpc
              & #script .~ inject script
      defMessage
        & #nativeBytes .~ "" -- TODO where to get that from? run cbor serialisation of utxos list?
        & #txoRef .~ inject txIn
        & #cardano .~ protoTxOut

instance L.ConwayEraPParams lera => Inject (L.PParams lera) (Proto UtxoRpc.PParams) where
  inject pparams = do
    let pparamsCostModels :: Map L.Language [Int64] =
          L.getCostModelParams <$> pparams ^. L.ppCostModelsL . to L.costModelsValid
        poolVotingThresholds :: L.PoolVotingThresholds =
          pparams ^. L.ppPoolVotingThresholdsL
        drepVotingThresholds :: L.DRepVotingThresholds =
          pparams ^. L.ppDRepVotingThresholdsL
    def
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
      & #monetaryExpansion .~ pparams ^. L.ppRhoL . to L.unboundRational . to inject
      & #treasuryExpansion .~ pparams ^. L.ppTauL . to L.unboundRational . to inject
      & #minPoolCost .~ pparams ^. L.ppMinPoolCostL . to fromIntegral
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
      & #governanceActionDeposit .~ pparams ^. L.ppGovActionDepositL . to fromIntegral
      & #drepDeposit .~ pparams ^. L.ppDRepDepositL . to fromIntegral
      & #drepInactivityPeriod .~ pparams ^. L.ppDRepActivityL . to L.unEpochInterval . to fromIntegral

instance Message a => Default (Proto a) where
  def = defMessage

-----------
-- Errors
-----------

-- TODO add RIO to cardano-api and move this instance there

instance Error StringException where
  prettyError = pshow

instance IsString e => MonadFail (Either e) where
  fail = Left . fromString
