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
              -- TODO investigate if tag is the right field, or should any_constructor be used here
              -- https://github.com/IntersectMBO/plutus/blob/fc78c36b545ee287ae8796a0c1a7d04cf31f4cee/plutus-core/plutus-core/src/PlutusCore/Data.hs#L72
              & #tag .~ fromIntegral tag
              & #fields .~ map inject args
      defMessage & #constr .~ constr

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
