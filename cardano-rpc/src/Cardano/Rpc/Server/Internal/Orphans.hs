{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Rpc.Server.Internal.Orphans () where

import Cardano.Api.Block (ChainPoint (..), Hash (..), SlotNo (..))
import Cardano.Api.Era
import Cardano.Api.Era (Inject (..))
import Cardano.Api.Internal.Utils ((?!))
import Cardano.Api.Ledger qualified as L
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as UtxoRpc

import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.BaseTypes qualified as L
import Cardano.Ledger.Coin qualified as L
import Cardano.Ledger.Conway.PParams qualified as L
import Cardano.Ledger.Plutus qualified as L

import RIO

import Control.Arrow (Kleisli (..))
import Control.Category qualified as Cat
import Data.ByteString.Short qualified as SBS
import Data.Default
import Data.Monoid
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Message (Message)
import Data.Ratio (Ratio, denominator, numerator, (%))
import Network.GRPC.Spec

instance Message a => Default (Proto a) where
  def = defMessage

instance Inject (Proto UtxoRpc.RationalNumber) (Ratio Integer) where
  inject r = r ^. #numerator . to fromIntegral % r ^. #denominator . to fromIntegral

instance Inject (Ratio Integer) (Proto UtxoRpc.RationalNumber) where
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

instance Inject ChainPoint (Proto UtxoRpc.ChainPoint) where
  inject chainPoint = do
    let (slotNo, blockHash) =
          case chainPoint of
            ChainPointAtGenesis -> (0, mempty)
            ChainPoint (SlotNo slot) (HeaderHash hash) -> (slot, SBS.fromShort hash)
    defMessage
      & #slot .~ slotNo
      & #hash .~ blockHash

utxoRpcPParamsToPParams
  :: ConwayEraOnwards era
  -> Proto UtxoRpc.PParams
  -> Either String (L.PParams (ShelleyLedgerEra era))
utxoRpcPParamsToPParams ceo pp = conwayEraOnwardsConstraints ceo $ do
  def
    & (appEndoM . mconcat . fmap EndoM)
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
      , pure . (L.ppMinPoolCostL .~ pp ^. #minPoolCost . to fromIntegral)
      , \r -> do
          major <- L.mkVersion64 $ pp ^. #protocolVersion . #major . to fromIntegral
          pure $ set (L.ppProtocolVersionL . pvMajorL) major r
      , pure . (L.ppProtocolVersionL . pvMinorL .~ pp ^. #protocolVersion . #minor . to fromIntegral)
      ]
 where
  pvMajorL = sets $ \f p@L.ProtVer{L.pvMajor} -> p{L.pvMajor = f pvMajor}
  pvMinorL = sets $ \f p@L.ProtVer{L.pvMinor} -> p{L.pvMinor = f pvMinor}

type EndoM m a = Kleisli m a a

pattern EndoM :: (a -> m a) -> EndoM m a
pattern EndoM a = Kleisli a

appEndoM :: EndoM m a -> a -> m a
appEndoM = runKleisli

instance Monad m => Semigroup (EndoM m a) where
  a <> b = b Cat.. a

instance Monad m => Monoid (EndoM m a) where
  mempty = Kleisli pure

instance MonadFail (Either String) where
  fail = Left
