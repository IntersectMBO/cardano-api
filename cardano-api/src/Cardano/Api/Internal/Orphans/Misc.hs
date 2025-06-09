{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Api.Internal.Orphans.Misc
  (
  )
where

import Cardano.Api.Pretty (Pretty (..), prettyException, (<+>))

import Cardano.Ledger.Alonzo.PParams qualified as Ledger
import Cardano.Ledger.Babbage.PParams qualified as Ledger
import Cardano.Ledger.BaseTypes (strictMaybeToMaybe)
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Binary
import Cardano.Ledger.Binary qualified as CBOR
import Cardano.Ledger.Coin qualified as L
import Cardano.Ledger.Conway.PParams qualified as Ledger
import Cardano.Ledger.HKD (NoUpdate (..))
import Cardano.Ledger.Shelley.PParams qualified as Ledger
import PlutusLedgerApi.Common qualified as P

import Codec.Binary.Bech32 qualified as Bech32
import Data.Data (Data)
import Data.ListMap (ListMap)
import Data.ListMap qualified as ListMap
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Monoid
import GHC.Exts (IsList (..))
import Network.Mux qualified as Mux

deriving instance Data DecoderError

deriving instance Data CBOR.DeserialiseFailure

deriving instance Data Bech32.DecodingError

deriving instance Data Bech32.CharPosition

-- | These instances originally existed on the Lovelace type.
-- As the Lovelace type is deleted and we use L.Coin instead,
-- these instances are added to L.Coin.  The instances are
-- purely for the convenience of writing expressions involving
-- L.Coin but be aware that not all uses of these typeclasses
-- are valid.
deriving newtype instance Real L.Coin

deriving newtype instance Integral L.Coin

deriving newtype instance Num L.Coin

-- We wrap the individual records with Last and use Last's Semigroup instance.
-- In this instance we take the last 'Just' value or the only 'Just' value
instance Semigroup (Ledger.ShelleyPParams StrictMaybe era) where
  (<>) pp1 pp2 =
    let fsppMinFeeA = lastMappendWith Ledger.sppMinFeeA pp1 pp2
        fsppMinFeeB = lastMappendWith Ledger.sppMinFeeB pp1 pp2
        fsppMaxBBSize = lastMappendWith Ledger.sppMaxBBSize pp1 pp2
        fsppMaxTxSize = lastMappendWith Ledger.sppMaxTxSize pp1 pp2
        fsppMaxBHSize = lastMappendWith Ledger.sppMaxBHSize pp1 pp2
        fsppKeyDeposit = lastMappendWith Ledger.sppKeyDeposit pp1 pp2
        fsppPoolDeposit = lastMappendWith Ledger.sppPoolDeposit pp1 pp2
        fsppEMax = lastMappendWith Ledger.sppEMax pp1 pp2
        fsppNOpt = lastMappendWith Ledger.sppNOpt pp1 pp2
        fsppA0 = lastMappendWith Ledger.sppA0 pp1 pp2
        fsppRho = lastMappendWith Ledger.sppRho pp1 pp2
        fsppTau = lastMappendWith Ledger.sppTau pp1 pp2
        fsppD = lastMappendWith Ledger.sppD pp1 pp2
        fsppExtraEntropy = lastMappendWith Ledger.sppExtraEntropy pp1 pp2
        fsppProtocolVersion = lastMappendWith Ledger.sppProtocolVersion pp1 pp2
        fsppMinUTxOValue = lastMappendWith Ledger.sppMinUTxOValue pp1 pp2
        fsppMinPoolCost = lastMappendWith Ledger.sppMinPoolCost pp1 pp2
     in Ledger.ShelleyPParams
          { Ledger.sppMinFeeA = fsppMinFeeA
          , Ledger.sppMinFeeB = fsppMinFeeB
          , Ledger.sppMaxBBSize = fsppMaxBBSize
          , Ledger.sppMaxTxSize = fsppMaxTxSize
          , Ledger.sppMaxBHSize = fsppMaxBHSize
          , Ledger.sppKeyDeposit = fsppKeyDeposit
          , Ledger.sppPoolDeposit = fsppPoolDeposit
          , Ledger.sppEMax = fsppEMax
          , Ledger.sppNOpt = fsppNOpt
          , Ledger.sppA0 = fsppA0
          , Ledger.sppRho = fsppRho
          , Ledger.sppTau = fsppTau
          , Ledger.sppD = fsppD
          , Ledger.sppExtraEntropy = fsppExtraEntropy
          , Ledger.sppProtocolVersion = fsppProtocolVersion
          , Ledger.sppMinUTxOValue = fsppMinUTxOValue
          , Ledger.sppMinPoolCost = fsppMinPoolCost
          }

instance Semigroup (Ledger.AlonzoPParams StrictMaybe era) where
  (<>) p1 p2 =
    let fappMinFeeA = lastMappendWith Ledger.appMinFeeA p1 p2
        fappMinFeeB = lastMappendWith Ledger.appMinFeeB p1 p2
        fappMaxBBSize = lastMappendWith Ledger.appMaxBBSize p1 p2
        fappMaxTxSize = lastMappendWith Ledger.appMaxTxSize p1 p2
        fappMaxBHSize = lastMappendWith Ledger.appMaxBHSize p1 p2
        fappKeyDeposit = lastMappendWith Ledger.appKeyDeposit p1 p2
        fappPoolDeposit = lastMappendWith Ledger.appPoolDeposit p1 p2
        fappEMax = lastMappendWith Ledger.appEMax p1 p2
        fappNOpt = lastMappendWith Ledger.appNOpt p1 p2
        fappA0 = lastMappendWith Ledger.appA0 p1 p2
        fappRho = lastMappendWith Ledger.appRho p1 p2
        fappTau = lastMappendWith Ledger.appTau p1 p2
        fappD = lastMappendWith Ledger.appD p1 p2
        fappExtraEntropy = lastMappendWith Ledger.appExtraEntropy p1 p2
        fappProtocolVersion = lastMappendWith Ledger.appProtocolVersion p1 p2
        fappMinPoolCost = lastMappendWith Ledger.appMinPoolCost p1 p2
        fappCoinsPerUTxOWord = lastMappendWith Ledger.appCoinsPerUTxOWord p1 p2
        fappCostModels = lastMappendWith Ledger.appCostModels p1 p2
        fappPrices = lastMappendWith Ledger.appPrices p1 p2
        fappMaxTxExUnits = lastMappendWith Ledger.appMaxTxExUnits p1 p2
        fappMaxBlockExUnits = lastMappendWith Ledger.appMaxBlockExUnits p1 p2
        fappMaxValSize = lastMappendWith Ledger.appMaxValSize p1 p2
        fappCollateralPercentage = lastMappendWith Ledger.appCollateralPercentage p1 p2
        fappMaxCollateralInputs = lastMappendWith Ledger.appMaxCollateralInputs p1 p2
     in Ledger.AlonzoPParams
          { Ledger.appMinFeeA = fappMinFeeA
          , Ledger.appMinFeeB = fappMinFeeB
          , Ledger.appMaxBBSize = fappMaxBBSize
          , Ledger.appMaxTxSize = fappMaxTxSize
          , Ledger.appMaxBHSize = fappMaxBHSize
          , Ledger.appKeyDeposit = fappKeyDeposit
          , Ledger.appPoolDeposit = fappPoolDeposit
          , Ledger.appEMax = fappEMax
          , Ledger.appNOpt = fappNOpt
          , Ledger.appA0 = fappA0
          , Ledger.appRho = fappRho
          , Ledger.appTau = fappTau
          , Ledger.appD = fappD
          , Ledger.appExtraEntropy = fappExtraEntropy
          , Ledger.appProtocolVersion = fappProtocolVersion
          , Ledger.appMinPoolCost = fappMinPoolCost
          , Ledger.appCoinsPerUTxOWord = fappCoinsPerUTxOWord
          , Ledger.appCostModels = fappCostModels
          , Ledger.appPrices = fappPrices
          , Ledger.appMaxTxExUnits = fappMaxTxExUnits
          , Ledger.appMaxBlockExUnits = fappMaxBlockExUnits
          , Ledger.appMaxValSize = fappMaxValSize
          , Ledger.appCollateralPercentage = fappCollateralPercentage
          , Ledger.appMaxCollateralInputs = fappMaxCollateralInputs
          }

-- We're not interested in trying to mappend the underlying `Maybe` types
-- we only want to select one or the other therefore we use `Last`.
lastMappend :: StrictMaybe a -> StrictMaybe a -> StrictMaybe a
lastMappend a b = Ledger.maybeToStrictMaybe . getLast $ strictMaybeToLast a <> strictMaybeToLast b
 where
  strictMaybeToLast :: StrictMaybe a -> Last a
  strictMaybeToLast = Last . strictMaybeToMaybe

lastMappendWith :: (a -> StrictMaybe b) -> a -> a -> StrictMaybe b
lastMappendWith l = under2 l lastMappend
 where
  under2 :: (a -> c) -> (c -> c -> c) -> a -> a -> c
  under2 f g x y = g (f x) (f y)

instance Semigroup (Ledger.BabbagePParams StrictMaybe era) where
  (<>) p1 p2 =
    let fbppMinFeeA = lastMappendWith Ledger.bppMinFeeA p1 p2
        fbppMinFeeB = lastMappendWith Ledger.bppMinFeeB p1 p2
        fbppMaxBBSize = lastMappendWith Ledger.bppMaxBBSize p1 p2
        fbppMaxTxSize = lastMappendWith Ledger.bppMaxTxSize p1 p2
        fbppMaxBHSize = lastMappendWith Ledger.bppMaxBHSize p1 p2
        fbppKeyDeposit = lastMappendWith Ledger.bppKeyDeposit p1 p2
        fbppPoolDeposit = lastMappendWith Ledger.bppPoolDeposit p1 p2
        fbppEMax = lastMappendWith Ledger.bppEMax p1 p2
        fbppNOpt = lastMappendWith Ledger.bppNOpt p1 p2
        fbppA0 = lastMappendWith Ledger.bppA0 p1 p2
        fbppRho = lastMappendWith Ledger.bppRho p1 p2
        fbppTau = lastMappendWith Ledger.bppTau p1 p2
        fbppProtocolVersion = lastMappendWith Ledger.bppProtocolVersion p1 p2
        fbppMinPoolCost = lastMappendWith Ledger.bppMinPoolCost p1 p2
        fbppCoinsPerUTxOByte = lastMappendWith Ledger.bppCoinsPerUTxOByte p1 p2
        fbppCostModels = lastMappendWith Ledger.bppCostModels p1 p2
        fbppPrices = lastMappendWith Ledger.bppPrices p1 p2
        fbppMaxTxExUnits = lastMappendWith Ledger.bppMaxTxExUnits p1 p2
        fbppMaxBlockExUnits = lastMappendWith Ledger.bppMaxBlockExUnits p1 p2
        fbppMaxValSize = lastMappendWith Ledger.bppMaxValSize p1 p2
        fbppCollateralPercentage = lastMappendWith Ledger.bppCollateralPercentage p1 p2
        fbppMaxCollateralInputs = lastMappendWith Ledger.bppMaxCollateralInputs p1 p2
     in Ledger.BabbagePParams
          { Ledger.bppMinFeeA = fbppMinFeeA
          , Ledger.bppMinFeeB = fbppMinFeeB
          , Ledger.bppMaxBBSize = fbppMaxBBSize
          , Ledger.bppMaxTxSize = fbppMaxTxSize
          , Ledger.bppMaxBHSize = fbppMaxBHSize
          , Ledger.bppKeyDeposit = fbppKeyDeposit
          , Ledger.bppPoolDeposit = fbppPoolDeposit
          , Ledger.bppEMax = fbppEMax
          , Ledger.bppNOpt = fbppNOpt
          , Ledger.bppA0 = fbppA0
          , Ledger.bppRho = fbppRho
          , Ledger.bppTau = fbppTau
          , Ledger.bppProtocolVersion = fbppProtocolVersion
          , Ledger.bppMinPoolCost = fbppMinPoolCost
          , Ledger.bppCoinsPerUTxOByte = fbppCoinsPerUTxOByte
          , Ledger.bppCostModels = fbppCostModels
          , Ledger.bppPrices = fbppPrices
          , Ledger.bppMaxTxExUnits = fbppMaxTxExUnits
          , Ledger.bppMaxBlockExUnits = fbppMaxBlockExUnits
          , Ledger.bppMaxValSize = fbppMaxValSize
          , Ledger.bppCollateralPercentage = fbppCollateralPercentage
          , Ledger.bppMaxCollateralInputs = fbppMaxCollateralInputs
          }

instance Semigroup (Ledger.ConwayPParams StrictMaybe era) where
  (<>) p1 p2 =
    Ledger.ConwayPParams
      { Ledger.cppMinFeeA = lastMappendWithTHKD Ledger.cppMinFeeA p1 p2
      , Ledger.cppMinFeeB = lastMappendWithTHKD Ledger.cppMinFeeB p1 p2
      , Ledger.cppMaxBBSize = lastMappendWithTHKD Ledger.cppMaxBBSize p1 p2
      , Ledger.cppMaxTxSize = lastMappendWithTHKD Ledger.cppMaxTxSize p1 p2
      , Ledger.cppMaxBHSize = lastMappendWithTHKD Ledger.cppMaxBHSize p1 p2
      , Ledger.cppKeyDeposit = lastMappendWithTHKD Ledger.cppKeyDeposit p1 p2
      , Ledger.cppPoolDeposit = lastMappendWithTHKD Ledger.cppPoolDeposit p1 p2
      , Ledger.cppEMax = lastMappendWithTHKD Ledger.cppEMax p1 p2
      , Ledger.cppNOpt = lastMappendWithTHKD Ledger.cppNOpt p1 p2
      , Ledger.cppA0 = lastMappendWithTHKD Ledger.cppA0 p1 p2
      , Ledger.cppRho = lastMappendWithTHKD Ledger.cppRho p1 p2
      , Ledger.cppTau = lastMappendWithTHKD Ledger.cppTau p1 p2
      , Ledger.cppProtocolVersion = NoUpdate -- For conway, protocol version cannot be changed via `PParamsUpdate`
      , Ledger.cppMinPoolCost = lastMappendWithTHKD Ledger.cppMinPoolCost p1 p2
      , Ledger.cppCoinsPerUTxOByte = lastMappendWithTHKD Ledger.cppCoinsPerUTxOByte p1 p2
      , Ledger.cppCostModels = lastMappendWithTHKD Ledger.cppCostModels p1 p2
      , Ledger.cppPrices = lastMappendWithTHKD Ledger.cppPrices p1 p2
      , Ledger.cppMaxTxExUnits = lastMappendWithTHKD Ledger.cppMaxTxExUnits p1 p2
      , Ledger.cppMaxBlockExUnits = lastMappendWithTHKD Ledger.cppMaxBlockExUnits p1 p2
      , Ledger.cppMaxValSize = lastMappendWithTHKD Ledger.cppMaxValSize p1 p2
      , Ledger.cppCollateralPercentage = lastMappendWithTHKD Ledger.cppCollateralPercentage p1 p2
      , Ledger.cppMaxCollateralInputs = lastMappendWithTHKD Ledger.cppMaxCollateralInputs p1 p2
      , Ledger.cppPoolVotingThresholds = lastMappendWithTHKD Ledger.cppPoolVotingThresholds p1 p2
      , Ledger.cppDRepVotingThresholds = lastMappendWithTHKD Ledger.cppDRepVotingThresholds p1 p2
      , Ledger.cppCommitteeMinSize = lastMappendWithTHKD Ledger.cppCommitteeMinSize p1 p2
      , Ledger.cppCommitteeMaxTermLength = lastMappendWithTHKD Ledger.cppCommitteeMaxTermLength p1 p2
      , Ledger.cppGovActionLifetime = lastMappendWithTHKD Ledger.cppGovActionLifetime p1 p2
      , Ledger.cppGovActionDeposit = lastMappendWithTHKD Ledger.cppGovActionDeposit p1 p2
      , Ledger.cppDRepDeposit = lastMappendWithTHKD Ledger.cppDRepDeposit p1 p2
      , Ledger.cppDRepActivity = lastMappendWithTHKD Ledger.cppDRepActivity p1 p2
      , Ledger.cppMinFeeRefScriptCostPerByte =
          lastMappendWithTHKD Ledger.cppMinFeeRefScriptCostPerByte p1 p2
      }

lastMappendWithTHKD :: (a -> Ledger.THKD g StrictMaybe b) -> a -> a -> Ledger.THKD g StrictMaybe b
lastMappendWithTHKD f a b = Ledger.THKD $ lastMappendWith (Ledger.unTHKD . f) a b

instance Pretty Mux.Error where
  pretty err = "Mux layer error:" <+> prettyException err

-- TODO upstream to cardano-ledger
instance IsList (ListMap k a) where
  type Item (ListMap k a) = (k, a)
  fromList = ListMap.fromList
  toList = ListMap.toList

instance Error CBOR.DecoderError where
  prettyError = pshow

instance Error P.ScriptDecodeError where
  prettyError = pshow
