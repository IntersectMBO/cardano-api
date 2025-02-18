{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Gen.Cardano.Api
  ( genMetadata
  , genAlonzoGenesis
  )
where

import Cardano.Ledger.Alonzo.Core qualified as Ledger
import Cardano.Ledger.Alonzo.Genesis qualified as Alonzo
import Cardano.Ledger.Alonzo.Scripts qualified as Alonzo
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Coin qualified as Ledger
import Cardano.Ledger.Plutus.CostModels qualified as Plutus
import Cardano.Ledger.Plutus.Language qualified as Alonzo
import Cardano.Ledger.Shelley.TxAuxData (Metadatum (..), ShelleyTxAuxData (..))

import Data.Map.Strict qualified as Map
import Data.Word (Word64)
import GHC.Exts (IsList (..))

import Test.Gen.Cardano.Api.Typed (genCostModel, genRational)

import Hedgehog (Gen, Range)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Range qualified as Range

genMetadata :: Ledger.Era era => Gen (ShelleyTxAuxData era)
genMetadata = do
  numberOfIndices <- Gen.integral (Range.linear 1 15)
  let indices = map (\i -> fromIntegral i :: Word64) [1 .. numberOfIndices]
  mData <- Gen.list (Range.singleton numberOfIndices) genMetadatum
  return . ShelleyTxAuxData . fromList $ zip indices mData

genMetadatum :: Gen Metadatum
genMetadatum = do
  int <- Gen.list (Range.linear 1 5) (I <$> Gen.integral (Range.linear 1 100))
  bytes <- Gen.list (Range.linear 1 5) (B <$> Gen.bytes (Range.linear 1 20))
  str <- Gen.list (Range.linear 1 5) (S <$> Gen.text (Range.linear 1 20) Gen.alphaNum)
  let mDatumList = int ++ bytes ++ str

  singleMetadatum <- Gen.element mDatumList

  Gen.element
    [ List mDatumList
    , Map [(singleMetadatum, singleMetadatum)]
    , Map [(List mDatumList, singleMetadatum)]
    , Map [(singleMetadatum, List mDatumList)]
    ]

genCoin :: Range Integer -> Gen Ledger.Coin
genCoin r = do
  unCoin' <- Gen.integral r
  return $ Ledger.Coin unCoin'

genPrice :: Gen Ledger.NonNegativeInterval
genPrice = do
  unPrice <- genRational
  case Ledger.boundRational unPrice of
    Nothing -> fail "genPrice: genRational should give us a bounded rational"
    Just p -> pure p

genPrices :: Gen Alonzo.Prices
genPrices = do
  prMem' <- genPrice
  prSteps' <- genPrice

  return
    Alonzo.Prices
      { Alonzo.prMem = prMem'
      , Alonzo.prSteps = prSteps'
      }

genExUnits :: Gen Alonzo.ExUnits
genExUnits = do
  exUnitsMem' <- Gen.integral (Range.linear 0 10)
  exUnitsSteps' <- Gen.integral (Range.linear 0 10)
  return
    Alonzo.ExUnits
      { Alonzo.exUnitsMem = exUnitsMem'
      , Alonzo.exUnitsSteps = exUnitsSteps'
      }

genCostModels :: Gen Alonzo.CostModels
genCostModels = do
  alonzoCostModel <- genCostModel
  Plutus.mkCostModels . conv <$> Gen.list (Range.linear 1 3) (return alonzoCostModel)
 where
  conv :: [Alonzo.CostModel] -> Map.Map Alonzo.Language Alonzo.CostModel
  conv [] = mempty
  conv (c : rest) = Map.singleton (Alonzo.getCostModelLanguage c) c <> conv rest

genAlonzoGenesis :: Gen Alonzo.AlonzoGenesis
genAlonzoGenesis = do
  coinsPerUTxOWord <- genCoin (Range.linear 0 5)
  -- TODO: Babbage: Figure out how to deal with the asymmetric cost model JSON
  _costmdls' <- genCostModels
  prices' <- genPrices
  maxTxExUnits' <- genExUnits
  maxBlockExUnits' <- genExUnits
  maxValSize' <- Gen.integral (Range.linear 0 10)
  collateralPercentage' <- Gen.integral (Range.linear 0 10)
  maxCollateralInputs' <- Gen.integral (Range.linear 0 10)

  return
    Alonzo.AlonzoGenesis
      { Alonzo.agCoinsPerUTxOWord = Ledger.CoinPerWord coinsPerUTxOWord
      , Alonzo.agCostModels = mempty
      , Alonzo.agPrices = prices'
      , Alonzo.agMaxTxExUnits = maxTxExUnits'
      , Alonzo.agMaxBlockExUnits = maxBlockExUnits'
      , Alonzo.agMaxValSize = maxValSize'
      , Alonzo.agCollateralPercentage = collateralPercentage'
      , Alonzo.agMaxCollateralInputs = maxCollateralInputs'
      }
