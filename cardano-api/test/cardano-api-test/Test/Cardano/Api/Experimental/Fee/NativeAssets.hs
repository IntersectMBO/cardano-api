{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Sub-transaction native assets.
--
-- Tokens must balance across all transactions, with any excess returned as change.
--
-- +----------------------------------------------------------+-----------------------------------+
-- | Scenario                                                 | Expected                          |
-- +==========================================================+===================================+
-- | Sub-transaction leaves tokens unspent                    | Surplus in top-level change       |
-- +----------------------------------------------------------+-----------------------------------+
-- | One sub-transaction supplies tokens requested by another | Success; no tokens left in change |
-- +----------------------------------------------------------+-----------------------------------+
-- | Combined outputs require tokens absent from inputs       | Reject despite spare ada          |
-- +----------------------------------------------------------+-----------------------------------+
--
-- Every successful funding case checks exact change, value conservation,
-- sufficient fees and byte-for-byte preservation of signed sub-transactions.
-- The constraints module also checks preservation of two distinct sub-transactions.
module Test.Cardano.Api.Experimental.Fee.NativeAssets
  ( tests
  )
where

import Cardano.Api.Ledger qualified as L

import Cardano.Ledger.Hashes qualified as L (ScriptHash (..))
import Cardano.Ledger.Mary.Value qualified as Mary

import Data.Map.Strict qualified as Map

import Test.Gen.Cardano.Api.Experimental.SubTransaction
  ( SubTransactionBalanceTestData (..)
  , SubTxAmounts (..)
  , TopLevelTxAmounts (..)
  , TxAmounts (..)
  , genAdaAmount
  , genSubTransactionBalanceTestData
  )

import Test.Cardano.Api.Experimental.Fee.Common
  ( ada
  , adaAmounts
  , assertBalancesWithExpectedChange
  , assertRejectsInsufficientFunds
  , dijkstraTestParams
  )

import Hedgehog (Property)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup
    "nested balancing: native assets"
    [ testProperty
        "sub-transaction tokens reach top-level change"
        prop_sub_transaction_tokens_reach_change
    , testProperty
        "sub-transaction supplies tokens to another"
        prop_sub_transaction_supplies_tokens_to_another
    , testProperty "combined token deficit is rejected" prop_combined_token_deficit_rejected
    ]

-- | A generated token surplus in the sub-transaction appears in top-level change.
prop_sub_transaction_tokens_reach_change :: Property
prop_sub_transaction_tokens_reach_change = H.property $ do
  topOutput <- H.forAll genAdaAmount
  subOutput <- H.forAll genAdaAmount
  reserve <- H.forAll genAdaAmount
  quantity <- H.forAll $ Gen.integral $ Range.linear 1 1_000_000
  testData <-
    H.forAllWith (show . balanceTestUTxO) $
      genSubTransactionBalanceTestData
        dijkstraTestParams
        (TopLevelTxAmounts $ adaAmounts (topOutput + reserve) topOutput)
        [SubTxAmounts $ TxAmounts (ada subOutput <> testTokens quantity) (ada subOutput)]
  assertBalancesWithExpectedChange testData

-- | One sub-transaction supplies exactly the generated token quantity requested by another.
prop_sub_transaction_supplies_tokens_to_another :: Property
prop_sub_transaction_supplies_tokens_to_another = H.property $ do
  topOutput <- H.forAll genAdaAmount
  firstOutput <- H.forAll genAdaAmount
  secondOutput <- H.forAll genAdaAmount
  reserve <- H.forAll genAdaAmount
  quantity <- H.forAll $ Gen.integral $ Range.linear 1 1_000_000
  testData <-
    H.forAllWith (show . balanceTestUTxO) $
      genSubTransactionBalanceTestData
        dijkstraTestParams
        (TopLevelTxAmounts $ adaAmounts (topOutput + reserve) topOutput)
        [ SubTxAmounts $ TxAmounts (ada firstOutput <> testTokens quantity) (ada firstOutput)
        , SubTxAmounts $ TxAmounts (ada secondOutput) (ada secondOutput <> testTokens quantity)
        ]
  assertBalancesWithExpectedChange testData

-- | Generated ada reserves cannot cover a token shortfall in the combined inputs.
prop_combined_token_deficit_rejected :: Property
prop_combined_token_deficit_rejected = H.property $ do
  topOutput <- H.forAll genAdaAmount
  subOutput <- H.forAll genAdaAmount
  reserve <- H.forAll genAdaAmount
  supplied <- H.forAll $ Gen.integral $ Range.linear 0 1_000_000
  shortfall <- H.forAll $ Gen.integral $ Range.linear 1 1_000_000
  testData <-
    H.forAllWith (show . balanceTestUTxO) $
      genSubTransactionBalanceTestData
        dijkstraTestParams
        (TopLevelTxAmounts $ adaAmounts (topOutput + reserve) topOutput)
        [ SubTxAmounts $
            TxAmounts
              (ada subOutput <> testTokens supplied)
              (ada subOutput <> testTokens (supplied + shortfall))
        ]
  assertRejectsInsufficientFunds testData

testTokens :: Integer -> L.MaryValue
testTokens quantity =
  L.MaryValue mempty $
    L.MultiAsset $
      Map.singleton
        (L.PolicyID $ L.ScriptHash "1c14ee8e58fbcbd48dc7367c95a63fd1d937ba989820015db16ac7e5")
        (Map.singleton (Mary.AssetName "nested") quantity)
