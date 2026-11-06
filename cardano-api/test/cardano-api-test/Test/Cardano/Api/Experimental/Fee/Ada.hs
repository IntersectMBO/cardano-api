-- | Sub-transaction ADA movement.
--
-- Balance the top-level transaction and its signed sub-transactions together.
-- Fees and change belong to the top-level transaction; signed sub-transaction
-- bodies, witnesses, budgets and order must remain unchanged.
--
-- These illustrative amounts are inputs minus requested outputs, before fees.
-- Each property generates amounts that preserve its row's balance relationships.
-- Requested outputs and fee/change reserves are at least five ada.
--
-- +-----------------------------------------+-----------+------------------+------------------------------+
-- | Scenario                                | Top-level | Sub-transactions | Expected                     |
-- +=========================================+===========+==================+==============================+
-- | Sub-transaction balances itself         | +10 ADA   | 0 ADA            | Success; top-level pays fees |
-- +-----------------------------------------+-----------+------------------+------------------------------+
-- | Sub-transaction funds top-level outputs | -10 ADA   | +30 ADA          | Change = 20 ADA minus fees   |
-- +-----------------------------------------+-----------+------------------+------------------------------+
-- | Top-level funds sub-transaction outputs | +30 ADA   | -10 ADA          | Change = 20 ADA minus fees   |
-- +-----------------------------------------+-----------+------------------+------------------------------+
-- | One sub-transaction funds another       | +10 ADA   | +20 ADA, -20 ADA | Change = 10 ADA minus fees   |
-- +-----------------------------------------+-----------+------------------+------------------------------+
-- | Combined funds are insufficient         | +10 ADA   | -20 ADA          | Reject                       |
-- +-----------------------------------------+-----------+------------------+------------------------------+
-- | Nothing remains for fees                | +10 ADA   | -10 ADA          | Reject                       |
-- +-----------------------------------------+-----------+------------------+------------------------------+
module Test.Cardano.Api.Experimental.Fee.Ada
  ( tests
  )
where

import Test.Gen.Cardano.Api.Experimental.SubTransaction
  ( SubTransactionBalanceTestData (..)
  , SubTxAmounts (..)
  , TopLevelTxAmounts (..)
  , genAdaAmount
  , genSubTransactionBalanceTestData
  )

import Test.Cardano.Api.Experimental.Fee.Common
  ( adaAmounts
  , assertBalancesWithExpectedChange
  , assertRejectsInsufficientFunds
  , dijkstraTestParams
  )

import Hedgehog (Property)
import Hedgehog qualified as H
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup
    "nested balancing: ada"
    [ testProperty "sub-transaction balances itself" prop_sub_transaction_balances_itself
    , testProperty "sub-transaction funds top-level outputs" prop_sub_transaction_funds_top_level_outputs
    , testProperty "top-level funds sub-transaction outputs" prop_top_level_funds_sub_transaction_outputs
    , testProperty "one sub-transaction funds another" prop_sub_transaction_funds_another
    , testProperty "combined ada deficit is rejected" prop_combined_ada_deficit_rejected
    , testProperty "no funds remain for fees" prop_no_funds_for_fees_rejected
    ]

-- | A balanced sub-transaction leaves a generated top-level reserve for fees and change.
prop_sub_transaction_balances_itself :: Property
prop_sub_transaction_balances_itself = H.property $ do
  topOutput <- H.forAll genAdaAmount
  subOutput <- H.forAll genAdaAmount
  reserve <- H.forAll genAdaAmount
  testData <-
    H.forAllWith (show . balanceTestUTxO) $
      genSubTransactionBalanceTestData
        dijkstraTestParams
        (TopLevelTxAmounts $ adaAmounts (topOutput + reserve) topOutput)
        [SubTxAmounts $ adaAmounts subOutput subOutput]
  assertBalancesWithExpectedChange testData

-- | The sub-transaction covers a generated top-level deficit and leaves a reserve for fees and change.
prop_sub_transaction_funds_top_level_outputs :: Property
prop_sub_transaction_funds_top_level_outputs = H.property $ do
  topInput <- H.forAll genAdaAmount
  subOutput <- H.forAll genAdaAmount
  deficit <- H.forAll genAdaAmount
  reserve <- H.forAll genAdaAmount
  testData <-
    H.forAllWith (show . balanceTestUTxO) $
      genSubTransactionBalanceTestData
        dijkstraTestParams
        (TopLevelTxAmounts $ adaAmounts topInput (topInput + deficit))
        [SubTxAmounts $ adaAmounts (subOutput + deficit + reserve) subOutput]
  assertBalancesWithExpectedChange testData

-- | The top-level transaction covers a generated sub-transaction deficit and leaves a fee/change reserve.
prop_top_level_funds_sub_transaction_outputs :: Property
prop_top_level_funds_sub_transaction_outputs = H.property $ do
  topOutput <- H.forAll genAdaAmount
  subInput <- H.forAll genAdaAmount
  deficit <- H.forAll genAdaAmount
  reserve <- H.forAll genAdaAmount
  testData <-
    H.forAllWith (show . balanceTestUTxO) $
      genSubTransactionBalanceTestData
        dijkstraTestParams
        (TopLevelTxAmounts $ adaAmounts (topOutput + deficit + reserve) topOutput)
        [SubTxAmounts $ adaAmounts subInput (subInput + deficit)]
  assertBalancesWithExpectedChange testData

-- | One sub-transaction supplies exactly the generated deficit of another; the top-level pays fees.
prop_sub_transaction_funds_another :: Property
prop_sub_transaction_funds_another = H.property $ do
  topOutput <- H.forAll genAdaAmount
  firstOutput <- H.forAll genAdaAmount
  secondInput <- H.forAll genAdaAmount
  transfer <- H.forAll genAdaAmount
  reserve <- H.forAll genAdaAmount
  testData <-
    H.forAllWith (show . balanceTestUTxO) $
      genSubTransactionBalanceTestData
        dijkstraTestParams
        (TopLevelTxAmounts $ adaAmounts (topOutput + reserve) topOutput)
        [ SubTxAmounts $ adaAmounts (firstOutput + transfer) firstOutput
        , SubTxAmounts $ adaAmounts secondInput (secondInput + transfer)
        ]
  assertBalancesWithExpectedChange testData

-- | The sub-transaction deficit exceeds the generated top-level surplus, so balancing fails.
prop_combined_ada_deficit_rejected :: Property
prop_combined_ada_deficit_rejected = H.property $ do
  topOutput <- H.forAll genAdaAmount
  subInput <- H.forAll genAdaAmount
  surplus <- H.forAll genAdaAmount
  shortfall <- H.forAll genAdaAmount
  testData <-
    H.forAllWith (show . balanceTestUTxO) $
      genSubTransactionBalanceTestData
        dijkstraTestParams
        (TopLevelTxAmounts $ adaAmounts (topOutput + surplus) topOutput)
        [SubTxAmounts $ adaAmounts subInput (subInput + surplus + shortfall)]
  assertRejectsInsufficientFunds testData

-- | Combined inputs cover generated requested outputs exactly, leaving nothing for fees.
prop_no_funds_for_fees_rejected :: Property
prop_no_funds_for_fees_rejected = H.property $ do
  topOutput <- H.forAll genAdaAmount
  subInput <- H.forAll genAdaAmount
  transfer <- H.forAll genAdaAmount
  testData <-
    H.forAllWith (show . balanceTestUTxO) $
      genSubTransactionBalanceTestData
        dijkstraTestParams
        (TopLevelTxAmounts $ adaAmounts (topOutput + transfer) topOutput)
        [SubTxAmounts $ adaAmounts subInput (subInput + transfer)]
  assertRejectsInsufficientFunds testData
