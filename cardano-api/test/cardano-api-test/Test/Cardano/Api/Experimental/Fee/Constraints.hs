{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Additional constraints on sub-transaction balancing.
--
-- Each property isolates one constraint on an otherwise funded transaction.
--
-- +--------------------------------------+---------------------------------------------------------------------------------------+
-- | Area                                 | Scenario and expected result                                                          |
-- +======================================+=======================================================================================+
-- | Signed script budgets                | Sufficient budget succeeds unchanged; zero budget is rejected for ScriptValid         |
-- +--------------------------------------+---------------------------------------------------------------------------------------+
-- | ScriptInvalid transactions           | Accept failures at either or both levels; reject when every script passes             |
-- +--------------------------------------+---------------------------------------------------------------------------------------+
-- | Combined execution limit             | Sub-transaction fits alone; adding top-level execution exceeds memory or steps        |
-- +--------------------------------------+---------------------------------------------------------------------------------------+
-- | Execution fees                       | The fee includes the signed sub-transaction execution budget                          |
-- +--------------------------------------+---------------------------------------------------------------------------------------+
-- | Reference-script fees                | Auto-balancing, evaluation and recursive balancing include sub-transaction references |
-- +--------------------------------------+---------------------------------------------------------------------------------------+
-- | Reference fee encoding               | A reference charge that widens the fee also pays for the extra bytes                  |
-- +--------------------------------------+---------------------------------------------------------------------------------------+
-- | Collateral                           | Sub-transaction script uses top-level collateral; insufficient collateral is rejected |
-- +--------------------------------------+---------------------------------------------------------------------------------------+
-- | Minimum ada                          | Reject a sub-transaction output below minimum ada despite spare funds                 |
-- +--------------------------------------+---------------------------------------------------------------------------------------+
-- | Legacy Plutus at top level           | Accept independent balance; reject reliance on sub-transaction surplus                |
-- +--------------------------------------+---------------------------------------------------------------------------------------+
-- | Pool registration with legacy Plutus | Sub-transaction pays the deposit; top-level re-registration pays no second deposit    |
-- +--------------------------------------+---------------------------------------------------------------------------------------+
-- | Legacy Plutus in a sub-transaction   | Reject even with a sufficient execution budget                                        |
-- +--------------------------------------+---------------------------------------------------------------------------------------+
-- | Required inputs                      | Report the exact missing sub-transaction input                                        |
-- +--------------------------------------+---------------------------------------------------------------------------------------+
--
-- Input and certificate collection, and V4 script-context evaluation, have
-- separate test groups. Guard contexts are checked with direct and reference scripts.
module Test.Cardano.Api.Experimental.Fee.Constraints
  ( tests
  )
where

import Cardano.Api qualified as Api
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Experimental.Tx qualified as Exp
import Cardano.Api.Ledger qualified as L

import Cardano.Ledger.Alonzo.Core qualified as L (ppCollateralPercentageL, ppMaxTxExUnitsL)
import Cardano.Ledger.Alonzo.Plutus.Context qualified as L (CollectError (..))
import Cardano.Ledger.Alonzo.TxWits qualified as Alonzo
import Cardano.Ledger.Coin qualified as L
import Cardano.Ledger.Conway.PParams qualified as L (ppMinFeeRefScriptCostPerByteL)
import Cardano.Ledger.Conway.UTxO (txNonDistinctRefScriptsSize)
import Cardano.Ledger.Core qualified as L
import Cardano.Ledger.Dijkstra.TxBody qualified as Dijkstra
import Cardano.Ledger.Dijkstra.TxCert qualified as L (DijkstraDelegCert (..))
import Cardano.Ledger.Plutus.Evaluate qualified as Plutus
import Cardano.Ledger.Tools qualified as L (setMinFeeTx)

import Control.Monad (forM_)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import GHC.Exts (IsList (..))
import Lens.Micro

import Test.Gen.Cardano.Api.Experimental (genSignedSubTransaction)
import Test.Gen.Cardano.Api.Typed (genSigningKey, genStakeCredential)

import Test.Cardano.Api.Experimental.Fee.Common
  ( DijkstraBalanceResult
  , adaAmounts
  , assertChangeFeeAndUnchangedSubTransactions
  , autoBalanceTestTransaction
  , autoBalanceWithParams
  , balanceTransactionWithSubTransactions
  , dijkstraTestParams
  , testEpochInfo
  , testSystemStart
  )
import Test.Cardano.Api.Experimental.Fee.Gen.Guard
  ( GuardScriptSource (..)
  , PlutusV4ReferenceScriptGuardTestInputs (..)
  , ScriptBalanceCase (..)
  , alwaysSucceedsGuardScript
  , genFailingTopLevelGuardCase
  , genLegacyPoolRegistrationBalanceTestData
  , genLegacyTopLevelBalanceTestData
  , genLegacyWithdrawal
  , genPlutusV4ReferenceScriptGuardTestInputs
  , genScriptBalanceCase
  , genTopLevelGuardCase
  )
import Test.Cardano.Api.Experimental.Fee.Gen.SubTransaction
  ( SubTransactionBalanceTestData (..)
  , SubTxAmounts (..)
  , TopLevelTxAmounts (..)
  , TxAmounts (..)
  , genSubTransactionBalanceTestData
  )

import Hedgehog (Property, (===))
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup
    "nested balancing: constraints"
    [ testGroup
        "balancing"
        [ testProperty
            "signed bodies, witnesses and order are preserved"
            prop_signed_sub_transactions_preserved
        , testProperty
            "sub-transaction output below minimum ada is rejected"
            prop_sub_transaction_output_below_minimum_ada_rejected
        , testProperty
            "signed script budget contributes to fee"
            prop_sub_transaction_budget_contributes_to_fee
        , testProperty
            "sub-transaction reference script contributes to fee"
            prop_sub_transaction_reference_script_contributes_to_fee
        , testProperty
            "evaluation includes sub-transaction reference fees"
            prop_evaluation_includes_sub_transaction_reference_fees
        , testProperty
            "recursive balancing retains sub-transaction reference fees"
            prop_recursive_balancing_retains_sub_transaction_reference_fees
        , testProperty
            "sub-transaction reference fees cross a fee encoding boundary"
            prop_sub_transaction_reference_fee_encoding_boundary
        , testProperty
            "sub-transaction uses top-level collateral"
            prop_sub_transaction_uses_top_level_collateral
        , testProperty
            "insufficient top-level collateral is rejected"
            prop_sub_transaction_insufficient_collateral_rejected
        , testProperty
            "insufficient signed script budget is rejected"
            prop_insufficient_sub_transaction_budget_rejected
        , testProperty
            "script failures identify each sub-transaction"
            prop_script_failures_identify_sub_transactions
        , testProperty
            "ScriptInvalid accepts a sub-transaction failure"
            prop_script_invalid_accepts_sub_transaction_failure
        , testProperty
            "ScriptInvalid accepts a sub-transaction failure with a passing top-level guard"
            prop_script_invalid_accepts_sub_failure_with_passing_top_guard
        , testProperty
            "ScriptInvalid accepts a top-level failure"
            prop_script_invalid_accepts_top_level_failure
        , testProperty
            "ScriptInvalid accepts failures at both levels"
            prop_script_invalid_accepts_failures_at_both_levels
        , testProperty
            "ScriptInvalid rejects entirely passing scripts"
            prop_script_invalid_rejects_passing_scripts
        , testProperty
            "legacy top-level re-registers a pool from a sub-transaction"
            prop_legacy_top_level_reregisters_sub_transaction_pool
        , testProperty
            "sub-transaction stake deregistration refund contributes to change"
            prop_sub_transaction_deregistration_refund
        , testProperty
            "combined execution budget accepts the exact limit"
            prop_combined_execution_budget_boundary
        , testProperty "combined execution budget exceeds the limit" prop_combined_execution_budget_rejected
        , testProperty "legacy top-level balances independently" prop_legacy_top_level_balances_independently
        , testProperty
            "legacy top-level cannot use sub-transaction surplus"
            prop_legacy_top_level_cannot_use_sub_transaction_surplus
        , testProperty
            "legacy Plutus in a sub-transaction is rejected"
            prop_legacy_plutus_in_sub_transaction_rejected
        ]
    , testGroup
        "inputs and certificates"
        [ testProperty "queries include all transaction inputs" prop_queries_include_sub_transaction_inputs
        , testProperty
            "queries include sub-transaction references and top-level collateral"
            prop_queries_include_sub_transaction_references
        , testProperty "missing sub-transaction input is reported" prop_missing_sub_transaction_input_reported
        , testProperty
            "sub-transactions cannot spend the same input"
            prop_duplicate_sub_transaction_spending_inputs
        , testProperty
            "top-level and sub-transaction cannot spend the same input"
            prop_duplicate_top_level_spending_inputs
        , testProperty "transactions can share reference inputs" prop_shared_reference_inputs
        , testProperty
            "queries include sub-transaction certificates"
            prop_queries_include_sub_transaction_certificates
        ]
    , testGroup
        "script contexts"
        [ testProperty
            "top-level guard evaluates with sub-transactions"
            prop_top_level_guard_evaluates_with_sub_transactions
        , testProperty "unmatched guard redeemer is reported" prop_unmatched_guard_redeemer_reported
        , testProperty
            "missing sub-transaction input fails guard evaluation"
            prop_missing_sub_transaction_input_prevents_guard_context
        ]
    ]

-- | Two distinct signed sub-transactions retain their bodies, witnesses and order.
prop_signed_sub_transactions_preserved :: Property
prop_signed_sub_transactions_preserved = H.withTests 10 $ H.property $ do
  testData <-
    H.forAllWith (show . balanceTestUTxO) $
      genSubTransactionBalanceTestData
        dijkstraTestParams
        (TopLevelTxAmounts $ adaAmounts 20 10)
        [SubTxAmounts $ adaAmounts 30 10, SubTxAmounts $ adaAmounts 10 30]
  let signed = reverse $ balanceTestSignedSubTransactions testData
      reversed =
        testData
          { balanceTestSignedSubTransactions = signed
          , balanceTestTxBodyContent = balanceTestTxBodyContent testData & Exp.setTxSignedSubTransactions signed
          }
  (Exp.UnsignedTx tx, _) <- H.evalEither $ balanceTransactionWithSubTransactions reversed
  assertChangeFeeAndUnchangedSubTransactions reversed tx

-- | Reject a one-lovelace sub-transaction output even though the transaction has spare ada.
prop_sub_transaction_output_below_minimum_ada_rejected :: Property
prop_sub_transaction_output_below_minimum_ada_rejected = H.withTests 10 $ H.property $ do
  testData <-
    H.forAllWith (show . balanceTestUTxO) $
      genSubTransactionBalanceTestData
        dijkstraTestParams
        (TopLevelTxAmounts $ adaAmounts 20 10)
        [SubTxAmounts $ TxAmounts (Api.ada 10) (L.MaryValue (L.Coin 1) mempty)]
  case balanceTransactionWithSubTransactions testData of
    Left (Exp.TxBodyErrorMinUTxONotMet (Exp.TxOut out) minimumAda) -> do
      out ^. L.coinTxOutL === L.Coin 1
      H.assertWith minimumAda (> L.Coin 1)
    other -> H.annotateShow (fmap fst other) >> H.failure

-- | Legacy Plutus succeeds when the sub-transaction balances independently.
prop_legacy_top_level_balances_independently :: Property
prop_legacy_top_level_balances_independently = H.withTests 10 $ H.property $ do
  testData <-
    H.forAllWith (show . balanceTestUTxO) $
      genLegacyTopLevelBalanceTestData
        dijkstraTestParams
        (TopLevelTxAmounts $ adaAmounts 20 10)
        [SubTxAmounts $ adaAmounts 10 10]
  (Exp.UnsignedTx tx, _) <- H.evalEither $ balanceTransactionWithSubTransactions testData
  assertChangeFeeAndUnchangedSubTransactions testData tx

-- | Legacy Plutus rejects a top-level deficit even when a sub-transaction can cover it.
prop_legacy_top_level_cannot_use_sub_transaction_surplus :: Property
prop_legacy_top_level_cannot_use_sub_transaction_surplus = H.withTests 10 $ H.property $ do
  testData <-
    H.forAllWith (show . balanceTestUTxO) $
      genLegacyTopLevelBalanceTestData
        dijkstraTestParams
        (TopLevelTxAmounts $ adaAmounts 10 20)
        [SubTxAmounts $ adaAmounts 40 10]
  case balanceTransactionWithSubTransactions testData of
    Left (Exp.TxBodyErrorLegacyTopLevelBalance balance) -> balance === Api.ada (-30)
    other -> H.annotateShow (fmap fst other) >> H.failure

-- | The sub-transaction pays the pool deposit; legacy top-level re-registration owes none.
prop_legacy_top_level_reregisters_sub_transaction_pool :: Property
prop_legacy_top_level_reregisters_sub_transaction_pool = H.withTests 10 $ H.property $ do
  let params = dijkstraTestParams & L.ppPoolDepositL .~ L.Coin 500_000_000
  testData <- H.forAllWith (show . balanceTestUTxO) $ genLegacyPoolRegistrationBalanceTestData params
  (Exp.UnsignedTx tx, _) <-
    H.evalEither $
      autoBalanceWithParams
        params
        (balanceTestAddress testData)
        (balanceTestUTxO testData)
        (balanceTestTxBodyContent testData)
  -- Use the same pool deposit when checking the aggregate balance.
  let evaluation =
        Exp.evaluateTransaction
          testSystemStart
          testEpochInfo
          params
          mempty
          mempty
          (balanceTestUTxO testData)
          tx
  Exp.txEvalBalance evaluation === mempty
  map
    (Api.serialiseToCBOR . Exp.SignedSubTx)
    (toList $ tx ^. L.bodyTxL . Dijkstra.subTransactionsTxBodyL)
    === map Api.serialiseToCBOR (balanceTestSignedSubTransactions testData)
  let fee = tx ^. L.bodyTxL . L.feeTxBodyL
  sum (map (^. L.coinTxOutL) $ toList $ tx ^. L.bodyTxL . L.outputsTxBodyL) <> fee
    === L.Coin 20_000_000

-- | A sub-transaction refunds 2 ada: 20 ada in minus 10 ada out leaves 12 ada minus the fee.
prop_sub_transaction_deregistration_refund :: Property
prop_sub_transaction_deregistration_refund = H.withTests 10 $ H.property $ do
  testData <-
    H.forAllWith (show . balanceTestUTxO) $
      genSubTransactionBalanceTestData
        dijkstraTestParams
        (TopLevelTxAmounts $ adaAmounts 20 10)
        []
  stakeKey <- H.forAll $ genSigningKey Api.AsStakeKey
  let credential = Api.StakeCredentialByKey $ Api.verificationKeyHash $ Api.getVerificationKey stakeKey
      certificate =
        Exp.Certificate $
          L.DijkstraTxCertDeleg $
            L.DijkstraUnRegCert (Api.toShelleyStakeCredential credential) (L.Coin 2_000_000)
  unsigned <-
    H.evalEither $
      Exp.makeUnsignedSubTx $
        Exp.defaultSubTxBodyContent
          & Exp.setTxCertificates
            (Exp.mkTxCertificates Exp.DijkstraEra [(certificate, Exp.AnyKeyWitnessPlaceholder)])
  let signed = Exp.signSubTx [] [Exp.makeSubTxKeyWitness unsigned $ Api.WitnessStakeKey stakeKey] unsigned
      withRefund =
        testData
          { balanceTestTxBodyContent =
              balanceTestTxBodyContent testData & Exp.setTxSignedSubTransactions [signed]
          , balanceTestSignedSubTransactions = [signed]
          , balanceTestSurplus = Api.ada 12
          }
  -- Dijkstra carries the refund in the certificate, so no deposit lookup is needed.
  (Exp.UnsignedTx tx, _) <- H.evalEither $ balanceTransactionWithSubTransactions withRefund
  assertChangeFeeAndUnchangedSubTransactions withRefund tx

-- | Input queries include the top-level input and both sub-transaction inputs.
prop_queries_include_sub_transaction_inputs :: Property
prop_queries_include_sub_transaction_inputs = H.withTests 10 $ H.property $ do
  testData <-
    H.forAllWith (show . balanceTestUTxO) $
      genSubTransactionBalanceTestData
        dijkstraTestParams
        (TopLevelTxAmounts $ adaAmounts 20 10)
        [SubTxAmounts $ adaAmounts 30 10, SubTxAmounts $ adaAmounts 10 30]
  unsigned <- H.evalEither $ Exp.makeUnsignedTx Exp.DijkstraEra (balanceTestTxBodyContent testData)
  Set.fromList (Exp.getAllTxInputs unsigned)
    === Set.fromList (balanceTestTopLevelInput testData : balanceTestSubTxInputs testData)

-- | Query the sub-transaction spending and reference inputs plus top-level collateral.
prop_queries_include_sub_transaction_references :: Property
prop_queries_include_sub_transaction_references = H.withTests 10 $ H.property $ do
  example <-
    H.forAllWith (show . scriptInputs) $
      genScriptBalanceCase dijkstraTestParams ReferenceScript sufficientGuardBudget
  unsigned <- H.evalEither $ Exp.makeUnsignedTx Exp.DijkstraEra (scriptTxContent example)
  let inputs = scriptInputs example
  Set.fromList (Exp.getAllTxInputs unsigned)
    === Set.fromList
      [guardFundingInput inputs, guardReferenceInput inputs, guardCollateralInput inputs]

-- | Removing a sub-transaction input reports exactly that missing input.
prop_missing_sub_transaction_input_reported :: Property
prop_missing_sub_transaction_input_reported = H.withTests 10 $ H.property $ do
  testData <-
    H.forAllWith (show . balanceTestUTxO) $
      genSubTransactionBalanceTestData
        dijkstraTestParams
        (TopLevelTxAmounts $ adaAmounts 20 10)
        [SubTxAmounts $ adaAmounts 10 10]
  missing <- H.evalMaybe $ List.uncons (balanceTestSubTxInputs testData)
  let input = fst missing
      utxo = L.UTxO $ Map.delete (Api.toShelleyTxIn input) $ L.unUTxO $ balanceTestUTxO testData
  case balanceTransactionWithSubTransactions testData{balanceTestUTxO = utxo} of
    Left (Exp.TxBodyErrorMissingInputs inputs) -> inputs === Set.singleton input
    other -> H.annotateShow (fmap fst other) >> H.failure

-- | Include a sub-transaction deregistration when querying certificate deposits.
prop_queries_include_sub_transaction_certificates :: Property
prop_queries_include_sub_transaction_certificates = H.property $ do
  credential <- H.forAll genStakeCredential
  let certificate =
        Exp.Certificate $
          L.DijkstraTxCertDeleg $
            L.DijkstraUnRegCert (Api.toShelleyStakeCredential credential) (L.Coin 2_000_000)
  unsigned <-
    H.evalEither $
      Exp.makeUnsignedSubTx $
        Exp.defaultSubTxBodyContent
          & Exp.setTxCertificates
            (Exp.mkTxCertificates Exp.DijkstraEra [(certificate, Exp.AnyKeyWitnessPlaceholder)])
  tx <-
    H.evalEither $
      Exp.makeUnsignedTx Exp.DijkstraEra $
        Exp.defaultTxBodyContent & Exp.setTxSignedSubTransactions [Exp.signSubTx [] [] unsigned]
  let certificates = Exp.getAllTxCertificates tx
  -- Include the sub-transaction's deregistration so its deposit refund can be queried.
  certificates === [certificate]

-- | A sufficient signed budget is preserved and its execution cost is charged.
prop_sub_transaction_budget_contributes_to_fee :: Property
prop_sub_transaction_budget_contributes_to_fee = H.withTests 10 $ H.property $ do
  example <-
    H.forAllWith (show . scriptInputs) $
      genScriptBalanceCase dijkstraTestParams ScriptWitness sufficientGuardBudget
  (Exp.UnsignedTx tx, _) <- H.evalEither $ balanceScriptCase dijkstraTestParams example
  assertSignedSubTransactionBytesUnchanged example tx
  let fee = tx ^. L.bodyTxL . L.feeTxBodyL
      withoutBudget =
        tx
          & L.bodyTxL . Dijkstra.subTransactionsTxBodyL
            %~ (fromList . map (L.witsTxL . L.rdmrsTxWitsL .~ Alonzo.Redeemers mempty) . toList)
      minimumFee = L.getMinFeeTx dijkstraTestParams tx 0
  H.assertWith (minimumFee, L.getMinFeeTx dijkstraTestParams withoutBudget 0) (uncurry (>))
  H.assertWith (fee, minimumFee) (uncurry (>=))

-- | Charge for a reference script used only by a signed sub-transaction.
prop_sub_transaction_reference_script_contributes_to_fee :: Property
prop_sub_transaction_reference_script_contributes_to_fee = H.withTests 10 $ H.property $ do
  example <-
    H.forAllWith (show . scriptInputs) $
      genScriptBalanceCase dijkstraTestParams ReferenceScript sufficientGuardBudget
  -- Use a high price so omitted reference fees cannot hide in fee padding.
  price <- H.evalMaybe $ L.boundRational 10_000
  let params = dijkstraTestParams & L.ppMinFeeRefScriptCostPerByteL .~ price
  (Exp.UnsignedTx tx, _) <- H.evalEither $ balanceScriptCase params example
  assertSignedSubTransactionBytesUnchanged example tx
  let Exp.SignedSubTx sub = signedSubTransaction example
      utxo = guardUTxO $ scriptInputs example
      referenceSize = txNonDistinctRefScriptsSize utxo sub
      minimumFee = L.getMinFeeTx params tx referenceSize
  txNonDistinctRefScriptsSize utxo tx === 0
  H.assertWith referenceSize (> 0)
  H.assertWith (minimumFee, L.getMinFeeTx params tx 0) (uncurry (>))
  H.assertWith (tx ^. L.bodyTxL . L.feeTxBodyL, minimumFee) (uncurry (>=))

-- | Evaluation charges for a reference script used only by the signed sub-transaction.
prop_evaluation_includes_sub_transaction_reference_fees :: Property
prop_evaluation_includes_sub_transaction_reference_fees = H.withTests 10 $ H.property $ do
  price <- H.evalMaybe $ L.boundRational 10_000
  let params = dijkstraTestParams & L.ppMinFeeRefScriptCostPerByteL .~ price
  example <-
    H.forAllWith (show . scriptInputs) $
      genScriptBalanceCase params ReferenceScript sufficientGuardBudget
  Exp.UnsignedTx tx <- H.evalEither $ Exp.makeUnsignedTx Exp.DijkstraEra (scriptTxContent example)
  let utxo = guardUTxO $ scriptInputs example
      Exp.SignedSubTx sub = signedSubTransaction example
      referenceSize = txNonDistinctRefScriptsSize utxo sub
      expectedFee = L.setMinFeeTx params tx referenceSize ^. L.bodyTxL . L.feeTxBodyL
      evaluation = Exp.evaluateTransaction testSystemStart testEpochInfo params mempty mempty utxo tx
  H.assertWith referenceSize (> 0)
  Exp.txEvalFee evaluation === expectedFee

-- | Recalculating a balanced top-level transaction's fee retains the sub-transaction reference charge and its signed bytes.
prop_recursive_balancing_retains_sub_transaction_reference_fees :: Property
prop_recursive_balancing_retains_sub_transaction_reference_fees = H.withTests 10 $ H.property $ do
  price <- H.evalMaybe $ L.boundRational 10_000
  let params = dijkstraTestParams & L.ppMinFeeRefScriptCostPerByteL .~ price
  example <-
    H.forAllWith (show . scriptInputs) $
      genScriptBalanceCase params ReferenceScript sufficientGuardBudget
  (unsigned, _) <- H.evalEither $ balanceScriptCase params example
  let inputs = scriptInputs example
      utxo = guardUTxO inputs
      Exp.SignedSubTx sub = signedSubTransaction example
      referenceSize = txNonDistinctRefScriptsSize utxo sub
      recalculate tx =
        Exp.calcMinFeeRecursive
          (Api.toShelleyAddr $ guardTestOutputAddress inputs)
          tx
          utxo
          params
          mempty
          mempty
          0
  balanced@(Exp.UnsignedTx tx) <- H.evalEither $ recalculate unsigned
  H.assertWith referenceSize (> 0)
  H.assertWith (tx ^. L.bodyTxL . L.feeTxBodyL, L.getMinFeeTx params tx referenceSize) (uncurry (>=))
  Exp.txEvalBalance
    (Exp.evaluateTransaction testSystemStart testEpochInfo params mempty mempty utxo tx)
    === mempty
  assertSignedSubTransactionBytesUnchanged example tx
  repeated <- H.evalEither $ recalculate balanced
  Api.serialiseToRawBytes repeated === Api.serialiseToRawBytes balanced

-- | Adding the reference charge widens the encoded fee; both fee APIs include the extra bytes.
prop_sub_transaction_reference_fee_encoding_boundary :: Property
prop_sub_transaction_reference_fee_encoding_boundary = H.withTests 10 $ H.property $ do
  price <- H.evalMaybe $ L.boundRational 10_000
  let params =
        dijkstraTestParams
          & L.ppMinFeeRefScriptCostPerByteL .~ price
          & L.ppTxFeeFixedL .~ L.Coin 40_000
          & L.ppTxFeePerByteL .~ L.CoinPerByte (L.CompactCoin 1)
          & L.ppPricesL .~ L.Prices minBound minBound
  example <-
    H.forAllWith (show . scriptInputs) $
      genScriptBalanceCase params ReferenceScript sufficientGuardBudget
  unsigned@(Exp.UnsignedTx tx) <-
    H.evalEither $ Exp.makeUnsignedTx Exp.DijkstraEra (scriptTxContent example)
  let inputs = scriptInputs example
      utxo = guardUTxO inputs
      Exp.SignedSubTx sub = signedSubTransaction example
      referenceSize = txNonDistinctRefScriptsSize utxo sub
      withoutReferenceFee = L.setMinFeeTx params tx 0 ^. L.bodyTxL . L.feeTxBodyL
      expectedFee = L.setMinFeeTx params tx referenceSize ^. L.bodyTxL . L.feeTxBodyL
      evaluation = Exp.evaluateTransaction testSystemStart testEpochInfo params mempty mempty utxo tx
      assertFee balanced = do
        let fee = balanced ^. L.bodyTxL . L.feeTxBodyL
            minimumFee = L.getMinFeeTx params balanced referenceSize
        txNonDistinctRefScriptsSize utxo balanced === 0
        H.assertWith (L.getMinFeeTx params balanced 0) (< L.Coin 65_536)
        H.assertWith minimumFee (>= L.Coin 65_536)
        H.assertWith (fee, minimumFee) (uncurry (>=))
        -- Allow the builder's witness and size estimates, but reject a second
        -- reference-script charge, which would add at least 10,000 lovelace.
        H.assertWith (fee, minimumFee) $ \(actual, required) -> actual - required < L.Coin 1000
  H.assertWith referenceSize (> 0)
  H.assertWith withoutReferenceFee (< L.Coin 65_536)
  H.assertWith expectedFee (>= L.Coin 65_536)
  Exp.txEvalFee evaluation === expectedFee
  assertFee $ tx & L.bodyTxL . L.feeTxBodyL .~ Exp.txEvalFee evaluation
  (Exp.UnsignedTx balanced, _) <- H.evalEither $ balanceScriptCase params example
  assertFee balanced
  assertSignedSubTransactionBytesUnchanged example balanced
  Exp.UnsignedTx result <-
    H.evalEither $
      Exp.calcMinFeeRecursive
        (Api.toShelleyAddr $ guardTestOutputAddress inputs)
        unsigned
        utxo
        params
        mempty
        mempty
        0
  assertFee result
  Exp.txEvalBalance
    (Exp.evaluateTransaction testSystemStart testEpochInfo params mempty mempty utxo result)
    === mempty
  assertSignedSubTransactionBytesUnchanged example result

-- | A script only in the sub-transaction uses collateral from the top-level body.
prop_sub_transaction_uses_top_level_collateral :: Property
prop_sub_transaction_uses_top_level_collateral = H.withTests 10 $ H.property $ do
  example <-
    H.forAllWith (show . scriptInputs) $
      genScriptBalanceCase dijkstraTestParams ScriptWitness sufficientGuardBudget
  (Exp.UnsignedTx tx, _) <- H.evalEither $ balanceScriptCase dijkstraTestParams example
  let inputs = scriptInputs example
      fee = tx ^. L.bodyTxL . L.feeTxBodyL
      percentage = dijkstraTestParams ^. L.ppCollateralPercentageL
      feeTimesPercentage = L.unCoin fee * fromIntegral percentage
  tx ^. L.bodyTxL . L.collateralInputsTxBodyL
    === Set.singleton (Api.toShelleyTxIn $ guardCollateralInput inputs)
  totalCollateral <- H.evalMaybe $ L.strictMaybeToMaybe $ tx ^. L.bodyTxL . L.totalCollateralTxBodyL
  let collateral = L.unCoin totalCollateral
  -- The collateral covers the required percentage.
  H.assertWith (collateral * 100, feeTimesPercentage) $ \(provided, required) -> provided >= required
  -- One lovelace less would be insufficient.
  H.assertWith ((collateral - 1) * 100, feeTimesPercentage) $ \(provided, required) -> provided < required
  returned <- H.evalMaybe $ L.strictMaybeToMaybe $ tx ^. L.bodyTxL . L.collateralReturnTxBodyL
  returned ^. L.coinTxOutL <> totalCollateral === L.Coin 100_000_000

-- | Reject insufficient top-level collateral when only the sub-transaction runs Plutus.
prop_sub_transaction_insufficient_collateral_rejected :: Property
prop_sub_transaction_insufficient_collateral_rejected = H.withTests 10 $ H.property $ do
  example <-
    H.forAllWith (show . scriptInputs) $
      genScriptBalanceCase dijkstraTestParams ScriptWitness sufficientGuardBudget
  let inputs = scriptInputs example
      utxo =
        L.UTxO $
          Map.adjust
            (L.coinTxOutL .~ L.Coin 1)
            (Api.toShelleyTxIn $ guardCollateralInput inputs)
            (L.unUTxO $ guardUTxO inputs)
  case autoBalanceTestTransaction (guardTestOutputAddress inputs) utxo (scriptTxContent example) of
    Left (Exp.TxBodyErrorCollateral (Api.InsufficientCollateral available required)) -> do
      available === L.Coin 1
      H.assertWith (required, available) (uncurry (>))
    other -> H.annotateShow (fmap fst other) >> H.failure

-- | Exhaust memory or steps separately while leaving the other resource sufficient.
-- Require a script-budget failure, not just any sub-transaction validation error.
prop_insufficient_sub_transaction_budget_rejected :: Property
prop_insufficient_sub_transaction_budget_rejected = H.withTests 10 $ H.property $ do
  let budgets =
        [ ("memory", sufficientGuardBudget{Api.executionMemory = 0}, "mem: -")
        , ("steps", sufficientGuardBudget{Api.executionSteps = 0}, "cpu: -")
        ]
  forM_ budgets $ \(resource, budget, exhaustedResource) -> do
    H.annotate $ "Only " <> resource <> " is insufficient in the signed budget:"
    H.annotateShow budget
    example <-
      H.forAllWith (show . scriptInputs) $
        genScriptBalanceCase dijkstraTestParams ScriptWitness budget
    case balanceScriptCase dijkstraTestParams example of
      Left
        err@(Exp.TxBodyErrorTopLevelScriptFailures ((source, Api.ScriptErrorEvaluationFailed details) :| [])) -> do
          source === Exp.SubTransaction (Exp.getSignedSubTxId $ signedSubTransaction example)
          Api.dpfExecutionUnits details === Api.toAlonzoExUnits budget
          let Exp.SignedSubTx sub = signedSubTransaction example
          Map.keys (sub ^. L.witsTxL . L.scriptTxWitsL)
            === [Plutus.pwcScriptHash $ Api.dpfScriptWithContext details]
          let reason = Api.prettyShow $ Api.dpfEvaluationError details
          H.assertWith reason ("due to overspending the budget" `List.isInfixOf`)
          H.assertWith reason (exhaustedResource `List.isInfixOf`)
          let rendered = Api.displayError err
          H.assertWith
            rendered
            (Api.prettyShow (Exp.getSignedSubTxId $ signedSubTransaction example) `List.isInfixOf`)
          H.assertWith rendered (not . List.isInfixOf "pwcCostModel")
      other -> H.annotateShow (fmap fst other) >> H.failure

-- | Two failing sub-transactions report their own IDs and script contexts.
prop_script_failures_identify_sub_transactions :: Property
prop_script_failures_identify_sub_transactions = H.withTests 10 $ H.property $ do
  example <-
    H.forAllWith (show . scriptInputs) $
      genScriptBalanceCase dijkstraTestParams ScriptWitness (Api.ExecutionUnits 0 0)
  let inputs = scriptInputs example
  second <-
    H.forAll $
      genSignedSubTransaction $
        guardSubTxBodyContent inputs
          & Exp.setTxIns [(guardReferenceInput inputs, Exp.AnyKeyWitnessPlaceholder)]
          & Exp.setTxGuards (scriptCaseGuards example)
  let signed = [signedSubTransaction example, second]
      content = scriptTxContent example & Exp.setTxSignedSubTransactions signed
  case balancePlutusGuardTransaction inputs content of
    Left (Exp.TxBodyErrorTopLevelScriptFailures failures) -> do
      map fst (toList failures) === map (Exp.SubTransaction . Exp.getSignedSubTxId) signed
      forM_ (zip signed $ toList failures) $ \(Exp.SignedSubTx sub, (_, failure)) ->
        case failure of
          Api.ScriptErrorEvaluationFailed details -> do
            Api.dpfExecutionUnits details === L.ExUnits 0 0
            Map.keys (sub ^. L.witsTxL . L.scriptTxWitsL)
              === [Plutus.pwcScriptHash $ Api.dpfScriptWithContext details]
          other -> H.annotateShow other >> H.failure
    other -> H.annotateShow (fmap fst other) >> H.failure

-- | ScriptInvalid accepts a signed sub-transaction that exhausts its unchanged zero budget.
prop_script_invalid_accepts_sub_transaction_failure :: Property
prop_script_invalid_accepts_sub_transaction_failure = H.withTests 10 $ H.property $ do
  example <-
    H.forAllWith (show . scriptInputs) $
      genScriptBalanceCase dijkstraTestParams ScriptWitness (Api.ExecutionUnits 0 0)
  assertScriptInvalidBalancesWithUnchangedSubTransaction example

-- | A passing top-level guard does not hide the sub-transaction failure expected by ScriptInvalid.
prop_script_invalid_accepts_sub_failure_with_passing_top_guard :: Property
prop_script_invalid_accepts_sub_failure_with_passing_top_guard = H.withTests 10 $ H.property $ do
  example <-
    H.forAllWith (show . scriptInputs) $
      genTopLevelGuardCase dijkstraTestParams ScriptWitness (Api.ExecutionUnits 0 0)
  assertScriptInvalidBalancesWithUnchangedSubTransaction example

-- | A failing top-level guard satisfies ScriptInvalid while the signed sub-transaction passes.
prop_script_invalid_accepts_top_level_failure :: Property
prop_script_invalid_accepts_top_level_failure = H.withTests 10 $ H.property $ do
  example <-
    H.forAllWith (show . scriptInputs) $
      genFailingTopLevelGuardCase dijkstraTestParams sufficientGuardBudget
  assertScriptInvalidBalancesWithUnchangedSubTransaction example

-- | ScriptInvalid accepts a failing top-level guard and a sub-transaction that exhausts its budget.
prop_script_invalid_accepts_failures_at_both_levels :: Property
prop_script_invalid_accepts_failures_at_both_levels = H.withTests 10 $ H.property $ do
  example <-
    H.forAllWith (show . scriptInputs) $
      genFailingTopLevelGuardCase dijkstraTestParams (Api.ExecutionUnits 0 0)
  assertScriptInvalidBalancesWithUnchangedSubTransaction example

-- | ScriptInvalid is rejected when both the top-level guard and signed sub-transaction pass.
prop_script_invalid_rejects_passing_scripts :: Property
prop_script_invalid_rejects_passing_scripts = H.withTests 10 $ H.property $ do
  example <-
    H.forAllWith (show . scriptInputs) $
      genTopLevelGuardCase dijkstraTestParams ScriptWitness sufficientGuardBudget
  let invalid = example{scriptTxContent = scriptTxContent example & Exp.setTxScriptValidity Api.ScriptInvalid}
  case balanceScriptCase dijkstraTestParams invalid of
    Left Exp.TxBodyScriptBadScriptValidity -> pure ()
    other -> H.annotateShow (fmap fst other) >> H.failure

-- | Balance with ScriptInvalid, retaining that flag, collateral and the signed sub-transaction bytes.
assertScriptInvalidBalancesWithUnchangedSubTransaction :: ScriptBalanceCase -> H.PropertyT IO ()
assertScriptInvalidBalancesWithUnchangedSubTransaction example = do
  let invalid = example{scriptTxContent = scriptTxContent example & Exp.setTxScriptValidity Api.ScriptInvalid}
  (Exp.UnsignedTx tx, content) <- H.evalEither $ balanceScriptCase dijkstraTestParams invalid
  Exp.txScriptValidity content === Api.ScriptInvalid
  tx ^. L.bodyTxL . L.collateralInputsTxBodyL
    === Set.singleton (Api.toShelleyTxIn $ guardCollateralInput $ scriptInputs example)
  assertSignedSubTransactionBytesUnchanged example tx

-- | Accept the combined budget at the limit; reject a limit one unit lower.
-- Check memory and steps separately, preserving the signed sub-transaction.
prop_combined_execution_budget_boundary :: Property
prop_combined_execution_budget_boundary = H.withTests 10 $ H.property $ do
  example <-
    H.forAllWith (show . scriptInputs) $
      genTopLevelGuardCase dijkstraTestParams ScriptWitness sufficientGuardBudget
  (Exp.UnsignedTx measured, _) <- H.evalEither $ balanceScriptCase dijkstraTestParams example
  -- Measure only top-level redeemers, then add the budget signed by the sub-transaction.
  let topUnits = foldMap snd $ L.unRedeemers $ measured ^. L.witsTxL . L.rdmrsTxWitsL
      signedUnits = Api.toAlonzoExUnits sufficientGuardBudget
      combined = topUnits <> signedUnits
      maxUnits = dijkstraTestParams ^. L.ppMaxTxExUnitsL
      limits =
        [
          ( "memory"
          , maxUnits{L.exUnitsMem = L.exUnitsMem combined}
          , maxUnits{L.exUnitsMem = L.exUnitsMem combined - 1}
          )
        ,
          ( "steps"
          , maxUnits{L.exUnitsSteps = L.exUnitsSteps combined}
          , maxUnits{L.exUnitsSteps = L.exUnitsSteps combined - 1}
          )
        ]
  H.annotate "Measured top-level budget:"
  H.annotateShow topUnits
  H.assertWith topUnits $ \units -> L.exUnitsMem units > 0 && L.exUnitsSteps units > 0
  H.annotate "Signed sub-transaction budget:"
  H.annotateShow signedUnits
  forM_ limits $ \(name, exactLimit, lowerLimit) -> do
    H.annotate name
    H.annotate "The combined budget must be accepted at this limit:"
    H.annotateShow exactLimit
    let exactParams = dijkstraTestParams & L.ppMaxTxExUnitsL .~ exactLimit
        lowerParams = dijkstraTestParams & L.ppMaxTxExUnitsL .~ lowerLimit
    (Exp.UnsignedTx tx, _) <- H.evalEither $ balanceScriptCase exactParams example
    foldMap snd (L.unRedeemers $ tx ^. L.witsTxL . L.rdmrsTxWitsL) === topUnits
    assertSignedSubTransactionBytesUnchanged example tx
    H.annotate
      "The same top-level transaction and its sub-transactions must exceed this limit by exactly one unit:"
    H.annotateShow lowerLimit
    case balanceScriptCase lowerParams example of
      Left (Exp.TxBodyErrorTopLevelExecutionUnitsTooBig used maximumAllowed) -> do
        used === Api.fromAlonzoExUnits combined
        maximumAllowed === Api.fromAlonzoExUnits lowerLimit
      other -> H.annotateShow (fmap fst other) >> H.failure

-- | Each script fits by itself, but top-level plus sub-transaction budgets exceed memory or steps.
prop_combined_execution_budget_rejected :: Property
prop_combined_execution_budget_rejected = H.withTests 10 $ H.property $ do
  example <-
    H.forAllWith (show . scriptInputs) $
      genScriptBalanceCase dijkstraTestParams ScriptWitness sufficientGuardBudget
  let withTopGuard = scriptTxContent example & Exp.setTxGuards (scriptCaseGuards example)
      maxUnits = dijkstraTestParams ^. L.ppMaxTxExUnitsL
      signedUnits = Api.toAlonzoExUnits sufficientGuardBudget
      limits =
        [ ("memory", maxUnits{L.exUnitsMem = L.exUnitsMem signedUnits})
        , ("steps", maxUnits{L.exUnitsSteps = L.exUnitsSteps signedUnits})
        ]
      inputs = scriptInputs example
  (Exp.UnsignedTx measured, _) <-
    H.evalEither $
      autoBalanceTestTransaction (guardTestOutputAddress inputs) (guardUTxO inputs) withTopGuard
  let topUnits = foldMap snd $ L.unRedeemers $ measured ^. L.witsTxL . L.rdmrsTxWitsL
      combined = topUnits <> signedUnits
  forM_ limits $ \(name, limit) -> do
    H.annotate name
    let params = dijkstraTestParams & L.ppMaxTxExUnitsL .~ limit
    _ <- H.evalEither $ balanceScriptCase params example
    case autoBalanceWithParams params (guardTestOutputAddress inputs) (guardUTxO inputs) withTopGuard of
      Left (Exp.TxBodyErrorTopLevelExecutionUnitsTooBig used maximumAllowed) -> do
        used === Api.fromAlonzoExUnits combined
        maximumAllowed === Api.fromAlonzoExUnits limit
      other -> H.annotateShow (fmap fst other) >> H.failure

-- | Reject a Plutus V3 withdrawal inside a sub-transaction under either script-validity flag.
prop_legacy_plutus_in_sub_transaction_rejected :: Property
prop_legacy_plutus_in_sub_transaction_rejected = H.withTests 10 $ H.property $ do
  script <- H.evalEither alwaysSucceedsGuardScript
  inputs <- H.forAll $ genPlutusV4ReferenceScriptGuardTestInputs script
  withdrawal <- H.forAll $ genLegacyWithdrawal sufficientGuardBudget
  signed <-
    H.forAll $
      genSignedSubTransaction $
        guardSubTxBodyContent inputs & Exp.setTxWithdrawals withdrawal
  forM_ [Api.ScriptValid, Api.ScriptInvalid] $ \validity -> do
    H.annotateShow validity
    let content = topLevelTxBodyContentWithSignedSubTransaction inputs signed & Exp.setTxScriptValidity validity
    case balancePlutusGuardTransaction inputs content of
      Left (Exp.TxBodyErrorTopLevelScriptContexts (L.BadTranslation{} :| [])) -> pure ()
      other -> H.annotateShow (fmap fst other) >> H.failure

-- | Both evaluation APIs evaluate the top-level V4 guard with signed sub-transactions present.
prop_top_level_guard_evaluates_with_sub_transactions :: Property
prop_top_level_guard_evaluates_with_sub_transactions = H.withTests 10 $ H.property $ do
  forM_ [ScriptWitness, ReferenceScript] $ \source -> do
    H.annotateShow source
    example <-
      H.forAllWith (show . scriptInputs) $
        genTopLevelGuardCase dijkstraTestParams source sufficientGuardBudget
    (Exp.UnsignedTx tx, _) <- H.evalEither $ balanceScriptCase dijkstraTestParams example
    let inputs = scriptInputs example
    assertTopLevelGuardEvaluates inputs tx

-- | An extra redeemer at index 42 reports that exact unmatched index.
prop_unmatched_guard_redeemer_reported :: Property
prop_unmatched_guard_redeemer_reported = H.withTests 10 $ H.property $ do
  forM_ [ScriptWitness, ReferenceScript] $ \source -> do
    H.annotateShow source
    example <-
      H.forAllWith (show . scriptInputs) $
        genTopLevelGuardCase dijkstraTestParams source sufficientGuardBudget
    (Exp.UnsignedTx tx, _) <- H.evalEither $ balanceScriptCase dijkstraTestParams example
    let inputs = scriptInputs example
    assertTopLevelGuardEvaluates inputs tx
    let orphanIndex = Api.ScriptWitnessIndexGuarding 42
        orphan = (Api.toAlonzoData $ Api.unsafeHashableScriptData $ Api.ScriptDataNumber 42, L.ExUnits 0 0)
        txWithOrphan =
          tx
            & L.witsTxL . L.rdmrsTxWitsL
              %~ (Alonzo.Redeemers . Map.insert (L.DijkstraGuarding $ L.AsIx 42) orphan . L.unRedeemers)
        report = evaluateGuards (guardUTxO inputs) txWithOrphan
    Map.keys report === [Api.ScriptWitnessIndexGuarding 0, orphanIndex]
    case Map.lookup orphanIndex report of
      Just (Left (Api.ScriptErrorRedeemerPointsToUnknownScriptHash index)) -> index === orphanIndex
      other -> H.annotateShow other >> H.failure

-- | Removing a sub-transaction input prevents construction of the top-level V4 script context.
prop_missing_sub_transaction_input_prevents_guard_context :: Property
prop_missing_sub_transaction_input_prevents_guard_context = H.withTests 10 $ H.property $ do
  forM_ [ScriptWitness, ReferenceScript] $ \source -> do
    H.annotateShow source
    example <-
      H.forAllWith (show . scriptInputs) $
        genTopLevelGuardCase dijkstraTestParams source sufficientGuardBudget
    (Exp.UnsignedTx tx, _) <- H.evalEither $ balanceScriptCase dijkstraTestParams example
    let inputs = scriptInputs example
    assertTopLevelGuardEvaluates inputs tx
    let utxo = L.UTxO $ Map.delete (Api.toShelleyTxIn $ guardFundingInput inputs) $ L.unUTxO $ guardUTxO inputs
    case Map.lookup (Api.ScriptWitnessIndexGuarding 0) (evaluateGuards utxo tx) of
      Just (Left Api.ScriptErrorTranslationError{}) -> pure ()
      other -> H.annotateShow other >> H.failure

-- | Run the auto-balancer; properties report any failure with 'H.evalEither'.
balanceScriptCase
  :: L.PParams (Exp.LedgerEra Exp.DijkstraEra)
  -> ScriptBalanceCase
  -> DijkstraBalanceResult
balanceScriptCase params example =
  let inputs = scriptInputs example
   in autoBalanceWithParams
        params
        (guardTestOutputAddress inputs)
        (guardUTxO inputs)
        (scriptTxContent example)

-- | Require exactly the original signed sub-transaction, comparing serialized bytes.
-- This checks its body, witnesses and signed execution budgets.
assertSignedSubTransactionBytesUnchanged
  :: ScriptBalanceCase -> L.Tx L.TopTx (Exp.LedgerEra Exp.DijkstraEra) -> H.PropertyT IO ()
assertSignedSubTransactionBytesUnchanged example tx =
  map
    (Api.serialiseToCBOR . Exp.SignedSubTx)
    (toList $ tx ^. L.bodyTxL . Dijkstra.subTransactionsTxBodyL)
    === [Api.serialiseToCBOR $ signedSubTransaction example]

evaluateGuards
  :: L.UTxO (Exp.LedgerEra Exp.DijkstraEra)
  -> L.Tx L.TopTx (Exp.LedgerEra Exp.DijkstraEra)
  -> Map.Map
       Api.ScriptWitnessIndex
       (Either Api.ScriptExecutionError Api.ExecutionUnits)
evaluateGuards utxo tx =
  fmap snd
    <$> Exp.evaluateTransactionExecutionUnits testSystemStart testEpochInfo dijkstraTestParams utxo tx

-- | Require a successful result only at top-level guard index zero, with positive
-- memory and step costs matching those reported by 'Exp.evaluateTransaction'.
-- Sub-transaction redeemers must not appear in the top-level execution-unit report.
assertTopLevelGuardEvaluates
  :: PlutusV4ReferenceScriptGuardTestInputs
  -> L.Tx L.TopTx (Exp.LedgerEra Exp.DijkstraEra)
  -> H.PropertyT IO ()
assertTopLevelGuardEvaluates inputs tx = do
  let utxo = guardUTxO inputs
      report = evaluateGuards utxo tx
      index = Api.ScriptWitnessIndexGuarding 0
  Map.keys report === [index]
  measured <- H.evalEither =<< H.evalMaybe (Map.lookup index report)
  H.assertWith measured $ \units -> Api.executionSteps units > 0 && Api.executionMemory units > 0
  let evaluation = Exp.evaluateTransaction testSystemStart testEpochInfo dijkstraTestParams mempty mempty utxo tx
  (_, evaluated) <-
    H.evalEither =<< H.evalMaybe (Map.lookup index $ Exp.txEvalExecutionUnits evaluation)
  evaluated === measured

-- | Build 'Exp.SubTxBodyContent' with funding and test protocol parameters.
-- The caller adds Plutus V4 script guards; the top-level transaction supplies collateral.
guardSubTxBodyContent
  :: PlutusV4ReferenceScriptGuardTestInputs -> Exp.SubTxBodyContent (Exp.LedgerEra Exp.DijkstraEra)
guardSubTxBodyContent testInputs =
  Exp.defaultSubTxBodyContent
    & Exp.setTxIns [(guardFundingInput testInputs, Exp.AnyKeyWitnessPlaceholder)]
    & Exp.setTxProtocolParams dijkstraTestParams

-- | Build 'Exp.TxBodyContent' with collateral and a signed sub-transaction.
-- The sub-transaction supplies the spending inputs.
topLevelTxBodyContentWithSignedSubTransaction
  :: PlutusV4ReferenceScriptGuardTestInputs
  -> Exp.SignedSubTx
  -> Exp.TxBodyContent (Exp.LedgerEra Exp.DijkstraEra)
topLevelTxBodyContentWithSignedSubTransaction testInputs signed =
  Exp.defaultTxBodyContent
    & Exp.setTxInsCollateral [guardCollateralInput testInputs]
    & Exp.setTxSignedSubTransactions [signed]
    & Exp.setTxProtocolParams dijkstraTestParams

-- | Balance a Plutus V4 script guard transaction using 'guardUTxO' and 'guardTestOutputAddress'.
balancePlutusGuardTransaction
  :: PlutusV4ReferenceScriptGuardTestInputs
  -> Exp.TxBodyContent (Exp.LedgerEra Exp.DijkstraEra)
  -> DijkstraBalanceResult
balancePlutusGuardTransaction testInputs = autoBalanceTestTransaction (guardTestOutputAddress testInputs) (guardUTxO testInputs)

-- | Sufficient Plutus V4 script budget, set before signing a sub-transaction.
sufficientGuardBudget :: Api.ExecutionUnits
sufficientGuardBudget = Api.ExecutionUnits{Api.executionSteps = 1_000_000_000, Api.executionMemory = 1_000_000}

-- | Reject two signed sub-transactions that spend the same UTxO entry.
-- Report only their shared input; the top-level spending input is distinct.
prop_duplicate_sub_transaction_spending_inputs :: Property
prop_duplicate_sub_transaction_spending_inputs = H.withTests 10 $ H.property $ do
  testData <-
    H.forAllWith (show . balanceTestUTxO) $
      genSubTransactionBalanceTestData
        dijkstraTestParams
        (TopLevelTxAmounts $ adaAmounts 100 10)
        [SubTxAmounts $ adaAmounts 100 10]
  (input, _) <- H.evalMaybe $ List.uncons (balanceTestSubTxInputs testData)
  -- 'input' is already spent by the first sub-transaction. Spend it again in
  -- a different body, distinguished here by having no outputs.
  signed <-
    H.forAllWith (const "<signed sub-transaction>") $
      genSignedSubTransaction $
        Exp.defaultSubTxBodyContent
          & Exp.setTxIns [(input, Exp.AnyKeyWitnessPlaceholder)]
  let content =
        balanceTestTxBodyContent testData
          & Exp.setTxSignedSubTransactions (balanceTestSignedSubTransactions testData <> [signed])
  H.annotate "The top-level spending input is separate and must not be reported:"
  H.annotateShow $ balanceTestTopLevelInput testData
  H.annotate "Both signed sub-transactions spend this input:"
  H.annotateShow input
  H.annotate
    "Expected TxBodyErrorDuplicateSpendingInputs containing only the shared sub-transaction input."
  case autoBalanceTestTransaction (balanceTestAddress testData) (balanceTestUTxO testData) content of
    Left (Exp.TxBodyErrorDuplicateSpendingInputs inputs) -> inputs === Set.singleton input
    other -> H.annotateShow (fmap fst other) >> H.failure

-- | Report every input spent by both the top level and a signed sub-transaction.
-- Exclude the original top-level input, which is spent only once.
prop_duplicate_top_level_spending_inputs :: Property
prop_duplicate_top_level_spending_inputs = H.withTests 10 $ H.property $ do
  testData <-
    H.forAllWith (show . balanceTestUTxO) $
      genSubTransactionBalanceTestData
        dijkstraTestParams
        (TopLevelTxAmounts $ adaAmounts 100 10)
        [SubTxAmounts $ adaAmounts 100 10, SubTxAmounts $ adaAmounts 100 10]
  -- Adding 'balanceTestSubTxInputs' here makes those two inputs duplicates:
  -- each is already spent by a signed sub-transaction.
  let content =
        balanceTestTxBodyContent testData
          & Exp.setTxIns
            [ (input, Exp.AnyKeyWitnessPlaceholder)
            | input <- balanceTestTopLevelInput testData : balanceTestSubTxInputs testData
            ]
  H.annotate "Top-level spending inputs after adding both sub-transaction inputs:"
  H.annotateShow $ map fst $ Exp.txIns content
  H.annotate "Inputs still spent by the signed sub-transactions, one input per body:"
  H.annotateShow $ balanceTestSubTxInputs testData
  H.annotate
    "Expected TxBodyErrorDuplicateSpendingInputs containing both sub-transaction inputs and excluding the original top-level input."
  case autoBalanceTestTransaction (balanceTestAddress testData) (balanceTestUTxO testData) content of
    Left (Exp.TxBodyErrorDuplicateSpendingInputs inputs) -> inputs === Set.fromList (balanceTestSubTxInputs testData)
    other -> H.annotateShow (fmap fst other) >> H.failure

prop_shared_reference_inputs :: Property
prop_shared_reference_inputs = H.withTests 10 $ H.property $ do
  example <-
    H.forAllWith (show . scriptInputs) $
      genScriptBalanceCase dijkstraTestParams ReferenceScript sufficientGuardBudget
  let inputs = scriptInputs example
      content =
        scriptTxContent example
          & Exp.setTxInsReference (Exp.TxInsReference [guardReferenceInput inputs] Set.empty)
  (Exp.UnsignedTx tx, _) <-
    H.evalEither $ autoBalanceTestTransaction (guardTestOutputAddress inputs) (guardUTxO inputs) content
  assertSignedSubTransactionBytesUnchanged example tx
  Exp.txEvalBalance
    ( Exp.evaluateTransaction
        testSystemStart
        testEpochInfo
        dijkstraTestParams
        mempty
        mempty
        (guardUTxO inputs)
        tx
    )
    === mempty
