{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

-- | Shared inputs, protocol parameters and assertions for sub-transaction balancing tests.
module Test.Cardano.Api.Experimental.Fee.Common
  ( DijkstraBalanceResult
  , ada
  , adaAmounts
  , assertChangeFeeAndUnchangedSubTransactions
  , autoBalanceTestTransaction
  , autoBalanceWithParams
  , balanceTransactionWithSubTransactions
  , dijkstraTestParams
  , assertRejectsInsufficientFunds
  , assertBalancesWithExpectedChange
  , testEpochInfo
  , testSystemStart
  )
where

import Cardano.Api qualified as Api
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Experimental.Tx qualified as Exp
import Cardano.Api.Ledger qualified as L

import Cardano.Ledger.Core qualified as L
import Cardano.Ledger.Dijkstra.TxBody qualified as Dijkstra
import Cardano.Ledger.Tools qualified as L (calcMinFeeTx)
import Cardano.Ledger.Val qualified as L (invert)
import Cardano.Slotting.EpochInfo qualified as Slotting
import Cardano.Slotting.Slot qualified as Slotting
import Cardano.Slotting.Time qualified as Slotting

import Data.Time.Clock.POSIX qualified as Time
import GHC.Exts (IsList (..))
import Lens.Micro

import Test.Gen.Cardano.Api.Experimental.SubTransaction
  ( SubTransactionBalanceTestData (..)
  , TxAmounts (..)
  )

import Test.Cardano.Api.Experimental (exampleProtocolParamsEra)

import Hedgehog ((===))
import Hedgehog qualified as H

-- | Auto-balancing error, or the balanced transaction and updated TxBodyContent.
type DijkstraBalanceResult =
  Either
    (Exp.TxBodyErrorAutoBalance (Exp.LedgerEra Exp.DijkstraEra))
    (Exp.UnsignedTx (Exp.LedgerEra Exp.DijkstraEra), Exp.TxBodyContent (Exp.LedgerEra Exp.DijkstraEra))

-- | Balance with fixed Dijkstra test parameters and timing, and no stake deposits or pools.
autoBalanceTestTransaction
  :: Api.AddressInEra Api.DijkstraEra
  -> L.UTxO (Exp.LedgerEra Exp.DijkstraEra)
  -> Exp.TxBodyContent (Exp.LedgerEra Exp.DijkstraEra)
  -> DijkstraBalanceResult
autoBalanceTestTransaction = autoBalanceWithParams dijkstraTestParams

-- | Balance with the supplied protocol parameters, including execution limits.
autoBalanceWithParams
  :: L.PParams (Exp.LedgerEra Exp.DijkstraEra)
  -> Api.AddressInEra Api.DijkstraEra
  -> L.UTxO (Exp.LedgerEra Exp.DijkstraEra)
  -> Exp.TxBodyContent (Exp.LedgerEra Exp.DijkstraEra)
  -> DijkstraBalanceResult
autoBalanceWithParams params address utxo txBodyContent =
  Exp.makeTransactionBodyAutoBalance
    testSystemStart
    testEpochInfo
    params
    mempty
    mempty
    utxo
    (txBodyContent & Exp.setTxProtocolParams params)
    address
    Nothing

-- | Test protocol parameters using Dijkstra's minimum protocol version.
dijkstraTestParams :: L.PParams (Exp.LedgerEra Exp.DijkstraEra)
dijkstraTestParams =
  exampleProtocolParamsEra Exp.DijkstraEra
    & L.ppProtocolVersionL .~ L.ProtVer (L.eraProtVerLow @(Exp.LedgerEra Exp.DijkstraEra)) 0

-- | Fixed chain start at the Unix epoch for deterministic script contexts.
testSystemStart :: Api.SystemStart
testSystemStart = Api.SystemStart $ Time.posixSecondsToUTCTime 0

-- | Fixed 100-slot epochs for translating validity bounds in test script contexts.
testEpochInfo :: Api.LedgerEpochInfo
testEpochInfo =
  Api.LedgerEpochInfo $ Slotting.fixedEpochInfo (Slotting.EpochSize 100) (Slotting.mkSlotLength 1000)

-- | Input and requested output amounts, expressed in ada.
adaAmounts :: Integer -> Integer -> TxAmounts
adaAmounts supplied requested = TxAmounts (ada supplied) (ada requested)

ada :: Integer -> L.MaryValue
ada amount = L.MaryValue (L.Coin $ amount * 1_000_000) mempty

-- | Balance the generated transaction, requiring:
--
-- * Requested top-level outputs followed by one change output containing the
--   combined input value minus all requested outputs and the calculated fee.
-- * Byte-for-byte preservation of signed sub-transactions, in their original order.
-- * Zero remaining balance for ada and every native asset.
-- * A fee at least as large as the ledger minimum for the resulting transaction.
--
-- Amount generation belongs to the property; this assertion only balances and checks.
assertBalancesWithExpectedChange :: SubTransactionBalanceTestData -> H.PropertyT IO ()
assertBalancesWithExpectedChange testData = do
  (Exp.UnsignedTx tx, _) <- H.evalEither $ balanceTransactionWithSubTransactions testData
  assertChangeFeeAndUnchangedSubTransactions testData tx

-- | Require an insufficient-funds error for the supplied input and output amounts.
-- Accept 'Exp.TxBodyErrorBalanceNegative' before fee calculation or
-- 'Exp.BalanceIsNegative' after fees are included. Success and all other errors fail.
-- This covers missing ada, missing native assets and no ada left to pay fees.
assertRejectsInsufficientFunds :: SubTransactionBalanceTestData -> H.PropertyT IO ()
assertRejectsInsufficientFunds testData = do
  H.annotate "Expected insufficient funds, either before or after including fees."
  case balanceTransactionWithSubTransactions testData of
    Left Exp.TxBodyErrorBalanceNegative{} -> pure ()
    Left Exp.BalanceIsNegative{} -> pure ()
    other -> H.annotateShow (fmap fst other) >> H.failure

balanceTransactionWithSubTransactions :: SubTransactionBalanceTestData -> DijkstraBalanceResult
balanceTransactionWithSubTransactions testData =
  autoBalanceTestTransaction
    (balanceTestAddress testData)
    (balanceTestUTxO testData)
    (balanceTestTxBodyContent testData)

-- | Check the four guarantees of 'assertBalancesWithExpectedChange' on a returned
-- transaction: exact top-level outputs and change, unchanged signed sub-transaction
-- bytes and order, zero remaining ada and native assets, and a sufficient fee.
-- Compute expected change from the supplied amounts and the returned fee.
assertChangeFeeAndUnchangedSubTransactions
  :: SubTransactionBalanceTestData -> L.Tx L.TopTx (Exp.LedgerEra Exp.DijkstraEra) -> H.PropertyT IO ()
assertChangeFeeAndUnchangedSubTransactions testData tx = do
  let utxo = balanceTestUTxO testData
      fee = tx ^. L.bodyTxL . L.feeTxBodyL
      change = balanceTestSurplus testData <> L.invert (L.MaryValue fee mempty)
      expectedChange = L.mkBasicTxOut (Api.toShelleyAddr $ balanceTestAddress testData) change
      requestedOutputs = [out | Exp.TxOut out <- Exp.txOuts $ balanceTestTxBodyContent testData]
      actualSubs = map Exp.SignedSubTx $ toList $ tx ^. L.bodyTxL . Dijkstra.subTransactionsTxBodyL
  H.annotate "Top-level outputs must be the requested outputs followed by surplus minus fee."
  toList (tx ^. L.bodyTxL . L.outputsTxBodyL) === requestedOutputs <> [expectedChange]
  H.annotate "Signed sub-transaction bodies, witnesses and order must be unchanged."
  map Api.serialiseToCBOR actualSubs
    === map Api.serialiseToCBOR (balanceTestSignedSubTransactions testData)
  let evaluation = Exp.evaluateTransaction testSystemStart testEpochInfo dijkstraTestParams mempty mempty utxo tx
  H.annotate "The combined transaction must have no remaining ada or native-asset balance."
  Exp.txEvalBalance evaluation === mempty
  let minimumFee = L.calcMinFeeTx utxo dijkstraTestParams tx 0
  H.annotate "The returned fee must cover the ledger minimum for the resulting transaction."
  H.annotateShow (fee, minimumFee)
  H.assert $ fee >= minimumFee
