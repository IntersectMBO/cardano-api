{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}

module Test.Gen.Cardano.Api.Experimental.SubTransaction
  ( TxAmounts (..)
  , TopLevelTxAmounts (..)
  , SubTxAmounts (..)
  , SubTransactionBalanceTestData (..)
  , genSubTransactionBalanceTestData
  , genAdaAmount
  )
where

import Cardano.Api qualified as Api
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Experimental.Tx qualified as Exp
import Cardano.Api.Ledger qualified as L

import Cardano.Ledger.Val qualified as L (invert)

import Data.Map.Strict qualified as Map
import Lens.Micro

import Test.Gen.Cardano.Api.Experimental (genSignedSubTransaction)
import Test.Gen.Cardano.Api.Typed (genAddressInEra, genTxId)

import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

-- | Value supplied by one spending input and value requested in one output.
data TxAmounts = TxAmounts
  { inputValue :: L.MaryValue
  , outputValue :: L.MaryValue
  }
  deriving Show

-- | Input value and requested output value of the top-level transaction.
newtype TopLevelTxAmounts = TopLevelTxAmounts TxAmounts
  deriving Show

-- | Input value and requested output value of one signed sub-transaction.
newtype SubTxAmounts = SubTxAmounts TxAmounts
  deriving Show

-- | The inputs, address, UTxO and signed sub-transactions used by a balancing test.
data SubTransactionBalanceTestData = SubTransactionBalanceTestData
  { balanceTestTopLevelInput :: Api.TxIn
  , balanceTestSubTxInputs :: [Api.TxIn]
  , balanceTestAddress :: Api.AddressInEra Api.DijkstraEra
  , balanceTestUTxO :: L.UTxO (Exp.LedgerEra Exp.DijkstraEra)
  , balanceTestTxBodyContent :: Exp.TxBodyContent (Exp.LedgerEra Exp.DijkstraEra)
  , balanceTestSignedSubTransactions :: [Exp.SignedSubTx]
  , balanceTestSurplus :: L.MaryValue
  -- ^ Combined inputs minus requested outputs and deposits, before fees.
  }

-- | Build exactly the requested transactions, each with one input and one output.
-- Randomize addresses, transaction IDs and signing keys; keep the amounts explicit.
genSubTransactionBalanceTestData
  :: L.PParams (Exp.LedgerEra Exp.DijkstraEra)
  -> TopLevelTxAmounts
  -- ^ Top-level input and output values.
  -> [SubTxAmounts]
  -- ^ Sub-transaction input and output values, in order.
  -> Gen SubTransactionBalanceTestData
genSubTransactionBalanceTestData params (TopLevelTxAmounts topAmounts) subTxAmounts = do
  txId <- genTxId
  address <- genAddressInEra Api.ShelleyBasedEraDijkstra
  let subAmounts = [values | SubTxAmounts values <- subTxAmounts]
      topInput = Api.TxIn txId (Api.TxIx 0)
      subInputs = zipWith (\i _ -> Api.TxIn txId (Api.TxIx i)) [1 ..] subAmounts
      output = L.mkBasicTxOut (Api.toShelleyAddr address)
      amounts = topAmounts : subAmounts
      utxo =
        L.UTxO $
          Map.fromList $
            zipWith
              (\input value -> (Api.toShelleyTxIn input, output $ inputValue value))
              (topInput : subInputs)
              amounts
  signed <-
    traverse
      ( \(input, value) ->
          genSignedSubTransaction $
            Exp.defaultSubTxBodyContent
              & Exp.setTxIns [(input, Exp.AnyKeyWitnessPlaceholder)]
              & Exp.setTxOuts [Exp.TxOut $ output $ outputValue value]
      )
      (zip subInputs subAmounts)
  let content =
        Exp.defaultTxBodyContent
          & Exp.setTxIns [(topInput, Exp.AnyKeyWitnessPlaceholder)]
          & Exp.setTxOuts [Exp.TxOut $ output $ outputValue topAmounts]
          & Exp.setTxSignedSubTransactions signed
          & Exp.setTxProtocolParams params
  pure
    SubTransactionBalanceTestData
      { balanceTestTopLevelInput = topInput
      , balanceTestSubTxInputs = subInputs
      , balanceTestAddress = address
      , balanceTestUTxO = utxo
      , balanceTestTxBodyContent = content
      , balanceTestSignedSubTransactions = signed
      , balanceTestSurplus =
          mconcat (map inputValue amounts) <> L.invert (mconcat $ map outputValue amounts)
      }

-- | Whole ada for requested outputs, deficits and change reserves. Five ada is
-- enough for these outputs and leaves room for fees when used as a reserve.
genAdaAmount :: Gen Integer
genAdaAmount = Gen.integral $ Range.linear 5 10_000
