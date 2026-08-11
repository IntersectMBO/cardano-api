{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Api.Experimental.Fee
  ( tests
  )
where

import Cardano.Api qualified as Api
import Cardano.Api.Compatible.Tx
  ( CompatibleTxBodyContent (..)
  , CompatibleTxError (..)
  , createCompatibleTx
  , defaultCompatibleTxBodyContent
  )
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Experimental.AnyScriptWitness
  ( AnyPlutusScriptWitness (AnyPlutusSpendingScriptWitness)
  , PlutusSpendingScriptWitness (PlutusSpendingScriptWitnessV3)
  )
import Cardano.Api.Experimental.Era (convert)
import Cardano.Api.Experimental.Plutus (plutusScriptInEraToScript)
import Cardano.Api.Experimental.Tx qualified as Exp
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Monad.Error (failEitherWith)

import Cardano.Ledger.Alonzo.TxWits qualified as Alonzo
import Cardano.Ledger.Api qualified as UnexportedLedger
import Cardano.Ledger.Core qualified as L
import Cardano.Ledger.Mary.Value qualified as Mary
import Cardano.Ledger.Plutus.Language qualified as Plutus
import Cardano.Ledger.Tools qualified as L (calcMinFeeTx)
import Cardano.Slotting.EpochInfo qualified as Slotting
import Cardano.Slotting.Slot qualified as Slotting
import Cardano.Slotting.Time qualified as Slotting

import Data.Foldable (toList)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Sequence.Strict qualified as Seq
import Data.Set qualified as Set
import Data.Time.Clock.POSIX qualified as Time
import Lens.Micro

import Test.Gen.Cardano.Api.Typed
  ( genAddressInEra
  , genPlutusScriptInEra
  , genStakeCredential
  , genTxIn
  )

import Test.Cardano.Api.Experimental (exampleProtocolParams, exampleProtocolParamsEra)

import Hedgehog (Gen, Property)
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

-- | Tests in this module can be run by themselves by writing:
-- ```bash
-- cabal test cardano-api-test --test-options="--pattern=Test.Cardano.Api.Experimental.Fee"
-- ```
tests :: TestTree
tests =
  testGroup
    "Test.Cardano.Api.Experimental.Fee"
    [ testGroup
        "substituteExecutionUnits"
        [ testProperty
            "all certificates are preserved after execution unit substitution"
            prop_substituteExecutionUnits_preserves_certs
        ]
    , testGroup
        "collectTxBodyScriptWitnesses"
        [ testProperty
            "unwitnessed certs produce no script witnesses"
            prop_collectTxBodyScriptWitnesses_ignores_unwitnessed_certs
        ]
    , testGroup
        "createCompatibleTx"
        [ testProperty
            "all certs (witnessed and unwitnessed) appear in the ledger tx"
            prop_createCompatibleTx_preserves_all_certs
        , testProperty
            "spending redeemer indices follow sorted input order, not argument order"
            prop_createCompatibleTx_redeemer_indices_follow_sorted_inputs
        , testProperty
            "an inline plutus spending script witness lands in the tx witness set"
            prop_createCompatibleTx_inline_plutus_script_in_witness_set
        , testProperty
            "plutus witnesses without protocol parameters are rejected"
            prop_createCompatibleTx_missing_pparams_with_plutus_witness
        , testProperty
            "collateral, validity upper bound and metadata are threaded through"
            prop_createCompatibleTx_extra_content_is_threaded_through
        ]
    , testGroup
        "calcMinFeeRecursive"
        [ testProperty
            "well-funded transaction always succeeds"
            prop_calcMinFeeRecursive_well_funded_succeeds
        , testProperty
            "well-funded multi-asset transaction always succeeds"
            prop_calcMinFeeRecursive_well_funded_multi_asset
        , testProperty
            "fee calculation is idempotent"
            prop_calcMinFeeRecursive_fee_fixpoint
        , testProperty
            "underfunded transaction (outputs exceed inputs) always fails"
            prop_calcMinFeeRecursive_insufficient_funds
        , testProperty
            "Precondition: outputs with tokens not in UTxO returns NonAdaAssetsUnbalanced"
            prop_calcMinFeeRecursive_non_ada_unbalanced
        , testProperty
            "Case 1: output with multi-assets below min UTxO returns MinUTxONotMet"
            prop_calcMinFeeRecursive_min_utxo_not_met
        , testProperty
            "Case 2: transaction with no outputs creates change output"
            prop_calcMinFeeRecursive_no_tx_outs
        , testProperty
            "Tiny surplus consumed by fee increase yields NotEnoughAdaForChangeOutput"
            prop_calcMinFeeRecursive_tiny_surplus_not_enough_ada
        , testProperty
            "withdrawal-funded transaction (output > input) succeeds"
            prop_calcMinFeeRecursive_withdrawal_funded_succeeds
        , testProperty
            "withdrawal-funded transaction with tiny input fails gracefully"
            prop_calcMinFeeRecursive_withdrawal_tiny_input_fails
        ]
    , testGroup
        "makeTransactionBodyAutoBalance"
        [ testProperty
            "underfunded transaction fails with TxBodyErrorBalanceNegative"
            prop_makeTransactionBodyAutoBalance_balance_negative
        ]
    , testGroup
        "evaluateTransaction"
        [ testProperty
            "well-funded simple tx returns positive fee"
            prop_evaluateTransaction_positive_fee
        , testProperty
            "script-free tx returns empty execution units map"
            prop_evaluateTransaction_no_scripts_empty_exunits
        , testProperty
            "balanced tx has mempty balance"
            prop_evaluateSignedTx_balanced_mempty
        ]
    ]

-- ---------------------------------------------------------------------------
-- Generators
-- ---------------------------------------------------------------------------

-- | Generates a simple lovelace-only transaction with generous UTxO funding.
-- @sendCoin@ values span different CBOR unsigned integer encoding sizes
-- (5-byte and 9-byte), including values near the 2^32 boundary.
-- The minimum UTxO requirement (~1 ADA) prevents values in the 1–3 byte ranges.
-- @fundingCoin = sendCoin + surplus@, where surplus is 2–17 ADA, ensuring the
-- transaction is always well-funded for any realistic fee.
genFundedSimpleTx
  :: Exp.Era era
  -> Gen
       ( Exp.UnsignedTx (Exp.LedgerEra era)
       , L.UTxO (Exp.LedgerEra era)
       , L.Addr
       )
genFundedSimpleTx era = do
  let sbe = convert era
  txIn <- genTxIn
  addr <- Api.toShelleyAddr <$> genAddressInEra sbe
  changeAddr <- Api.toShelleyAddr <$> genAddressInEra sbe
  -- CBOR unsigned integer encoding sizes: ≤23 → 1 byte, ≤255 → 2 bytes,
  -- ≤65535 → 3 bytes, ≤4294967295 → 5 bytes, >4294967295 → 9 bytes.
  -- Minimum UTxO (~1 ADA = 1_000_000 lovelace) constrains sendCoin to
  -- the 5-byte range at minimum.
  sendCoin <-
    L.Coin
      <$> Gen.choice
        [ Gen.integral (Range.linear 1_000_000 3_000_000) -- 5-byte CBOR (low)
        , Gen.integral (Range.linear 100_000_000 500_000_000) -- 5-byte CBOR (mid)
        , Gen.integral (Range.linear 4_290_000_000 4_300_000_000) -- near 2^32 boundary
        , Gen.integral (Range.linear 5_000_000_000 10_000_000_000) -- 9-byte CBOR
        ]
  -- Surplus of 2–17 ADA ensures funding always exceeds sendCoin + fees.
  -- Fees are typically < 1000 lovelace with test protocol parameters
  -- (feePerByte=1, feeFixed=0).
  surplus <- L.Coin <$> Gen.integral (Range.linear 2_000_000 17_000_000)
  let fundingCoin = sendCoin + surplus
  let ledgerTxIn = Api.toShelleyTxIn txIn
      fundingTxOut =
        Exp.obtainCommonConstraints era $
          L.mkBasicTxOut addr (L.MaryValue fundingCoin mempty)
      utxo = L.UTxO $ Map.singleton ledgerTxIn fundingTxOut
      sendTxOut =
        Exp.obtainCommonConstraints era $
          Exp.TxOut $
            L.mkBasicTxOut addr (L.MaryValue sendCoin mempty)
      txBodyContent =
        Exp.defaultTxBodyContent
          & Exp.setTxIns [(txIn, Exp.AnyKeyWitnessPlaceholder)]
          & Exp.setTxOuts [sendTxOut]
          & Exp.setTxFee 0
  tx <- failEitherWith (("makeUnsignedTx: " <>) . show) $ Exp.makeUnsignedTx era txBodyContent
  return (tx, utxo, changeAddr)

-- | Like 'genFundedSimpleTx' but the UTxO and output both carry native tokens.
-- The output sends all tokens; the surplus ADA goes to the change output.
-- This exercises Case 2's multi-asset handling on the success path.
genFundedMultiAssetTx
  :: Exp.Era era
  -> Gen
       ( Exp.UnsignedTx (Exp.LedgerEra era)
       , L.UTxO (Exp.LedgerEra era)
       , L.Addr
       )
genFundedMultiAssetTx era = do
  let sbe = convert era
  txIn <- genTxIn
  addr <- Api.toShelleyAddr <$> genAddressInEra sbe
  changeAddr <- Api.toShelleyAddr <$> genAddressInEra sbe
  sendCoin <- L.Coin <$> Gen.integral (Range.linear 2_000_000 5_000_000)
  surplus <- L.Coin <$> Gen.integral (Range.linear 2_000_000 17_000_000)
  tokenQty <- Gen.integral (Range.linear 1 1_000_000)
  let fundingCoin = sendCoin + surplus
      policyId = L.PolicyID $ L.ScriptHash "1c14ee8e58fbcbd48dc7367c95a63fd1d937ba989820015db16ac7e5"
      multiAsset = L.MultiAsset $ Map.singleton policyId (Map.singleton (Mary.AssetName "testtoken") tokenQty)
      ledgerTxIn = Api.toShelleyTxIn txIn
      fundingTxOut =
        Exp.obtainCommonConstraints era $
          L.mkBasicTxOut addr (L.MaryValue fundingCoin multiAsset)
      utxo = L.UTxO $ Map.singleton ledgerTxIn fundingTxOut
      sendTxOut =
        Exp.obtainCommonConstraints era $
          Exp.TxOut $
            L.mkBasicTxOut addr (L.MaryValue sendCoin multiAsset)
      txBodyContent =
        Exp.defaultTxBodyContent
          & Exp.setTxIns [(txIn, Exp.AnyKeyWitnessPlaceholder)]
          & Exp.setTxOuts [sendTxOut]
          & Exp.setTxFee 0
  tx <- failEitherWith (("makeUnsignedTx: " <>) . show) $ Exp.makeUnsignedTx era txBodyContent
  return (tx, utxo, changeAddr)

-- | Generates a simple lovelace-only transaction where the single output
-- (5-10 ADA) greatly exceeds the UTxO funding (0.5-2 ADA).
genUnderfundedTx
  :: forall era
   . Exp.Era era
  -> Gen
       ( Exp.UnsignedTx (Exp.LedgerEra era)
       , L.UTxO (Exp.LedgerEra era)
       , L.Addr
       )
genUnderfundedTx era = do
  let sbe = convert era
  txIn <- genTxIn
  addr <- Api.toShelleyAddr <$> genAddressInEra sbe
  changeAddr <- Api.toShelleyAddr <$> genAddressInEra sbe
  fundingCoin <- L.Coin <$> Gen.integral (Range.linear 500_000 2_000_000)
  sendCoin <- L.Coin <$> Gen.integral (Range.linear 5_000_000 10_000_000)
  let ledgerTxIn = Api.toShelleyTxIn txIn
      fundingTxOut =
        Exp.obtainCommonConstraints era $
          L.mkBasicTxOut addr (L.MaryValue fundingCoin mempty)
      utxo = L.UTxO $ Map.singleton ledgerTxIn fundingTxOut
      sendTxOut =
        Exp.obtainCommonConstraints era $
          Exp.TxOut $
            L.mkBasicTxOut addr (L.MaryValue sendCoin mempty)
      txBodyContent =
        Exp.defaultTxBodyContent
          & Exp.setTxIns [(txIn, Exp.AnyKeyWitnessPlaceholder)]
          & Exp.setTxOuts [sendTxOut]
          & Exp.setTxFee 0
  tx <- failEitherWith (("makeUnsignedTx: " <>) . show) $ Exp.makeUnsignedTx era txBodyContent
  return (tx, utxo, changeAddr)

-- | Generates a transaction whose output demands a native token that does
-- not exist in the UTxO (which is ADA-only). This guarantees a negative
-- multi-asset balance, triggering the multi-asset precondition check ('NonAdaAssetsUnbalanced').
genNonAdaUnbalancedTx
  :: Exp.Era era
  -> Gen
       ( Exp.UnsignedTx (Exp.LedgerEra era)
       , L.UTxO (Exp.LedgerEra era)
       , L.Addr
       )
genNonAdaUnbalancedTx era = do
  let sbe = convert era
  txIn <- genTxIn
  addr <- Api.toShelleyAddr <$> genAddressInEra sbe
  changeAddr <- Api.toShelleyAddr <$> genAddressInEra sbe
  fundingCoin <- L.Coin <$> Gen.integral (Range.linear 5_000_000 20_000_000)
  sendCoin <- L.Coin <$> Gen.integral (Range.linear 1_000_000 3_000_000)
  tokenQty <- Gen.integral (Range.linear 1 1_000_000)
  let ledgerTxIn = Api.toShelleyTxIn txIn
      fundingTxOut =
        Exp.obtainCommonConstraints era $
          L.mkBasicTxOut addr (L.MaryValue fundingCoin mempty)
      utxo = L.UTxO $ Map.singleton ledgerTxIn fundingTxOut
      -- Output demands tokens that don't exist in the ADA-only UTxO
      policyId = L.PolicyID $ L.ScriptHash "1c14ee8e58fbcbd48dc7367c95a63fd1d937ba989820015db16ac7e5"
      sendValue =
        L.MaryValue sendCoin $
          L.MultiAsset $
            Map.singleton policyId (Map.singleton (Mary.AssetName "testtoken") tokenQty)
      sendTxOut =
        Exp.obtainCommonConstraints era $
          Exp.TxOut $
            L.mkBasicTxOut addr sendValue
      txBodyContent =
        Exp.defaultTxBodyContent
          & Exp.setTxIns [(txIn, Exp.AnyKeyWitnessPlaceholder)]
          & Exp.setTxOuts [sendTxOut]
          & Exp.setTxFee 0
  tx <- failEitherWith (("makeUnsignedTx: " <>) . show) $ Exp.makeUnsignedTx era txBodyContent
  return (tx, utxo, changeAddr)

-- | Generates a two-output transaction where the second output carries native
-- tokens with only 1000 lovelace — well below the minimum UTxO for a
-- token-bearing output. The surplus ADA is distributed to the first
-- output (Case 2), so the second output stays below minimum, triggering
-- Case 1 ('MinUTxONotMet').
genMinUTxOViolatingTx
  :: Exp.Era era
  -> Gen
       ( Exp.UnsignedTx (Exp.LedgerEra era)
       , L.UTxO (Exp.LedgerEra era)
       , L.Addr
       )
genMinUTxOViolatingTx era = do
  let sbe = convert era
  txIn <- genTxIn
  addr <- Api.toShelleyAddr <$> genAddressInEra sbe
  changeAddr <- Api.toShelleyAddr <$> genAddressInEra sbe
  tokenQty <- Gen.integral (Range.linear 1 1_000_000)
  let policyId = L.PolicyID $ L.ScriptHash "1c14ee8e58fbcbd48dc7367c95a63fd1d937ba989820015db16ac7e5"
      multiAsset = L.MultiAsset $ Map.singleton policyId (Map.singleton (Mary.AssetName "testtoken") tokenQty)
      -- UTxO has plenty of ADA and the same tokens
      fundingValue = L.MaryValue (L.Coin 5_000_000) multiAsset
      ledgerTxIn = Api.toShelleyTxIn txIn
      fundingTxOut =
        Exp.obtainCommonConstraints era $
          L.mkBasicTxOut addr fundingValue
      utxo = L.UTxO $ Map.singleton ledgerTxIn fundingTxOut
      -- Output 1: ADA only, will receive surplus via balanceTxOuts
      sendTxOut1 =
        Exp.obtainCommonConstraints era $
          Exp.TxOut $
            L.mkBasicTxOut addr (L.MaryValue (L.Coin 1_000_000) mempty)
      -- Output 2: tokens with tiny ADA (below min UTxO)
      sendTxOut2 =
        Exp.obtainCommonConstraints era $
          Exp.TxOut $
            L.mkBasicTxOut addr (L.MaryValue (L.Coin 1_000) multiAsset)
      txBodyContent =
        Exp.defaultTxBodyContent
          & Exp.setTxIns [(txIn, Exp.AnyKeyWitnessPlaceholder)]
          & Exp.setTxOuts [sendTxOut1, sendTxOut2]
          & Exp.setTxFee 0
  tx <- failEitherWith (("makeUnsignedTx: " <>) . show) $ Exp.makeUnsignedTx era txBodyContent
  return (tx, utxo, changeAddr)

-- | Generates a transaction with inputs but no outputs. Once the fee
-- converges (Case 3), the positive surplus triggers Case 2, and
-- 'balanceTxOuts' creates a change output with the surplus.
genNoOutputsTx
  :: Exp.Era era
  -> Gen
       ( Exp.UnsignedTx (Exp.LedgerEra era)
       , L.UTxO (Exp.LedgerEra era)
       , L.Addr
       )
genNoOutputsTx era = do
  let sbe = convert era
  txIn <- genTxIn
  addr <- Api.toShelleyAddr <$> genAddressInEra sbe
  changeAddr <- Api.toShelleyAddr <$> genAddressInEra sbe
  fundingCoin <- L.Coin <$> Gen.integral (Range.linear 5_000_000 20_000_000)
  let ledgerTxIn = Api.toShelleyTxIn txIn
      fundingTxOut =
        Exp.obtainCommonConstraints era $
          L.mkBasicTxOut addr (L.MaryValue fundingCoin mempty)
      utxo = L.UTxO $ Map.singleton ledgerTxIn fundingTxOut
      txBodyContent =
        Exp.defaultTxBodyContent
          & Exp.setTxIns [(txIn, Exp.AnyKeyWitnessPlaceholder)]
          & Exp.setTxOuts [] -- No outputs!
          & Exp.setTxFee 0
  tx <- failEitherWith (("makeUnsignedTx: " <>) . show) $ Exp.makeUnsignedTx era txBodyContent
  return (tx, utxo, changeAddr)

-- | Generates a transaction designed to trigger 'NotEnoughAdaForChangeOutput'.
--
-- The generator:
--
--   1. Generates random tx parts (TxIn, addresses, send amount 2–5 ADA).
--   2. Builds a 1-output tx with fee=0.
--   3. Computes F1 via 'calcMinFeeTx' — the actual min fee for this specific
--      tx. F1 varies per run because different random addresses have different
--      serialized sizes.
--   4. Picks a surplus in @[F1+4, F1+10]@ — enough for 'calcMinFeeRecursive'
--      to converge the fee and attempt a change output, but not enough to
--      survive the fee increase (~23 bytes) caused by that extra output.
--   5. Builds the UTxO with @sendCoin + surplus@ as the funding amount.
--
-- The property test calls 'calcMinFeeRecursive' on the result and expects
-- failure: the change output drives the balance negative.
genTinySurplusTx
  :: Exp.Era era
  -> Gen
       ( Exp.UnsignedTx (Exp.LedgerEra era)
       , L.UTxO (Exp.LedgerEra era)
       , L.Addr
       )
genTinySurplusTx era = Exp.obtainCommonConstraints era $ do
  let sbe = convert era
  txIn <- genTxIn
  addr <- Api.toShelleyAddr <$> genAddressInEra sbe
  changeAddr <- Api.toShelleyAddr <$> genAddressInEra sbe
  sendCoin <- L.Coin <$> Gen.integral (Range.linear 2_000_000 5_000_000)
  -- Build a preliminary tx to measure F1 (min fee for the 1-output shape).
  -- The fee depends on serialized tx size, which varies with the generated
  -- address structure. We use sendCoin as the funding amount; adding
  -- a few hundred lovelace of surplus won't change the CBOR encoding size
  -- of the multi-million lovelace coin value.
  let ledgerTxIn = Api.toShelleyTxIn txIn
      prelimFundingTxOut =
        L.mkBasicTxOut addr (L.MaryValue sendCoin mempty)
      prelimUtxo = L.UTxO $ Map.singleton ledgerTxIn prelimFundingTxOut
      sendTxOut =
        Exp.TxOut $
          L.mkBasicTxOut addr (L.MaryValue sendCoin mempty)
      txBodyContent =
        Exp.defaultTxBodyContent
          & Exp.setTxIns [(txIn, Exp.AnyKeyWitnessPlaceholder)]
          & Exp.setTxOuts [sendTxOut]
          & Exp.setTxFee 0
  unsignedTx <-
    failEitherWith (("makeUnsignedTx: " <>) . show) $ Exp.makeUnsignedTx era txBodyContent
  let
    -- Compute F1 via case match on UnsignedTx (needed to bring EraTx into scope)
    L.Coin f1 = case unsignedTx of
      Exp.UnsignedTx prelimLedgerTx ->
        L.calcMinFeeTx prelimUtxo (exampleProtocolParamsEra era) prelimLedgerTx 0
  -- Surplus just above F1 but well below F2 (≈ F1 + 23). This is enough
  -- to pass fee convergence but not survive adding a change output.
  surplus <- L.Coin <$> Gen.integral (Range.linear (f1 + 4) (f1 + 10))
  let fundingCoin = sendCoin + surplus
      fundingTxOut =
        L.mkBasicTxOut addr (L.MaryValue fundingCoin mempty)
      utxo = L.UTxO $ Map.singleton ledgerTxIn fundingTxOut
  return (unsignedTx, utxo, changeAddr)

-- | Smoke-test generator: the output exceeds the input, and a withdrawal
-- covers the difference with some surplus. Exercises the withdrawal branch
-- of 'evaluateTransactionBalance' inside 'calcMinFeeRecursive':
-- @change = input + withdrawal - output - fee = surplus - fee@, which
-- stays positive for the ranges chosen below.
genWithdrawalFundedTx
  :: Exp.Era era
  -> Gen
       ( Exp.UnsignedTx (Exp.LedgerEra era)
       , L.UTxO (Exp.LedgerEra era)
       , L.Addr
       )
genWithdrawalFundedTx era = do
  let sbe = convert era
  txIn <- genTxIn
  addr <- Api.toShelleyAddr <$> genAddressInEra sbe
  changeAddr <- Api.toShelleyAddr <$> genAddressInEra sbe
  stakeCred <- genStakeCredential
  fundingCoin <- L.Coin <$> Gen.integral (Range.linear 2_000_000 5_000_000)
  deficit <- L.Coin <$> Gen.integral (Range.linear 1_000_000 5_000_000)
  surplus <- L.Coin <$> Gen.integral (Range.linear 2_000_000 5_000_000)
  let sendCoin = fundingCoin + deficit
      withdrawalCoin = deficit + surplus
      stakeAddr = Api.makeStakeAddress (Api.Testnet $ Api.NetworkMagic 1) stakeCred
      ledgerTxIn = Api.toShelleyTxIn txIn
      fundingTxOut =
        Exp.obtainCommonConstraints era $
          L.mkBasicTxOut addr (L.MaryValue fundingCoin mempty)
      utxo = L.UTxO $ Map.singleton ledgerTxIn fundingTxOut
      sendTxOut =
        Exp.obtainCommonConstraints era $
          Exp.TxOut $
            L.mkBasicTxOut addr (L.MaryValue sendCoin mempty)
      txBodyContent =
        Exp.defaultTxBodyContent
          & Exp.setTxIns [(txIn, Exp.AnyKeyWitnessPlaceholder)]
          & Exp.setTxOuts [sendTxOut]
          & Exp.setTxWithdrawals (Exp.TxWithdrawals [(stakeAddr, withdrawalCoin, Exp.AnyKeyWitnessPlaceholder)])
          & Exp.setTxFee 0
  tx <- failEitherWith (("makeUnsignedTx: " <>) . show) $ Exp.makeUnsignedTx era txBodyContent
  return (tx, utxo, changeAddr)

-- | Like 'genUnderfundedTx' but returns the 'TxBodyContent' rather than an
-- 'UnsignedTx', so it can feed 'makeTransactionBodyAutoBalance'. The output
-- (5-10 ADA) greatly exceeds the UTxO funding (0.5-1 ADA) and no withdrawal
-- makes up the difference, driving @input - output@ negative.
genAutoBalanceNegativeTx
  :: Exp.Era era
  -> Gen
       ( Exp.TxBodyContent (Exp.LedgerEra era)
       , L.UTxO (Exp.LedgerEra era)
       , Api.AddressInEra era
       )
genAutoBalanceNegativeTx era = do
  let sbe = convert era
  txIn <- genTxIn
  addr <- genAddressInEra sbe
  changeAddr <- genAddressInEra sbe
  fundingCoin <- L.Coin <$> Gen.integral (Range.linear 500_000 1_000_000)
  sendCoin <- L.Coin <$> Gen.integral (Range.linear 5_000_000 10_000_000)
  let ledgerTxIn = Api.toShelleyTxIn txIn
      shelleyAddr = Api.toShelleyAddr addr
      fundingTxOut =
        Exp.obtainCommonConstraints era $
          L.mkBasicTxOut shelleyAddr (L.MaryValue fundingCoin mempty)
      utxo = L.UTxO $ Map.singleton ledgerTxIn fundingTxOut
      sendTxOut =
        Exp.obtainCommonConstraints era $
          Exp.TxOut $
            L.mkBasicTxOut shelleyAddr (L.MaryValue sendCoin mempty)
      txBodyContent =
        Exp.defaultTxBodyContent
          & Exp.setTxIns [(txIn, Exp.AnyKeyWitnessPlaceholder)]
          & Exp.setTxOuts [sendTxOut]
          & Exp.setTxFee 0
  return (txBodyContent, utxo, changeAddr)

-- | Like 'genWithdrawalFundedTx' but with a tiny input (100 lovelace), well
-- below the minimum transaction fee, and no surplus over @output - input@.
-- Initial @change = input + withdrawal - output = input = 100@ (fee is still
-- 0 at this point), but once fee estimation runs, @change = input - fee@
-- goes negative and 'calcMinFeeRecursive' returns a balancing error.
genWithdrawalTinyInputTx
  :: Exp.Era era
  -> Gen
       ( Exp.UnsignedTx (Exp.LedgerEra era)
       , L.UTxO (Exp.LedgerEra era)
       , L.Addr
       )
genWithdrawalTinyInputTx era = do
  let sbe = convert era
  txIn <- genTxIn
  addr <- Api.toShelleyAddr <$> genAddressInEra sbe
  changeAddr <- Api.toShelleyAddr <$> genAddressInEra sbe
  stakeCred <- genStakeCredential
  let inputCoin = L.Coin 100
  withdrawalCoin <- L.Coin <$> Gen.integral (Range.linear 3_000_000 10_000_000)
  let sendCoin = withdrawalCoin
      stakeAddr = Api.makeStakeAddress (Api.Testnet $ Api.NetworkMagic 1) stakeCred
      ledgerTxIn = Api.toShelleyTxIn txIn
      fundingTxOut =
        Exp.obtainCommonConstraints era $
          L.mkBasicTxOut addr (L.MaryValue inputCoin mempty)
      utxo = L.UTxO $ Map.singleton ledgerTxIn fundingTxOut
      sendTxOut =
        Exp.obtainCommonConstraints era $
          Exp.TxOut $
            L.mkBasicTxOut addr (L.MaryValue sendCoin mempty)
      txBodyContent =
        Exp.defaultTxBodyContent
          & Exp.setTxIns [(txIn, Exp.AnyKeyWitnessPlaceholder)]
          & Exp.setTxOuts [sendTxOut]
          & Exp.setTxWithdrawals (Exp.TxWithdrawals [(stakeAddr, withdrawalCoin, Exp.AnyKeyWitnessPlaceholder)])
          & Exp.setTxFee 0
  tx <- failEitherWith (("makeUnsignedTx: " <>) . show) $ Exp.makeUnsignedTx era txBodyContent
  return (tx, utxo, changeAddr)

-- ---------------------------------------------------------------------------
-- Property tests
-- ---------------------------------------------------------------------------

-- | A well-funded transaction (UTxO >> output + fee) always produces a
-- successful, fully balanced result with a positive fee.
prop_calcMinFeeRecursive_well_funded_succeeds :: Property
prop_calcMinFeeRecursive_well_funded_succeeds = H.property $ do
  (unsignedTx, utxo, changeAddr) <- H.forAll $ genFundedSimpleTx Exp.ConwayEra
  Exp.UnsignedTx resultLedgerTx <-
    H.leftFail $
      Exp.calcMinFeeRecursive changeAddr unsignedTx utxo exampleProtocolParams mempty mempty 0
  let resultFee = resultLedgerTx ^. L.bodyTxL . L.feeTxBodyL
  H.assertWith resultFee (> L.Coin 0)
  -- The resulting transaction must be fully balanced (zero balance).
  let balance =
        UnexportedLedger.evalBalanceTxBody
          exampleProtocolParams
          (const Nothing)
          (const False)
          utxo
          (resultLedgerTx ^. L.bodyTxL)
  balance H.=== mempty

-- | Like 'prop_calcMinFeeRecursive_well_funded_succeeds' but the UTxO and
-- output carry native tokens. Verifies that surplus tokens are correctly
-- distributed to the change output and the result is fully balanced.
prop_calcMinFeeRecursive_well_funded_multi_asset :: Property
prop_calcMinFeeRecursive_well_funded_multi_asset = H.property $ do
  (unsignedTx, utxo, changeAddr) <- H.forAll $ genFundedMultiAssetTx Exp.ConwayEra
  Exp.UnsignedTx resultLedgerTx <-
    H.leftFail $
      Exp.calcMinFeeRecursive changeAddr unsignedTx utxo exampleProtocolParams mempty mempty 0
  let resultFee = resultLedgerTx ^. L.bodyTxL . L.feeTxBodyL
  H.assertWith resultFee (> L.Coin 0)
  let balance =
        UnexportedLedger.evalBalanceTxBody
          exampleProtocolParams
          (const Nothing)
          (const False)
          utxo
          (resultLedgerTx ^. L.bodyTxL)
  balance H.=== mempty

-- | 'calcMinFeeRecursive' is idempotent: applying it to its own result
-- yields the same 'UnsignedTx'.  This confirms the fee has reached a
-- fixed point and that any surplus was already distributed to outputs.
prop_calcMinFeeRecursive_fee_fixpoint :: Property
prop_calcMinFeeRecursive_fee_fixpoint = H.property $ do
  (unsignedTx, utxo, changeAddr) <- H.forAll $ genFundedSimpleTx Exp.ConwayEra
  resultTx <-
    H.leftFail $
      Exp.calcMinFeeRecursive changeAddr unsignedTx utxo exampleProtocolParams mempty mempty 0
  secondResult <-
    H.leftFail $
      Exp.calcMinFeeRecursive changeAddr resultTx utxo exampleProtocolParams mempty mempty 0
  resultTx H.=== secondResult

-- | When the outputs exceed the UTxO value the function returns
-- 'Left (NotEnoughAdaForNewOutput _)' with a negative deficit coin.
prop_calcMinFeeRecursive_insufficient_funds :: Property
prop_calcMinFeeRecursive_insufficient_funds = H.property $ do
  (unsignedTx, utxo, changeAddr) <- H.forAll $ genUnderfundedTx Exp.ConwayEra
  case Exp.calcMinFeeRecursive changeAddr unsignedTx utxo exampleProtocolParams mempty mempty 0 of
    Left (Exp.NotEnoughAdaForNewOutput deficit) -> H.assertWith deficit (< L.Coin 0)
    Left Exp.NonAdaAssetsUnbalanced{} -> H.annotate "Unexpected NonAdaAssetsUnbalanced error" >> H.failure
    Left Exp.MinUTxONotMet{} -> H.annotate "Unexpected MinUTxONotMet error" >> H.failure
    Left Exp.FeeCalculationDidNotConverge -> H.annotate "Unexpected FeeCalculationDidNotConverge error" >> H.failure
    Left err -> H.annotateShow err >> H.failure
    Right _ -> H.failure

-- | When the output demands tokens not present in the ADA-only UTxO,
-- the function returns 'Left (NonAdaAssetsUnbalanced _)'.
prop_calcMinFeeRecursive_non_ada_unbalanced :: Property
prop_calcMinFeeRecursive_non_ada_unbalanced = H.property $ do
  (unsignedTx, utxo, changeAddr) <- H.forAll $ genNonAdaUnbalancedTx Exp.ConwayEra
  case Exp.calcMinFeeRecursive changeAddr unsignedTx utxo exampleProtocolParams mempty mempty 0 of
    Left (Exp.NonAdaAssetsUnbalanced _) -> H.success
    Left Exp.NotEnoughAdaForChangeOutput{} -> H.annotate "Unexpected NotEnoughAdaForChangeOutput" >> H.failure
    Left Exp.NotEnoughAdaForNewOutput{} -> H.annotate "Unexpected NotEnoughAdaForNewOutput" >> H.failure
    Left Exp.MinUTxONotMet{} -> H.annotate "Unexpected MinUTxONotMet" >> H.failure
    Left Exp.FeeCalculationDidNotConverge -> H.annotate "Unexpected FeeCalculationDidNotConverge" >> H.failure
    Right _ -> H.annotate "Expected NonAdaAssetsUnbalanced but got Right" >> H.failure

-- | When a token-bearing output has less ADA than the minimum UTxO,
-- the function returns 'Left (MinUTxONotMet actual required)' with
-- @actual < required@.
prop_calcMinFeeRecursive_min_utxo_not_met :: Property
prop_calcMinFeeRecursive_min_utxo_not_met = H.property $ do
  (unsignedTx, utxo, changeAddr) <- H.forAll $ genMinUTxOViolatingTx Exp.ConwayEra
  case Exp.calcMinFeeRecursive changeAddr unsignedTx utxo exampleProtocolParams mempty mempty 0 of
    Left (Exp.MinUTxONotMet actual required) ->
      H.assertWith (actual, required) (uncurry (<))
    Left Exp.NotEnoughAdaForChangeOutput{} -> H.annotate "Unexpected NotEnoughAdaForChangeOutput" >> H.failure
    Left Exp.NotEnoughAdaForNewOutput{} -> H.annotate "Unexpected NotEnoughAdaForNewOutput" >> H.failure
    Left Exp.NonAdaAssetsUnbalanced{} -> H.annotate "Unexpected NonAdaAssetsUnbalanced" >> H.failure
    Left Exp.FeeCalculationDidNotConverge -> H.annotate "Unexpected FeeCalculationDidNotConverge" >> H.failure
    Right _ -> H.annotate "Expected MinUTxONotMet but got Right" >> H.failure

-- | When the transaction has no outputs, the surplus is sent to a new
-- change output at the provided change address.
prop_calcMinFeeRecursive_no_tx_outs :: Property
prop_calcMinFeeRecursive_no_tx_outs = H.property $ do
  (unsignedTx, utxo, changeAddr) <- H.forAll $ genNoOutputsTx Exp.ConwayEra
  Exp.UnsignedTx resultLedgerTx <-
    H.leftFail $
      Exp.calcMinFeeRecursive changeAddr unsignedTx utxo exampleProtocolParams mempty mempty 0
  let outs = toList $ resultLedgerTx ^. L.bodyTxL . L.outputsTxBodyL
  -- The result should have exactly one output (the change output)
  length outs H.=== 1

-- | When the surplus is just barely enough to cover the initial fee but not
-- the higher fee after adding a change output, the change output balance
-- goes negative and the function returns NotEnoughAdaForChangeOutput.
prop_calcMinFeeRecursive_tiny_surplus_not_enough_ada :: Property
prop_calcMinFeeRecursive_tiny_surplus_not_enough_ada = H.property $ do
  (unsignedTx, utxo, changeAddr) <- H.forAll $ genTinySurplusTx Exp.ConwayEra
  case Exp.calcMinFeeRecursive changeAddr unsignedTx utxo exampleProtocolParams mempty mempty 0 of
    Left (Exp.NotEnoughAdaForChangeOutput deficit) ->
      H.assertWith deficit (< L.Coin 0)
    Left (Exp.MinUTxONotMet actual required) ->
      -- If surplus - F2 >= 0 (barely), we may land in MinUTxONotMet instead.
      -- This is also a valid failure for this border region.
      H.assertWith (actual, required) (uncurry (<))
    Left err -> H.annotateShow err >> H.failure
    Right _ ->
      H.annotate "Expected NotEnoughAdaForChangeOutput or MinUTxONotMet but tx balanced successfully"
        >> H.failure

-- | Smoke test: when the output exceeds the input but a withdrawal covers
-- the difference (plus some surplus), 'calcMinFeeRecursive' balances the
-- transaction successfully and the result is zero-balance with a positive
-- fee.
prop_calcMinFeeRecursive_withdrawal_funded_succeeds :: Property
prop_calcMinFeeRecursive_withdrawal_funded_succeeds = H.property $ do
  (unsignedTx, utxo, changeAddr) <- H.forAll $ genWithdrawalFundedTx Exp.ConwayEra
  Exp.UnsignedTx resultLedgerTx <-
    H.leftFail $
      Exp.calcMinFeeRecursive changeAddr unsignedTx utxo exampleProtocolParams mempty mempty 0
  let resultFee = resultLedgerTx ^. L.bodyTxL . L.feeTxBodyL
  H.assertWith resultFee (> L.Coin 0)
  let balance =
        UnexportedLedger.evalBalanceTxBody
          exampleProtocolParams
          (const Nothing)
          (const False)
          utxo
          (resultLedgerTx ^. L.bodyTxL)
  balance H.=== mempty

-- | When the input is tiny (below the minimum fee) and the withdrawal only
-- covers @output - input@ exactly, 'calcMinFeeRecursive' must fail
-- gracefully after fee estimation with 'NotEnoughAdaForChangeOutput' /
-- 'NotEnoughAdaForNewOutput' / 'MinUTxONotMet' - not a crash.
prop_calcMinFeeRecursive_withdrawal_tiny_input_fails :: Property
prop_calcMinFeeRecursive_withdrawal_tiny_input_fails = H.property $ do
  (unsignedTx, utxo, changeAddr) <- H.forAll $ genWithdrawalTinyInputTx Exp.ConwayEra
  case Exp.calcMinFeeRecursive changeAddr unsignedTx utxo exampleProtocolParams mempty mempty 0 of
    Left (Exp.NotEnoughAdaForChangeOutput deficit) ->
      H.assertWith deficit (< L.Coin 0)
    Left (Exp.NotEnoughAdaForNewOutput deficit) ->
      H.assertWith deficit (< L.Coin 0)
    Left Exp.MinUTxONotMet{} -> H.success
    Left err -> H.annotateShow err >> H.failure
    Right _ ->
      H.annotate "Expected failure (input < fee) but tx balanced successfully"
        >> H.failure

-- | Regression test for the 'Illegal Value in TxOut' crash: prior to the
-- balance-check guard in 'makeTransactionBodyAutoBalance', an underfunded
-- transaction (output > inputs + withdrawals) would reach
-- 'L.mkBasicTxOut' with a negative 'Coin', which calls 'toCompact' and
-- throws an unrecoverable error. The guard now returns
-- 'TxBodyErrorBalanceNegative' with the negative balance instead.
prop_makeTransactionBodyAutoBalance_balance_negative :: Property
prop_makeTransactionBodyAutoBalance_balance_negative = H.property $ do
  (txBodyContent, utxo, changeAddr) <-
    H.forAllWith (const "<TxBodyContent, UTxO, AddressInEra>") $
      genAutoBalanceNegativeTx Exp.ConwayEra
  let systemStart = Api.SystemStart $ Time.posixSecondsToUTCTime 0
      epochInfo =
        Api.LedgerEpochInfo $
          Slotting.fixedEpochInfo (Slotting.EpochSize 100) (Slotting.mkSlotLength 1000)
  case Exp.makeTransactionBodyAutoBalance
    systemStart
    epochInfo
    exampleProtocolParams
    mempty
    mempty
    utxo
    txBodyContent
    changeAddr
    Nothing of
    Left (Exp.TxBodyErrorBalanceNegative coin _multiAsset) ->
      H.assertWith coin (< L.Coin 0)
    Left err -> H.annotateShow err >> H.failure
    Right _ ->
      H.annotate "Expected TxBodyErrorBalanceNegative but tx balanced successfully"
        >> H.failure

-- | A well-funded transaction returns a positive fee from 'evaluateTransaction'.
prop_evaluateTransaction_positive_fee :: Property
prop_evaluateTransaction_positive_fee = H.property $ do
  (result, _utxo, _unsignedTx, _changeAddr) <- H.forAll evalSimpleTx
  H.assertWith (Exp.txEvalFee result) (> L.Coin 0)

-- | A script-free transaction returns an empty execution units map.
prop_evaluateTransaction_no_scripts_empty_exunits :: Property
prop_evaluateTransaction_no_scripts_empty_exunits = H.property $ do
  (result, _utxo, _unsignedTx, _changeAddr) <- H.forAll evalSimpleTx
  H.assertWith (Exp.txEvalExecutionUnits result) Map.null

-- | A transaction whose fee was set by 'calcMinFeeRecursive' has mempty
-- balance when checked by 'evaluateSignedTx', because the generator
-- produces a tx whose inputs exactly cover outputs plus the computed fee.
-- Witnesses are irrelevant for balance checking, so we wrap the ledger tx
-- directly in 'SignedTx' without calling 'signTx'.
prop_evaluateSignedTx_balanced_mempty :: Property
prop_evaluateSignedTx_balanced_mempty = H.property $ do
  (unsignedTx, utxo, changeAddr) <- H.forAll $ genFundedSimpleTx Exp.ConwayEra
  Exp.UnsignedTx balancedLedgerTx <-
    H.leftFail $
      Exp.calcMinFeeRecursive changeAddr unsignedTx utxo exampleProtocolParams mempty mempty 0
  let signedTx = Exp.SignedTx balancedLedgerTx
      systemStart = Api.SystemStart $ Time.posixSecondsToUTCTime 0
      epochInfo =
        Api.LedgerEpochInfo $
          Slotting.fixedEpochInfo (Slotting.EpochSize 100) (Slotting.mkSlotLength 1000)
      result =
        Exp.evaluateSignedTx
          systemStart
          epochInfo
          exampleProtocolParams
          mempty
          mempty
          utxo
          signedTx
  Exp.txEvalBalance result H.=== mempty

-- | Evaluate a simple signed transaction, returning the result and UTxO.
evalSimpleTx
  :: Gen
       ( Exp.TxEvaluationResult (Exp.LedgerEra Exp.ConwayEra)
       , L.UTxO (Exp.LedgerEra Exp.ConwayEra)
       , Exp.UnsignedTx (Exp.LedgerEra Exp.ConwayEra)
       , L.Addr
       )
evalSimpleTx = do
  (unsignedTx, utxo, changeAddr) <- genFundedSimpleTx Exp.ConwayEra
  let signedTx = Exp.signTx Exp.ConwayEra [] [] unsignedTx
      systemStart = Api.SystemStart $ Time.posixSecondsToUTCTime 0
      epochInfo =
        Api.LedgerEpochInfo $
          Slotting.fixedEpochInfo (Slotting.EpochSize 100) (Slotting.mkSlotLength 1000)
      result =
        Exp.evaluateSignedTx
          systemStart
          epochInfo
          exampleProtocolParams
          mempty
          mempty
          utxo
          signedTx
  pure (result, utxo, unsignedTx, changeAddr)

-- | Regression test for the bug where 'mapScriptWitnessesCertificates' silently
-- dropped certs stored with a @Nothing@ witness (e.g. shelley stake registration
-- certificates) when rebuilding 'TxCertificates' during fee balancing.
--
-- We build a 'TxCertificates' covering a range of Conway cert types — some that
-- store as @Nothing@ in the OMap (no witness required) and some as @Just@ (witness
-- required) — then verify that all survive 'substituteExecutionUnits' unchanged.
prop_substituteExecutionUnits_preserves_certs :: Property
prop_substituteExecutionUnits_preserves_certs = H.property $ do
  (allCerts, _) <- H.forAll genShuffledCertsWithCount
  let inputCerts = Exp.mkTxCertificates Exp.ConwayEra allCerts
      txBodyContent =
        Exp.defaultTxBodyContent
          & Exp.setTxCertificates inputCerts
  result <- H.evalEither $ Exp.substituteExecutionUnits Map.empty txBodyContent
  Exp.txCertificates result H.=== inputCerts

-- | 'collectTxBodyScriptWitnesses' must return exactly the script-witnessed
-- certs (1 simple script witness in the generator) and must not include
-- unwitnessed or key-witnessed certs as spurious script witnesses.
prop_collectTxBodyScriptWitnesses_ignores_unwitnessed_certs :: Property
prop_collectTxBodyScriptWitnesses_ignores_unwitnessed_certs = H.property $ do
  (allCerts, _) <- H.forAll genShuffledCertsWithCount
  let inputCerts = Exp.mkTxCertificates Exp.ConwayEra allCerts
      txBodyContent =
        Exp.defaultTxBodyContent
          & Exp.setTxCertificates inputCerts
      scriptWitnesses = Exp.collectTxBodyScriptWitnesses txBodyContent
  length scriptWitnesses H.=== 1

-- | 'createCompatibleTx' must include every certificate (both witnessed and
-- unwitnessed) in the resulting ledger transaction body. This ensures that
-- the @setCerts@ / @convCertificates@ path does not silently drop certs.
prop_createCompatibleTx_preserves_all_certs :: Property
prop_createCompatibleTx_preserves_all_certs = H.property $ do
  (allCerts, expectedCount) <- H.forAll genShuffledCertsWithCount
  let sbe = convert Exp.ConwayEra
      inputCerts = Exp.mkTxCertificates Exp.ConwayEra allCerts
  Api.ShelleyTx _ ledgerTx <-
    H.evalEither $
      createCompatibleTx sbe (defaultCompatibleTxBodyContent sbe){compatibleTxCertificates = inputCerts}
  let bodyCerts = ledgerTx ^. L.bodyTxL . L.certsTxBodyL
  Seq.length bodyCerts H.=== expectedCount

-- | 'createCompatibleTx' must index spending redeemer pointers by the
-- input's position in the final, sorted ledger input set, never by the
-- order the caller supplied inputs in. The ledger stores tx inputs in a
-- 'Data.Set.Set', not a list, so swapping the argument order of two
-- plutus-witnessed inputs must not change which redeemer lands at which
-- index.
prop_createCompatibleTx_redeemer_indices_follow_sorted_inputs :: Property
prop_createCompatibleTx_redeemer_indices_follow_sorted_inputs = H.property $ do
  txIn1@(Api.TxIn txId (Api.TxIx txIx)) <- H.forAll genTxIn
  let txIn2 = Api.TxIn txId (Api.TxIx (txIx + 1))

      sbe = convert Exp.ConwayEra

      mkRedeemer :: Integer -> Api.HashableScriptData
      mkRedeemer n = Api.unsafeHashableScriptData $ Api.ScriptDataConstructor n []

      mkSpendingWitness redeemer =
        Exp.AnyPlutusScriptWitness $
          AnyPlutusSpendingScriptWitness $
            PlutusSpendingScriptWitnessV3 $
              Exp.PlutusScriptWitness
                Plutus.SPlutusV3
                (Exp.PReferenceScript txIn1)
                Exp.NoScriptDatum
                redeemer
                (Api.ExecutionUnits 0 0)

      ins =
        [ (txIn1, mkSpendingWitness (mkRedeemer 1))
        , (txIn2, mkSpendingWitness (mkRedeemer 2))
        ]

      baseContent =
        (defaultCompatibleTxBodyContent sbe)
          { compatibleTxProtocolParams = Just exampleProtocolParams
          , compatibleTxCertificates = Exp.mkTxCertificates Exp.ConwayEra []
          }

      build orderedIns = createCompatibleTx sbe baseContent{compatibleTxIns = orderedIns}

  Api.ShelleyTx _ forwardTx <- H.evalEither $ build ins
  Api.ShelleyTx _ reversedTx <- H.evalEither $ build (reverse ins)

  let forwardRedeemers = forwardTx ^. L.witsTxL . Alonzo.rdmrsTxWitsL
      reversedRedeemers = reversedTx ^. L.witsTxL . Alonzo.rdmrsTxWitsL

  -- Order-invariance is the core claim: if indices were derived from
  -- argument-list position instead of sorted 'TxIn' order, reversing the
  -- argument order would swap which redeemer lands at which index.
  forwardRedeemers H.=== reversedRedeemers

  -- Pin down the exact mapping too, so a different bug (e.g. always
  -- indexing everything to 0) cannot slip through the check above.
  --
  -- 'txIn1' and 'txIn2' share a 'TxId' and only differ by 'TxIx' ('ix' vs
  -- 'ix + 1'), so 'txIn1' sorts first: its redeemer must land at index 0,
  -- 'txIn2''s at index 1.
  let expectedRedeemers =
        L.Redeemers $
          Map.fromList
            [
              ( L.ConwaySpending (L.AsIx 0)
              , (Api.toAlonzoData (mkRedeemer 1), Api.toAlonzoExUnits (Api.ExecutionUnits 0 0))
              )
            ,
              ( L.ConwaySpending (L.AsIx 1)
              , (Api.toAlonzoData (mkRedeemer 2), Api.toAlonzoExUnits (Api.ExecutionUnits 0 0))
              )
            ]

  forwardRedeemers H.=== expectedRedeemers

-- | An inline (non-reference) plutus spending script witness must land in
-- the resulting ledger transaction's witness set ('L.scriptTxWitsL').
prop_createCompatibleTx_inline_plutus_script_in_witness_set :: Property
prop_createCompatibleTx_inline_plutus_script_in_witness_set = H.property $ do
  txIn <- H.forAll genTxIn
  scriptInEra <- H.forAll genPlutusScriptInEra
  let sbe = convert Exp.ConwayEra

      redeemer = Api.unsafeHashableScriptData $ Api.ScriptDataConstructor 0 []

      witness =
        Exp.AnyPlutusScriptWitness $
          AnyPlutusSpendingScriptWitness $
            PlutusSpendingScriptWitnessV3 $
              Exp.PlutusScriptWitness
                Plutus.SPlutusV3
                (Exp.PScript scriptInEra)
                Exp.NoScriptDatum
                redeemer
                (Api.ExecutionUnits 0 0)

  Api.ShelleyTx _ ledgerTx <-
    H.evalEither $
      createCompatibleTx sbe $
        (defaultCompatibleTxBodyContent sbe)
          { compatibleTxIns = [(txIn, witness)]
          , compatibleTxProtocolParams = Just exampleProtocolParams
          , compatibleTxCertificates = Exp.mkTxCertificates Exp.ConwayEra []
          }

  let scriptWits = ledgerTx ^. L.witsTxL . L.scriptTxWitsL
      expectedHash = L.hashScript $ plutusScriptInEraToScript scriptInEra

  H.assertWith scriptWits (not . null)
  H.assertWith scriptWits (Map.member expectedHash)

-- | 'createCompatibleTx' must reject plutus script witnesses when no
-- protocol parameters were supplied, since those are required to compute
-- the script integrity hash.
prop_createCompatibleTx_missing_pparams_with_plutus_witness :: Property
prop_createCompatibleTx_missing_pparams_with_plutus_witness = H.property $ do
  txIn <- H.forAll genTxIn
  let sbe = convert Exp.ConwayEra

      redeemer = Api.unsafeHashableScriptData $ Api.ScriptDataConstructor 0 []

      witness =
        Exp.AnyPlutusScriptWitness $
          AnyPlutusSpendingScriptWitness $
            PlutusSpendingScriptWitnessV3 $
              Exp.PlutusScriptWitness
                Plutus.SPlutusV3
                (Exp.PReferenceScript txIn)
                Exp.NoScriptDatum
                redeemer
                (Api.ExecutionUnits 0 0)

  case createCompatibleTx sbe $
    (defaultCompatibleTxBodyContent sbe)
      { compatibleTxIns = [(txIn, witness)]
      , compatibleTxCertificates = Exp.mkTxCertificates Exp.ConwayEra []
      } of
    Left CompatibleTxMissingScriptIntegrityPParams -> H.success
    Left err -> H.annotateShow err >> H.failure
    Right _ ->
      H.annotate "Expected CompatibleTxMissingScriptIntegrityPParams but tx was built successfully"
        >> H.failure

-- | 'createCompatibleTx' must thread the collateral inputs, the validity
-- interval's upper bound, and metadata parts of 'CompatibleTxBodyContent'
-- into the resulting transaction (both the aux data hash on the body and
-- the aux data itself on the tx).
prop_createCompatibleTx_extra_content_is_threaded_through :: Property
prop_createCompatibleTx_extra_content_is_threaded_through = H.property $ do
  collateralTxIn <- H.forAll genTxIn
  let sbe = convert Exp.ConwayEra

      upperBound = Slotting.SlotNo 100

      metadata = Api.TxMetadata $ Map.fromList [(1, Api.TxMetaText "createCompatibleTx")]

  Api.ShelleyTx _ ledgerTx <-
    H.evalEither $
      createCompatibleTx sbe $
        (defaultCompatibleTxBodyContent sbe)
          { compatibleTxInsCollateral = [collateralTxIn]
          , compatibleTxValidityUpperBound = Just upperBound
          , compatibleTxMetadata = Api.TxMetadataInEra sbe metadata
          , compatibleTxCertificates = Exp.mkTxCertificates Exp.ConwayEra []
          }

  let ledgerBody = ledgerTx ^. L.bodyTxL

  ledgerBody ^. UnexportedLedger.collateralInputsTxBodyL
    H.=== Set.singleton (Api.toShelleyTxIn collateralTxIn)
  ledgerBody ^. UnexportedLedger.vldtTxBodyL . UnexportedLedger.invalidHereAfterL
    H.=== L.SJust upperBound
  H.assertWith (ledgerBody ^. UnexportedLedger.auxDataHashTxBodyL) (isJust . L.strictMaybeToMaybe)
  H.assertWith (ledgerTx ^. UnexportedLedger.auxDataTxL) (isJust . L.strictMaybeToMaybe)

-- ---------------------------------------------------------------------------
-- Shared cert generators
-- ---------------------------------------------------------------------------

-- | Generate a shuffled list of Conway certs (mix of witnessed and unwitnessed,
-- with both key and simple script witnesses) along with the expected count.
genShuffledCertsWithCount
  :: Gen
       ( [(Exp.Certificate (Exp.LedgerEra Exp.ConwayEra), Exp.AnyWitness (Exp.LedgerEra Exp.ConwayEra))]
       , Int
       )
genShuffledCertsWithCount = do
  stakeCred1 <- genStakeCredential
  stakeCred2 <- genStakeCredential
  stakeCred3 <- genStakeCredential
  stakeCred4 <- genStakeCredential
  refTxIn <- genTxIn
  let
    shelleyCred1 = Api.toShelleyStakeCredential stakeCred1
    shelleyCred2 = Api.toShelleyStakeCredential stakeCred2
    shelleyCred3 = Api.toShelleyStakeCredential stakeCred3
    shelleyCred4 = Api.toShelleyStakeCredential stakeCred4

    -- Unwitnessed: ConwayRegCert without deposit
    regCert =
      Exp.Certificate $
        L.ConwayTxCertDeleg
          (L.ConwayRegCert shelleyCred1 L.SNothing)

    -- Key-witnessed: ConwayUnRegCert with deposit
    unRegCert =
      Exp.Certificate $
        L.ConwayTxCertDeleg
          (L.ConwayUnRegCert shelleyCred2 (L.SJust (L.Coin 2_000_000)))

    -- Simple-script-witnessed (via reference input): ConwayDelegCert
    delegCert =
      Exp.Certificate $
        L.ConwayTxCertDeleg
          (L.ConwayDelegCert shelleyCred3 (L.DelegVote L.DRepAlwaysAbstain))

    -- Key-witnessed: ConwayRegDelegCert
    regDelegCert =
      Exp.Certificate $
        L.ConwayTxCertDeleg
          (L.ConwayRegDelegCert shelleyCred4 (L.DelegVote L.DRepAlwaysAbstain) (L.Coin 2_000_000))

    simpleScriptWitness = Exp.AnySimpleScriptWitness (Exp.SReferenceScript refTxIn)

    allCerts =
      [ (regCert, Exp.AnyKeyWitnessPlaceholder)
      , (unRegCert, Exp.AnyKeyWitnessPlaceholder)
      , (delegCert, simpleScriptWitness)
      , (regDelegCert, Exp.AnyKeyWitnessPlaceholder)
      ]
  shuffled <- Gen.shuffle allCerts
  pure (shuffled, length shuffled)
