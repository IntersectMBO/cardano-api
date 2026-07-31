{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Test.Cardano.Api.Transaction.Collateral
  ( tests
  )
where

import Cardano.Api
import Cardano.Api.Experimental.Tx qualified as Exp
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Tx qualified as Api

import Cardano.Ledger.Alonzo.Core qualified as L
import Cardano.Ledger.Coin qualified as L
import Cardano.Ledger.Mary.Value qualified as L
import Cardano.Ledger.Val ((<->))
import Cardano.Slotting.EpochInfo qualified as CS
import Cardano.Slotting.Slot qualified as CS
import Cardano.Slotting.Time qualified as CS

import Data.Default (def)
import Data.Function
import Data.Map.Strict qualified as M
import Data.Ratio ((%))
import GHC.Exts (IsList (..))
import Lens.Micro ((^.))

import Test.Gen.Cardano.Api.Typed

import Test.Cardano.Api.Orphans ()
import Test.Cardano.Api.Transaction.Utils

import Hedgehog (Property, forAll, (===))
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

prop_make_transaction_body_autobalance_multi_asset_collateral :: Property
prop_make_transaction_body_autobalance_multi_asset_collateral = H.propertyOnce $ do
  let ceo = ConwayEraOnwardsConway
      beo = convert ceo
      sbe = convert beo
      meo = convert beo
      aeo = convert beo

  systemStart <- parseSystemStart "2021-09-01T00:00:00Z"
  let epochInfo = LedgerEpochInfo $ CS.fixedEpochInfo (CS.EpochSize 100) (CS.mkSlotLength 1000)

  pparams <-
    LedgerProtocolParameters
      <$> H.readJsonFileOk "test/cardano-api-test/files/input/protocol-parameters/conway.json"

  (sh@(ScriptHash scriptHash), plutusWitness) <- loadPlutusWitness ceo
  let policyId' = PolicyId sh
  -- one UTXO with an asset - the same we're minting in the transaction
  let utxos = mkUtxos beo (Just scriptHash)
      txInputs = map (,BuildTxWith (KeyWitness KeyWitnessForSpending)) . toList . M.keys . unUTxO $ utxos
      txInputsCollateral = TxInsCollateral aeo $ toList . M.keys . unUTxO $ utxos
  let address = mkAddress sbe scriptHash
  let txMint =
        TxMintValue
          meo
          [(policyId', ([(UnsafeAssetName "eeee", 1)], BuildTxWith plutusWitness))]

  let content =
        defaultTxBodyContent sbe
          & setTxIns txInputs
          & setTxInsCollateral txInputsCollateral
          & setTxOuts (mkTxOutput beo address (L.Coin 2_000_000) Nothing)
          & setTxMintValue txMint
          & setTxProtocolParams (pure $ pure pparams)

  -- autobalanced body has assets and ADA in the change txout
  (BalancedTxBody balancedContent _ _ fee) <-
    H.leftFail $
      makeTransactionBodyAutoBalance
        sbe
        systemStart
        epochInfo
        pparams
        mempty
        mempty
        mempty
        utxos
        content
        address
        Nothing

  scriptWitReqsBalanced <-
    H.evalEither $ collectTxBodyScriptWitnessRequirements aeo balancedContent

  -- check if execution units have changed
  [ ExecutionUnits
      { executionSteps = 84_851_308
      , executionMemory = 325_610
      }
    ]
    === Exp.extractExecutionUnits scriptWitReqsBalanced

  335_299 === fee
  TxReturnCollateral _ (TxOut _ txOutValue _ _) <- H.noteShow $ txReturnCollateral balancedContent
  let assets = [a | a@(AssetId _ _, _) <- toList $ txOutValueToValue txOutValue]
  H.note_ "Check that all assets from UTXO, from the collateral txin, are in the return collateral."
  [(AssetId policyId' $ UnsafeAssetName "eeee", 1)] === assets

-- | Implements collateral validation from Babbage spec, from
-- https://github.com/IntersectMBO/cardano-ledger/releases, babbage-ledger.pdf, Figure 2.
--
-- Seems that under 400 runs the test is not able to detect the violation of properties.
prop_calcReturnAndTotalCollateral :: Property
prop_calcReturnAndTotalCollateral = H.withTests 400 . H.property $ do
  let beo = BabbageEraOnwardsConway
      sbe = convert beo
      era = convert beo
      address = AddressInEra (ShelleyAddressInEra sbe) (ShelleyAddress L.Testnet def L.StakeRefNull)
  feeCoin@(L.Coin fee) <- forAll genLovelace
  pparams <-
    H.readJsonFileOk "test/cardano-api-test/files/input/protocol-parameters/conway.json"
  requiredCollateralPct <- H.noteShow . fromIntegral $ pparams ^. L.ppCollateralPercentageL
  requiredCollateralAda <-
    H.noteShow . L.rationalToCoinViaCeiling $ (fee * requiredCollateralPct) % 100
  -- The ada left for the return collateral output is the collateral ada minus
  -- the required collateral, so 'genLedgerValueForTxOut' alone (1 or 2
  -- lovelace of ada) always lands far below the minimum UTxO value of that
  -- output. Add ada around that minimum so that the property exercises both
  -- sides of the boundary, and shrinks towards it.
  let L.Coin adaOnlyMinUTxO =
        calculateMinimumUTxO sbe pparams $
          TxOut address (lovelaceToTxOutValue sbe 0) TxOutDatumNone ReferenceScriptNone
  totalCollateral <- forAll $ do
    generatedCollateral <- genLedgerValueForTxOut sbe
    -- Keeping some collateral without the extra ada preserves the coverage of
    -- the insufficient collateral case.
    extraAda <-
      Gen.frequency
        [ (1, pure 0)
        , (4, Gen.integral $ Range.linearFrom adaOnlyMinUTxO 0 (2 * adaOnlyMinUTxO))
        ]
    pure $ generatedCollateral <> Api.mkAdaValue sbe (L.Coin extraAda)
  let totalCollateralAda = totalCollateral ^. Api.adaAssetL sbe
  txInsColl <- forAll $ genTxInsCollateral era
  txRetColl <-
    forAll $ Gen.frequency [(4, pure TxReturnCollateralNone), (1, genTxReturnCollateral sbe)]
  txTotColl <- forAll $ Gen.frequency [(4, pure TxTotalCollateralNone), (1, genTxTotalCollateral era)]

  let result =
        calcReturnAndTotalCollateral
          beo
          feeCoin
          pparams
          txInsColl
          txRetColl
          txTotColl
          address
          totalCollateral

  H.annotateShow result

  if
    | txInsColl == TxInsCollateralNone ->
        -- no inputs - no collateral fields; this is the only case producing
        -- two empty fields, the pass-through and computed cases below always
        -- set at least one of them
        Right (TxReturnCollateralNone, TxTotalCollateralNone) === result
    | txRetColl /= TxReturnCollateralNone || txTotColl /= TxTotalCollateralNone ->
        -- got collateral values as function arguments - passed through
        -- unchanged, except that a provided return collateral output must
        -- meet its own minimum UTxO value
        case txRetColl of
          TxReturnCollateral _ rcTxOut@(TxOut _ rcValue _ _)
            | txOutValueToLovelace rcValue < calculateMinimumUTxO sbe pparams rcTxOut ->
                Left
                  ( ReturnCollateralBelowMinimumUTxO
                      (txOutValueToLovelace rcValue)
                      (calculateMinimumUTxO sbe pparams rcTxOut)
                  )
                  === result
          _ -> Right (txRetColl, txTotColl) === result
    | totalCollateralAda < requiredCollateralAda ->
        -- provided collateral not enough, the caller has to raise an error
        Left (InsufficientCollateral totalCollateralAda requiredCollateralAda) === result
    | otherwise ->
        -- no explicit collateral or return collateral was provided, we do the calculation
        case result of
          Left (ReturnCollateralBelowMinimumUTxO returnAda minUTxO) ->
            -- the leftover cannot form a valid return collateral output
            H.diff returnAda (<) minUTxO
          Right (resRetColl, resTotColl) -> do
            let resRetCollValue =
                  mconcat
                    [ txOutValue
                    | TxReturnCollateral _ (TxOut _ (TxOutValueShelleyBased _ txOutValue) _ _) <- pure resRetColl
                    ]
                collBalance = totalCollateral <-> resRetCollValue
            resTotCollValue <-
              H.noteShow $ mconcat [Api.mkAdaValue sbe lovelace | TxTotalCollateral _ lovelace <- pure resTotColl]
            H.annotateShow collBalance
            H.note_ "Check if collateral balance is positive"
            H.assertWith collBalance $ L.pointwise (<=) mempty
            H.note_ "Check if collateral balance contains only ada"
            H.assertWith collBalance L.isAdaOnly
            H.note_ "Check if collateral balance is at least minimum required"
            H.assertWith collBalance $ L.pointwise (<=) (L.inject requiredCollateralAda)
            H.note_ "Check that collateral balance is equal to collateral in tx body"
            resTotCollValue === collBalance
            -- Pin the minimum UTxO boundary of the return collateral output in
            -- both directions: an output that is produced must be spendable,
            -- and one that is omitted must have been impossible to produce.
            case resRetColl of
              TxReturnCollateral _ resRetCollTxOut@(TxOut _ resRetCollTxOutValue _ _) -> do
                H.note_ "Check that the return collateral output meets its own minimum UTxO value"
                H.diff
                  (txOutValueToLovelace resRetCollTxOutValue)
                  (>=)
                  (calculateMinimumUTxO sbe pparams resRetCollTxOut)
              TxReturnCollateralNone -> do
                -- The return collateral output the function would have built
                -- had it not folded the leftover ada into the total collateral.
                let L.Coin candidateReturnAmount =
                      totalCollateralAda * 100 - L.Coin (fee * requiredCollateralPct)
                    candidateReturnAda = L.rationalToCoinViaFloor $ candidateReturnAmount % 100
                    candidateReturnValue =
                      Api.mkAdaValue sbe candidateReturnAda
                        <> L.modifyCoin (const mempty) totalCollateral
                candidateTxOut <-
                  H.noteShow $
                    TxOut
                      address
                      (TxOutValueShelleyBased sbe candidateReturnValue)
                      TxOutDatumNone
                      ReferenceScriptNone
                H.note_ "Check that the omitted return collateral output could not have met its minimum UTxO value"
                H.diff candidateReturnAda (<) (calculateMinimumUTxO sbe pparams candidateTxOut)
          Left err@InsufficientCollateral{} -> do
            H.annotateShow err
            H.annotate "Unreachable: the guard above ensures the collateral covers the requirement"
            H.failure
          Left err@CollateralWithoutPlutusScripts -> do
            H.annotateShow err
            H.annotate
              "Unreachable: calcReturnAndTotalCollateral does not check for Plutus scripts, \
              \the balancing functions do before calling it"
            H.failure

-- | Regression test for: https://github.com/IntersectMBO/cardano-api/issues/1261
--
-- The ledger requires collateral only for transactions that run Plutus
-- scripts, so balancing a transaction that provides collateral inputs but
-- runs no Plutus scripts must fail with 'CollateralWithoutPlutusScripts'.
-- In particular it must not compute a return collateral output: with a
-- collateral input holding exactly the minimum UTxO value, the computed
-- return collateral output would fall below its own minimum UTxO value,
-- and the ledger would reject the transaction.
prop_make_transaction_body_autobalance_fails_on_collateral_without_plutus :: Property
prop_make_transaction_body_autobalance_fails_on_collateral_without_plutus = H.propertyOnce $ do
  let ceo = ConwayEraOnwardsConway
      beo = convert ceo
      aeo = convert beo
      sbe = convert ceo

  systemStart <- parseSystemStart "2021-09-01T00:00:00Z"
  let epochInfo = LedgerEpochInfo $ CS.fixedEpochInfo (CS.EpochSize 100) (CS.mkSlotLength 1000)

  ledgerPParams <-
    H.readJsonFileOk "test/cardano-api-test/files/input/protocol-parameters/conway.json"
  let pparams = LedgerProtocolParameters ledgerPParams

  let address =
        AddressInEra
          (ShelleyAddressInEra sbe)
          ( ShelleyAddress
              L.Testnet
              (mkCredential "keyHash-ebe9de78a37f84cc819c0669791aa0474d4f0a764e54b9f90cfe2137")
              L.StakeRefNull
          )
      fundingTxIn = mkTxIn "01f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53#0"
      collateralTxIn = mkTxIn "01f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53#1"
      -- The smallest realistic collateral input: it holds exactly the
      -- minimum UTxO value.
      minUTxOCollateral =
        calculateMinimumUTxO sbe ledgerPParams $
          TxOut address (lovelaceToTxOutValue sbe 0) TxOutDatumNone ReferenceScriptNone
      utxos =
        UTxO
          [
            ( fundingTxIn
            , TxOut address (lovelaceToTxOutValue sbe 12_000_000) TxOutDatumNone ReferenceScriptNone
            )
          ,
            ( collateralTxIn
            , TxOut address (lovelaceToTxOutValue sbe minUTxOCollateral) TxOutDatumNone ReferenceScriptNone
            )
          ]
      content =
        defaultTxBodyContent sbe
          & setTxIns [(fundingTxIn, BuildTxWith (KeyWitness KeyWitnessForSpending))]
          & setTxInsCollateral (TxInsCollateral aeo [collateralTxIn])
          & setTxOuts (mkTxOutput beo address (L.Coin 5_000_000) Nothing)
          & setTxProtocolParams (pure $ pure pparams)

  case makeTransactionBodyAutoBalance
    sbe
    systemStart
    epochInfo
    pparams
    mempty
    mempty
    mempty
    utxos
    content
    address
    Nothing of
    -- The transaction provides collateral inputs but runs no Plutus
    -- scripts, so balancing must fail with exactly this error.
    Left (Api.TxBodyErrorCollateral CollateralWithoutPlutusScripts) -> H.success
    Left err -> do
      H.annotateShow err
      H.annotate "Expected balancing to fail with CollateralWithoutPlutusScripts"
      H.failure
    Right _ -> do
      H.annotate "Expected balancing to fail with CollateralWithoutPlutusScripts, but it succeeded"
      H.failure

-- | Regression test for: https://github.com/IntersectMBO/cardano-api/issues/1261
--
-- Like 'prop_make_transaction_body_autobalance_fails_on_collateral_without_plutus',
-- but for 'estimateBalancedTxBody'.
prop_estimate_balanced_tx_body_fails_on_collateral_without_plutus :: Property
prop_estimate_balanced_tx_body_fails_on_collateral_without_plutus = H.propertyOnce $ do
  let ceo = ConwayEraOnwardsConway
      beo = convert ceo
      aeo = convert beo
      meo = convert beo
      sbe = convert ceo

  ledgerPParams <-
    H.readJsonFileOk "test/cardano-api-test/files/input/protocol-parameters/conway.json"

  let address =
        AddressInEra
          (ShelleyAddressInEra sbe)
          ( ShelleyAddress
              L.Testnet
              (mkCredential "keyHash-ebe9de78a37f84cc819c0669791aa0474d4f0a764e54b9f90cfe2137")
              L.StakeRefNull
          )
      fundingTxIn = mkTxIn "01f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53#0"
      collateralTxIn = mkTxIn "01f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53#1"
      -- The smallest realistic collateral: exactly the minimum UTxO value.
      minUTxOCollateral =
        calculateMinimumUTxO sbe ledgerPParams $
          TxOut address (lovelaceToTxOutValue sbe 0) TxOutDatumNone ReferenceScriptNone
      content =
        defaultTxBodyContent sbe
          & setTxIns [(fundingTxIn, BuildTxWith (KeyWitness KeyWitnessForSpending))]
          & setTxInsCollateral (TxInsCollateral aeo [collateralTxIn])
          & setTxOuts (mkTxOutput beo address (L.Coin 5_000_000) Nothing)
          & setTxProtocolParams (pure $ pure (LedgerProtocolParameters ledgerPParams))

  case estimateBalancedTxBody
    meo
    content
    ledgerPParams
    mempty
    mempty
    mempty
    mempty
    minUTxOCollateral
    1
    0
    0
    address
    (lovelaceToValue 12_000_000) of
    -- The transaction provides collateral inputs but runs no Plutus
    -- scripts, so balancing must fail with exactly this error.
    Left (TxFeeEstimationBalanceError (Api.TxBodyErrorCollateral CollateralWithoutPlutusScripts)) ->
      H.success
    Left err -> do
      H.annotateShow err
      H.annotate "Expected balancing to fail with CollateralWithoutPlutusScripts"
      H.failure
    Right _ -> do
      H.annotate "Expected balancing to fail with CollateralWithoutPlutusScripts, but it succeeded"
      H.failure

-- | Regression test for: https://github.com/IntersectMBO/cardano-api/issues/1261
--
-- When the collateral inputs carry native tokens, the tokens must be given
-- back in a return collateral output. With a collateral input holding
-- exactly the minimum UTxO value, the ada left for the return collateral
-- output after covering the required collateral (150% of the fee) is
-- necessarily below the output's own minimum UTxO value, so no valid return
-- collateral output exists and balancing must fail.
prop_make_transaction_body_autobalance_return_collateral_with_tokens_below_min_utxo :: Property
prop_make_transaction_body_autobalance_return_collateral_with_tokens_below_min_utxo = H.propertyOnce $ do
  let ceo = ConwayEraOnwardsConway
      beo = convert ceo
      aeo = convert beo
      meo = convert beo
      sbe = convert ceo

  systemStart <- parseSystemStart "2021-09-01T00:00:00Z"
  let epochInfo = LedgerEpochInfo $ CS.fixedEpochInfo (CS.EpochSize 100) (CS.mkSlotLength 1000)

  ledgerPParams <-
    H.readJsonFileOk "test/cardano-api-test/files/input/protocol-parameters/conway.json"
  let pparams = LedgerProtocolParameters ledgerPParams

  (sh@(ScriptHash scriptHash), plutusWitness) <- loadPlutusWitness ceo
  let policyId' = PolicyId sh
      address =
        AddressInEra
          (ShelleyAddressInEra sbe)
          ( ShelleyAddress
              L.Testnet
              (mkCredential "keyHash-ebe9de78a37f84cc819c0669791aa0474d4f0a764e54b9f90cfe2137")
              L.StakeRefNull
          )
      fundingTxIn = mkTxIn "01f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53#0"
      collateralTxIn = mkTxIn "01f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53#1"
      tokenValue coin =
        TxOutValueShelleyBased sbe $
          L.MaryValue
            coin
            (L.MultiAsset $ fromList [(L.PolicyID scriptHash, [(L.AssetName "eeee", 1)])])
      -- The token-carrying collateral input holds exactly its minimum UTxO
      -- value.
      minUTxOCollateral =
        calculateMinimumUTxO sbe ledgerPParams $
          TxOut address (tokenValue 0) TxOutDatumNone ReferenceScriptNone
      utxos =
        UTxO
          [
            ( fundingTxIn
            , TxOut address (lovelaceToTxOutValue sbe 12_000_000) TxOutDatumNone ReferenceScriptNone
            )
          ,
            ( collateralTxIn
            , TxOut address (tokenValue minUTxOCollateral) TxOutDatumNone ReferenceScriptNone
            )
          ]
      txMint =
        TxMintValue
          meo
          [(policyId', ([(UnsafeAssetName "eeee", 1)], BuildTxWith plutusWitness))]
      content =
        defaultTxBodyContent sbe
          & setTxIns [(fundingTxIn, BuildTxWith (KeyWitness KeyWitnessForSpending))]
          & setTxInsCollateral (TxInsCollateral aeo [collateralTxIn])
          & setTxOuts (mkTxOutput beo address (L.Coin 5_000_000) Nothing)
          & setTxMintValue txMint
          & setTxProtocolParams (pure $ pure pparams)

  case makeTransactionBodyAutoBalance
    sbe
    systemStart
    epochInfo
    pparams
    mempty
    mempty
    mempty
    utxos
    content
    address
    Nothing of
    -- The leftover collateral ada cannot cover the token-carrying return
    -- collateral output's minimum UTxO value, so balancing must fail with
    -- exactly this error.
    Left (Api.TxBodyErrorCollateral (ReturnCollateralBelowMinimumUTxO _ _)) -> H.success
    Left err -> do
      H.annotateShow err
      H.annotate "Expected balancing to fail with ReturnCollateralBelowMinimumUTxO"
      H.failure
    Right _ -> do
      H.annotate "Expected balancing to fail with ReturnCollateralBelowMinimumUTxO, but it succeeded"
      H.failure

-- | Regression test for: https://github.com/IntersectMBO/cardano-api/issues/1261
--
-- With an ada-only collateral input holding exactly the minimum UTxO value,
-- the ada left after covering the required collateral (150% of the fee) is
-- necessarily below the minimum UTxO value of a return collateral output.
-- Instead of failing, balancing must use all of the collateral inputs as total
-- collateral and omit the return collateral output: the extra ada is only
-- lost if the Plutus script fails on chain.
prop_make_transaction_body_autobalance_folds_dust_into_total_collateral :: Property
prop_make_transaction_body_autobalance_folds_dust_into_total_collateral = H.propertyOnce $ do
  let ceo = ConwayEraOnwardsConway
      beo = convert ceo
      aeo = convert beo
      meo = convert beo
      sbe = convert ceo

  systemStart <- parseSystemStart "2021-09-01T00:00:00Z"
  let epochInfo = LedgerEpochInfo $ CS.fixedEpochInfo (CS.EpochSize 100) (CS.mkSlotLength 1000)

  ledgerPParams <-
    H.readJsonFileOk "test/cardano-api-test/files/input/protocol-parameters/conway.json"
  let pparams = LedgerProtocolParameters ledgerPParams

  (sh@(ScriptHash _), plutusWitness) <- loadPlutusWitness ceo
  let policyId' = PolicyId sh
      address =
        AddressInEra
          (ShelleyAddressInEra sbe)
          ( ShelleyAddress
              L.Testnet
              (mkCredential "keyHash-ebe9de78a37f84cc819c0669791aa0474d4f0a764e54b9f90cfe2137")
              L.StakeRefNull
          )
      fundingTxIn = mkTxIn "01f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53#0"
      collateralTxIn = mkTxIn "01f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53#1"
      -- The ada-only collateral input holds exactly its minimum UTxO value.
      minUTxOCollateral =
        calculateMinimumUTxO sbe ledgerPParams $
          TxOut address (lovelaceToTxOutValue sbe 0) TxOutDatumNone ReferenceScriptNone
      utxos =
        UTxO
          [
            ( fundingTxIn
            , TxOut address (lovelaceToTxOutValue sbe 12_000_000) TxOutDatumNone ReferenceScriptNone
            )
          ,
            ( collateralTxIn
            , TxOut address (lovelaceToTxOutValue sbe minUTxOCollateral) TxOutDatumNone ReferenceScriptNone
            )
          ]
      txMint =
        TxMintValue
          meo
          [(policyId', ([(UnsafeAssetName "eeee", 1)], BuildTxWith plutusWitness))]
      content =
        defaultTxBodyContent sbe
          & setTxIns [(fundingTxIn, BuildTxWith (KeyWitness KeyWitnessForSpending))]
          & setTxInsCollateral (TxInsCollateral aeo [collateralTxIn])
          & setTxOuts (mkTxOutput beo address (L.Coin 5_000_000) Nothing)
          & setTxMintValue txMint
          & setTxProtocolParams (pure $ pure pparams)

  BalancedTxBody balancedContent _ _ _ <-
    H.leftFail $
      makeTransactionBodyAutoBalance
        sbe
        systemStart
        epochInfo
        pparams
        mempty
        mempty
        mempty
        utxos
        content
        address
        Nothing

  txReturnCollateral balancedContent === TxReturnCollateralNone
  txTotalCollateral balancedContent === TxTotalCollateral beo minUTxOCollateral

-- | Regression test for: https://github.com/IntersectMBO/cardano-api/issues/1261
--
-- Like
-- 'prop_make_transaction_body_autobalance_folds_dust_into_total_collateral',
-- but for 'estimateBalancedTxBody'.
prop_estimate_balanced_tx_body_folds_dust_into_total_collateral :: Property
prop_estimate_balanced_tx_body_folds_dust_into_total_collateral = H.propertyOnce $ do
  let ceo = ConwayEraOnwardsConway
      beo = convert ceo
      aeo = convert beo
      meo = convert beo
      sbe = convert ceo

  ledgerPParams <-
    H.readJsonFileOk "test/cardano-api-test/files/input/protocol-parameters/conway.json"
  let pparams = LedgerProtocolParameters ledgerPParams

  (sh@(ScriptHash _), plutusWitness) <- loadPlutusWitness ceo
  let policyId' = PolicyId sh
      address =
        AddressInEra
          (ShelleyAddressInEra sbe)
          ( ShelleyAddress
              L.Testnet
              (mkCredential "keyHash-ebe9de78a37f84cc819c0669791aa0474d4f0a764e54b9f90cfe2137")
              L.StakeRefNull
          )
      fundingTxIn = mkTxIn "01f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53#0"
      collateralTxIn = mkTxIn "01f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53#1"
      -- The smallest realistic ada-only collateral: exactly the minimum UTxO
      -- value.
      minUTxOCollateral =
        calculateMinimumUTxO sbe ledgerPParams $
          TxOut address (lovelaceToTxOutValue sbe 0) TxOutDatumNone ReferenceScriptNone
      txMint =
        TxMintValue
          meo
          [(policyId', ([(UnsafeAssetName "eeee", 1)], BuildTxWith plutusWitness))]
      content =
        defaultTxBodyContent sbe
          & setTxIns [(fundingTxIn, BuildTxWith (KeyWitness KeyWitnessForSpending))]
          & setTxInsCollateral (TxInsCollateral aeo [collateralTxIn])
          & setTxOuts (mkTxOutput beo address (L.Coin 5_000_000) Nothing)
          & setTxMintValue txMint
          & setTxProtocolParams (pure $ pure pparams)
      exUnitsMap =
        [(ScriptWitnessIndexMint 0, ExecutionUnits 84_851_308 325_610)]

  BalancedTxBody balancedContent _ _ _ <-
    H.leftFail $
      estimateBalancedTxBody
        meo
        content
        ledgerPParams
        mempty
        mempty
        mempty
        exUnitsMap
        minUTxOCollateral
        1
        0
        0
        address
        (lovelaceToValue 12_000_000)

  txReturnCollateral balancedContent === TxReturnCollateralNone
  txTotalCollateral balancedContent === TxTotalCollateral beo minUTxOCollateral

tests :: TestTree
tests =
  testGroup
    "Test.Cardano.Api.Transaction.Collateral"
    [ testProperty
        "makeTransactionBodyAutoBalance autobalances multi-asset collateral"
        prop_make_transaction_body_autobalance_multi_asset_collateral
    , testProperty "calcReturnAndTotalCollateral constraints hold" prop_calcReturnAndTotalCollateral
    , testProperty
        "makeTransactionBodyAutoBalance fails on collateral without Plutus scripts"
        prop_make_transaction_body_autobalance_fails_on_collateral_without_plutus
    , testProperty
        "estimateBalancedTxBody fails on collateral without Plutus scripts"
        prop_estimate_balanced_tx_body_fails_on_collateral_without_plutus
    , testProperty
        "makeTransactionBodyAutoBalance fails on return collateral with tokens below min UTxO"
        prop_make_transaction_body_autobalance_return_collateral_with_tokens_below_min_utxo
    , testProperty
        "makeTransactionBodyAutoBalance folds return collateral dust into the total collateral"
        prop_make_transaction_body_autobalance_folds_dust_into_total_collateral
    , testProperty
        "estimateBalancedTxBody folds return collateral dust into the total collateral"
        prop_estimate_balanced_tx_body_folds_dust_into_total_collateral
    ]
