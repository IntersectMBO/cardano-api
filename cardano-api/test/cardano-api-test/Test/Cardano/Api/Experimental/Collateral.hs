{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Api.Experimental.Collateral
  ( tests
  )
where

import Cardano.Api qualified as Api
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Experimental.AnyScriptWitness qualified as Exp
import Cardano.Api.Experimental.Era (convert)
import Cardano.Api.Experimental.Plutus qualified as ExpPlutus
import Cardano.Api.Experimental.Tx qualified as Exp
import Cardano.Api.Ledger qualified as L

import Cardano.Ledger.Alonzo.Scripts qualified as Alonzo
import Cardano.Ledger.Conway.Scripts qualified as Conway
import Cardano.Ledger.Core qualified as L
import Cardano.Ledger.Mary.Value qualified as Mary
import Cardano.Slotting.EpochInfo qualified as Slotting
import Cardano.Slotting.Slot qualified as Slotting
import Cardano.Slotting.Time qualified as Slotting

import Data.ByteString qualified as B
import Data.Map.Strict qualified as Map
import Data.Time.Clock.POSIX qualified as Time
import GHC.Exts (fromList)
import Lens.Micro

import Test.Cardano.Api.Experimental (exampleProtocolParams)
import Test.Cardano.Api.Transaction.Fixtures (mkTxIn)

import Hedgehog (Property)
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

-- | Regression test for: https://github.com/IntersectMBO/cardano-api/issues/1261
--
-- The ledger requires collateral only for transactions that run Plutus
-- scripts, so balancing a transaction that provides collateral inputs but
-- runs no Plutus scripts must fail with 'Api.CollateralWithoutPlutusScripts'.
-- In particular it must not compute a return collateral output: with a
-- collateral input holding exactly the minimum UTxO value, the computed
-- return collateral output would fall below its own minimum UTxO value,
-- and the ledger would reject the transaction.
prop_makeTransactionBodyAutoBalance_fails_on_collateral_without_plutus :: Property
prop_makeTransactionBodyAutoBalance_fails_on_collateral_without_plutus = H.propertyOnce $ do
  let era = Exp.ConwayEra
      sbe = convert era
      systemStart = Api.SystemStart $ Time.posixSecondsToUTCTime 0
      epochInfo =
        Api.LedgerEpochInfo $
          Slotting.fixedEpochInfo (Slotting.EpochSize 100) (Slotting.mkSlotLength 1000)

  let fundingTxIn = mkTxIn "01f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53#0"
      collateralTxIn = mkTxIn "01f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53#1"
      addr =
        L.Addr
          L.Testnet
          (L.KeyHashObj $ L.KeyHash "1c14ee8e58fbcbd48dc7367c95a63fd1d937ba989820015db16ac7e5")
          L.StakeRefNull
      -- The smallest realistic collateral input: it holds exactly the
      -- minimum UTxO value.
      minUTxOCollateral =
        Exp.calculateMinimumUTxO exampleProtocolParams $
          Exp.TxOut (L.mkBasicTxOut addr (L.MaryValue (L.Coin 0) mempty))
      utxo =
        L.UTxO $
          Map.fromList
            [ (Api.toShelleyTxIn fundingTxIn, L.mkBasicTxOut addr (L.MaryValue (L.Coin 12_000_000) mempty))
            , (Api.toShelleyTxIn collateralTxIn, L.mkBasicTxOut addr (L.MaryValue minUTxOCollateral mempty))
            ]
      txBodyContent =
        Exp.defaultTxBodyContent
          & Exp.setTxIns [(fundingTxIn, Exp.AnyKeyWitnessPlaceholder)]
          & Exp.setTxInsCollateral [collateralTxIn]
          & Exp.setTxOuts [Exp.TxOut $ L.mkBasicTxOut addr (L.MaryValue (L.Coin 5_000_000) mempty)]

  case Exp.makeTransactionBodyAutoBalance
    systemStart
    epochInfo
    exampleProtocolParams
    mempty
    mempty
    utxo
    txBodyContent
    (Api.fromShelleyAddr sbe addr)
    Nothing of
    -- The transaction provides collateral inputs but runs no Plutus
    -- scripts, so balancing must fail with exactly this error.
    Left (Exp.TxBodyErrorCollateral Api.CollateralWithoutPlutusScripts) -> H.success
    Left err -> do
      H.annotateShow err
      H.annotate "Expected balancing to fail with CollateralWithoutPlutusScripts"
      H.failure
    Right _ -> do
      H.annotate "Expected balancing to fail with CollateralWithoutPlutusScripts, but it succeeded"
      H.failure

-- | Regression test for: https://github.com/IntersectMBO/cardano-api/issues/1261
--
-- Like 'prop_makeTransactionBodyAutoBalance_fails_on_collateral_without_plutus',
-- but for 'Exp.estimateBalancedTxBody'.
prop_estimateBalancedTxBody_fails_on_collateral_without_plutus :: Property
prop_estimateBalancedTxBody_fails_on_collateral_without_plutus = H.propertyOnce $ do
  let era = Exp.ConwayEra
      sbe = convert era

  let fundingTxIn = mkTxIn "01f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53#0"
      collateralTxIn = mkTxIn "01f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53#1"
      addr =
        L.Addr
          L.Testnet
          (L.KeyHashObj $ L.KeyHash "1c14ee8e58fbcbd48dc7367c95a63fd1d937ba989820015db16ac7e5")
          L.StakeRefNull
      -- The smallest realistic collateral: exactly the minimum UTxO value.
      minUTxOCollateral =
        Exp.calculateMinimumUTxO exampleProtocolParams $
          Exp.TxOut (L.mkBasicTxOut addr (L.MaryValue (L.Coin 0) mempty))
      txBodyContent =
        Exp.defaultTxBodyContent
          & Exp.setTxIns [(fundingTxIn, Exp.AnyKeyWitnessPlaceholder)]
          & Exp.setTxInsCollateral [collateralTxIn]
          & Exp.setTxOuts [Exp.TxOut $ L.mkBasicTxOut addr (L.MaryValue (L.Coin 5_000_000) mempty)]

  case Exp.estimateBalancedTxBody
    era
    txBodyContent
    exampleProtocolParams
    mempty
    mempty
    mempty
    minUTxOCollateral
    1
    0
    0
    (Api.fromShelleyAddr sbe addr)
    (L.MaryValue (L.Coin 12_000_000) mempty) of
    -- The transaction provides collateral inputs but runs no Plutus
    -- scripts, so balancing must fail with exactly this error.
    Left
      (Exp.TxFeeEstimationBalanceError (Exp.TxBodyErrorCollateral Api.CollateralWithoutPlutusScripts)) ->
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
prop_makeTransactionBodyAutoBalance_return_collateral_with_tokens_below_min_utxo :: Property
prop_makeTransactionBodyAutoBalance_return_collateral_with_tokens_below_min_utxo = H.propertyOnce $ do
  let era = Exp.ConwayEra
      sbe = convert era
      systemStart = Api.SystemStart $ Time.posixSecondsToUTCTime 0
      epochInfo =
        Api.LedgerEpochInfo $
          Slotting.fixedEpochInfo (Slotting.EpochSize 100) (Slotting.mkSlotLength 1000)

  -- Protocol parameters with cost models, so that the Plutus script can run.
  ledgerPParams <-
    H.readJsonFileOk "test/cardano-api-test/files/input/protocol-parameters/conway.json"

  scriptEnvelope <-
    H.evalIO $ B.readFile "test/cardano-api-test/files/input/plutus/v3.alwaysTrue.json"
  Exp.AnyPlutusScript plutusScript <- H.evalEither $ Exp.readAnyScriptBytes era scriptEnvelope

  let fundingTxIn = mkTxIn "01f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53#0"
      collateralTxIn = mkTxIn "01f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53#1"
      addr =
        L.Addr
          L.Testnet
          (L.KeyHashObj $ L.KeyHash "1c14ee8e58fbcbd48dc7367c95a63fd1d937ba989820015db16ac7e5")
          L.StakeRefNull
      ledgerScriptHash = ExpPlutus.hashPlutusScriptInEra plutusScript
      mintWitness =
        Exp.AnyScriptWitnessPlutus $
          Exp.AnyPlutusMintingScriptWitness $
            Exp.PlutusScriptWitness
              (ExpPlutus.plutusScriptInEraSLanguage plutusScript)
              (Exp.PScript plutusScript)
              Exp.NoScriptDatum
              (Api.unsafeHashableScriptData (Api.ScriptDataMap []))
              (Api.ExecutionUnits 0 0)
      tokenValue coin =
        L.MaryValue coin $
          L.MultiAsset $
            Map.singleton (L.PolicyID ledgerScriptHash) (Map.singleton (Mary.AssetName "eeee") 1)
      -- The token-carrying collateral input holds exactly its minimum UTxO
      -- value.
      minUTxOCollateral =
        Exp.calculateMinimumUTxO ledgerPParams $
          Exp.TxOut (L.mkBasicTxOut addr (tokenValue (L.Coin 0)))
      utxo =
        L.UTxO $
          Map.fromList
            [ (Api.toShelleyTxIn fundingTxIn, L.mkBasicTxOut addr (L.MaryValue (L.Coin 12_000_000) mempty))
            , (Api.toShelleyTxIn collateralTxIn, L.mkBasicTxOut addr (tokenValue minUTxOCollateral))
            ]
      txBodyContent =
        Exp.defaultTxBodyContent
          & Exp.setTxIns [(fundingTxIn, Exp.AnyKeyWitnessPlaceholder)]
          & Exp.setTxInsCollateral [collateralTxIn]
          & Exp.setTxOuts [Exp.TxOut $ L.mkBasicTxOut addr (L.MaryValue (L.Coin 5_000_000) mempty)]
          & Exp.setTxMintValue
            ( Exp.TxMintValue $
                Map.singleton
                  (Api.PolicyId $ Api.ScriptHash ledgerScriptHash)
                  (fromList [(Api.UnsafeAssetName "eeee", 1)], mintWitness)
            )
          & Exp.setTxProtocolParams ledgerPParams

  case Exp.makeTransactionBodyAutoBalance
    systemStart
    epochInfo
    ledgerPParams
    mempty
    mempty
    utxo
    txBodyContent
    (Api.fromShelleyAddr sbe addr)
    Nothing of
    -- The leftover collateral ada cannot cover the token-carrying return
    -- collateral output's minimum UTxO value, so balancing must fail with
    -- exactly this error.
    Left (Exp.TxBodyErrorCollateral (Api.ReturnCollateralBelowMinimumUTxO _ _)) -> H.success
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
prop_makeTransactionBodyAutoBalance_folds_dust_into_total_collateral :: Property
prop_makeTransactionBodyAutoBalance_folds_dust_into_total_collateral = H.propertyOnce $ do
  let era = Exp.ConwayEra
      sbe = convert era
      systemStart = Api.SystemStart $ Time.posixSecondsToUTCTime 0
      epochInfo =
        Api.LedgerEpochInfo $
          Slotting.fixedEpochInfo (Slotting.EpochSize 100) (Slotting.mkSlotLength 1000)

  -- Protocol parameters with cost models, so that the Plutus script can run.
  ledgerPParams <-
    H.readJsonFileOk "test/cardano-api-test/files/input/protocol-parameters/conway.json"

  scriptEnvelope <-
    H.evalIO $ B.readFile "test/cardano-api-test/files/input/plutus/v3.alwaysTrue.json"
  Exp.AnyPlutusScript plutusScript <- H.evalEither $ Exp.readAnyScriptBytes era scriptEnvelope

  let fundingTxIn = mkTxIn "01f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53#0"
      collateralTxIn = mkTxIn "01f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53#1"
      addr =
        L.Addr
          L.Testnet
          (L.KeyHashObj $ L.KeyHash "1c14ee8e58fbcbd48dc7367c95a63fd1d937ba989820015db16ac7e5")
          L.StakeRefNull
      ledgerScriptHash = ExpPlutus.hashPlutusScriptInEra plutusScript
      mintWitness =
        Exp.AnyScriptWitnessPlutus $
          Exp.AnyPlutusMintingScriptWitness $
            Exp.PlutusScriptWitness
              (ExpPlutus.plutusScriptInEraSLanguage plutusScript)
              (Exp.PScript plutusScript)
              Exp.NoScriptDatum
              (Api.unsafeHashableScriptData (Api.ScriptDataMap []))
              (Api.ExecutionUnits 0 0)
      -- The ada-only collateral input holds exactly its minimum UTxO value.
      minUTxOCollateral =
        Exp.calculateMinimumUTxO ledgerPParams $
          Exp.TxOut (L.mkBasicTxOut addr (L.MaryValue (L.Coin 0) mempty))
      utxo =
        L.UTxO $
          Map.fromList
            [ (Api.toShelleyTxIn fundingTxIn, L.mkBasicTxOut addr (L.MaryValue (L.Coin 12_000_000) mempty))
            , (Api.toShelleyTxIn collateralTxIn, L.mkBasicTxOut addr (L.MaryValue minUTxOCollateral mempty))
            ]
      txBodyContent =
        Exp.defaultTxBodyContent
          & Exp.setTxIns [(fundingTxIn, Exp.AnyKeyWitnessPlaceholder)]
          & Exp.setTxInsCollateral [collateralTxIn]
          & Exp.setTxOuts [Exp.TxOut $ L.mkBasicTxOut addr (L.MaryValue (L.Coin 5_000_000) mempty)]
          & Exp.setTxMintValue
            ( Exp.TxMintValue $
                Map.singleton
                  (Api.PolicyId $ Api.ScriptHash ledgerScriptHash)
                  (fromList [(Api.UnsafeAssetName "eeee", 1)], mintWitness)
            )
          & Exp.setTxProtocolParams ledgerPParams

  (_, balancedContent) <-
    H.leftFail $
      Exp.makeTransactionBodyAutoBalance
        systemStart
        epochInfo
        ledgerPParams
        mempty
        mempty
        utxo
        txBodyContent
        (Api.fromShelleyAddr sbe addr)
        Nothing

  let returnCollateralAda =
        (^. L.coinTxOutL) . Exp.unTxReturnCollateral <$> Exp.txReturnCollateral balancedContent
      totalCollateralAda = Exp.unTxTotalCollateral <$> Exp.txTotalCollateral balancedContent

  H.note_ "Check that there is no return collateral output"
  returnCollateralAda H.=== Nothing
  H.note_ "Check that all of the collateral inputs are used as total collateral"
  totalCollateralAda H.=== Just minUTxOCollateral

-- | Regression test for: https://github.com/IntersectMBO/cardano-api/issues/1261
--
-- Like 'prop_makeTransactionBodyAutoBalance_folds_dust_into_total_collateral',
-- but for 'Exp.estimateBalancedTxBody'.
prop_estimateBalancedTxBody_folds_dust_into_total_collateral :: Property
prop_estimateBalancedTxBody_folds_dust_into_total_collateral = H.propertyOnce $ do
  let era = Exp.ConwayEra
      sbe = convert era

  -- Protocol parameters with cost models, so that the Plutus script can run.
  ledgerPParams <-
    H.readJsonFileOk "test/cardano-api-test/files/input/protocol-parameters/conway.json"

  scriptEnvelope <-
    H.evalIO $ B.readFile "test/cardano-api-test/files/input/plutus/v3.alwaysTrue.json"
  Exp.AnyPlutusScript plutusScript <- H.evalEither $ Exp.readAnyScriptBytes era scriptEnvelope

  let fundingTxIn = mkTxIn "01f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53#0"
      collateralTxIn = mkTxIn "01f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53#1"
      addr =
        L.Addr
          L.Testnet
          (L.KeyHashObj $ L.KeyHash "1c14ee8e58fbcbd48dc7367c95a63fd1d937ba989820015db16ac7e5")
          L.StakeRefNull
      ledgerScriptHash = ExpPlutus.hashPlutusScriptInEra plutusScript
      mintWitness =
        Exp.AnyScriptWitnessPlutus $
          Exp.AnyPlutusMintingScriptWitness $
            Exp.PlutusScriptWitness
              (ExpPlutus.plutusScriptInEraSLanguage plutusScript)
              (Exp.PScript plutusScript)
              Exp.NoScriptDatum
              (Api.unsafeHashableScriptData (Api.ScriptDataMap []))
              (Api.ExecutionUnits 0 0)
      -- The smallest realistic ada-only collateral: exactly the minimum UTxO
      -- value.
      minUTxOCollateral =
        Exp.calculateMinimumUTxO ledgerPParams $
          Exp.TxOut (L.mkBasicTxOut addr (L.MaryValue (L.Coin 0) mempty))
      txBodyContent =
        Exp.defaultTxBodyContent
          & Exp.setTxIns [(fundingTxIn, Exp.AnyKeyWitnessPlaceholder)]
          & Exp.setTxInsCollateral [collateralTxIn]
          & Exp.setTxOuts [Exp.TxOut $ L.mkBasicTxOut addr (L.MaryValue (L.Coin 5_000_000) mempty)]
          & Exp.setTxMintValue
            ( Exp.TxMintValue $
                Map.singleton
                  (Api.PolicyId $ Api.ScriptHash ledgerScriptHash)
                  (fromList [(Api.UnsafeAssetName "eeee", 1)], mintWitness)
            )
          & Exp.setTxProtocolParams ledgerPParams
      exUnitsMap =
        Map.singleton
          (Conway.ConwayMinting (Alonzo.AsIx 0))
          (Api.ExecutionUnits 84_851_308 325_610)

  balancedContent <-
    H.leftFail $
      Exp.estimateBalancedTxBody
        era
        txBodyContent
        ledgerPParams
        mempty
        mempty
        exUnitsMap
        minUTxOCollateral
        1
        0
        0
        (Api.fromShelleyAddr sbe addr)
        (L.MaryValue (L.Coin 12_000_000) mempty)

  let returnCollateralAda =
        (^. L.coinTxOutL) . Exp.unTxReturnCollateral <$> Exp.txReturnCollateral balancedContent
      totalCollateralAda = Exp.unTxTotalCollateral <$> Exp.txTotalCollateral balancedContent

  H.note_ "Check that there is no return collateral output"
  returnCollateralAda H.=== Nothing
  H.note_ "Check that all of the collateral inputs are used as total collateral"
  totalCollateralAda H.=== Just minUTxOCollateral

-- | Tests in this module can be run by themselves by writing:
-- ```bash
-- cabal test cardano-api-test --test-options="--pattern=Test.Cardano.Api.Experimental.Collateral"
-- ```
tests :: TestTree
tests =
  testGroup
    "Test.Cardano.Api.Experimental.Collateral"
    [ testGroup
        "makeTransactionBodyAutoBalance"
        [ testProperty
            "fails on collateral without Plutus scripts"
            prop_makeTransactionBodyAutoBalance_fails_on_collateral_without_plutus
        , testProperty
            "fails on return collateral with tokens below min UTxO"
            prop_makeTransactionBodyAutoBalance_return_collateral_with_tokens_below_min_utxo
        , testProperty
            "folds return collateral dust into the total collateral"
            prop_makeTransactionBodyAutoBalance_folds_dust_into_total_collateral
        ]
    , testGroup
        "estimateBalancedTxBody"
        [ testProperty
            "fails on collateral without Plutus scripts"
            prop_estimateBalancedTxBody_fails_on_collateral_without_plutus
        , testProperty
            "folds return collateral dust into the total collateral"
            prop_estimateBalancedTxBody_folds_dust_into_total_collateral
        ]
    ]
