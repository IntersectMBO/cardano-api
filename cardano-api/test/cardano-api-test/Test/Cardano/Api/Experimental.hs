{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Api.Experimental
  ( tests
  )
where

import Cardano.Api qualified as Api
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Genesis qualified as Genesis
import Cardano.Api.Ledger qualified as Ledger
import Cardano.Api.Plutus qualified as Script
import Cardano.Api.Tx (Tx (ShelleyTx))

import Cardano.Ledger.Alonzo.Scripts qualified as UnexportedLedger
import Cardano.Ledger.Api qualified as UnexportedLedger
import Cardano.Slotting.EpochInfo qualified as Slotting
import Cardano.Slotting.Slot qualified as Slotting
import Cardano.Slotting.Time qualified as Slotting

import Control.Monad.Identity (Identity)
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Data.Time qualified as Time
import Data.Time.Clock.POSIX qualified as Time
import Lens.Micro ((&))

import Hedgehog (Property)
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

-- | Tests in this module can be run by themselves by writing:
-- ```bash
-- cabal test cardano-api-test --test-options="--pattern=Test.Cardano.Api.Experimental"
-- ```
--
-- IMPORTANT NOTE: If this file requires changes, please update the examples in the
-- documentation in 'cardano-api/src/Cardano/Api/Experimental.hs' too.
tests :: TestTree
tests =
  testGroup
    "Test.Cardano.Api.Experimental"
    [ testProperty
        "Created transaction with traditional and experimental APIs are equivalent"
        prop_created_transaction_with_both_apis_are_the_same
    , testProperty
        "Check two methods of balancing transaction are equivalent"
        prop_balance_transaction_two_ways
    ]

prop_created_transaction_with_both_apis_are_the_same :: Property
prop_created_transaction_with_both_apis_are_the_same = H.propertyOnce $ do
  let era = Exp.ConwayEra
  let sbe = Api.convert era

  signedTxTraditional <- exampleTransactionTraditionalWay sbe
  signedTxExperimental <- exampleTransactionExperimentalWay era sbe

  let oldStyleTx :: Api.Tx Api.ConwayEra = ShelleyTx sbe signedTxExperimental

  oldStyleTx H.=== signedTxTraditional
 where
  exampleTransactionTraditionalWay
    :: H.MonadTest m
    => Api.ShelleyBasedEra Exp.ConwayEra
    -> m (Tx Exp.ConwayEra)
  exampleTransactionTraditionalWay sbe = do
    txBodyContent <- exampleTxBodyContent Api.AsConwayEra sbe
    signingKey <- exampleSigningKey

    txBody <- H.evalEither $ Api.createTransactionBody sbe txBodyContent

    let signedTx :: Api.Tx Api.ConwayEra = Api.signShelleyTransaction sbe txBody [Api.WitnessPaymentKey signingKey]

    return signedTx

  exampleTransactionExperimentalWay
    :: H.MonadTest m
    => Exp.Era Exp.ConwayEra
    -> Api.ShelleyBasedEra Exp.ConwayEra
    -> m (Ledger.Tx (Exp.LedgerEra Exp.ConwayEra))
  exampleTransactionExperimentalWay era sbe = do
    txBodyContent <- exampleTxBodyContent Api.AsConwayEra sbe
    signingKey <- exampleSigningKey

    unsignedTx <- H.evalEither $ Exp.makeUnsignedTx era txBodyContent
    let witness = Exp.makeKeyWitness era unsignedTx (Api.WitnessPaymentKey signingKey)

    let bootstrapWitnesses = []
        keyWitnesses = [witness]

    let Exp.SignedTx (signedTx :: Ledger.Tx (Exp.LedgerEra Exp.ConwayEra)) = Exp.signTx era bootstrapWitnesses keyWitnesses unsignedTx
    return signedTx

prop_balance_transaction_two_ways :: Property
prop_balance_transaction_two_ways = H.propertyOnce $ do
  let era = Exp.ConwayEra
  let sbe = Api.convert era
  let meo = Api.MaryEraOnwardsConway

  changeAddress <- getExampleChangeAddress sbe

  txBodyContent <- exampleTxBodyContent Api.AsConwayEra sbe
  txBody <- H.evalEither $ Api.createTransactionBody sbe txBodyContent

  -- Simple way (fee calculation)
  let fees = Api.evaluateTransactionFee sbe exampleProtocolParams txBody 0 1 0
  H.note_ $ "Fees 1: " <> show fees

  -- Balance without ledger context (other that protocol parameters)
  Api.BalancedTxBody
    _txBodyContent2
    _txBody2
    _changeOutput2
    fees2 <-
    H.evalEither
      $ Api.estimateBalancedTxBody
        meo
        txBodyContent
        exampleProtocolParams
        mempty
        mempty
        mempty
        mempty
        0
        1
        0
        0
        changeAddress
      $ Api.lovelaceToValue 12_000_000

  H.note_ $ "Fees 2: " <> show fees2
  -- H.note_ $ "New TxBody 2: " <> show txBody2
  -- H.note_ $ "New TxBodyContent 2: " <> show txBodyContent2
  -- H.note_ $ "Change output 2: " <> show changeOutput2

  -- Automatically balance the transaction (with ledger context)
  currTime <- Api.liftIO Time.getCurrentTime
  srcTxId <- getExampleSrcTxId
  let startTime = Time.posixSecondsToUTCTime (Time.utcTimeToPOSIXSeconds currTime - Time.nominalDay)
  let epochInfo =
        Api.LedgerEpochInfo $ Slotting.fixedEpochInfo (Slotting.EpochSize 100) (Slotting.mkSlotLength 1000)
  let utxoToUse =
        Api.UTxO
          [
            ( srcTxId
            , Api.TxOut
                changeAddress
                (Api.lovelaceToTxOutValue sbe 12_000_000)
                Api.TxOutDatumNone
                Script.ReferenceScriptNone
            )
          ]

  Api.BalancedTxBody
    _txBodyContent3
    _txBody3
    _changeOutput3
    fees3 <-
    H.evalEither $
      Api.makeTransactionBodyAutoBalance
        sbe
        (Api.SystemStart startTime)
        epochInfo
        (Api.LedgerProtocolParameters exampleProtocolParams)
        mempty
        mempty
        mempty
        utxoToUse
        txBodyContent
        changeAddress
        Nothing

  H.note_ $ "Fees 3: " <> show fees3
  -- H.note_ $ "TxBody 3: " <> show txBody3
  -- H.note_ $ "TxBodyContent 3: " <> show txBodyContent3
  -- H.note_ $ "Change output 3: " <> show changeOutput3

  H.success

exampleProtocolParams :: Ledger.PParams UnexportedLedger.ConwayEra
exampleProtocolParams =
  UnexportedLedger.upgradePParams conwayUpgrade $
    UnexportedLedger.upgradePParams () $
      UnexportedLedger.upgradePParams alonzoUpgrade $
        UnexportedLedger.upgradePParams () $
          UnexportedLedger.upgradePParams () $
            Genesis.sgProtocolParams Genesis.shelleyGenesisDefaults
 where
  conwayUpgrade :: Ledger.UpgradeConwayPParams Identity
  conwayUpgrade = Ledger.cgUpgradePParams Genesis.conwayGenesisDefaults

  alonzoUpgrade :: UnexportedLedger.UpgradeAlonzoPParams Identity
  alonzoUpgrade =
    UnexportedLedger.UpgradeAlonzoPParams
      { UnexportedLedger.uappCoinsPerUTxOWord = Ledger.CoinPerWord $ Ledger.Coin 34_482
      , UnexportedLedger.uappCostModels = UnexportedLedger.emptyCostModels -- We are not using scripts for this tests, so this is fine for now
      , UnexportedLedger.uappPrices =
          Ledger.Prices
            { Ledger.prSteps = fromMaybe maxBound $ Ledger.boundRational $ 721 % 10_000_000
            , Ledger.prMem = fromMaybe maxBound $ Ledger.boundRational $ 577 % 10_000
            }
      , UnexportedLedger.uappMaxTxExUnits =
          Ledger.ExUnits
            { Ledger.exUnitsMem = 140_000_000
            , Ledger.exUnitsSteps = 10_000_000_000
            }
      , UnexportedLedger.uappMaxBlockExUnits =
          Ledger.ExUnits
            { Ledger.exUnitsMem = 62_000_000
            , Ledger.exUnitsSteps = 20_000_000_000
            }
      , UnexportedLedger.uappMaxValSize = 5000
      , UnexportedLedger.uappCollateralPercentage = 150
      , UnexportedLedger.uappMaxCollateralInputs = 3
      }

getExampleSrcTxId :: H.MonadTest m => m Api.TxIn
getExampleSrcTxId = do
  srcTxId <-
    H.evalEither $
      Api.deserialiseFromRawBytesHex
        "be6efd42a3d7b9a00d09d77a5d41e55ceaf0bd093a8aa8a893ce70d9caafd978"
  let srcTxIx = Api.TxIx 0
  return $ Api.TxIn srcTxId srcTxIx

getExampleDestAddress
  :: (H.MonadTest m, Api.IsCardanoEra era) => Script.AsType era -> m (Api.AddressInEra era)
getExampleDestAddress eraAsType = do
  H.evalMaybe $
    Api.deserialiseAddress
      (Api.AsAddressInEra eraAsType)
      "addr_test1vzpfxhjyjdlgk5c0xt8xw26avqxs52rtf69993j4tajehpcue4v2v"

getExampleChangeAddress :: H.MonadTest m => Api.ShelleyBasedEra era -> m (Api.AddressInEra era)
getExampleChangeAddress sbe = do
  signingKey <- exampleSigningKey
  return $
    Api.shelleyAddressInEra sbe $
      Api.makeShelleyAddress
        (Api.Testnet $ Api.NetworkMagic 2)
        (Api.PaymentCredentialByKey $ Api.verificationKeyHash $ Api.getVerificationKey signingKey)
        Api.NoStakeAddress

exampleTxBodyContent
  :: (Api.ShelleyBasedEraConstraints era, H.MonadTest m)
  => Api.AsType era
  -> Api.ShelleyBasedEra era
  -> m (Api.TxBodyContent Api.BuildTx era)
exampleTxBodyContent eraAsType sbe = do
  srcTxIn <- getExampleSrcTxId
  destAddress <- getExampleDestAddress eraAsType
  let txBodyContent =
        Api.defaultTxBodyContent sbe
          & Api.setTxIns
            [
              ( srcTxIn
              , Api.BuildTxWith (Api.KeyWitness Api.KeyWitnessForSpending)
              )
            ]
          & Api.setTxOuts
            [ Api.TxOut
                destAddress
                (Api.lovelaceToTxOutValue sbe 10_000_000)
                Api.TxOutDatumNone
                Script.ReferenceScriptNone
            ]
          & Api.setTxFee (Api.TxFeeExplicit sbe 2_000_000)

  return txBodyContent

exampleSigningKey :: H.MonadTest m => m (Api.SigningKey Api.PaymentKey)
exampleSigningKey =
  H.evalEither $
    Api.deserialiseFromBech32
      "addr_sk1648253w4tf6fv5fk28dc7crsjsaw7d9ymhztd4favg3cwkhz7x8sl5u3ms"
