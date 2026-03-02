{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Api.Experimental
  ( tests
  )
where

import Cardano.Api qualified as Api
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Experimental.Era (convert)
import Cardano.Api.Experimental.Tx qualified as Exp
import Cardano.Api.Genesis qualified as Genesis
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Ledger qualified as Ledger
import Cardano.Api.Parser.Text qualified as Api
import Cardano.Api.Plutus qualified as Script
import Cardano.Api.Tx (Tx (ShelleyTx))

import Cardano.Ledger.Address qualified as L
import Cardano.Ledger.Alonzo.Scripts qualified as UnexportedLedger
import Cardano.Ledger.Api qualified as UnexportedLedger
import Cardano.Ledger.Babbage.TxBody qualified as L
import Cardano.Ledger.Conway qualified as L
import Cardano.Ledger.Core qualified as L
import Cardano.Ledger.Credential qualified as L
import Cardano.Ledger.Mary.Value qualified as Mary
import Cardano.Ledger.Plutus.Data qualified as L
import Cardano.Slotting.EpochInfo qualified as Slotting
import Cardano.Slotting.Slot qualified as Slotting
import Cardano.Slotting.Time qualified as Slotting

import Control.Monad.Identity (Identity)
import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Ratio ((%))
import Data.Text.Encoding qualified as Text
import Data.Time qualified as Time
import Data.Time.Clock.POSIX qualified as Time
import Lens.Micro

import Test.Gen.Cardano.Api.Typed (genAddressInEra, genTx, genTxIn)

import Hedgehog (Gen, Property)
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Property qualified as H
import Hedgehog.Range qualified as Range
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
    , testProperty
        "Roundtrip SerialiseAsRawBytes UnsignedTx"
        prop_roundtrip_serialise_as_raw_bytes_unsigned_tx
    , testProperty
        "Roundtrip SerialiseAsRawBytes SignedTx"
        prop_roundtrip_serialise_as_raw_bytes_signed_tx
    , testGroup
        "calcMinFeeRecursive"
        [ testProperty
            "well-funded transaction always succeeds"
            prop_calcMinFeeRecursive_well_funded_succeeds
        , testProperty
            "fee calculation is idempotent"
            prop_calcMinFeeRecursive_fee_fixpoint
        , testProperty
            "underfunded transaction (outputs exceed inputs) always fails"
            prop_calcMinFeeRecursive_insufficient_funds
        , testProperty
            "Case 2: outputs with tokens not in UTxO returns NonAdaAssetsUnbalanced"
            prop_calcMinFeeRecursive_non_ada_unbalanced
        , testProperty
            "Case 3: output with multi-assets below min UTxO returns MinUTxONotMet"
            prop_calcMinFeeRecursive_min_utxo_not_met
        , testProperty
            "Case 4: transaction with no outputs creates change output"
            prop_calcMinFeeRecursive_no_tx_outs
        ]
    ]

prop_created_transaction_with_both_apis_are_the_same :: Property
prop_created_transaction_with_both_apis_are_the_same = H.propertyOnce $ do
  let era = Exp.ConwayEra
  let sbe = Api.convert era

  signedTxTraditional <- exampleTransactionTraditionalWay sbe
  signedTxExperimental <- exampleTransactionExperimentalWay era

  let oldStyleTx :: Api.Tx Api.ConwayEra = ShelleyTx sbe signedTxExperimental

  oldStyleTx H.=== signedTxTraditional
 where
  exampleTransactionTraditionalWay
    :: H.MonadTest m
    => Api.ShelleyBasedEra Exp.ConwayEra
    -> m (Tx Exp.ConwayEra)
  exampleTransactionTraditionalWay sbe = do
    txBodyContent <- exampleTxBodyContent sbe
    signingKey <- exampleSigningKey

    txBody <- H.evalEither $ Api.createTransactionBody sbe txBodyContent

    let signedTx :: Api.Tx Api.ConwayEra = Api.signShelleyTransaction sbe txBody [Api.WitnessPaymentKey signingKey]

    return signedTx

  exampleTransactionExperimentalWay
    :: H.MonadTest m
    => Exp.Era Exp.ConwayEra
    -> m (Ledger.Tx (Exp.LedgerEra Exp.ConwayEra))
  exampleTransactionExperimentalWay era = do
    txBodyContent <- exampleTxBodyContentExperimental era
    signingKey <- exampleSigningKey

    let unsignedTx = Exp.makeUnsignedTx era txBodyContent
        witness = Exp.makeKeyWitness era unsignedTx (Api.WitnessPaymentKey signingKey)

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
  (txBodyContent, newTxBodyContent) <- exampleOldAndNewStyleTxBodyContent era
  txBody <- H.evalEither $ Api.createTransactionBody sbe txBodyContent

  -- Simple fee estimate (no change output in tx body)
  -- Old API
  let oldFees = Api.evaluateTransactionFee sbe exampleProtocolParams txBody 0 1 0
      -- NEW API
      unSignTx = Exp.makeUnsignedTx era newTxBodyContent
      newFees = Exp.evaluateTransactionFee exampleProtocolParams unSignTx 0 1 0

  oldFees H.=== L.Coin 236
  newFees H.=== L.Coin 236

  -- Set up the change address used by both the dummy output and the
  -- recursive fee calculation, so the serialized output sizes match.
  let paymentCredential :: L.PaymentCredential
      paymentCredential =
        L.KeyHashObj $
          L.KeyHash
            "1c14ee8e58fbcbd48dc7367c95a63fd1d937ba989820015db16ac7e5"

      stakingCredential :: L.StakeCredential
      stakingCredential =
        L.KeyHashObj $
          L.KeyHash
            "e37a65ea2f9bcefb645de4312cf13d8ac12ae61cf242a9aa2973c9ee"
      initialFundedAddress :: L.Addr
      initialFundedAddress = L.Addr L.Testnet paymentCredential (L.StakeRefBase stakingCredential)

  -- Fee estimate with a dummy change output appended to the tx body.
  -- This gives a like-for-like comparison with the recursive fee
  -- calculation, which appends a change output during balancing. The
  -- dummy output uses an arbitrary ADA value — the exact lovelace amount
  -- does not affect the serialized size as long as it falls within the
  -- same CBOR integer encoding bucket (values up to ~4.3 billion
  -- lovelace use the same 5-byte encoding).
  let dummyChangeOutput =
        Api.TxOut
          (Api.fromShelleyAddr sbe initialFundedAddress)
          (Api.lovelaceToTxOutValue sbe 1_000_000)
          Api.TxOutDatumNone
          Script.ReferenceScriptNone
      txBodyContentWithChange =
        txBodyContent
          & Api.setTxOuts (Api.txOuts txBodyContent ++ [dummyChangeOutput])
  txBodyWithChange <- H.evalEither $ Api.createTransactionBody sbe txBodyContentWithChange
  let oldFeesWithChange = Api.evaluateTransactionFee sbe exampleProtocolParams txBodyWithChange 0 1 0

  -- Recursive calc
  dummyTxIn <-
    H.evalEither
      ( Api.toShelleyTxIn
          <$> Api.runParser
            Api.parseTxIn
            "be6efd42a3d7b9a00d09d77a5d41e55ceaf0bd093a8aa8a893ce70d9caafd978#0"
      )

  let dummyLargeTxOut :: L.BabbageTxOut L.ConwayEra =
        Exp.obtainCommonConstraints era $
          L.BabbageTxOut
            initialFundedAddress
            (L.MaryValue (L.Coin 12_000_000) mempty)
            L.NoDatum
            SNothing

      dummyUTxO = L.UTxO $ Map.singleton dummyTxIn dummyLargeTxOut
  Exp.UnsignedTx recFeeTx <-
    H.evalEither $
      Exp.calcMinFeeRecursive
        initialFundedAddress
        unSignTx
        dummyUTxO
        exampleProtocolParams
        mempty
        mempty
        mempty
        0
  let recFee = recFeeTx ^. (L.bodyTxL . L.feeTxBodyL)

  -- The old-API fee with a dummy change output is higher than the
  -- recursive fee because the old API's TxOut encoding (via
  -- createTransactionBody) includes optional Babbage-era fields (datum,
  -- reference script) even when absent, making the serialized output
  -- larger. The recursive calculation uses the ledger's mkBasicTxOut
  -- which produces a more compact encoding.
  H.note_ $ "Old fees (no change output): " <> show oldFees
  H.note_ $ "Old fees (with dummy change output): " <> show oldFeesWithChange
  H.note_ $ "Recursive fees: " <> show recFee
  oldFeesWithChange H.=== L.Coin 302
  recFee H.=== L.Coin 259

  -- Balance without ledger context (other that protocol parameters)
  -- Old api
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
  -- New api
  balancedTxBodyContent <-
    H.evalEither $
      Exp.estimateBalancedTxBody
        era
        newTxBodyContent
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
        (Ledger.valueFromList 12_000_000 [])

  fees2 H.=== Exp.txFee balancedTxBodyContent
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

  -- Check old and new api serialises a tx the same way

  let newTx = Api.serialiseToRawBytes $ Exp.makeUnsignedTx era newTxBodyContent
      oldTx = Api.serialiseToCBOR $ Api.makeSignedTransaction [] txBody
  newTx H.=== oldTx
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
  :: forall era m. (H.MonadTest m, Api.IsCardanoEra era) => m (Api.AddressInEra era)
getExampleDestAddress = do
  H.evalMaybe $
    Api.deserialiseAddress
      (Api.AsAddressInEra (Api.proxyToAsType (Api.Proxy @era)))
      "addr_test1vzpfxhjyjdlgk5c0xt8xw26avqxs52rtf69993j4tajehpcue4v2v"

getExampleDestAddressExp
  :: H.MonadTest m => m Ledger.Addr
getExampleDestAddressExp = do
  Api.toShelleyAddr
    <$> H.evalMaybe
      ( Api.deserialiseAddress
          (Api.AsAddressInEra (Api.proxyToAsType (Api.Proxy @Api.ConwayEra)))
          "addr_test1vzpfxhjyjdlgk5c0xt8xw26avqxs52rtf69993j4tajehpcue4v2v"
      )

getExampleChangeAddress :: H.MonadTest m => Api.ShelleyBasedEra era -> m (Api.AddressInEra era)
getExampleChangeAddress sbe = do
  signingKey <- exampleSigningKey
  return $
    Api.shelleyAddressInEra sbe $
      Api.makeShelleyAddress
        (Api.Testnet $ Api.NetworkMagic 2)
        (Api.PaymentCredentialByKey $ Api.verificationKeyHash $ Api.getVerificationKey signingKey)
        Api.NoStakeAddress

exampleTxBodyContentExperimental
  :: forall era m
   . H.MonadTest m
  => Exp.Era era
  -> m (Exp.TxBodyContent (Exp.LedgerEra era))
exampleTxBodyContentExperimental era = do
  srcTxIn <- getExampleSrcTxId
  addr <- getExampleDestAddressExp
  let value = Ledger.valueFromList 10_000_000 []
      out :: Ledger.TxOut (Exp.LedgerEra era)
      out = Exp.obtainCommonConstraints era $ Ledger.mkBasicTxOut addr value
  let txBodyContent =
        Exp.defaultTxBodyContent
          & Exp.setTxIns
            [
              ( srcTxIn
              , Exp.AnyKeyWitnessPlaceholder
              )
            ]
          & Exp.setTxOuts
            [ Exp.obtainCommonConstraints era $ Exp.TxOut out
            ]
          & Exp.setTxFee 2_000_000
  return txBodyContent

exampleTxBodyContent
  :: forall m era
   . H.MonadTest m
  => Api.IsCardanoEra era
  => Api.ShelleyBasedEra era
  -> m (Api.TxBodyContent Api.BuildTx era)
exampleTxBodyContent sbe = do
  srcTxIn <- getExampleSrcTxId
  destAddress <- getExampleDestAddress @era
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

exampleOldAndNewStyleTxBodyContent
  :: forall m era
   . H.MonadTest m
  => Api.IsCardanoEra era
  => Exp.Era era
  -> m
       ( Api.TxBodyContent Api.BuildTx era
       , Exp.TxBodyContent (Exp.LedgerEra era)
       )
exampleOldAndNewStyleTxBodyContent era = do
  let sbe = convert era
  srcTxIn <- getExampleSrcTxId
  destAddress <- getExampleDestAddress @era
  let txBodyContentOldApi =
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

  let txBodyContentNewApi =
        Exp.defaultTxBodyContent
          & Exp.setTxIns
            [
              ( srcTxIn
              , Exp.AnyKeyWitnessPlaceholder
              )
            ]
          & Exp.setTxOuts
            [ Exp.obtainCommonConstraints era $
                Exp.TxOut
                  ( Exp.obtainCommonConstraints era $
                      Ledger.mkBasicTxOut (Api.toShelleyAddr destAddress) (Ledger.valueFromList 10_000_000 [])
                  )
            ]
          & Exp.setTxFee 2_000_000
  return (txBodyContentOldApi, txBodyContentNewApi)

exampleSigningKey :: H.MonadTest m => m (Api.SigningKey Api.PaymentKey)
exampleSigningKey =
  H.evalEither $
    Api.deserialiseFromBech32
      "addr_sk1648253w4tf6fv5fk28dc7crsjsaw7d9ymhztd4favg3cwkhz7x8sl5u3ms"

expEraGen :: Gen (Exp.Some Exp.Era)
expEraGen =
  let eras :: [Exp.Some Exp.Era] = [minBound .. maxBound]
   in Gen.element eras

expTxForEraGen :: Exp.Era era -> Gen (Ledger.Tx (Exp.LedgerEra era))
expTxForEraGen era = do
  Exp.obtainCommonConstraints era $ do
    ShelleyTx _ tx <- genTx (convert era)
    return tx

prop_roundtrip_serialise_as_raw_bytes_unsigned_tx :: Property
prop_roundtrip_serialise_as_raw_bytes_unsigned_tx = H.withTests (H.TestLimit 20) $ H.property $ do
  Exp.Some era <- H.forAll expEraGen
  Exp.obtainCommonConstraints era $ do
    tx <- H.forAll $ expTxForEraGen era
    let signedTx = Exp.UnsignedTx tx
    signedTx H.=== signedTx
    H.tripping
      signedTx
      (Text.decodeUtf8 . Api.serialiseToRawBytesHex)
      (first show . Api.deserialiseFromRawBytesHex . Text.encodeUtf8)

prop_roundtrip_serialise_as_raw_bytes_signed_tx :: Property
prop_roundtrip_serialise_as_raw_bytes_signed_tx = H.withTests (H.TestLimit 20) $ H.property $ do
  Exp.Some era <- H.forAll expEraGen
  Exp.obtainCommonConstraints era $ do
    tx <- H.forAll $ expTxForEraGen era
    let signedTx = Exp.SignedTx tx
    signedTx H.=== signedTx
    H.tripping
      signedTx
      (Text.decodeUtf8 . Api.serialiseToRawBytesHex)
      (first show . Api.deserialiseFromRawBytesHex . Text.encodeUtf8)

-- ---------------------------------------------------------------------------
-- Property tests for calcMinFeeRecursive
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
  -- (minFeeA=1, minFeeB=0).
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
            Exp.obtainCommonConstraints era $
              Ledger.mkBasicTxOut addr (L.MaryValue sendCoin mempty)
      txBodyContent =
        Exp.defaultTxBodyContent
          & Exp.setTxIns [(txIn, Exp.AnyKeyWitnessPlaceholder)]
          & Exp.setTxOuts [sendTxOut]
          & Exp.setTxFee 0
  return (Exp.makeUnsignedTx era txBodyContent, utxo, changeAddr)

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
            Exp.obtainCommonConstraints era $
              Ledger.mkBasicTxOut addr (L.MaryValue sendCoin mempty)
      txBodyContent =
        Exp.defaultTxBodyContent
          & Exp.setTxIns [(txIn, Exp.AnyKeyWitnessPlaceholder)]
          & Exp.setTxOuts [sendTxOut]
          & Exp.setTxFee 0
  return (Exp.makeUnsignedTx era txBodyContent, utxo, changeAddr)

-- | A well-funded transaction (UTxO >> output + fee) always produces a
-- successful positive fee calculation.
prop_calcMinFeeRecursive_well_funded_succeeds :: Property
prop_calcMinFeeRecursive_well_funded_succeeds = H.property $ do
  (unsignedTx, utxo, changeAddr) <- H.forAll $ genFundedSimpleTx Exp.ConwayEra
  case Exp.calcMinFeeRecursive changeAddr unsignedTx utxo exampleProtocolParams mempty mempty mempty 0 of
    Left err -> H.annotateShow err >> H.failure
    Right (Exp.UnsignedTx resultLedgerTx) -> do
      let resultFee = resultLedgerTx ^. L.bodyTxL . L.feeTxBodyL
      H.assert $ resultFee > L.Coin 0

-- | 'calcMinFeeRecursive' is idempotent: applying it to its own result
-- yields the same 'UnsignedTx'.  This confirms the fee has reached a
-- fixed point and that any surplus was already distributed to outputs.
prop_calcMinFeeRecursive_fee_fixpoint :: Property
prop_calcMinFeeRecursive_fee_fixpoint = H.property $ do
  (unsignedTx, utxo, changeAddr) <- H.forAll $ genFundedSimpleTx Exp.ConwayEra
  case Exp.calcMinFeeRecursive changeAddr unsignedTx utxo exampleProtocolParams mempty mempty mempty 0 of
    Left err -> H.annotateShow err >> H.failure
    Right resultTx -> do
      secondResult <-
        H.evalEither $
          Exp.calcMinFeeRecursive changeAddr resultTx utxo exampleProtocolParams mempty mempty mempty 0
      resultTx H.=== secondResult

-- | When the outputs exceed the UTxO value the function returns
-- 'Left (NotEnoughAda _)' with a negative deficit coin.
prop_calcMinFeeRecursive_insufficient_funds :: Property
prop_calcMinFeeRecursive_insufficient_funds = H.property $ do
  (unsignedTx, utxo, changeAddr) <- H.forAll $ genUnderfundedTx Exp.ConwayEra
  case Exp.calcMinFeeRecursive changeAddr unsignedTx utxo exampleProtocolParams mempty mempty mempty 0 of
    Left (Exp.NotEnoughAda deficit) -> H.assert $ deficit < L.Coin 0
    Left Exp.NonAdaAssetsUnbalanced{} -> H.annotate "Unexpected NonAdaAssetsUnbalanced error" >> H.failure
    Left Exp.MinUTxONotMet{} -> H.annotate "Unexpected MinUTxONotMet error" >> H.failure
    Left Exp.FeeCalculationDidNotConverge -> H.annotate "Unexpected FeeCalculationDidNotConverge error" >> H.failure
    Right _ -> H.failure

-- | Generates a transaction whose output demands a native token that does
-- not exist in the UTxO (which is ADA-only). This guarantees a negative
-- multi-asset balance, triggering Case 2 ('NonAdaAssetsUnbalanced').
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
            Exp.obtainCommonConstraints era $
              Ledger.mkBasicTxOut addr sendValue
      txBodyContent =
        Exp.defaultTxBodyContent
          & Exp.setTxIns [(txIn, Exp.AnyKeyWitnessPlaceholder)]
          & Exp.setTxOuts [sendTxOut]
          & Exp.setTxFee 0
  return (Exp.makeUnsignedTx era txBodyContent, utxo, changeAddr)

-- | Generates a two-output transaction where the second output carries native
-- tokens with only 1000 lovelace — well below the minimum UTxO for a
-- token-bearing output. The surplus ADA is distributed to the first
-- output (Case 4), so the second output stays below minimum, triggering
-- Case 3 ('MinUTxONotMet').
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
            Exp.obtainCommonConstraints era $
              Ledger.mkBasicTxOut addr (L.MaryValue (L.Coin 1_000_000) mempty)
      -- Output 2: tokens with tiny ADA (below min UTxO)
      sendTxOut2 =
        Exp.obtainCommonConstraints era $
          Exp.TxOut $
            Exp.obtainCommonConstraints era $
              Ledger.mkBasicTxOut addr (L.MaryValue (L.Coin 1_000) multiAsset)
      txBodyContent =
        Exp.defaultTxBodyContent
          & Exp.setTxIns [(txIn, Exp.AnyKeyWitnessPlaceholder)]
          & Exp.setTxOuts [sendTxOut1, sendTxOut2]
          & Exp.setTxFee 0
  return (Exp.makeUnsignedTx era txBodyContent, utxo, changeAddr)

-- | Generates a transaction with inputs but no outputs. Once the fee
-- converges (Case 5), the positive surplus triggers Case 4, and
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
  return (Exp.makeUnsignedTx era txBodyContent, utxo, changeAddr)

-- | When the output demands tokens not present in the ADA-only UTxO,
-- the function returns 'Left (NonAdaAssetsUnbalanced _)'.
prop_calcMinFeeRecursive_non_ada_unbalanced :: Property
prop_calcMinFeeRecursive_non_ada_unbalanced = H.property $ do
  (unsignedTx, utxo, changeAddr) <- H.forAll $ genNonAdaUnbalancedTx Exp.ConwayEra
  case Exp.calcMinFeeRecursive changeAddr unsignedTx utxo exampleProtocolParams mempty mempty mempty 0 of
    Left (Exp.NonAdaAssetsUnbalanced _) -> H.success
    Left Exp.NotEnoughAda{} -> H.annotate "Unexpected NotEnoughAda" >> H.failure
    Left Exp.MinUTxONotMet{} -> H.annotate "Unexpected MinUTxONotMet" >> H.failure
    Left Exp.FeeCalculationDidNotConverge -> H.annotate "Unexpected FeeCalculationDidNotConverge" >> H.failure
    Right _ -> H.annotate "Expected NonAdaAssetsUnbalanced but got Right" >> H.failure

-- | When a token-bearing output has less ADA than the minimum UTxO,
-- the function returns 'Left (MinUTxONotMet actual required)' with
-- @actual < required@.
prop_calcMinFeeRecursive_min_utxo_not_met :: Property
prop_calcMinFeeRecursive_min_utxo_not_met = H.property $ do
  (unsignedTx, utxo, changeAddr) <- H.forAll $ genMinUTxOViolatingTx Exp.ConwayEra
  case Exp.calcMinFeeRecursive changeAddr unsignedTx utxo exampleProtocolParams mempty mempty mempty 0 of
    Left (Exp.MinUTxONotMet actual required) -> do
      H.annotate $ "Actual: " <> show actual <> ", Required: " <> show required
      H.assert $ actual < required
    Left Exp.NotEnoughAda{} -> H.annotate "Unexpected NotEnoughAda" >> H.failure
    Left Exp.NonAdaAssetsUnbalanced{} -> H.annotate "Unexpected NonAdaAssetsUnbalanced" >> H.failure
    Left Exp.FeeCalculationDidNotConverge -> H.annotate "Unexpected FeeCalculationDidNotConverge" >> H.failure
    Right _ -> H.annotate "Expected MinUTxONotMet but got Right" >> H.failure

-- | When the transaction has no outputs, the surplus is sent to a new
-- change output at the provided change address.
prop_calcMinFeeRecursive_no_tx_outs :: Property
prop_calcMinFeeRecursive_no_tx_outs = H.property $ do
  (unsignedTx, utxo, changeAddr) <- H.forAll $ genNoOutputsTx Exp.ConwayEra
  case Exp.calcMinFeeRecursive changeAddr unsignedTx utxo exampleProtocolParams mempty mempty mempty 0 of
    Left err -> H.annotateShow err >> H.failure
    Right (Exp.UnsignedTx resultLedgerTx) -> do
      let outs = toList $ resultLedgerTx ^. L.bodyTxL . L.outputsTxBodyL
      -- The result should have exactly one output (the change output)
      length outs H.=== 1
