{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Test.Cardano.Api.Experimental
  ( tests
  , exampleProtocolParams
  , exampleProtocolParamsEra
  )
where

import Cardano.Api qualified as Api
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Experimental.AnyScriptWitness
  ( AnyPlutusScriptWitness
      ( AnyPlutusCertifyingScriptWitness
      , AnyPlutusProposingScriptWitness
      , AnyPlutusReceivingScriptWitness
      , AnyPlutusSpendingScriptWitness
      )
  , PlutusSpendingScriptWitness (PlutusSpendingScriptWitnessV3)
  )
import Cardano.Api.Experimental.AnyScriptWitness qualified as Exp
import Cardano.Api.Experimental.Era (convert)
import Cardano.Api.Experimental.Plutus qualified as Exp hiding (AnyPlutusScript)
import Cardano.Api.Experimental.Tx qualified as Exp
import Cardano.Api.Genesis qualified as Genesis
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Ledger qualified as Ledger
import Cardano.Api.Parser.Text qualified as Api
import Cardano.Api.Plutus qualified as Script
import Cardano.Api.Tx (Tx (ShelleyTx), toShelleyTxId)

import Cardano.Ledger.Address qualified as L
import Cardano.Ledger.Alonzo.PParams qualified as L (ppMaxTxExUnitsL)
import Cardano.Ledger.Alonzo.Scripts qualified as L
import Cardano.Ledger.Alonzo.TxWits qualified as Alonzo
import Cardano.Ledger.Api qualified as UnexportedLedger
import Cardano.Ledger.Babbage.TxBody qualified as L
import Cardano.Ledger.Babbage.TxOut qualified as L
import Cardano.Ledger.Conway qualified as L
import Cardano.Ledger.Core qualified as L
import Cardano.Ledger.Dijkstra qualified as L
import Cardano.Ledger.Dijkstra.Genesis (DijkstraGenesis (..))
import Cardano.Ledger.Dijkstra.Scripts qualified as DS
import Cardano.Ledger.Dijkstra.TxBody qualified as Dijkstra
import Cardano.Ledger.Keys qualified as L
import Cardano.Ledger.Mary.Value qualified as Mary
import Cardano.Ledger.Plutus.Data qualified as L
import Cardano.Ledger.Plutus.Language qualified as Plutus
import Cardano.Ledger.Tools qualified as LedgerTools
import Cardano.Slotting.EpochInfo qualified as Slotting
import Cardano.Slotting.Slot qualified as Slotting
import Cardano.Slotting.Time qualified as Slotting

import Control.Monad (forM, forM_)
import Control.Monad.Identity (Identity)
import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.Either (isLeft, isRight)
import Data.Foldable (foldl', toList)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.Maybe.Strict (StrictMaybe (..), strictMaybeToMaybe)
import Data.OMap.Strict qualified as LOMap
import Data.Ratio ((%))
import Data.Set qualified as Set
import Data.Text.Encoding qualified as Text
import Data.Time qualified as Time
import Data.Time.Clock.POSIX qualified as Time
import Data.Word (Word32)
import Lens.Micro

import Test.Gen.Cardano.Api.Experimental (genAnyScript, genSignedSubTx, genUnsignedSubTx)
import Test.Gen.Cardano.Api.Typed
  ( genAddressInEra
  , genPlutusScriptInEra
  , genProposal
  , genShelleyBootstrapWitness
  , genShelleyWitnessSigningKey
  , genSimpleScript
  , genStakeCredential
  , genTx
  , genTxIn
  , genVerificationKeyHash
  )

import Hedgehog (Gen, Property)
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Gen.QuickCheck qualified as Q
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
        "Per-output Receiving retains raw gaps, duplicate outputs and independent redeemers"
        prop_receiving_native_index_gap
    , testProperty
        "Repeated hashes receive their own output context and redeemer"
        prop_receiving_output_context
    , testProperty
        "Equal Receiving output indices remain isolated between parent and child"
        prop_receiving_nested_output_context
    , testProperty
        "Repeated-hash output budgets aggregate against the batch limit"
        prop_receiving_per_output_budget_limit
    , testProperty
        "Unsigned child signature containers are covered by final fees"
        prop_receiving_child_signature_fees
    , testProperty
        "Receiving change preserves authored indices and final per-output budgets"
        prop_receiving_change_final_domain
    , testProperty
        "Native Receiving inline and reference signers are covered by fees"
        prop_receiving_native_fees
    , testProperty "Expected invalidity is decided over the full batch" prop_receiving_invalid_batch
    , testProperty
        "Balancing preserves signed children and refuses changed signed budgets"
        prop_receiving_signed_children
    , testProperty
        "Protected collateral return is rejected before construction"
        prop_receiving_protected_collateral
    , testProperty
        "Receiving witnesses reject signed and inconsistent declarations"
        prop_receiving_witnesses_reject_invalid_declarations
    , testProperty
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
        "SerialiseAsCBOR AnyScript"
        [ testProperty
            "Roundtrip serialiseToCBOR/deserialiseFromCBOR AnyScript"
            prop_roundtrip_cbor_any_script
        ]
    , testGroup
        "readAnyScriptBytes"
        [ testProperty
            "Roundtrip Plutus script text envelope"
            prop_roundtrip_plutus_script_text_envelope
        , testProperty
            "Read old API simple script text envelope"
            prop_read_old_api_simple_script_text_envelope
        , testProperty
            "Read legacy JSON simple script"
            prop_read_legacy_json_simple_script
        , testProperty
            "Roundtrip readFileAnyScript"
            prop_roundtrip_read_file_any_script
        ]
    , testGroup
        "Sub-transactions"
        [ testProperty
            "Roundtrip SerialiseAsCBOR UnsignedSubTx"
            prop_roundtrip_cbor_unsigned_sub_tx
        , testProperty
            "Roundtrip SerialiseAsCBOR SignedSubTx"
            prop_roundtrip_cbor_signed_sub_tx
        , testProperty
            "Roundtrip TextEnvelope UnsignedSubTx"
            prop_roundtrip_text_envelope_unsigned_sub_tx
        , testProperty
            "Roundtrip TextEnvelope SignedSubTx"
            prop_roundtrip_text_envelope_signed_sub_tx
        , testProperty
            "Sub-transaction envelope types name the era"
            prop_sub_tx_envelope_types
        , testProperty
            "Signing a sub-transaction does not change its id"
            prop_sign_sub_tx_preserves_id
        , testProperty
            "Sub-transaction built from content is embedded under its id"
            prop_sub_tx_embedded_in_top_level_body
        , testProperty
            "Top-level body keeps required top-level guards and starting account balance intervals"
            prop_makeUnsignedTx_dijkstra_top_level_only_fields
        ]
    , testGroup
        "makeUnsignedTx"
        [ testProperty
            "Plutus scripts without protocol params returns MakeUnsignedTxMissingProtocolParams"
            prop_makeUnsignedTx_plutus_without_pparams
        , testProperty
            "Dijkstra-only fields set on a Conway body are rejected, not dropped"
            prop_makeUnsignedTx_conway_rejects_dijkstra_only_fields
        , testProperty
            "Proposal-procedure redeemer pointers follow OMap insertion order, not Ord order"
            prop_makeUnsignedTx_proposal_redeemer_indices_follow_insertion_order
        , testProperty
            "Certifying redeemer indices count unwitnessed certs preceding a plutus-witnessed one"
            prop_makeUnsignedTx_cert_redeemer_indices_count_unwitnessed_certs
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
        ]
    ]

prop_roundtrip_cbor_unsigned_sub_tx :: Property
prop_roundtrip_cbor_unsigned_sub_tx = H.property $ do
  subTx <- H.forAll genUnsignedSubTx
  H.tripping
    subTx
    Api.serialiseToCBOR
    (Api.deserialiseFromCBOR Exp.AsUnsignedSubTx)

prop_roundtrip_cbor_signed_sub_tx :: Property
prop_roundtrip_cbor_signed_sub_tx = H.property $ do
  subTx <- H.forAll genSignedSubTx
  H.tripping subTx Api.serialiseToCBOR (Api.deserialiseFromCBOR Exp.AsSignedSubTx)

prop_roundtrip_text_envelope_unsigned_sub_tx :: Property
prop_roundtrip_text_envelope_unsigned_sub_tx = H.property $ do
  subTx <- H.forAll genUnsignedSubTx
  H.tripping subTx (Api.serialiseToTextEnvelope Nothing) Api.deserialiseFromTextEnvelope

prop_roundtrip_text_envelope_signed_sub_tx :: Property
prop_roundtrip_text_envelope_signed_sub_tx = H.property $ do
  subTx <- H.forAll genSignedSubTx
  H.tripping subTx (Api.serialiseToTextEnvelope Nothing) Api.deserialiseFromTextEnvelope

prop_sub_tx_envelope_types :: Property
prop_sub_tx_envelope_types = H.propertyOnce $ do
  Api.textEnvelopeType Exp.AsUnsignedSubTx
    H.=== Api.TextEnvelopeType "Unwitnessed SubTx DijkstraEra"
  Api.textEnvelopeType Exp.AsSignedSubTx
    H.=== Api.TextEnvelopeType "Witnessed SubTx DijkstraEra"

prop_sign_sub_tx_preserves_id :: Property
prop_sign_sub_tx_preserves_id = H.property $ do
  subTx <- H.forAll genUnsignedSubTx
  sk <- H.forAllWith (const "<ShelleyWitnessSigningKey>") genShelleyWitnessSigningKey
  let wit = Exp.makeSubTxKeyWitness subTx sk
      signed@(Exp.SignedSubTx ledgerTx) = Exp.signSubTx [] [wit] subTx
  Exp.getUnsignedSubTxId subTx H.=== Exp.getSignedSubTxId signed
  H.assert $ wit `Set.member` (ledgerTx ^. L.witsTxL . UnexportedLedger.addrTxWitsL)

-- | The construction path end to end: 'Exp.SubTxBodyContent' through
-- 'Exp.makeUnsignedSubTx', signing, and embedding in a Dijkstra top-level
-- body, where the ledger keys the sub-transaction by its id. The setters are
-- the ones shared with 'Exp.TxBodyContent'.
prop_sub_tx_embedded_in_top_level_body :: Property
prop_sub_tx_embedded_in_top_level_body = H.property $ do
  donation <- H.forAll Q.arbitrary
  guards <- H.forAll Q.arbitrary
  sk <- H.forAllWith (const "<ShelleyWitnessSigningKey>") genShelleyWitnessSigningKey
  let content =
        Exp.defaultSubTxBodyContent
          & Exp.setTxTreasuryDonation donation
          & Exp.setTxGuards guards
  unsigned <- H.evalEither $ Exp.makeUnsignedSubTx content
  let signed@(Exp.SignedSubTx ledgerSubTx) =
        Exp.signSubTx [] [Exp.makeSubTxKeyWitness unsigned sk] unsigned
      subTxId = UnexportedLedger.txIdTx ledgerSubTx
  ledgerSubTx ^. L.bodyTxL . L.treasuryDonationTxBodyL H.=== donation
  ledgerSubTx ^. L.bodyTxL . UnexportedLedger.guardsTxBodyL H.=== guards
  toShelleyTxId (Exp.getSignedSubTxId signed) H.=== subTxId
  Exp.UnsignedTx tx <-
    H.evalEither $
      Exp.makeUnsignedTx Exp.DijkstraEra $
        Exp.defaultTxBodyContent & Exp.setTxSignedSubTransactions [signed]
  let subTxs = tx ^. L.bodyTxL . Dijkstra.subTransactionsTxBodyL
  LOMap.lookup subTxId subTxs H.=== Just ledgerSubTx
  length subTxs H.=== 1

prop_makeUnsignedTx_dijkstra_top_level_only_fields :: Property
prop_makeUnsignedTx_dijkstra_top_level_only_fields = H.property $ do
  requiredGuards <- H.forAll Q.arbitrary
  startingIntervals <- H.forAll Q.arbitrary
  let bodyContent =
        Exp.defaultTxBodyContent
          & Exp.setTxRequiredTopLevelGuards requiredGuards
          & Exp.setTxStartingAccountBalanceIntervals startingIntervals
  Exp.UnsignedTx tx <- H.evalEither $ Exp.makeUnsignedTx Exp.DijkstraEra bodyContent
  let body = tx ^. L.bodyTxL
  body ^. Dijkstra.requiredTopLevelGuardsL H.=== requiredGuards
  body ^. Dijkstra.startingAccountBalanceIntervalsTxBodyL H.=== startingIntervals

prop_roundtrip_cbor_any_script :: Property
prop_roundtrip_cbor_any_script = H.property $ do
  script <- H.forAll genAnyScript
  H.tripping script Api.serialiseToCBOR (Api.deserialiseFromCBOR Exp.AsAnyScript)

prop_roundtrip_plutus_script_text_envelope :: Property
prop_roundtrip_plutus_script_text_envelope = H.property $ do
  ps <- H.forAll genPlutusScriptInEra
  let envelopeJson = Api.serialiseToJSON $ Api.serialiseToTextEnvelope Nothing ps
  script <- H.evalEither $ Exp.readAnyScriptBytes Exp.ConwayEra envelopeJson
  script H.=== Exp.AnyPlutusScript ps

-- | The experimental API has no text envelope format for simple scripts, but
-- the old API produces one via its 'Api.Script' instance (type
-- \"SimpleScript\", Allegra-era 'Timelock' CBOR), so the simple script arm of
-- 'Exp.readAnyScriptBytes' is tested against what the old API writes.
prop_read_old_api_simple_script_text_envelope :: Property
prop_read_old_api_simple_script_text_envelope = H.property $ do
  oldScript <- H.forAll genSimpleScript
  let envelopeJson =
        Api.serialiseToJSON $ Api.serialiseToTextEnvelope Nothing (Api.SimpleScript oldScript)
  script <- H.evalEither $ Exp.readAnyScriptBytes Exp.ConwayEra envelopeJson
  script H.=== Exp.AnySimpleScript (Exp.SimpleScript (Api.toAllegraTimelock oldScript))

prop_read_legacy_json_simple_script :: Property
prop_read_legacy_json_simple_script = H.property $ do
  oldScript <- H.forAll genSimpleScript
  script <- H.evalEither $ Exp.readAnyScriptBytes Exp.ConwayEra (Api.serialiseToJSON oldScript)
  script H.=== Exp.AnySimpleScript (Exp.SimpleScript (Api.toAllegraTimelock oldScript))

prop_roundtrip_read_file_any_script :: Property
prop_roundtrip_read_file_any_script = H.propertyOnce . H.moduleWorkspace "any-script" $ \ws -> do
  oldScript <- H.forAll genSimpleScript
  let envelopeJson =
        Api.serialiseToJSON $ Api.serialiseToTextEnvelope Nothing (Api.SimpleScript oldScript)
      path = ws <> "/simple-script.json"
  H.evalIO $ BS.writeFile path envelopeJson
  result <- H.evalIO $ Exp.readFileAnyScript Exp.ConwayEra (Api.File path)
  script <- H.evalEither result
  script H.=== Exp.AnySimpleScript (Exp.SimpleScript (Api.toAllegraTimelock oldScript))

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
    -> m (Ledger.Tx L.TopTx (Api.ShelleyLedgerEra Exp.ConwayEra))
  exampleTransactionExperimentalWay era = do
    txBodyContent <- exampleTxBodyContentExperimental era
    signingKey <- exampleSigningKey

    unsignedTx <- H.evalEither $ Exp.makeUnsignedTx era txBodyContent
    let witness = Exp.makeKeyWitness era unsignedTx (Api.WitnessPaymentKey signingKey)

    let bootstrapWitnesses = []
        keyWitnesses = [witness]

    let Exp.SignedTx (signedTx :: Ledger.Tx L.TopTx (Api.ShelleyLedgerEra Exp.ConwayEra)) = Exp.signTx era bootstrapWitnesses keyWitnesses unsignedTx
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
  unSignTx <- H.evalEither $ Exp.makeUnsignedTx era newTxBodyContent
  let newFees = Exp.evaluateTransactionFee exampleProtocolParams unSignTx 0 1 0

  oldFees H.=== L.Coin 236
  newFees H.=== L.Coin 236

  -- Set up the change address used by both the dummy output and the
  -- recursive fee calculation, so the serialized output sizes match.
  let paymentCredential :: L.Credential L.Payment
      paymentCredential =
        L.KeyHashObj $
          L.KeyHash
            "1c14ee8e58fbcbd48dc7367c95a63fd1d937ba989820015db16ac7e5"

      stakingCredential :: L.Credential L.Staking
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
        utxoToUse
        txBodyContent
        changeAddress
        Nothing

  H.note_ $ "Fees 3: " <> show fees3

  -- Check old and new api serialises a tx the same way

  newUnsignedTx <- H.evalEither $ Exp.makeUnsignedTx era newTxBodyContent
  let newTx = Api.serialiseToRawBytes newUnsignedTx
      oldTx = Api.serialiseToCBOR $ Api.makeSignedTransaction [] txBody
  newTx H.=== oldTx
  H.success

exampleProtocolParamsEra :: Exp.Era era -> L.PParams (Exp.LedgerEra era)
exampleProtocolParamsEra = \case
  Exp.ConwayEra -> exampleProtocolParams
  Exp.DijkstraEra ->
    UnexportedLedger.upgradePParams
      (dgUpgradePParams Genesis.dijkstraGenesisDefaults)
      exampleProtocolParams
      & L.ppProtocolVersionL .~ L.ProtVer (L.eraProtVerLow @L.DijkstraEra) 0

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
      , UnexportedLedger.uappPlutusV1CostModel = Genesis.defaultV1CostModel -- We are not using scripts for this tests, so this is fine for now
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

expTxForEraGen :: Exp.Era era -> Gen (Ledger.Tx L.TopTx (Exp.LedgerEra era))
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
-- Regression test for makeUnsignedTx
-- ---------------------------------------------------------------------------

-- | The body content record is shared by Conway and Dijkstra, so a Dijkstra-only
-- field can be set on a Conway body. Building must fail, naming the field,
-- rather than silently producing a body without it. The same content builds in
-- Dijkstra.
prop_makeUnsignedTx_conway_rejects_dijkstra_only_fields :: Property
prop_makeUnsignedTx_conway_rejects_dijkstra_only_fields = H.property $ do
  guardCredential <- H.forAll Q.arbitrary
  let bodyContent :: Exp.TxBodyContent era
      bodyContent =
        Exp.defaultTxBodyContent
          & Exp.setTxRequiredTopLevelGuards (Map.singleton guardCredential SNothing)
  case Exp.makeUnsignedTx Exp.ConwayEra bodyContent of
    Left err ->
      err
        H.=== Exp.MakeUnsignedTxFieldsNotSupportedInEra
          (Exp.Some Exp.ConwayEra)
          ("txRequiredTopLevelGuards" :| [])
    Right _ -> H.failure
  H.assert . isRight $ Exp.makeUnsignedTx Exp.DijkstraEra bodyContent

-- | Regression test: 'makeUnsignedTx' must return 'Left MakeUnsignedTxMissingProtocolParams'
-- when the transaction body contains a Plutus script witness but no protocol parameters.
-- Protocol parameters are required to compute the script integrity hash (script_data_hash).
prop_makeUnsignedTx_plutus_without_pparams :: Property
prop_makeUnsignedTx_plutus_without_pparams = H.propertyOnce $ do
  srcTxIn <- getExampleSrcTxId
  let dummyRedeemer = Script.unsafeHashableScriptData $ Script.ScriptDataConstructor 0 []
      plutusWit =
        Exp.AnyPlutusScriptWitness $
          AnyPlutusSpendingScriptWitness $
            PlutusSpendingScriptWitnessV3 $
              Exp.PlutusScriptWitness
                Plutus.SPlutusV3
                (Exp.PReferenceScript srcTxIn)
                Exp.NoScriptDatum
                dummyRedeemer
                (Script.ExecutionUnits 0 0)
      txBodyContent =
        Exp.defaultTxBodyContent
          & Exp.setTxIns [(srcTxIn, plutusWit)]
          & Exp.setTxFee 0
  Exp.makeUnsignedTx Exp.ConwayEra txBodyContent
    H.=== Left Exp.MakeUnsignedTxMissingProtocolParams

-- | 'makeUnsignedTx' must index plutus-witnessed governance proposals'
-- redeemer pointers ('L.ConwayProposing') by insertion order, never by
-- 'Ord' order. Insertion order is what the ledger's 'OSet'-backed
-- 'proposalProceduresTxBodyL' stores them in.
--
-- 'propA' and 'propB' only differ in 'pProcDeposit' (the first field
-- 'Ord' compares), chosen so 'propB' sorts before 'propA' by 'Ord' but is
-- inserted after it. A regression to 'Ord'-sorted indexing would swap
-- which redeemer lands at which index.
prop_makeUnsignedTx_proposal_redeemer_indices_follow_insertion_order :: Property
prop_makeUnsignedTx_proposal_redeemer_indices_follow_insertion_order = H.property $ do
  scriptTxIn <- H.forAll genTxIn
  baseA <- H.forAll (genProposal Api.ConwayEraOnwardsConway)
  baseB <- H.forAll (genProposal Api.ConwayEraOnwardsConway)
  let propA = baseA{L.pProcDeposit = 2_000_000}
      propB = baseB{L.pProcDeposit = 1_000_000}

      mkRedeemer :: Integer -> Script.HashableScriptData
      mkRedeemer n = Script.unsafeHashableScriptData $ Script.ScriptDataConstructor n []

      mkProposingWitness redeemer =
        Exp.AnyPlutusScriptWitness $
          AnyPlutusProposingScriptWitness $
            Exp.PlutusScriptWitness
              Plutus.SPlutusV3
              (Exp.PReferenceScript scriptTxIn)
              Exp.NoScriptDatum
              redeemer
              (Script.ExecutionUnits 0 0)

      txBodyContent =
        Exp.defaultTxBodyContent
          & Exp.setTxProtocolParams exampleProtocolParams
          & Exp.setTxProposalProcedures
            ( Exp.mkTxProposalProcedures
                [ (propA, mkProposingWitness (mkRedeemer 1))
                , (propB, mkProposingWitness (mkRedeemer 2))
                ]
            )
          & Exp.setTxFee 0

  Exp.UnsignedTx ledgerTx <- H.evalEither $ Exp.makeUnsignedTx Exp.ConwayEra txBodyContent

  -- Sanity check: the body itself is in insertion order regardless of the
  -- bug under test (the bug only affects redeemer indexing, not the body).
  let bodyProposals = toList $ ledgerTx ^. L.bodyTxL . UnexportedLedger.proposalProceduresTxBodyL
  bodyProposals H.=== [propA, propB]

  -- The redeemer map must key 'propA''s witness to index 0 and 'propB''s
  -- to index 1 (insertion order). 'Ord'-sorted indexing would give the
  -- opposite, since 'propB' has the smaller deposit and sorts first.
  let redeemers = ledgerTx ^. L.witsTxL . Alonzo.rdmrsTxWitsL
      expectedRedeemers =
        L.Redeemers $
          Map.fromList
            [
              ( L.ConwayProposing (L.AsIx 0)
              , (Api.toAlonzoData (mkRedeemer 1), Api.toAlonzoExUnits (Script.ExecutionUnits 0 0))
              )
            ,
              ( L.ConwayProposing (L.AsIx 1)
              , (Api.toAlonzoData (mkRedeemer 2), Api.toAlonzoExUnits (Script.ExecutionUnits 0 0))
              )
            ]
  redeemers H.=== expectedRedeemers

-- | 'makeUnsignedTx' must index a plutus-witnessed certificate's
-- 'L.ConwayCertifying' redeemer pointer by its position among all
-- certificates, witnessed and unwitnessed alike, never just among the
-- witnessed subset.
--
-- 'unwitnessedCert' (a plain stake registration, which the ledger never
-- requires a witness for) is placed before 'witnessedCert'. If unwitnessed
-- certs were skipped when assigning indices, 'witnessedCert' would land
-- at index 0 instead of the correct index 1.
prop_makeUnsignedTx_cert_redeemer_indices_count_unwitnessed_certs :: Property
prop_makeUnsignedTx_cert_redeemer_indices_count_unwitnessed_certs = H.property $ do
  stakeCred1 <- H.forAll genStakeCredential
  stakeCred2 <- H.forAll genStakeCredential
  scriptTxIn <- H.forAll genTxIn
  let shelleyCred1 = Api.toShelleyStakeCredential stakeCred1
      shelleyCred2 = Api.toShelleyStakeCredential stakeCred2

      -- Unwitnessed: a plain stake registration cert needs no witness.
      unwitnessedCert =
        Exp.Certificate $ L.ConwayTxCertDeleg (L.ConwayRegCert shelleyCred1 L.SNothing)

      -- Plutus-witnessed: a stake delegation cert witnessed by a plutus script.
      witnessedCert =
        Exp.Certificate $
          L.ConwayTxCertDeleg (L.ConwayDelegCert shelleyCred2 (L.DelegVote L.DRepAlwaysAbstain))

      redeemer = Script.unsafeHashableScriptData $ Script.ScriptDataConstructor 0 []

      certWitness =
        Exp.AnyPlutusScriptWitness $
          AnyPlutusCertifyingScriptWitness $
            Exp.PlutusScriptWitness
              Plutus.SPlutusV3
              (Exp.PReferenceScript scriptTxIn)
              Exp.NoScriptDatum
              redeemer
              (Script.ExecutionUnits 0 0)

      certs =
        Exp.mkTxCertificates
          Exp.ConwayEra
          [ (unwitnessedCert, Exp.AnyKeyWitnessPlaceholder)
          , (witnessedCert, certWitness)
          ]

      txBodyContent =
        Exp.defaultTxBodyContent
          & Exp.setTxCertificates certs
          & Exp.setTxProtocolParams exampleProtocolParams
          & Exp.setTxFee 0

  Exp.UnsignedTx ledgerTx <- H.evalEither $ Exp.makeUnsignedTx Exp.ConwayEra txBodyContent

  let redeemers = ledgerTx ^. L.witsTxL . Alonzo.rdmrsTxWitsL
      expectedRedeemers =
        L.Redeemers $
          Map.fromList
            [
              ( L.ConwayCertifying (L.AsIx 1)
              , (Api.toAlonzoData redeemer, Api.toAlonzoExUnits (Script.ExecutionUnits 0 0))
              )
            ]
  redeemers H.=== expectedRedeemers

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
            Ledger.mkBasicTxOut addr (L.MaryValue sendCoin mempty)
      txBodyContent =
        Exp.defaultTxBodyContent
          & Exp.setTxIns [(txIn, Exp.AnyKeyWitnessPlaceholder)]
          & Exp.setTxOuts [sendTxOut]
          & Exp.setTxFee 0
  case Exp.makeUnsignedTx era txBodyContent of
    Left err -> fail $ "makeUnsignedTx: " <> show err
    Right tx -> return (tx, utxo, changeAddr)

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
            Ledger.mkBasicTxOut addr (L.MaryValue sendCoin multiAsset)
      txBodyContent =
        Exp.defaultTxBodyContent
          & Exp.setTxIns [(txIn, Exp.AnyKeyWitnessPlaceholder)]
          & Exp.setTxOuts [sendTxOut]
          & Exp.setTxFee 0
  case Exp.makeUnsignedTx era txBodyContent of
    Left err -> fail $ "makeUnsignedTx: " <> show err
    Right tx -> return (tx, utxo, changeAddr)

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
            Ledger.mkBasicTxOut addr (L.MaryValue sendCoin mempty)
      txBodyContent =
        Exp.defaultTxBodyContent
          & Exp.setTxIns [(txIn, Exp.AnyKeyWitnessPlaceholder)]
          & Exp.setTxOuts [sendTxOut]
          & Exp.setTxFee 0
  case Exp.makeUnsignedTx era txBodyContent of
    Left err -> fail $ "makeUnsignedTx: " <> show err
    Right tx -> return (tx, utxo, changeAddr)

-- | A well-funded transaction (UTxO >> output + fee) always produces a
-- successful, fully balanced result with a positive fee.
prop_calcMinFeeRecursive_well_funded_succeeds :: Property
prop_calcMinFeeRecursive_well_funded_succeeds = H.property $ do
  (unsignedTx, utxo, changeAddr) <- H.forAll $ genFundedSimpleTx Exp.ConwayEra
  case Exp.calcMinFeeRecursive changeAddr unsignedTx utxo exampleProtocolParams mempty mempty 0 of
    Left err -> H.annotateShow err >> H.failure
    Right (Exp.UnsignedTx resultLedgerTx) -> do
      let resultFee = resultLedgerTx ^. L.bodyTxL . L.feeTxBodyL
      H.assert $ resultFee > L.Coin 0
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
  case Exp.calcMinFeeRecursive changeAddr unsignedTx utxo exampleProtocolParams mempty mempty 0 of
    Left err -> H.annotateShow err >> H.failure
    Right (Exp.UnsignedTx resultLedgerTx) -> do
      let resultFee = resultLedgerTx ^. L.bodyTxL . L.feeTxBodyL
      H.assert $ resultFee > L.Coin 0
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
  case Exp.calcMinFeeRecursive changeAddr unsignedTx utxo exampleProtocolParams mempty mempty 0 of
    Left err -> H.annotateShow err >> H.failure
    Right resultTx -> do
      secondResult <-
        H.evalEither $
          Exp.calcMinFeeRecursive changeAddr resultTx utxo exampleProtocolParams mempty mempty 0
      resultTx H.=== secondResult

-- | When the outputs exceed the UTxO value the function returns
-- 'Left (NotEnoughAdaForNewOutput _)' with a negative deficit coin.
prop_calcMinFeeRecursive_insufficient_funds :: Property
prop_calcMinFeeRecursive_insufficient_funds = H.property $ do
  (unsignedTx, utxo, changeAddr) <- H.forAll $ genUnderfundedTx Exp.ConwayEra
  case Exp.calcMinFeeRecursive changeAddr unsignedTx utxo exampleProtocolParams mempty mempty 0 of
    Left (Exp.NotEnoughAdaForNewOutput deficit) -> H.assert $ deficit < L.Coin 0
    Left Exp.NonAdaAssetsUnbalanced{} -> H.annotate "Unexpected NonAdaAssetsUnbalanced error" >> H.failure
    Left Exp.MinUTxONotMet{} -> H.annotate "Unexpected MinUTxONotMet error" >> H.failure
    Left Exp.FeeCalculationDidNotConverge -> H.annotate "Unexpected FeeCalculationDidNotConverge error" >> H.failure
    Left err -> H.annotateShow err >> H.failure
    Right _ -> H.failure

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
            Ledger.mkBasicTxOut addr sendValue
      txBodyContent =
        Exp.defaultTxBodyContent
          & Exp.setTxIns [(txIn, Exp.AnyKeyWitnessPlaceholder)]
          & Exp.setTxOuts [sendTxOut]
          & Exp.setTxFee 0
  case Exp.makeUnsignedTx era txBodyContent of
    Left err -> fail $ "makeUnsignedTx: " <> show err
    Right tx -> return (tx, utxo, changeAddr)

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
            Ledger.mkBasicTxOut addr (L.MaryValue (L.Coin 1_000_000) mempty)
      -- Output 2: tokens with tiny ADA (below min UTxO)
      sendTxOut2 =
        Exp.obtainCommonConstraints era $
          Exp.TxOut $
            Ledger.mkBasicTxOut addr (L.MaryValue (L.Coin 1_000) multiAsset)
      txBodyContent =
        Exp.defaultTxBodyContent
          & Exp.setTxIns [(txIn, Exp.AnyKeyWitnessPlaceholder)]
          & Exp.setTxOuts [sendTxOut1, sendTxOut2]
          & Exp.setTxFee 0
  case Exp.makeUnsignedTx era txBodyContent of
    Left err -> fail $ "makeUnsignedTx: " <> show err
    Right tx -> return (tx, utxo, changeAddr)

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
  case Exp.makeUnsignedTx era txBodyContent of
    Left err -> fail $ "makeUnsignedTx: " <> show err
    Right tx -> return (tx, utxo, changeAddr)

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
    Left (Exp.MinUTxONotMet actual required) -> do
      H.annotate $ "Actual: " <> show actual <> ", Required: " <> show required
      H.assert $ actual < required
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
  case Exp.calcMinFeeRecursive changeAddr unsignedTx utxo exampleProtocolParams mempty mempty 0 of
    Left err -> H.annotateShow err >> H.failure
    Right (Exp.UnsignedTx resultLedgerTx) -> do
      let outs = toList $ resultLedgerTx ^. L.bodyTxL . L.outputsTxBodyL
      -- The result should have exactly one output (the change output)
      length outs H.=== 1

-- Exercise the public helper against pre-existing reference scripts. A wrong
-- inline witness must fail even when a correct reference is independently
-- available; native/Plutus metadata must match the resolved script category.
prop_receiving_witnesses_reject_invalid_declarations :: Property
prop_receiving_witnesses_reject_invalid_declarations = H.property $ do
  input <- H.forAll genTxIn
  oldPlutus <- H.forAll genPlutusScriptInEra
  v4 <- H.evalEither $ Exp.deserialisePlutusScriptInEra L.SPlutusV4 receivingEvenFixture
  Api.ShelleyBootstrapWitness _ bootstrap <-
    H.forAll $ genShelleyBootstrapWitness Api.ShelleyBasedEraDijkstra
  let native = DS.upgradeTimelock $ Api.toAllegraTimelock $ Api.RequireAllOf []
      wrongNative = DS.upgradeTimelock $ Api.toAllegraTimelock $ Api.RequireAnyOf []
      nativeScript = L.fromNativeScript native :: L.Script (Exp.LedgerEra Exp.DijkstraEra)
      legacyScript = L.upgradeScript $ Exp.plutusScriptInEraToScript oldPlutus
      pp = exampleProtocolParamsEra Exp.DijkstraEra
      referenceWitness = Exp.AnyScriptWitnessSimple $ Exp.SReferenceScript input
      mkReceivingReference redeemer budget =
        Exp.AnyScriptWitnessPlutus $
          AnyPlutusReceivingScriptWitness $
            Exp.PlutusScriptWitness
              L.SPlutusV4
              (Exp.PReferenceScript input)
              Exp.NoScriptDatum
              (Api.unsafeHashableScriptData (Api.ScriptDataNumber redeemer))
              budget
      receivingReference = mkReceivingReference 0 (Api.ExecutionUnits 0 0)
      (nativeTx, nativeUtxo) = receivingReferenceFixture input nativeScript
      (legacyTx, legacyUtxo) = receivingReferenceFixture input legacyScript
      v4Script = Exp.plutusScriptInEraToScript v4
      v4Hash = L.hashScript v4Script
      (v4Tx, v4Utxo) = receivingReferenceFixture input v4Script
      repeated =
        v4Tx
          & L.bodyTxL . L.outputsTxBodyL .~ [receivingFixtureOutput v4Hash 2, receivingFixtureOutput v4Hash 4]
      budgets = [Api.ExecutionUnits 100 200, Api.ExecutionUnits 300 400]
      declarations =
        Map.fromList
          [ (0, mkReceivingReference 2 $ Api.ExecutionUnits 100 200)
          , (1, mkReceivingReference 4 $ Api.ExecutionUnits 300 400)
          ]
  H.assert $
    isRight $
      Exp.addReceivingWitnesses pp nativeUtxo (Map.singleton 0 referenceWitness) nativeTx
  sharedReference <- H.evalEither $ Exp.addReceivingWitnesses pp v4Utxo declarations repeated
  let actualRedeemers = sharedReference ^. L.witsTxL . L.rdmrsTxWitsL . Alonzo.unRedeemersL
  Map.keys actualRedeemers H.=== [L.DijkstraReceiving (L.AsIx 0), L.DijkstraReceiving (L.AsIx 1)]
  map fst (Map.elems actualRedeemers)
    H.=== map (Api.toAlonzoData . Api.unsafeHashableScriptData . Api.ScriptDataNumber) [2, 4]
  map snd (Map.elems actualRedeemers) H.=== map Api.toAlonzoExUnits budgets
  Map.null (sharedReference ^. L.witsTxL . L.scriptTxWitsL) H.=== True
  Exp.addReceivingWitnesses pp v4Utxo (Map.delete 1 declarations) repeated
    H.=== Left "Missing V4 Receiving redeemer and budget for output index 1"
  Exp.addReceivingWitnesses
    pp
    nativeUtxo
    (Map.singleton 0 (Exp.AnyScriptWitnessSimple $ Exp.SScript $ Exp.SimpleScript wrongNative))
    nativeTx
    H.=== Left "Receiving witness script hash does not match the protected output"
  Exp.addReceivingWitnesses pp nativeUtxo (Map.singleton 0 receivingReference) nativeTx
    H.=== Left "V4 Receiving witness resolves to a native script"
  Exp.addReceivingWitnesses pp legacyUtxo (Map.singleton 0 referenceWitness) legacyTx
    H.=== Left "Native Receiving witness resolves to a Plutus script"
  Exp.addReceivingWitnesses pp legacyUtxo (Map.singleton 0 receivingReference) legacyTx
    H.=== Left "Receiving reference script must use Plutus V4"
  Exp.addReceivingWitnesses
    pp
    nativeUtxo
    (Map.singleton 0 referenceWitness)
    (nativeTx & L.witsTxL . L.bootAddrTxWitsL .~ Set.singleton bootstrap)
    H.=== Left "Receiving witnesses must be attached before collecting signatures"

receivingReferenceFixture
  :: Api.TxIn
  -> L.Script (Exp.LedgerEra Exp.DijkstraEra)
  -> (L.Tx L.TopTx (Exp.LedgerEra Exp.DijkstraEra), L.UTxO (Exp.LedgerEra Exp.DijkstraEra))
receivingReferenceFixture input script =
  let address = L.AddrProtected L.Testnet (L.ScriptHashObj $ L.hashScript script) L.StakeRefNull
      output = L.mkBasicTxOut address (Mary.MaryValue (L.Coin 10_000_000) mempty)
      refInput = Api.toShelleyTxIn input
      refOutput = output & L.referenceScriptTxOutL .~ SJust script
      body =
        L.mkBasicTxBody
          & L.outputsTxBodyL .~ [output]
          & L.referenceInputsTxBodyL .~ Set.singleton refInput
   in (L.mkBasicTx body, L.UTxO $ Map.singleton refInput refOutput)

-- Genuine compiled V4 fixture bytes; see test/data/cip160/README.md.
receivingEvenFixture :: BS.ByteString
receivingEvenFixture =
  Base16.decodeLenient
    "58ce0102009800aba4aba1ab9cabd8488888c8ca64cdc3a401d20052200264cdc3a400520052200252866400a44002914c9804000ca00260120029400923004001a5019900291000a4465266e1d20049001910012942600860132003220010dd5499803240086eb0c02000644004264c6600a921035054350049a930a6600200329375400522801246500237560052328011bac00294004dd68012465002375c00549900748cdc3a400066e180052004a501baa993758003229001914800c8a00200d0048014dd718050008488880081"

receivingChangeFixture :: BS.ByteString
receivingChangeFixture =
  Base16.decodeLenient
    "59010e0102009800aba3aba1ab9c4888a64dd6000c8a40064520032232323293370e90014801c88009300149b320072200148a4cc0108004c0380064942600693023299300e33001222222222222222222201126980719800911111111111111111100793528a9469404526eb002e452003229001914800c8a4006452003229001914800c8a4006452003229001914800c8a4006452003229001914800c8a4006452003223298009bae0259800811cdd6010cdd600fcdd600ecdd580dcdd600ccdd580bcdd580acdd5809c0466eb003e6eac0366eac02e6eac0266eac01e6eb00166002007375a60660048138c0a1d680000452003280098019806800a50021baa0021326330024901035054350049a931"

receivingMatchesFixture :: BS.ByteString
receivingMatchesFixture =
  Base16.decodeLenient
    "5901d70102009800aba4aba1ab9c4888c88c8ca64cdc3a401d20032200252866400644002911919499b894800000a943266ebc0064cdc4001240013300800293759200d222200448a4006452003229001914800c8a4006452003229001914800c8a4006452003229001914800c8a4006452003229001914800c8a4006446eb0070000099319804a48103505436004994a13232993370e90014800c880094a19900191000a4465266e1d2002900191001294266e3cdd71807c800c88005201722220010dd500114a13293370e90024800c880094a13299800800ca4dd500148a0049194008dd580148ca0046eb000a5001375a0049194008dd7001526403d23299800800ca4dd500148a0049194008dd580148ca0046eb000a5001375a0049194008dd7001526404523370e0060034a0900b9111001a940601b2003220010dd5198011111001130dd5198009111002131149bac003914800c8a40064530010069bab00480164c0480065001375c6026002940088040060120046eb400899319802a49035054350049a930dd54800c888800926eb000645200322900191400401a0090029bae300a001032300100122239001911499b8748000016653001007800c00d00719b810054800a006919319802a4810350543700498931"

receivingWitness
  :: Exp.PlutusScriptInEra 'Plutus.PlutusV4 L.DijkstraEra
  -> Exp.AnyScriptWitness L.DijkstraEra
receivingWitness script = receivingWitnessWith 0 (Api.ExecutionUnits 0 0) script

receivingWitnessWith
  :: Integer
  -> Api.ExecutionUnits
  -> Exp.PlutusScriptInEra 'Plutus.PlutusV4 L.DijkstraEra
  -> Exp.AnyScriptWitness L.DijkstraEra
receivingWitnessWith redeemer budget script =
  Exp.AnyScriptWitnessPlutus $
    AnyPlutusReceivingScriptWitness $
      Exp.PlutusScriptWitness
        L.SPlutusV4
        (Exp.PScript script)
        Exp.NoScriptDatum
        (Api.unsafeHashableScriptData $ Api.ScriptDataNumber redeemer)
        budget

receivingFixtureOutput :: L.ScriptHash -> Integer -> L.TxOut L.DijkstraEra
receivingFixtureOutput hash datum =
  L.mkBasicTxOut
    (L.AddrProtected L.Testnet (L.ScriptHashObj hash) L.StakeRefNull)
    (Mary.MaryValue (L.Coin 3_000_000) mempty)
    & L.datumTxOutL
      .~ L.Datum
        ( L.dataToBinaryData $
            Api.toAlonzoData $
              Api.unsafeHashableScriptData $
                Api.ScriptDataNumber datum
        )

receivingBalanceInputs
  :: H.PropertyT
       IO
       (Api.AddressInEra Exp.DijkstraEra, Api.TxIn, Api.TxIn, L.UTxO L.DijkstraEra)
receivingBalanceInputs = do
  Api.PaymentKeyHash keyHash <- H.forAll $ genVerificationKeyHash Api.AsPaymentKey
  [input, collateral] <-
    H.forAll $ Set.toList <$> Gen.set (Range.singleton 2) genTxIn
  let address = L.Addr L.Testnet (L.KeyHashObj keyHash) L.StakeRefNull
      output coin = L.mkBasicTxOut address (Mary.MaryValue (L.Coin coin) mempty)
  pure
    ( Api.fromShelleyAddr Api.ShelleyBasedEraDijkstra address
    , input
    , collateral
    , L.UTxO $
        Map.fromList
          [ (Api.toShelleyTxIn input, output 100_000_000)
          , (Api.toShelleyTxIn collateral, output 5_000_000)
          ]
    )

receivingAutoBalance
  :: Api.AddressInEra Exp.DijkstraEra
  -> L.UTxO L.DijkstraEra
  -> Exp.TxBodyContent L.DijkstraEra
  -> Either
       (Exp.TxBodyErrorAutoBalance L.DijkstraEra)
       (Exp.UnsignedTx L.DijkstraEra, Exp.TxBodyContent L.DijkstraEra)
receivingAutoBalance = receivingAutoBalanceWithPParams (exampleProtocolParamsEra Exp.DijkstraEra)

receivingAutoBalanceWithPParams
  :: L.PParams L.DijkstraEra
  -> Api.AddressInEra Exp.DijkstraEra
  -> L.UTxO L.DijkstraEra
  -> Exp.TxBodyContent L.DijkstraEra
  -> Either
       (Exp.TxBodyErrorAutoBalance L.DijkstraEra)
       (Exp.UnsignedTx L.DijkstraEra, Exp.TxBodyContent L.DijkstraEra)
receivingAutoBalanceWithPParams pp change utxo content =
  Exp.makeTransactionBodyAutoBalance
    (Api.SystemStart $ Time.posixSecondsToUTCTime 0)
    (Api.LedgerEpochInfo $ Slotting.fixedEpochInfo (Slotting.EpochSize 100) (Slotting.mkSlotLength 1000))
    pp
    mempty
    mempty
    utxo
    content
    change
    Nothing

receivingBalanceContent
  :: Api.AddressInEra Exp.DijkstraEra
  -> Api.TxIn
  -> Api.TxIn
  -> Exp.TxBodyContent L.DijkstraEra
receivingBalanceContent change input collateral =
  Exp.defaultTxBodyContent
    & Exp.setTxProtocolParams (exampleProtocolParamsEra Exp.DijkstraEra)
    & Exp.setTxIns [(input, Exp.AnyKeyWitnessPlaceholder)]
    & Exp.setTxInsCollateral [collateral]
    & Exp.setTxReturnCollateral
      ( Exp.TxReturnCollateral $
          L.mkBasicTxOut
            (Api.toShelleyAddr change)
            (Mary.MaryValue (L.Coin 2_000_000) mempty)
      )
    & Exp.setTxTotalCollateral (Exp.TxTotalCollateral $ L.Coin 3_000_000)

-- Original output positions survive ordinary, protected-key and native gaps.
-- Identical V4 outputs have independent redeemers/budgets; one native script
-- declaration supplies both native outputs without a Plutus redeemer.
prop_receiving_native_index_gap :: Property
prop_receiving_native_index_gap = H.withTests 1 $ H.property $ do
  script <- H.evalEither $ Exp.deserialisePlutusScriptInEra L.SPlutusV4 receivingEvenFixture
  Api.PaymentKeyHash keyHash <- H.forAll $ genVerificationKeyHash Api.AsPaymentKey
  let native = DS.upgradeTimelock $ Api.toAllegraTimelock $ Api.RequireMOf 0 []
      nativeScript = L.fromNativeScript native :: L.Script L.DijkstraEra
      nativeHash = L.hashScript nativeScript
      hash = L.hashScript $ Exp.plutusScriptInEraToScript script
      nativeWitness = Exp.AnyScriptWitnessSimple $ Exp.SScript $ Exp.SimpleScript native
      ordinary =
        receivingFixtureOutput hash 2
          & L.addrTxOutL .~ L.Addr L.Testnet (L.ScriptHashObj hash) L.StakeRefNull
      protectedKey =
        ordinary & L.addrTxOutL .~ L.AddrProtected L.Testnet (L.KeyHashObj keyHash) L.StakeRefNull
      witnesses =
        Map.fromList
          [ (1, receivingWitnessWith 10 (Api.ExecutionUnits 100 200) script)
          , (2, nativeWitness)
          , (3, receivingWitnessWith 20 (Api.ExecutionUnits 300 400) script)
          ]
      content =
        Exp.defaultTxBodyContent
          & Exp.setTxProtocolParams (exampleProtocolParamsEra Exp.DijkstraEra)
          & Exp.setTxOuts
            ( map
                Exp.TxOut
                [ ordinary
                , receivingFixtureOutput hash 2
                , receivingFixtureOutput nativeHash 2
                , receivingFixtureOutput hash 2
                , protectedKey
                , receivingFixtureOutput nativeHash 4
                , ordinary
                ]
            )
          & Exp.setTxReceivingWitnesses witnesses
  indexed <- H.evalEither $ Exp.extractAllIndexedPlutusScriptWitnesses Exp.DijkstraEra content
  let actual :: [(Word32, L.ScriptHash, L.PlutusPurpose L.AsIx L.DijkstraEra)]
      actual =
        [ (outputIndex, destination, pointer)
        | Exp.AnyIndexedPlutusScriptWitness
            (Exp.IndexedPlutusScriptWitness (Exp.WitReceiving outputIndex destination) pointer _) <-
            indexed
        ]
  actual H.=== [(1, hash, L.DijkstraReceiving (L.AsIx 1)), (3, hash, L.DijkstraReceiving (L.AsIx 3))]
  Exp.UnsignedTx tx <- H.evalEither $ Exp.makeUnsignedTx Exp.DijkstraEra content
  let redeemers = tx ^. L.witsTxL . L.rdmrsTxWitsL . Alonzo.unRedeemersL
  Map.keys redeemers H.=== [L.DijkstraReceiving (L.AsIx 1), L.DijkstraReceiving (L.AsIx 3)]
  forM_
    ( [(1, 10, Api.ExecutionUnits 100 200), (3, 20, Api.ExecutionUnits 300 400)]
        :: [(Word32, Integer, Api.ExecutionUnits)]
    )
    $ \(outputIndex, redeemer, budget) -> do
      (actualRedeemer, actualBudget) <-
        H.evalMaybe $ Map.lookup (L.DijkstraReceiving $ L.AsIx outputIndex) redeemers
      actualRedeemer H.=== Api.toAlonzoData (Api.unsafeHashableScriptData $ Api.ScriptDataNumber redeemer)
      actualBudget H.=== Api.toAlonzoExUnits budget
  H.assert $
    isLeft $
      Exp.makeUnsignedTx Exp.DijkstraEra $
        content & Exp.setTxReceivingWitnesses (Map.delete 3 witnesses)
  forM_ ([0, 4, 99] :: [Word32]) $ \invalidIndex ->
    H.assert $
      isLeft $
        Exp.makeUnsignedTx Exp.DijkstraEra $
          content & Exp.setTxReceivingWitnesses (Map.insert invalidIndex (receivingWitness script) witnesses)

-- This actual compiled validator checks the redeemer against only the resolved
-- protected output's inline datum. Swapping same-hash entries must fail both;
-- changing one entry must leave the sibling's invocation successful.
prop_receiving_output_context :: Property
prop_receiving_output_context = H.withTests 3 $ H.property $ do
  (change, input, collateral, utxo) <- receivingBalanceInputs
  script <- H.evalEither $ Exp.deserialisePlutusScriptInEra L.SPlutusV4 receivingMatchesFixture
  let hash = L.hashScript $ Exp.plutusScriptInEraToScript script
      ordinary = receivingFixtureOutput hash 2 & L.addrTxOutL .~ Api.toShelleyAddr change
      content redeemer1 redeemer3 datum3 =
        receivingBalanceContent change input collateral
          & Exp.setTxOuts
            ( map
                Exp.TxOut
                [ordinary, receivingFixtureOutput hash 2, ordinary, receivingFixtureOutput hash datum3]
            )
          & Exp.setTxReceivingWitnesses
            ( Map.fromList
                [ (1, receivingWitnessWith redeemer1 (Api.ExecutionUnits 0 0) script)
                , (3, receivingWitnessWith redeemer3 (Api.ExecutionUnits 0 0) script)
                ]
            )
      evaluate tx =
        Exp.evaluateDijkstraTransactionExecutionUnits
          (Api.SystemStart $ Time.posixSecondsToUTCTime 0)
          (Api.LedgerEpochInfo $ Slotting.fixedEpochInfo (Slotting.EpochSize 100) (Slotting.mkSlotLength 1000))
          (exampleProtocolParamsEra Exp.DijkstraEra)
          utxo
          tx
  forM_
    ( [(2, 4, 4, True, True), (4, 2, 4, False, False), (2, 99, 4, True, False), (2, 2, 2, True, True)]
        :: [(Integer, Integer, Integer, Bool, Bool)]
    )
    $ \(redeemer1, redeemer3, datum3, succeeds1, succeeds3) -> do
      Exp.UnsignedTx tx <-
        H.evalEither $ Exp.makeUnsignedTx Exp.DijkstraEra (content redeemer1 redeemer3 datum3)
      let report = evaluate tx
      Map.keys report
        H.=== [(SNothing, Api.ScriptWitnessIndexReceiving 1), (SNothing, Api.ScriptWitnessIndexReceiving 3)]
      result1 <- H.evalMaybe $ Map.lookup (SNothing, Api.ScriptWitnessIndexReceiving 1) report
      result3 <- H.evalMaybe $ Map.lookup (SNothing, Api.ScriptWitnessIndexReceiving 3) report
      isRight result1 H.=== succeeds1
      isRight result3 H.=== succeeds3

prop_receiving_nested_output_context :: Property
prop_receiving_nested_output_context = H.withTests 3 $ H.property $ do
  (change, input, collateral, initialUTxO) <- receivingBalanceInputs
  childInput <-
    H.forAll $ Gen.filter (\candidate -> candidate /= input && candidate /= collateral) genTxIn
  script <- H.evalEither $ Exp.deserialisePlutusScriptInEra L.SPlutusV4 receivingMatchesFixture
  let hash = L.hashScript $ Exp.plutusScriptInEraToScript script
      ordinary = receivingFixtureOutput hash 2 & L.addrTxOutL .~ Api.toShelleyAddr change
      L.UTxO initial = initialUTxO
      funding = L.mkBasicTxOut (Api.toShelleyAddr change) (Mary.MaryValue (L.Coin 5_000_000) mempty)
      utxo = L.UTxO $ Map.insert (Api.toShelleyTxIn childInput) funding initial
      parent =
        receivingBalanceContent change input collateral
          & Exp.setTxOuts (map Exp.TxOut [ordinary, receivingFixtureOutput hash 2])
          & Exp.setTxReceivingWitnesses
            (Map.singleton 1 $ receivingWitnessWith 2 (Api.ExecutionUnits 0 0) script)
  forM_ ([4, 2] :: [Integer]) $ \childRedeemer -> do
    Exp.UnsignedSubTx child <-
      H.evalEither $
        Exp.makeUnsignedSubTx $
          Exp.defaultSubTxBodyContent
            & Exp.setTxProtocolParams (exampleProtocolParamsEra Exp.DijkstraEra)
            & Exp.setTxIns [(childInput, Exp.AnyKeyWitnessPlaceholder)]
            & Exp.setTxOuts (map Exp.TxOut [ordinary, receivingFixtureOutput hash 4])
            & Exp.setTxReceivingWitnesses
              (Map.singleton 1 $ receivingWitnessWith childRedeemer (Api.ExecutionUnits 0 0) script)
    Exp.UnsignedTx tx <-
      H.evalEither $ Exp.makeUnsignedTx Exp.DijkstraEra (parent & Exp.setTxSubTransactions [child])
    let childId = Api.fromShelleyTxId $ L.txIdTx child
        report =
          Exp.evaluateDijkstraTransactionExecutionUnits
            (Api.SystemStart $ Time.posixSecondsToUTCTime 0)
            (Api.LedgerEpochInfo $ Slotting.fixedEpochInfo (Slotting.EpochSize 100) (Slotting.mkSlotLength 1000))
            (exampleProtocolParamsEra Exp.DijkstraEra)
            utxo
            tx
    parentResult <- H.evalMaybe $ Map.lookup (SNothing, Api.ScriptWitnessIndexReceiving 1) report
    childResult <- H.evalMaybe $ Map.lookup (SJust childId, Api.ScriptWitnessIndexReceiving 1) report
    H.assert $ isRight parentResult
    isRight childResult H.=== (childRedeemer == 4)
    Map.size report H.=== 2

prop_receiving_per_output_budget_limit :: Property
prop_receiving_per_output_budget_limit = H.withTests 3 $ H.property $ do
  (change, input, collateral, utxo) <- receivingBalanceInputs
  script <- H.evalEither $ Exp.deserialisePlutusScriptInEra L.SPlutusV4 receivingEvenFixture
  let hash = L.hashScript $ Exp.plutusScriptInEraToScript script
      pp = exampleProtocolParamsEra Exp.DijkstraEra
      content =
        receivingBalanceContent change input collateral
          & Exp.setTxOuts (map Exp.TxOut [receivingFixtureOutput hash 2, receivingFixtureOutput hash 2])
          & Exp.setTxReceivingWitnesses
            (Map.fromList [(0, receivingWitness script), (1, receivingWitness script)])
  (Exp.UnsignedTx tx, _) <- H.evalEither $ receivingAutoBalance change utxo content
  let budgets = map snd $ Map.elems $ tx ^. L.witsTxL . L.rdmrsTxWitsL . Alonzo.unRedeemersL
      apiBudgets = map Api.fromAlonzoExUnits budgets
  length apiBudgets H.=== 2
  let memories = map Api.executionMemory apiBudgets
      totalMemory = sum memories
      largestMemory = foldl' max 0 memories
      totalSteps = sum $ map Api.executionSteps apiBudgets
  H.assert $ largestMemory < totalMemory
  let cap = (largestMemory + totalMemory) `div` 2
      limited =
        pp
          & L.ppMaxTxExUnitsL
            .~ Api.toAlonzoExUnits
              Api.ExecutionUnits
                { Api.executionSteps = 2 * totalSteps
                , Api.executionMemory = cap
                }
  case receivingAutoBalanceWithPParams limited change utxo (content & Exp.setTxProtocolParams limited) of
    Left (Exp.TxBodyErrorDijkstraBalance reason) ->
      reason H.=== "Estimated Dijkstra batch exceeds the transaction execution-unit limit"
    other ->
      H.annotate (either show (const "Unexpected acceptance of aggregate budget overflow") other)
        >> H.failure

-- Appended protected change gets its own raw position. Both per-output budgets
-- must match actual evaluation of the returned final body without reindexing.

prop_receiving_change_final_domain :: Property
prop_receiving_change_final_domain = H.withTests 5 $ H.property $ do
  (ordinaryChange, input, collateral, utxo) <- receivingBalanceInputs
  script <- H.evalEither $ Exp.deserialisePlutusScriptInEra L.SPlutusV4 receivingEvenFixture
  changeScript <- H.evalEither $ Exp.deserialisePlutusScriptInEra L.SPlutusV4 receivingChangeFixture
  let hash = L.hashScript $ Exp.plutusScriptInEraToScript script
      changeHash = L.hashScript $ Exp.plutusScriptInEraToScript changeScript
      change =
        Api.fromShelleyAddr Api.ShelleyBasedEraDijkstra $
          L.AddrProtected L.Testnet (L.ScriptHashObj changeHash) L.StakeRefNull
      witnesses = Map.fromList [(0, receivingWitness script), (1, receivingWitness changeScript)]
      blueprint =
        receivingBalanceContent ordinaryChange input collateral
          & Exp.setTxOuts [Exp.TxOut $ receivingFixtureOutput hash 2]
          & Exp.setTxReceivingWitnesses witnesses
  (Exp.UnsignedTx tx, finalContent) <- H.evalEither $ receivingAutoBalance change utxo blueprint
  -- Appending change must preserve both original output-index entries.
  Map.keysSet (Exp.txReceivingWitnesses finalContent) H.=== Map.keysSet witnesses
  let body = tx ^. L.bodyTxL
      expected =
        Exp.evaluateDijkstraTransactionExecutionUnits
          (Api.SystemStart $ Time.posixSecondsToUTCTime 0)
          (Api.LedgerEpochInfo $ Slotting.fixedEpochInfo (Slotting.EpochSize 100) (Slotting.mkSlotLength 1000))
          (exampleProtocolParamsEra Exp.DijkstraEra)
          utxo
          tx
      actual = tx ^. L.witsTxL . L.rdmrsTxWitsL . Alonzo.unRedeemersL
  budgets <- forM ([1, 0] :: [Word32]) $ \outputIndex -> do
    pointer@(L.DijkstraReceiving (L.AsIx index)) <-
      H.evalMaybe $
        strictMaybeToMaybe $
          L.redeemerPointer body (L.DijkstraReceiving $ L.AsItem outputIndex)
    (_, used) <- H.evalMaybe $ Map.lookup pointer actual
    Right (_, estimated) <-
      H.evalMaybe $
        Map.lookup
          (SNothing, Api.ScriptWitnessIndexReceiving index)
          expected
    used H.=== Api.toAlonzoExUnits estimated
    pure used
  case budgets of
    [changeBudget, recipientBudget] -> H.assert $ changeBudget /= recipientBudget
    _ -> do
      H.annotate "Expected exactly two Receiving budgets for change and recipient"
      H.annotateShow budgets
      H.failure
  -- A script invocation that exists only in protected change is also budgeted.
  let changeOnly =
        receivingBalanceContent ordinaryChange input collateral
          & Exp.setTxReceivingWitnesses (Map.singleton 0 $ receivingWitness changeScript)
  (Exp.UnsignedTx changeTx, _) <- H.evalEither $ receivingAutoBalance change utxo changeOnly
  Map.size (changeTx ^. L.witsTxL . L.rdmrsTxWitsL . Alonzo.unRedeemersL) H.=== 1

-- A failing Receiving child is sufficient for ScriptInvalid; passing siblings
-- and the script-free parent must not be required to fail independently.
prop_receiving_invalid_batch :: Property
prop_receiving_invalid_batch = H.withTests 5 $ H.property $ do
  (change, input, collateral, initialUTxO) <- receivingBalanceInputs
  script <- H.evalEither $ Exp.deserialisePlutusScriptInEra L.SPlutusV4 receivingEvenFixture
  let hash = L.hashScript $ Exp.plutusScriptInEraToScript script
      parent = receivingBalanceContent change input collateral & Exp.setTxScriptValidity Api.ScriptInvalid
  forM_ ([[1], [1, 2], [2], []] :: [[Integer]]) $ \datums -> do
    childInputs <-
      Set.toList
        <$> H.forAll
          ( Gen.set (Range.singleton $ length datums) $
              Gen.filter (\candidate -> candidate /= input && candidate /= collateral) genTxIn
          )
    let L.UTxO initial = initialUTxO
        childFunding = L.mkBasicTxOut (Api.toShelleyAddr change) (Mary.MaryValue (L.Coin 5_000_000) mempty)
        utxo =
          L.UTxO $ Map.union initial $ Map.fromList [(Api.toShelleyTxIn i, childFunding) | i <- childInputs]
    children <- forM (zip childInputs datums) $ \(childInput, datum) -> do
      Exp.UnsignedSubTx child <-
        H.evalEither $
          Exp.makeUnsignedSubTx $
            Exp.defaultSubTxBodyContent
              & Exp.setTxProtocolParams (exampleProtocolParamsEra Exp.DijkstraEra)
              & Exp.setTxIns [(childInput, Exp.AnyKeyWitnessPlaceholder)]
              & Exp.setTxOuts [Exp.TxOut $ receivingFixtureOutput hash datum]
              & Exp.setTxReceivingWitnesses (Map.singleton 0 $ receivingWitness script)
      pure child
    let candidateParent =
          if null datums
            then
              parent
                { Exp.txInsCollateral = []
                , Exp.txReturnCollateral = Nothing
                , Exp.txTotalCollateral = Nothing
                }
            else parent
    H.annotateShow datums
    case receivingAutoBalance change utxo (Exp.setTxSubTransactions children candidateParent) of
      Left Exp.TxBodyScriptBadScriptValidity | all even datums -> H.success
      Right{} | any odd datums -> H.success
      other -> H.annotate (either show (const "Unexpected balancing success") other) >> H.failure

prop_receiving_protected_collateral :: Property
prop_receiving_protected_collateral = H.withTests 5 $ H.property $ do
  (change, input, collateral, utxo) <- receivingBalanceInputs
  script <- H.evalEither $ Exp.deserialisePlutusScriptInEra L.SPlutusV4 receivingChangeFixture
  let hash = L.hashScript $ Exp.plutusScriptInEraToScript script
      protectedChange =
        Api.fromShelleyAddr Api.ShelleyBasedEraDijkstra $
          L.AddrProtected L.Testnet (L.ScriptHashObj hash) L.StakeRefNull
      explicitReturn =
        receivingBalanceContent change input collateral
          & Exp.setTxReceivingWitnesses (Map.singleton 0 $ receivingWitness script)
      autoReturn = explicitReturn{Exp.txReturnCollateral = Nothing, Exp.txTotalCollateral = Nothing}
      protectedReturn =
        explicitReturn
          { Exp.txReturnCollateral =
              Just $
                Exp.TxReturnCollateral $
                  L.mkBasicTxOut (Api.toShelleyAddr protectedChange) (Mary.MaryValue (L.Coin 2_000_000) mempty)
          }
          & Exp.setTxOuts [Exp.TxOut $ receivingFixtureOutput hash 2]
  H.assert $ isRight $ receivingAutoBalance protectedChange utxo explicitReturn
  -- Explicit total collateral does not imply an automatically generated return.
  let totalOnly = explicitReturn{Exp.txReturnCollateral = Nothing}
  (Exp.UnsignedTx totalOnlyTx, totalOnlyContent) <-
    H.evalEither $ receivingAutoBalance protectedChange utxo totalOnly
  H.assert $ isNothing $ Exp.txReturnCollateral totalOnlyContent
  totalOnlyTx ^. L.bodyTxL . L.collateralReturnTxBodyL H.=== SNothing
  case receivingAutoBalance protectedChange utxo autoReturn of
    Left (Exp.TxBodyErrorMakeUnsignedTx Exp.MakeUnsignedTxProtectedCollateralReturn) -> H.success
    other -> H.annotate (either show (const "Unexpected balancing success") other) >> H.failure
  Exp.makeUnsignedTx Exp.DijkstraEra protectedReturn
    H.=== Left Exp.MakeUnsignedTxProtectedCollateralReturn

-- Child signatures remain usable when their body is unchanged. Changing a
-- script budget changes that body and must be refused before returning a tx.
prop_receiving_signed_children :: Property
prop_receiving_signed_children = H.withTests 5 $ H.property $ do
  (change, input, collateral, initialUTxO) <- receivingBalanceInputs
  childInput <-
    H.forAll $ Gen.filter (\candidate -> candidate /= input && candidate /= collateral) genTxIn
  sk <- H.forAllWith (const "<ShelleyWitnessSigningKey>") genShelleyWitnessSigningKey
  script <- H.evalEither $ Exp.deserialisePlutusScriptInEra L.SPlutusV4 receivingEvenFixture
  let hash = L.hashScript $ Exp.plutusScriptInEraToScript script
      childFunding = L.mkBasicTxOut (Api.toShelleyAddr change) (Mary.MaryValue (L.Coin 5_000_000) mempty)
      L.UTxO initial = initialUTxO
      utxo = L.UTxO $ Map.insert (Api.toShelleyTxIn childInput) childFunding initial
      parent = receivingBalanceContent change input collateral
      childContent =
        Exp.defaultSubTxBodyContent
          & Exp.setTxProtocolParams (exampleProtocolParamsEra Exp.DijkstraEra)
          & Exp.setTxIns [(childInput, Exp.AnyKeyWitnessPlaceholder)]
  plain <- H.evalEither $ Exp.makeUnsignedSubTx childContent
  let signedPlain@(Exp.SignedSubTx plainLedger) =
        Exp.signSubTx [] [Exp.makeSubTxKeyWitness plain sk] plain
      plainParent =
        parent
          { Exp.txInsCollateral = []
          , Exp.txReturnCollateral = Nothing
          , Exp.txTotalCollateral = Nothing
          }
  (Exp.UnsignedTx balanced, _) <-
    H.evalEither $
      receivingAutoBalance change utxo (Exp.setTxSignedSubTransactions [signedPlain] plainParent)
  LOMap.lookup (L.txIdTx plainLedger) (balanced ^. L.bodyTxL . Dijkstra.subTransactionsTxBodyL)
    H.=== Just plainLedger
  receiving <-
    H.evalEither $
      Exp.makeUnsignedSubTx $
        childContent
          & Exp.setTxOuts [Exp.TxOut $ receivingFixtureOutput hash 2]
          & Exp.setTxReceivingWitnesses (Map.singleton 0 $ receivingWitness script)
  let signedReceiving = Exp.signSubTx [] [Exp.makeSubTxKeyWitness receiving sk] receiving
  case receivingAutoBalance change utxo (Exp.setTxSignedSubTransactions [signedReceiving] parent) of
    Left (Exp.TxBodyErrorDijkstraBalance reason) ->
      reason H.=== "Dijkstra child budgets must be estimated before collecting child signatures"
    other -> H.annotate (either show (const "Unexpected balancing success") other) >> H.failure

-- More native signer keys than the funding-input estimate must be covered,
-- including when only the UTxO reference reveals those keys.
prop_receiving_native_fees :: Property
prop_receiving_native_fees = H.withTests 5 $ H.property $ do
  (change, input, collateral, initialUTxO) <- receivingBalanceInputs
  reference <-
    H.forAll $ Gen.filter (\candidate -> candidate /= input && candidate /= collateral) genTxIn
  keys <-
    Set.toList <$> H.forAll (Gen.set (Range.singleton 5) $ genVerificationKeyHash Api.AsPaymentKey)
  let native = DS.upgradeTimelock $ Api.toAllegraTimelock $ Api.RequireAllOf $ map Api.RequireSignature keys
      script = L.fromNativeScript native :: L.Script L.DijkstraEra
      hash = L.hashScript script
      recipient = L.AddrProtected L.Testnet (L.ScriptHashObj hash) L.StakeRefNull
      output = L.mkBasicTxOut recipient (Mary.MaryValue (L.Coin 2_000_000) mempty)
      referenceOutput =
        L.mkBasicTxOut (Api.toShelleyAddr change) (Mary.MaryValue (L.Coin 2_000_000) mempty)
          & L.referenceScriptTxOutL .~ SJust script
      L.UTxO initial = initialUTxO
      utxo = L.UTxO $ Map.insert (Api.toShelleyTxIn reference) referenceOutput initial
      base =
        Exp.defaultTxBodyContent
          & Exp.setTxProtocolParams (exampleProtocolParamsEra Exp.DijkstraEra)
          & Exp.setTxIns [(input, Exp.AnyKeyWitnessPlaceholder)]
          & Exp.setTxOuts [Exp.TxOut output]
      inline =
        base
          & Exp.setTxReceivingWitnesses
            (Map.singleton 0 $ Exp.AnyScriptWitnessSimple $ Exp.SScript $ Exp.SimpleScript native)
      byReference =
        base
          & Exp.setTxInsReference (Exp.TxInsReference [reference] Set.empty)
          & Exp.setTxReceivingWitnesses
            (Map.singleton 0 $ Exp.AnyScriptWitnessSimple $ Exp.SReferenceScript reference)
  forM_ ([inline, byReference] :: [Exp.TxBodyContent L.DijkstraEra]) $ \content -> do
    (Exp.UnsignedTx tx, _) <- H.evalEither $ receivingAutoBalance change utxo content
    let nativeHashes = Set.fromList [L.asWitness $ Api.unPaymentKeyHash key | key <- keys]
        requiredFee =
          LedgerTools.calcMinFeeTxNativeScriptWits
            utxo
            (exampleProtocolParamsEra Exp.DijkstraEra)
            tx
            nativeHashes
    H.assert $ tx ^. L.bodyTxL . L.feeTxBodyL >= requiredFee

prop_receiving_child_signature_fees :: Property
prop_receiving_child_signature_fees = H.withTests 5 $ H.property $ do
  (_, input, collateral, initialUTxO) <- receivingBalanceInputs
  childInputs <-
    Set.toList
      <$> H.forAll
        ( Gen.set (Range.singleton 2) $
            Gen.filter (\candidate -> candidate /= input && candidate /= collateral) genTxIn
        )
  signingKey <- exampleSigningKey
  let key = Api.verificationKeyHash $ Api.getVerificationKey signingKey
      ordinary = L.Addr L.Testnet (L.KeyHashObj $ Api.unPaymentKeyHash key) L.StakeRefNull
      protected = L.AddrProtected L.Testnet (L.KeyHashObj $ Api.unPaymentKeyHash key) L.StakeRefNull
      change = Api.fromShelleyAddr Api.ShelleyBasedEraDijkstra ordinary
      L.UTxO initial = initialUTxO
      funding = L.mkBasicTxOut ordinary (Mary.MaryValue (L.Coin 5_000_000) mempty)
      utxo =
        L.UTxO $
          Map.union
            (Map.fromList [(Api.toShelleyTxIn childInput, funding) | childInput <- childInputs])
            (Map.map (\out -> out & L.addrTxOutL .~ ordinary) initial)
      pp = exampleProtocolParamsEra Exp.DijkstraEra
  children <- forM childInputs $ \childInput -> do
    Exp.UnsignedSubTx child <-
      H.evalEither $
        Exp.makeUnsignedSubTx $
          Exp.defaultSubTxBodyContent
            & Exp.setTxProtocolParams pp
            & Exp.setTxIns [(childInput, Exp.AnyKeyWitnessPlaceholder)]
            & Exp.setTxOuts [Exp.TxOut $ L.mkBasicTxOut protected (Mary.MaryValue (L.Coin 2_000_000) mempty)]
    pure child
  (Exp.UnsignedTx balanced, finalContent) <-
    H.evalEither $
      receivingAutoBalance change utxo $
        Exp.defaultTxBodyContent
          & Exp.setTxProtocolParams pp
          & Exp.setTxIns [(input, Exp.AnyKeyWitnessPlaceholder)]
          & Exp.setTxSubTransactions children
  signedChildren <- forM (toList $ balanced ^. L.bodyTxL . Dijkstra.subTransactionsTxBodyL) $ \child -> do
    Set.null (child ^. L.witsTxL . L.addrTxWitsL) H.=== True
    let unsigned = Exp.UnsignedSubTx child
    pure $
      Exp.signSubTx [] [Exp.makeSubTxKeyWitness unsigned $ Api.WitnessPaymentKey signingKey] unsigned
  unsignedParent <-
    H.evalEither $
      Exp.makeUnsignedTx Exp.DijkstraEra $
        finalContent & Exp.setTxSignedSubTransactions signedChildren
  let witness = Exp.makeKeyWitness Exp.DijkstraEra unsignedParent $ Api.WitnessPaymentKey signingKey
      Exp.SignedTx signed = Exp.signTx Exp.DijkstraEra [] [witness] unsignedParent
      requiredFee = L.getMinFeeTx pp signed 0
  H.assert $ signed ^. L.bodyTxL . L.feeTxBodyL >= requiredFee
