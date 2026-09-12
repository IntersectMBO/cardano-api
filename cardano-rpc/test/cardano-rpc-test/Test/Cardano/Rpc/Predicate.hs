{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Rpc.Predicate where

import Cardano.Api.Address
import Cardano.Api.Era (MaryEraOnwards, ShelleyBasedEra)
import Cardano.Api.Experimental.Era
import Cardano.Api.Plutus (ReferenceScript (..))
import Cardano.Api.Serialise.Raw
import Cardano.Api.Tx
import Cardano.Api.Value
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as U5c
import Cardano.Rpc.Proto.Api.UtxoRpc.Submit qualified as Submit
import Cardano.Rpc.Server.Internal.UtxoRpc.Predicate

import RIO

import Data.ByteString qualified as BS
import Data.ProtoLens (defMessage)
import Data.Set qualified as Set
import Network.GRPC.Spec (Proto)

import Test.Gen.Cardano.Api.Typed
  ( genAddressByron
  , genAddressInEra
  , genAddressShelley
  , genAssetName
  , genNetworkId
  , genPaymentCredential
  , genPolicyId
  , genPositiveQuantity
  , genStakeAddressReference
  , genStakeCredential
  , genTxOutUTxOContext
  )

import Hedgehog as H
import Hedgehog.Extras qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

-- ---------------------------------------------------------------------------
-- A. Default/empty patterns match everything
-- ---------------------------------------------------------------------------

hprop_default_predicate_matches_everything :: Property
hprop_default_predicate_matches_everything = H.property $ do
  txOut <- forAll genTxOut
  H.assertWith txOut $ matchesUtxoPredicate defMessage

hprop_default_address_pattern_matches_any_address :: Property
hprop_default_address_pattern_matches_any_address = H.property $ do
  address <- forAll $ genAddressInEra sbe
  H.assertWith address $ matchesAddressPattern defMessage

hprop_default_asset_pattern_matches_value_with_native_asset :: Property
hprop_default_asset_pattern_matches_value_with_native_asset = H.property $ do
  (value, _) <- forAll genValueWithNativeAsset
  H.assertWith value $ matchesAssetPattern defMessage

-- ---------------------------------------------------------------------------
-- B. Address matching — exact
-- ---------------------------------------------------------------------------

hprop_exact_address_matches_same :: Property
hprop_exact_address_matches_same = H.property $ do
  address <- forAll $ genAddressInEra sbe
  let addressPattern = defMessage & U5c.exactAddress .~ serialiseToRawBytes address
  H.assertWith address $ matchesAddressPattern addressPattern

hprop_exact_byron_address_matches_same :: Property
hprop_exact_byron_address_matches_same = H.property $ do
  byronAddress <- forAll genAddressByron
  let address :: AddressInEra TestEra
      address = AddressInEra ByronAddressInAnyEra byronAddress
      addressPattern = defMessage & U5c.exactAddress .~ serialiseToRawBytes address
  H.assertWith address $ matchesAddressPattern addressPattern

hprop_exact_address_rejects_different :: Property
hprop_exact_address_rejects_different = H.property $ do
  address1 <- forAll $ genAddressInEra sbe
  address2 <- forAll $ genAddressInEra sbe
  let addressPattern = defMessage & U5c.exactAddress .~ serialiseToRawBytes address1
  when (serialiseToRawBytes address1 /= serialiseToRawBytes address2) $
    H.assertWith address2 $
      not . matchesAddressPattern addressPattern

hprop_exact_byron_address_rejects_shelley :: Property
hprop_exact_byron_address_rejects_shelley = H.property $ do
  byronAddress <- forAll genAddressByron
  shelleyAddress <- forAll $ genAddressInEra sbe
  let address :: AddressInEra TestEra
      address = AddressInEra ByronAddressInAnyEra byronAddress
      addressPattern = defMessage & U5c.exactAddress .~ serialiseToRawBytes address
  -- Byron and Shelley addresses always differ (different header byte)
  H.assertWith shelleyAddress $ not . matchesAddressPattern addressPattern

hprop_payment_part_ignored_for_byron :: Property
hprop_payment_part_ignored_for_byron = H.property $ do
  byronAddress <- forAll genAddressByron
  credential <- forAll genPaymentCredential
  let address :: AddressInEra TestEra
      address = AddressInEra ByronAddressInAnyEra byronAddress
      addressPattern = defMessage & U5c.paymentPart .~ serialisePaymentCredential credential
  -- Byron addresses have no payment credential, so a paymentPart filter must reject
  H.assertWith address $ not . matchesAddressPattern addressPattern

hprop_delegation_part_ignored_for_byron :: Property
hprop_delegation_part_ignored_for_byron = H.property $ do
  byronAddress <- forAll genAddressByron
  credential <- forAll genStakeCredential
  let address :: AddressInEra TestEra
      address = AddressInEra ByronAddressInAnyEra byronAddress
      addressPattern = defMessage & U5c.delegationPart .~ serialiseStakeCredential credential
  -- Byron addresses have no delegation credential, so a delegationPart filter must reject
  H.assertWith address $ not . matchesAddressPattern addressPattern

-- ---------------------------------------------------------------------------
-- C. Address matching — payment & delegation parts
-- ---------------------------------------------------------------------------

hprop_payment_part_matches_same_credential :: Property
hprop_payment_part_matches_same_credential = H.property $ do
  credential <- forAll genPaymentCredential
  network <- forAll genNetworkId
  stakeReference <- forAll genStakeAddressReference
  let shelleyAddress = makeShelleyAddress network credential stakeReference
      address = shelleyAddressInEra sbe shelleyAddress
      addressPattern = defMessage & U5c.paymentPart .~ serialisePaymentCredential credential
  H.assertWith address $ matchesAddressPattern addressPattern

hprop_payment_part_rejects_different_credential :: Property
hprop_payment_part_rejects_different_credential = H.property $ do
  credential1 <- forAll genPaymentCredential
  credential2 <- forAll genPaymentCredential
  network <- forAll genNetworkId
  stakeReference <- forAll genStakeAddressReference
  let shelleyAddress = makeShelleyAddress network credential1 stakeReference
      address = shelleyAddressInEra sbe shelleyAddress
      addressPattern = defMessage & U5c.paymentPart .~ serialisePaymentCredential credential2
  when (serialisePaymentCredential credential1 /= serialisePaymentCredential credential2) $
    H.assertWith address $
      not . matchesAddressPattern addressPattern

hprop_delegation_part_matches_same_credential :: Property
hprop_delegation_part_matches_same_credential = H.property $ do
  stakeCredential <- forAll genStakeCredential
  paymentCredential <- forAll genPaymentCredential
  network <- forAll genNetworkId
  let stakeReference = StakeAddressByValue stakeCredential
      shelleyAddress = makeShelleyAddress network paymentCredential stakeReference
      address = shelleyAddressInEra sbe shelleyAddress
      addressPattern = defMessage & U5c.delegationPart .~ serialiseStakeCredential stakeCredential
  H.assertWith address $ matchesAddressPattern addressPattern

hprop_delegation_part_rejects_when_no_stake_address :: Property
hprop_delegation_part_rejects_when_no_stake_address = H.property $ do
  stakeCredential <- forAll genStakeCredential
  paymentCredential <- forAll genPaymentCredential
  network <- forAll genNetworkId
  let shelleyAddress = makeShelleyAddress network paymentCredential NoStakeAddress
      address = shelleyAddressInEra sbe shelleyAddress
      addressPattern = defMessage & U5c.delegationPart .~ serialiseStakeCredential stakeCredential
  H.assertWith address $ not . matchesAddressPattern addressPattern

hprop_address_pattern_all_fields_must_match :: Property
hprop_address_pattern_all_fields_must_match = H.property $ do
  paymentCredential <- forAll genPaymentCredential
  stakeCredential <- forAll genStakeCredential
  network <- forAll genNetworkId
  let stakeReference = StakeAddressByValue stakeCredential
      shelleyAddress = makeShelleyAddress network paymentCredential stakeReference
      address = shelleyAddressInEra sbe shelleyAddress
      -- Pattern with all three fields matching
      addressPattern =
        defMessage
          & U5c.exactAddress .~ serialiseToRawBytes shelleyAddress
          & U5c.paymentPart .~ serialisePaymentCredential paymentCredential
          & U5c.delegationPart .~ serialiseStakeCredential stakeCredential
  -- Should match when all fields agree
  H.assertWith address $ matchesAddressPattern addressPattern
  -- Break the payment part → fail
  otherPaymentCredential <- forAll genPaymentCredential
  when
    (serialisePaymentCredential paymentCredential /= serialisePaymentCredential otherPaymentCredential)
    $ do
      let brokenPattern = addressPattern & U5c.paymentPart .~ serialisePaymentCredential otherPaymentCredential
      H.assertWith address $ not . matchesAddressPattern brokenPattern
  -- Break the exact address → fail
  otherAddress <- forAll genAddressShelley
  when (serialiseToRawBytes shelleyAddress /= serialiseToRawBytes otherAddress) $ do
    let brokenPattern = addressPattern & U5c.exactAddress .~ serialiseToRawBytes otherAddress
    H.assertWith address $ not . matchesAddressPattern brokenPattern
  -- Break the delegation part → fail
  otherStakeCredential <- forAll genStakeCredential
  when
    (serialiseStakeCredential stakeCredential /= serialiseStakeCredential otherStakeCredential)
    $ do
      let brokenPattern = addressPattern & U5c.delegationPart .~ serialiseStakeCredential otherStakeCredential
      H.assertWith address $ not . matchesAddressPattern brokenPattern

-- ---------------------------------------------------------------------------
-- D. Asset matching
-- ---------------------------------------------------------------------------

hprop_asset_pattern_matches_by_policy :: Property
hprop_asset_pattern_matches_by_policy = H.property $ do
  (value, AssetId policy _tokenName) <- forAll genValueWithNativeAsset
  let assetPattern = defMessage & U5c.policyId .~ serialiseToRawBytes policy
  H.assertWith value $ matchesAssetPattern assetPattern

hprop_asset_pattern_matches_by_policy_and_name :: Property
hprop_asset_pattern_matches_by_policy_and_name = H.property $ do
  (value, AssetId policy tokenName) <- forAll genValueWithNativeAsset
  let assetPattern =
        defMessage
          & U5c.policyId .~ serialiseToRawBytes policy
          & U5c.assetName .~ serialiseToRawBytes tokenName
  H.assertWith value $ matchesAssetPattern assetPattern

hprop_asset_pattern_rejects_wrong_policy :: Property
hprop_asset_pattern_rejects_wrong_policy = H.property $ do
  (value, AssetId policy _tokenName) <- forAll genValueWithNativeAsset
  otherPolicy <- forAll genPolicyId
  when (serialiseToRawBytes policy /= serialiseToRawBytes otherPolicy) $ do
    let assetPattern = defMessage & U5c.policyId .~ serialiseToRawBytes otherPolicy
    H.assertWith value $ not . matchesAssetPattern assetPattern

hprop_asset_pattern_skips_ada :: Property
hprop_asset_pattern_skips_ada = H.property $ do
  policy <- forAll genPolicyId
  let value = [(AdaAssetId, Quantity 1_000_000)]
      assetPattern = defMessage & U5c.policyId .~ serialiseToRawBytes policy
  H.assertWith value $ not . matchesAssetPattern assetPattern

hprop_asset_pattern_rejects_zero_quantity :: Property
hprop_asset_pattern_rejects_zero_quantity = H.property $ do
  policy <- forAll genPolicyId
  tokenName <- forAll genAssetName
  let value = [(AdaAssetId, Quantity 1_000_000), (AssetId policy tokenName, Quantity 0)]
      assetPattern = defMessage & U5c.policyId .~ serialiseToRawBytes policy
  H.assertWith value $ not . matchesAssetPattern assetPattern

hprop_asset_pattern_matches_by_name_only :: Property
hprop_asset_pattern_matches_by_name_only = H.property $ do
  (value, AssetId _policy tokenName) <- forAll genValueWithNativeAsset
  let assetPattern = defMessage & U5c.assetName .~ serialiseToRawBytes tokenName
  H.assertWith value $ matchesAssetPattern assetPattern

hprop_asset_pattern_matches_one_of_multiple_assets :: Property
hprop_asset_pattern_matches_one_of_multiple_assets = H.property $ do
  policy1 <- forAll genPolicyId
  tokenName1 <- forAll genAssetName
  quantity1 <- forAll genPositiveQuantity
  policy2 <- forAll genPolicyId
  tokenName2 <- forAll genAssetName
  quantity2 <- forAll genPositiveQuantity
  let value =
        [ (AdaAssetId, Quantity 2_000_000)
        , (AssetId policy1 tokenName1, quantity1)
        , (AssetId policy2 tokenName2, quantity2)
        ]
      -- Match only the second asset by policy
      assetPattern = defMessage & U5c.policyId .~ serialiseToRawBytes policy2
  H.assertWith value $ matchesAssetPattern assetPattern

hprop_default_asset_pattern_rejects_ada_only :: Property
hprop_default_asset_pattern_rejects_ada_only = H.propertyOnce $ do
  -- An empty AssetPattern requires at least one native asset to exist;
  -- Ada alone is never considered a native asset.
  let value = [(AdaAssetId, Quantity 2_000_000)]
  H.assertWith value $ not . matchesAssetPattern defMessage

-- ---------------------------------------------------------------------------
-- E. TxOutputPattern (AND of address + asset)
-- ---------------------------------------------------------------------------

hprop_tx_output_pattern_requires_both :: Property
hprop_tx_output_pattern_requires_both = H.property $ do
  paymentCredential <- forAll genPaymentCredential
  network <- forAll genNetworkId
  stakeReference <- forAll genStakeAddressReference
  (value, AssetId policy tokenName) <- forAll genValueWithNativeAsset

  let shelleyAddress = makeShelleyAddress network paymentCredential stakeReference
      address = shelleyAddressInEra sbe shelleyAddress
      ledgerValue = toLedgerValue meo value
      txOutValue = TxOutValueShelleyBased sbe ledgerValue
      txOut = TxOut address txOutValue TxOutDatumNone ReferenceScriptNone
      addressPattern = defMessage & U5c.exactAddress .~ serialiseToRawBytes address
      assetPattern =
        defMessage
          & U5c.policyId .~ serialiseToRawBytes policy
          & U5c.assetName .~ serialiseToRawBytes tokenName
      -- Matching address + matching asset → match
      outputPattern =
        defMessage
          & U5c.address .~ addressPattern
          & U5c.asset .~ assetPattern
  H.assertWith txOut $ matchesTxOutputPattern outputPattern

  -- Matching address + wrong asset → fail
  otherPolicy <- forAll genPolicyId
  when (serialiseToRawBytes policy /= serialiseToRawBytes otherPolicy) $ do
    let wrongOutputPattern =
          defMessage
            & U5c.address .~ addressPattern
            & U5c.asset .~ (defMessage & U5c.policyId .~ serialiseToRawBytes otherPolicy)
    H.assertWith txOut $ not . matchesTxOutputPattern wrongOutputPattern

hprop_tx_output_pattern_address_only :: Property
hprop_tx_output_pattern_address_only = H.property $ do
  paymentCredential <- forAll genPaymentCredential
  network <- forAll genNetworkId
  stakeReference <- forAll genStakeAddressReference
  (value, _) <- forAll genValueWithNativeAsset

  let shelleyAddress = makeShelleyAddress network paymentCredential stakeReference
      address = shelleyAddressInEra sbe shelleyAddress
      ledgerValue = toLedgerValue meo value
      txOutValue = TxOutValueShelleyBased sbe ledgerValue
      txOut = TxOut address txOutValue TxOutDatumNone ReferenceScriptNone
      -- Address-only pattern; absent asset field is vacuously true
      outputPattern = defMessage & U5c.address .~ (defMessage & U5c.exactAddress .~ serialiseToRawBytes address)
  H.assertWith txOut $ matchesTxOutputPattern outputPattern

hprop_tx_output_pattern_asset_only :: Property
hprop_tx_output_pattern_asset_only = H.property $ do
  paymentCredential <- forAll genPaymentCredential
  network <- forAll genNetworkId
  stakeReference <- forAll genStakeAddressReference
  (value, AssetId policy tokenName) <- forAll genValueWithNativeAsset

  let shelleyAddress = makeShelleyAddress network paymentCredential stakeReference
      address = shelleyAddressInEra sbe shelleyAddress
      ledgerValue = toLedgerValue meo value
      txOutValue = TxOutValueShelleyBased sbe ledgerValue
      txOut = TxOut address txOutValue TxOutDatumNone ReferenceScriptNone
      -- Asset-only pattern; absent address field is vacuously true
      outputPattern =
        defMessage
          & U5c.asset
            .~ ( defMessage
                   & U5c.policyId .~ serialiseToRawBytes policy
                   & U5c.assetName .~ serialiseToRawBytes tokenName
               )
  H.assertWith txOut $ matchesTxOutputPattern outputPattern

-- ---------------------------------------------------------------------------
-- F. Boolean combinators (via matchesUtxoPredicate)
-- ---------------------------------------------------------------------------

hprop_not_inverts_match :: Property
hprop_not_inverts_match = H.property $ do
  txOut <- forAll genTxOut
  let inner = wrapInPredicate defMessage -- defMessage matches everything
      predicate = defMessage & U5c.not .~ [inner]
  -- not [match-everything] should reject everything
  H.assertWith txOut $ not . matchesUtxoPredicate predicate

hprop_allOf_conjunction :: Property
hprop_allOf_conjunction = H.property $ do
  txOut <- forAll genTxOut
  let predicate1 = wrapInPredicate defMessage -- matches everything
      predicate2 = wrapInPredicate defMessage -- matches everything
      predicate = defMessage & U5c.allOf .~ [predicate1, predicate2]
  H.assertWith txOut $ matchesUtxoPredicate predicate

hprop_anyOf_disjunction :: Property
hprop_anyOf_disjunction = H.property $ do
  txOut <- forAll genTxOut
  -- one that matches everything, one with impossible asset
  let matchAll = wrapInPredicate defMessage
      impossibleAsset = defMessage & U5c.asset .~ (defMessage & U5c.policyId .~ BS.replicate 28 0xff)
      matchNone = wrapInPredicate impossibleAsset
      predicate = defMessage & U5c.anyOf .~ [matchAll, matchNone]
  H.assertWith txOut $ matchesUtxoPredicate predicate

hprop_anyOf_empty_is_vacuously_true :: Property
hprop_anyOf_empty_is_vacuously_true = H.property $ do
  txOut <- forAll genTxOut
  let predicate = defMessage & U5c.anyOf .~ []
  H.assertWith txOut $ matchesUtxoPredicate predicate

hprop_match_and_not_combined :: Property
hprop_match_and_not_combined = H.property $ do
  txOut <- forAll genTxOut
  -- match=defMessage matches everything, not=[defMessage] negates everything → always fails
  let inner = wrapInPredicate defMessage
      predicate =
        defMessage
          & U5c.match .~ (defMessage & U5c.cardano .~ defMessage)
          & U5c.not .~ [inner]
  H.assertWith txOut $ not . matchesUtxoPredicate predicate

hprop_nested_allOf_anyOf :: Property
hprop_nested_allOf_anyOf = H.property $ do
  txOut <- forAll genTxOut
  -- allOf [match-everything, anyOf [match-everything, impossible]]
  let matchAll = wrapInPredicate defMessage
      impossibleAsset = defMessage & U5c.asset .~ (defMessage & U5c.policyId .~ BS.replicate 28 0xff)
      matchNone = wrapInPredicate impossibleAsset
      anyOfPredicate = defMessage & U5c.anyOf .~ [matchAll, matchNone]
      predicate = defMessage & U5c.allOf .~ [matchAll, anyOfPredicate]
  H.assertWith txOut $ matchesUtxoPredicate predicate

hprop_allOf_rejects_when_one_fails :: Property
hprop_allOf_rejects_when_one_fails = H.property $ do
  txOut <- forAll genTxOut
  let matchAll = wrapInPredicate defMessage
      impossibleAsset = defMessage & U5c.asset .~ (defMessage & U5c.policyId .~ BS.replicate 28 0xff)
      matchNone = wrapInPredicate impossibleAsset
      -- allOf requires all to match; one impossible → always rejects
      predicate = defMessage & U5c.allOf .~ [matchAll, matchNone]
  H.assertWith txOut $ not . matchesUtxoPredicate predicate

hprop_not_rejects_when_any_element_matches :: Property
hprop_not_rejects_when_any_element_matches = H.property $ do
  txOut <- forAll genTxOut
  let impossibleAsset = defMessage & U5c.asset .~ (defMessage & U5c.policyId .~ BS.replicate 28 0xff)
      matchNone = wrapInPredicate impossibleAsset
      matchAll = wrapInPredicate defMessage
      -- not [match-none, match-all]: match-all triggers, so not-clause rejects
      predicate = defMessage & U5c.not .~ [matchNone, matchAll]
  H.assertWith txOut $ not . matchesUtxoPredicate predicate

-- ---------------------------------------------------------------------------
-- G. extractAddressesFromPredicate
-- ---------------------------------------------------------------------------

hprop_extract_simple_exact_address :: Property
hprop_extract_simple_exact_address = H.property $ do
  address <- forAll genAddressShelley
  let addressBytes = serialiseToRawBytes address
      outputPattern :: Proto U5c.TxOutputPattern
      outputPattern = defMessage & U5c.address .~ (defMessage & U5c.exactAddress .~ addressBytes)
      predicate = wrapInPredicate outputPattern
  addresses <- H.nothingFail $ extractAddressesFromPredicate predicate
  H.annotate $ "Extracted: " <> show addresses
  Set.size addresses === 1

hprop_extract_nothing_for_complex_predicates :: Property
hprop_extract_nothing_for_complex_predicates = H.property $ do
  address <- forAll genAddressShelley
  let addressBytes = serialiseToRawBytes address
      outputPattern = defMessage & U5c.address .~ (defMessage & U5c.exactAddress .~ addressBytes)
      inner = wrapInPredicate outputPattern
      -- Predicate with not → should be Nothing
      predicate = defMessage & U5c.not .~ [inner]
  extractAddressesFromPredicate predicate === Nothing

hprop_extract_nothing_for_non_exact_pattern :: Property
hprop_extract_nothing_for_non_exact_pattern = H.property $ do
  credential <- forAll genPaymentCredential
  let outputPattern =
        defMessage & U5c.address .~ (defMessage & U5c.paymentPart .~ serialisePaymentCredential credential)
      predicate = wrapInPredicate outputPattern
  extractAddressesFromPredicate predicate === Nothing

hprop_extract_anyOf_unions_addresses :: Property
hprop_extract_anyOf_unions_addresses = H.property $ do
  address1 <- forAll genAddressShelley
  address2 <- forAll genAddressShelley
  let makePredicate address =
        let addressBytes = serialiseToRawBytes address
            outputPattern = defMessage & U5c.address .~ (defMessage & U5c.exactAddress .~ addressBytes)
         in wrapInPredicate outputPattern
      predicate = defMessage & U5c.anyOf .~ [makePredicate address1, makePredicate address2]
  addresses <- H.nothingFail $ extractAddressesFromPredicate predicate
  H.annotate $ "Extracted: " <> show addresses
  -- Should contain at least 1, at most 2 (might be same address)
  H.assertWith addresses $ \a -> Set.size a >= 1 && Set.size a <= 2

hprop_extract_consistent_with_matches :: Property
hprop_extract_consistent_with_matches = H.property $ do
  -- If we can extract addresses, then each should match the predicate
  address <- forAll genAddressShelley
  let addressBytes = serialiseToRawBytes address
      outputPattern = defMessage & U5c.address .~ (defMessage & U5c.exactAddress .~ addressBytes)
      predicate = wrapInPredicate outputPattern
      addressInEra = shelleyAddressInEra sbe address
  _addresses <- H.nothingFail $ extractAddressesFromPredicate predicate
  -- The extracted address should match via matchesAddressPattern
  let addressPattern :: Proto U5c.AddressPattern
      addressPattern = defMessage & U5c.exactAddress .~ addressBytes
  H.assertWith addressInEra $ matchesAddressPattern addressPattern

hprop_extract_nothing_for_invalid_address_bytes :: Property
hprop_extract_nothing_for_invalid_address_bytes = H.propertyOnce $ do
  let invalidBytes = BS.pack [0xff, 0xfe, 0xfd]
      outputPattern = defMessage & U5c.address .~ (defMessage & U5c.exactAddress .~ invalidBytes)
      predicate = wrapInPredicate outputPattern
  extractAddressesFromPredicate predicate === Nothing

hprop_extract_nothing_for_allOf_with_exact_address :: Property
hprop_extract_nothing_for_allOf_with_exact_address = H.property $ do
  address <- forAll genAddressShelley
  let addressBytes = serialiseToRawBytes address
      outputPattern = defMessage & U5c.address .~ (defMessage & U5c.exactAddress .~ addressBytes)
      inner = wrapInPredicate outputPattern
      -- allOf path is not handled by the extract optimization
      predicate = defMessage & U5c.allOf .~ [inner]
  extractAddressesFromPredicate predicate === Nothing

hprop_extract_nothing_for_default_predicate :: Property
hprop_extract_nothing_for_default_predicate = H.propertyOnce $ do
  -- Empty predicate: no match field, all lists empty → Nothing
  extractAddressesFromPredicate defMessage === Nothing

-- ---------------------------------------------------------------------------
-- H. TxPattern — consumes / produces
-- ---------------------------------------------------------------------------

hprop_tx_consumes_matches_resolved_input :: Property
hprop_tx_consumes_matches_resolved_input = H.property $ do
  address <- forAll genAddressShelley
  let output = mkTxOutput (serialiseToRawBytes address) []
      tx = mkTx [mkResolvedTxInput output] [] [] []
      pat = defMessage & U5c.consumes .~ (defMessage & U5c.address .~ exactAddressPattern address)
  H.assertWith tx $ matchesTxPattern pat

hprop_tx_consumes_rejects_wrong_output :: Property
hprop_tx_consumes_rejects_wrong_output = H.property $ do
  address <- forAll genAddressShelley
  otherAddress <- forAll genAddressShelley
  when (serialiseToRawBytes address /= serialiseToRawBytes otherAddress) $ do
    let output = mkTxOutput (serialiseToRawBytes address) []
        tx = mkTx [mkResolvedTxInput output] [] [] []
        pat = defMessage & U5c.consumes .~ (defMessage & U5c.address .~ exactAddressPattern otherAddress)
    H.assertWith tx $ not . matchesTxPattern pat

hprop_tx_consumes_rejects_unresolved_input :: Property
hprop_tx_consumes_rejects_unresolved_input = H.propertyOnce $ do
  -- an input without 'as_output' contributes nothing (see 'matchesTxPattern' haddock):
  -- even the default (match-everything) pattern must not fire on it
  let tx = mkTx [defMessage] [] [] []
      pat = defMessage & U5c.consumes .~ defMessage
  H.assertWith tx $ not . matchesTxPattern pat

hprop_tx_produces_matches_output :: Property
hprop_tx_produces_matches_output = H.property $ do
  address <- forAll genAddressShelley
  let output = mkTxOutput (serialiseToRawBytes address) []
      tx = mkTx [] [output] [] []
      pat = defMessage & U5c.produces .~ (defMessage & U5c.address .~ exactAddressPattern address)
  H.assertWith tx $ matchesTxPattern pat

hprop_tx_produces_rejects_when_no_output_matches :: Property
hprop_tx_produces_rejects_when_no_output_matches = H.property $ do
  address <- forAll genAddressShelley
  otherAddress <- forAll genAddressShelley
  when (serialiseToRawBytes address /= serialiseToRawBytes otherAddress) $ do
    let output = mkTxOutput (serialiseToRawBytes address) []
        tx = mkTx [] [output] [] []
        pat = defMessage & U5c.produces .~ (defMessage & U5c.address .~ exactAddressPattern otherAddress)
    H.assertWith tx $ not . matchesTxPattern pat

-- ---------------------------------------------------------------------------
-- I. TxPattern — has_address
-- ---------------------------------------------------------------------------

hprop_tx_has_address_matches_output_address :: Property
hprop_tx_has_address_matches_output_address = H.property $ do
  address <- forAll genAddressShelley
  let output = mkTxOutput (serialiseToRawBytes address) []
      tx = mkTx [] [output] [] []
      pat = defMessage & U5c.hasAddress .~ exactAddressPattern address
  H.assertWith tx $ matchesTxPattern pat

hprop_tx_has_address_matches_resolved_input_address :: Property
hprop_tx_has_address_matches_resolved_input_address = H.property $ do
  address <- forAll genAddressShelley
  let output = mkTxOutput (serialiseToRawBytes address) []
      tx = mkTx [mkResolvedTxInput output] [] [] []
      pat = defMessage & U5c.hasAddress .~ exactAddressPattern address
  H.assertWith tx $ matchesTxPattern pat

hprop_tx_has_address_rejects_when_absent :: Property
hprop_tx_has_address_rejects_when_absent = H.property $ do
  address <- forAll genAddressShelley
  otherAddress <- forAll genAddressShelley
  when (serialiseToRawBytes address /= serialiseToRawBytes otherAddress) $ do
    let output = mkTxOutput (serialiseToRawBytes address) []
        tx = mkTx [] [output] [] []
        pat = defMessage & U5c.hasAddress .~ exactAddressPattern otherAddress
    H.assertWith tx $ not . matchesTxPattern pat

-- ---------------------------------------------------------------------------
-- J. TxPattern — moves_asset / mints_asset
-- ---------------------------------------------------------------------------

hprop_tx_moves_asset_matches_output_asset :: Property
hprop_tx_moves_asset_matches_output_asset = H.property $ do
  policy <- forAll gen28Bytes
  tokenName <- forAll genAssetName
  let output = mkTxOutput mempty [mkMultiasset policy (serialiseToRawBytes tokenName) 1]
      tx = mkTx [] [output] [] []
      pat = defMessage & U5c.movesAsset .~ (defMessage & U5c.policyId .~ policy)
  H.assertWith tx $ matchesTxPattern pat

hprop_tx_moves_asset_matches_resolved_input_asset :: Property
hprop_tx_moves_asset_matches_resolved_input_asset = H.property $ do
  policy <- forAll gen28Bytes
  tokenName <- forAll genAssetName
  let output = mkTxOutput mempty [mkMultiasset policy (serialiseToRawBytes tokenName) 1]
      tx = mkTx [mkResolvedTxInput output] [] [] []
      pat = defMessage & U5c.movesAsset .~ (defMessage & U5c.policyId .~ policy)
  H.assertWith tx $ matchesTxPattern pat

hprop_tx_moves_asset_rejects_zero_quantity :: Property
hprop_tx_moves_asset_rejects_zero_quantity = H.property $ do
  policy <- forAll gen28Bytes
  tokenName <- forAll genAssetName
  let output = mkTxOutput mempty [mkMultiasset policy (serialiseToRawBytes tokenName) 0]
      tx = mkTx [] [output] [] []
      pat = defMessage & U5c.movesAsset .~ (defMessage & U5c.policyId .~ policy)
  H.assertWith tx $ not . matchesTxPattern pat

hprop_tx_mints_asset_matches_positive_mint :: Property
hprop_tx_mints_asset_matches_positive_mint = H.property $ do
  policy <- forAll gen28Bytes
  tokenName <- forAll genAssetName
  let tx = mkTx [] [] [mkMultiasset policy (serialiseToRawBytes tokenName) 1] []
      pat = defMessage & U5c.mintsAsset .~ (defMessage & U5c.policyId .~ policy)
  H.assertWith tx $ matchesTxPattern pat

hprop_tx_mints_asset_matches_burn :: Property
hprop_tx_mints_asset_matches_burn = H.property $ do
  -- burns are recorded as a negative quantity; mints_asset must still match them
  policy <- forAll gen28Bytes
  tokenName <- forAll genAssetName
  let tx = mkTx [] [] [mkMultiasset policy (serialiseToRawBytes tokenName) (-1)] []
      pat = defMessage & U5c.mintsAsset .~ (defMessage & U5c.policyId .~ policy)
  H.assertWith tx $ matchesTxPattern pat

hprop_tx_mints_asset_rejects_zero_quantity :: Property
hprop_tx_mints_asset_rejects_zero_quantity = H.property $ do
  policy <- forAll gen28Bytes
  tokenName <- forAll genAssetName
  let tx = mkTx [] [] [mkMultiasset policy (serialiseToRawBytes tokenName) 0] []
      pat = defMessage & U5c.mintsAsset .~ (defMessage & U5c.policyId .~ policy)
  H.assertWith tx $ not . matchesTxPattern pat

-- ---------------------------------------------------------------------------
-- K. TxPattern — has_certificate
-- ---------------------------------------------------------------------------

hprop_tx_has_certificate_matches_stake_registration :: Property
hprop_tx_has_certificate_matches_stake_registration = H.property $ do
  credential <- forAll genStakeCredential
  let cert = defMessage & U5c.stakeRegistration .~ mkStakeCredential credential
      tx = mkTx [] [] [] [cert]
      pat =
        defMessage
          & U5c.hasCertificate .~ (defMessage & U5c.stakeRegistration .~ mkStakeCredential credential)
  H.assertWith tx $ matchesTxPattern pat

hprop_tx_has_certificate_rejects_different_credential :: Property
hprop_tx_has_certificate_rejects_different_credential = H.property $ do
  credential <- forAll genStakeCredential
  otherCredential <- forAll genStakeCredential
  when (serialiseStakeCredential credential /= serialiseStakeCredential otherCredential) $ do
    let cert = defMessage & U5c.stakeRegistration .~ mkStakeCredential credential
        tx = mkTx [] [] [] [cert]
        pat =
          defMessage
            & U5c.hasCertificate .~ (defMessage & U5c.stakeRegistration .~ mkStakeCredential otherCredential)
    H.assertWith tx $ not . matchesTxPattern pat

hprop_tx_has_certificate_rejects_different_cert_shape :: Property
hprop_tx_has_certificate_rejects_different_cert_shape = H.property $ do
  -- a stake_registration pattern must not match a differently-shaped certificate,
  -- even one carrying the exact same credential
  credential <- forAll genStakeCredential
  let cert = defMessage & U5c.stakeDeregistration .~ mkStakeCredential credential
      tx = mkTx [] [] [] [cert]
      pat =
        defMessage
          & U5c.hasCertificate .~ (defMessage & U5c.stakeRegistration .~ mkStakeCredential credential)
  H.assertWith tx $ not . matchesTxPattern pat

hprop_tx_has_certificate_any_stake_credential_matches_reg_cert :: Property
hprop_tx_has_certificate_any_stake_credential_matches_reg_cert = H.property $ do
  -- the any_stake_credential wildcard reaches into the Conway 'RegCert', which
  -- the discriminated stake_registration branch does not (see the haddock on
  -- 'matchesCertificatePattern')
  credential <- forAll genStakeCredential
  let cert = defMessage & U5c.regCert .~ (defMessage & U5c.stakeCredential .~ mkStakeCredential credential)
      tx = mkTx [] [] [] [cert]
      pat =
        defMessage
          & U5c.hasCertificate .~ (defMessage & U5c.anyStakeCredential .~ serialiseStakeCredential credential)
  H.assertWith tx $ matchesTxPattern pat

hprop_tx_has_certificate_any_pool_keyhash_matches_registration :: Property
hprop_tx_has_certificate_any_pool_keyhash_matches_registration = H.property $ do
  poolKeyHash <- forAll gen28Bytes
  let cert = defMessage & U5c.poolRegistration .~ (defMessage & U5c.operator .~ poolKeyHash)
      tx = mkTx [] [] [] [cert]
      pat = defMessage & U5c.hasCertificate .~ (defMessage & U5c.anyPoolKeyhash .~ poolKeyHash)
  H.assertWith tx $ matchesTxPattern pat

hprop_tx_has_certificate_pool_retirement_matches :: Property
hprop_tx_has_certificate_pool_retirement_matches = H.property $ do
  poolKeyHash <- forAll gen28Bytes
  let cert =
        defMessage & U5c.poolRetirement .~ (defMessage & U5c.poolKeyhash .~ poolKeyHash & U5c.epoch .~ 100)
      tx = mkTx [] [] [] [cert]
      pat =
        defMessage
          & U5c.hasCertificate
            .~ (defMessage & U5c.poolRetirement .~ (defMessage & U5c.poolKeyhash .~ poolKeyHash & U5c.epoch .~ 100))
  H.assertWith tx $ matchesTxPattern pat

hprop_tx_has_certificate_default_matches_tx_with_any_cert :: Property
hprop_tx_has_certificate_default_matches_tx_with_any_cert = H.property $ do
  credential <- forAll genStakeCredential
  let cert = defMessage & U5c.stakeRegistration .~ mkStakeCredential credential
      tx = mkTx [] [] [] [cert]
      pat = defMessage & U5c.hasCertificate .~ defMessage
  H.assertWith tx $ matchesTxPattern pat

hprop_tx_has_certificate_default_rejects_certless_tx :: Property
hprop_tx_has_certificate_default_rejects_certless_tx = H.propertyOnce $ do
  -- mirrors 'hprop_default_asset_pattern_rejects_ada_only': a present-but-empty
  -- pattern still requires at least one certificate to exist
  let tx = mkTx [] [] [] []
      pat = defMessage & U5c.hasCertificate .~ defMessage
  H.assertWith tx $ not . matchesTxPattern pat

-- ---------------------------------------------------------------------------
-- L. TxPredicate boolean combinators
-- ---------------------------------------------------------------------------

hprop_tx_predicate_default_matches_everything :: Property
hprop_tx_predicate_default_matches_everything = H.propertyOnce $ do
  let tx = mkTx [] [] [] []
  H.assertWith tx $ matchesTxPredicate defMessage

hprop_tx_predicate_not_inverts_match :: Property
hprop_tx_predicate_not_inverts_match = H.propertyOnce $ do
  let tx = mkTx [] [] [] []
      inner = wrapInTxPredicate defMessage -- matches everything
      predicate = defMessage & Submit.not .~ [inner]
  H.assertWith tx $ not . matchesTxPredicate predicate

hprop_tx_predicate_allOf_conjunction :: Property
hprop_tx_predicate_allOf_conjunction = H.propertyOnce $ do
  let tx = mkTx [] [] [] []
      matchAll = wrapInTxPredicate defMessage
      predicate = defMessage & Submit.allOf .~ [matchAll, matchAll]
  H.assertWith tx $ matchesTxPredicate predicate

hprop_tx_predicate_anyOf_disjunction :: Property
hprop_tx_predicate_anyOf_disjunction = H.propertyOnce $ do
  let tx = mkTx [] [] [] []
      matchAll = wrapInTxPredicate defMessage
      matchNone = wrapInTxPredicate (defMessage & U5c.hasCertificate .~ defMessage)
      predicate = defMessage & Submit.anyOf .~ [matchAll, matchNone]
  H.assertWith tx $ matchesTxPredicate predicate

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

type TestEra = ConwayEra

sbe :: ShelleyBasedEra TestEra
sbe = convert useEra

meo :: MaryEraOnwards TestEra
meo = convert useEra

genTxOut :: Gen (TxOut CtxUTxO TestEra)
genTxOut = genTxOutUTxOContext sbe

-- | Generate a Value that contains at least one non-Ada native asset with positive quantity.
genValueWithNativeAsset :: Gen (Value, AssetId)
genValueWithNativeAsset = do
  policy <- genPolicyId
  tokenName <- genAssetName
  quantity <- genPositiveQuantity
  let asset = AssetId policy tokenName
      value = [(AdaAssetId, Quantity 2_000_000), (asset, quantity)]
  pure (value, asset)

-- | Wrap a TxOutputPattern in a UtxoPredicate via match.cardano.
wrapInPredicate :: Proto U5c.TxOutputPattern -> Proto U5c.UtxoPredicate
wrapInPredicate outputPattern =
  defMessage
    & U5c.match
      .~ ( defMessage
             & U5c.cardano .~ outputPattern
         )

-- | Wrap a TxPattern in a TxPredicate via match.cardano.
wrapInTxPredicate :: Proto U5c.TxPattern -> Proto Submit.TxPredicate
wrapInTxPredicate txPattern =
  defMessage & Submit.match .~ (defMessage & Submit.cardano .~ txPattern)

-- | An 'AddressPattern' matching only the given address exactly.
exactAddressPattern :: SerialiseAsRawBytes addr => addr -> Proto U5c.AddressPattern
exactAddressPattern address = defMessage & U5c.exactAddress .~ serialiseToRawBytes address

-- | Build a 'U5c.StakeCredential' carrying the given credential's raw hash bytes.
-- Always uses the addrKeyHash branch: 'credentialBytes' treats both branches
-- alike, so which one is picked here does not affect what the tests exercise.
mkStakeCredential :: StakeCredential -> Proto U5c.StakeCredential
mkStakeCredential credential = defMessage & U5c.addrKeyHash .~ serialiseStakeCredential credential

-- | Build a proto TxOutput with the given address and assets.
mkTxOutput :: ByteString -> [Proto U5c.Multiasset] -> Proto U5c.TxOutput
mkTxOutput address assets = defMessage & U5c.address .~ address & U5c.assets .~ assets

-- | Build a proto Multiasset bundle with a single named asset of the given quantity.
-- @quantity@ may be negative (a burn); it is built directly via 'BigInt.int'
-- rather than going through the (package-internal) 'Inject' instance.
mkMultiasset :: ByteString -> ByteString -> Integer -> Proto U5c.Multiasset
mkMultiasset policy tokenName quantity =
  defMessage
    & U5c.policyId .~ policy
    & U5c.assets
      .~ [ defMessage & U5c.name .~ tokenName & U5c.quantity .~ (defMessage & U5c.int .~ fromIntegral quantity)
         ]

-- | A TxInput whose spent output has been resolved (see the 'matchesTxPattern' haddock).
mkResolvedTxInput :: Proto U5c.TxOutput -> Proto U5c.TxInput
mkResolvedTxInput output = defMessage & U5c.asOutput .~ output

-- | Build a proto Tx from its inputs, outputs, minted assets and certificates.
mkTx
  :: [Proto U5c.TxInput]
  -> [Proto U5c.TxOutput]
  -> [Proto U5c.Multiasset]
  -> [Proto U5c.Certificate]
  -> Proto U5c.Tx
mkTx inputs outputs mint certificates =
  defMessage
    & U5c.inputs .~ inputs
    & U5c.outputs .~ outputs
    & U5c.mint .~ mint
    & U5c.certificates .~ certificates

-- | Generate 28 raw bytes, the size of a Blake2b-224 hash (credentials, pool key hashes, DReps).
gen28Bytes :: Gen ByteString
gen28Bytes = Gen.bytes (Range.singleton 28)
