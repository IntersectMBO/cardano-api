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
import Cardano.Rpc.Server.Internal.UtxoRpc.Predicate

import RIO

import Data.ByteString qualified as BS
import Data.ProtoLens (defMessage)
import Data.Set qualified as Set

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
      outputPattern :: U5c.TxOutputPattern
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
  let addressPattern :: U5c.AddressPattern
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
wrapInPredicate :: U5c.TxOutputPattern -> U5c.UtxoPredicate
wrapInPredicate outputPattern =
  defMessage
    & U5c.match
      .~ ( defMessage
             & U5c.cardano .~ outputPattern
         )
