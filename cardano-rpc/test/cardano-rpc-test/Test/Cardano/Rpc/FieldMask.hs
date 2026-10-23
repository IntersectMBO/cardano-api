{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for field mask normalisation and resolution. Some unit cases are
-- ported from protobuf Java's @FieldMaskTreeTest@; the rest are our own.
module Test.Cardano.Rpc.FieldMask where

import Cardano.Rpc.Proto.Api.UtxoRpc.Query
  ( ReadGenesisResponse
  , ReadParamsResponse
  , ReadUtxosResponse
  )
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as U5c
import Cardano.Rpc.Proto.Api.UtxoRpc.Submit qualified as Submit
import Cardano.Rpc.Proto.Api.UtxoRpc.Sync qualified as Sync
import Cardano.Rpc.Server.Internal.FieldMask

import RIO hiding (toList)

import Data.List (isPrefixOf, nub, sort)
import Data.Map qualified as Map
import Data.ProtoLens.Message
  ( FieldDescriptor (..)
  , FieldTypeDescriptor (..)
  , Message (..)
  , defMessage
  )
import Data.Text qualified as T
import GHC.Exts (IsList (..))

import Hedgehog as H
import Hedgehog.Extras qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

-- | Port of Java's @testAddFieldPath@: each step adds one path to the tree
-- built so far and checks the resulting leaves.
hprop_field_mask_add_field_path :: Property
hprop_field_mask_add_field_path = H.propertyOnce $ do
  normalise [] === PathTree mempty
  paths [] === []
  paths [""] === []
  paths ["", "foo"] === ["foo"]
  paths ["", "foo", "foo"] === ["foo"]
  paths ["", "foo", "foo", "bar.baz"] === ["bar.baz", "foo"]
  paths ["", "foo", "foo", "bar.baz", "foo.bar"] === ["bar.baz", "foo"]
  paths ["", "foo", "foo", "bar.baz", "foo.bar", "bar.quz"] === ["bar.baz", "bar.quz", "foo"]
  paths ["", "foo", "foo", "bar.baz", "foo.bar", "bar.quz", "bar"] === ["bar", "foo"]

-- | Port of Java's @testMergeFromFieldMask@.
hprop_field_mask_merge_from_field_mask :: Property
hprop_field_mask_merge_from_field_mask = H.propertyOnce $ do
  paths ["foo", "bar.baz", "bar.quz"] === ["bar.baz", "bar.quz", "foo"]
  paths ["foo", "bar.baz", "bar.quz", "foo.bar", "bar"] === ["bar", "foo"]

hprop_field_mask_prefix_is_kept_in_any_order :: Property
hprop_field_mask_prefix_is_kept_in_any_order = H.propertyOnce $ do
  paths ["a.b.c", "a.b", "a"] === ["a"]
  paths ["a", "a.b", "a.b.c"] === ["a"]
  paths ["a.b", "a.b.c"] === ["a.b"]

-- | Port of Java's @testGetFieldPaths_excessivelyDeepTree@.
hprop_field_mask_deep_path :: Property
hprop_field_mask_deep_path = H.propertyOnce $ do
  let deepPath = T.intercalate "." $ replicate 250000 "a"
  paths [deepPath, deepPath] === [deepPath]

hprop_field_mask_empty_segment_paths_are_dropped :: Property
hprop_field_mask_empty_segment_paths_are_dropped = H.propertyOnce $ do
  paths ["", ".", "..", ".a", "a.", "a..b", "a..b.c"] === []
  paths ["a.", "a.b", ".a", "a..b"] === ["a.b"]

hprop_field_mask_star_is_an_ordinary_segment :: Property
hprop_field_mask_star_is_an_ordinary_segment = H.propertyOnce $ do
  paths ["*"] === ["*"]
  paths ["a.*", "a.b"] === ["a.*", "a.b"]
  paths ["a", "a.*"] === ["a"]

hprop_field_mask_order_independent :: Property
hprop_field_mask_order_independent = H.property $ do
  inputs <- H.forAll genPaths
  shuffled <- H.forAll $ Gen.shuffle inputs
  paths shuffled === paths inputs

hprop_field_mask_idempotent :: Property
hprop_field_mask_idempotent = H.property $ do
  inputs <- H.forAll genPaths
  paths (paths inputs) === paths inputs

hprop_field_mask_output_sorted_and_antichain :: Property
hprop_field_mask_output_sorted_and_antichain = H.property $ do
  inputs <- H.forAll genPaths
  let outputs = paths inputs
  H.assertWith outputs $ \xs -> map segments xs == sort (map segments xs)
  H.assertWith outputs $ \xs ->
    not $ or [isSegmentPrefix x y | x <- xs, y <- xs, x /= y]

hprop_field_mask_matches_minimal_elements_oracle :: Property
hprop_field_mask_matches_minimal_elements_oracle = H.property $ do
  inputs <- H.forAll genPaths
  let wellFormed = map segments $ filter (not . any T.null . segments) inputs
      minimal = filter (\x -> not $ any (\y -> y /= x && y `isPrefixOf` x) wellFormed) wellFormed
  paths inputs === map (T.intercalate ".") (sort $ nub minimal)

hprop_field_mask_output_covers_input_and_comes_from_input :: Property
hprop_field_mask_output_covers_input_and_comes_from_input = H.property $ do
  inputs <- H.forAll genPaths
  let outputs = paths inputs
      wellFormed = filter (not . any T.null . segments) inputs
  H.assertWith (outputs, wellFormed) $ \(os, ins) ->
    all (\input -> any (`isSegmentPrefix` input) os) ins
  H.assertWith (outputs, inputs) $ \(os, ins) -> all (`elem` ins) os

hprop_field_mask_resolve_select_all :: Property
hprop_field_mask_resolve_select_all = H.propertyOnce $ do
  resolve @ReadParamsResponse [] === SelectAll
  resolve @ReadParamsResponse ["*"] === SelectAll
  resolve @ReadParamsResponse ["ledger_tip", "*"] === SelectAll

hprop_field_mask_resolve_keeps_resolving_paths :: Property
hprop_field_mask_resolve_keeps_resolving_paths = H.propertyOnce $ do
  resolved @ReadParamsResponse ["ledger_tip"] === ["ledger_tip"]
  resolved @ReadParamsResponse ["values.cardano.max_tx_size"] === ["values.cardano.max_tx_size"]
  resolved @ReadParamsResponse ["values.cardano.pool_influence.numerator"]
    === ["values.cardano.pool_influence.numerator"]

hprop_field_mask_resolve_drops_unknown_paths :: Property
hprop_field_mask_resolve_drops_unknown_paths = H.propertyOnce $ do
  resolve @ReadParamsResponse ["zzz"] === selectsNothing
  resolve @ReadParamsResponse ["values.zzz"] === selectsNothing
  resolve @ReadParamsResponse ["values.cardano.zzz"] === selectsNothing
  resolve @ReadParamsResponse ["a..b"] === selectsNothing
  resolved @ReadParamsResponse ["ledger_tip", "zzz"] === ["ledger_tip"]
  resolved @ReadParamsResponse ["ledger_tip", "values.zzz"] === ["ledger_tip"]
  resolved @ReadParamsResponse ["values.cardano.max_tx_size", "values.cardano.zzz"]
    === ["values.cardano.max_tx_size"]
  resolve @ReadParamsResponse ["values.*"] === selectsNothing
  resolved @ReadParamsResponse ["values.*", "ledger_tip"] === ["ledger_tip"]

hprop_field_mask_resolve_keeps_oneof_member :: Property
hprop_field_mask_resolve_keeps_oneof_member =
  H.propertyOnce $
    resolved @ReadParamsResponse ["values.cardano"] === ["values.cardano"]

hprop_field_mask_resolve_drops_paths_below_scalars :: Property
hprop_field_mask_resolve_drops_paths_below_scalars = H.propertyOnce $ do
  resolved @ReadParamsResponse ["ledger_tip.slot.x"] === []
  resolved @ReadParamsResponse ["ledger_tip.slot", "ledger_tip.slot.x"] === ["ledger_tip.slot"]

hprop_field_mask_resolve_repeated_scalar_is_a_leaf :: Property
hprop_field_mask_resolve_repeated_scalar_is_a_leaf = H.propertyOnce $ do
  let costModel = "values.cardano.cost_models.plutus_v1.values"
  resolved @ReadParamsResponse [costModel] === [costModel]
  resolved @ReadParamsResponse [costModel <> ".x"] === []

hprop_field_mask_resolve_through_repeated_field :: Property
hprop_field_mask_resolve_through_repeated_field = H.propertyOnce $ do
  resolved @ReadUtxosResponse ["items.txo_ref.hash"] === ["items.txo_ref.hash"]
  resolved @ReadUtxosResponse ["items", "ledger_tip.slot"] === ["items", "ledger_tip.slot"]
  resolved @ReadUtxosResponse ["items.zzz"] === []

hprop_field_mask_resolve_map_field_is_a_leaf :: Property
hprop_field_mask_resolve_map_field_is_a_leaf = H.propertyOnce $ do
  resolved @ReadGenesisResponse ["cardano.avvm_distr"] === ["cardano.avvm_distr"]
  resolved @ReadGenesisResponse ["cardano.avvm_distr.key"] === []

-- | Valid paths follow a hand-written subset of the 'ReadParamsResponse' field
-- tree, optionally with one extra segment that names no field.
hprop_field_mask_resolve_matches_field_tree_oracle :: Property
hprop_field_mask_resolve_matches_field_tree_oracle = H.property $ do
  inputs <- H.forAll $ Gen.list (Range.linear 1 8) genTreePath
  resolve @ReadParamsResponse inputs
    === Select (normalise $ filter (isValidPath readParamsShape . segments) inputs)

hprop_field_mask_resolve_result_comes_from_input :: Property
hprop_field_mask_resolve_result_comes_from_input = H.property $ do
  inputs <- H.forAll genResolvePaths
  case resolve @ReadParamsResponse inputs of
    SelectAll -> H.success
    Select tree ->
      H.assertWith (toPaths tree, paths inputs) $ \(os, ins) -> all (`elem` ins) os

hprop_field_mask_resolve_idempotent :: Property
hprop_field_mask_resolve_idempotent = H.property $ do
  inputs <- H.forAll genResolvePaths
  case resolve @ReadParamsResponse inputs of
    SelectAll -> H.success
    Select tree -> do
      -- No paths would read back as an absent mask, which selects everything.
      let resolvedPaths = toPaths tree
      unless (null resolvedPaths) $
        resolve @ReadParamsResponse resolvedPaths === Select tree

hprop_field_mask_apply_select_all_is_identity :: Property
hprop_field_mask_apply_select_all_is_identity = H.propertyOnce $ do
  applyFieldMask SelectAll readParamsFixture === readParamsFixture
  applyFieldMask SelectAll followApplyFixture === followApplyFixture
  applyFieldMask SelectAll readGenesisFixture === readGenesisFixture

hprop_field_mask_apply_selecting_nothing_resets_everything :: Property
hprop_field_mask_apply_selecting_nothing_resets_everything = H.propertyOnce $ do
  applyFieldMask selectsNothing readParamsFixture === defMessage
  applyFieldMask selectsNothing followApplyFixture === defMessage
  applyFieldMask selectsNothing readUtxosFixture === defMessage
  applyFieldMask selectsNothing readGenesisFixture === defMessage
  applyFieldMask (resolve ["zzz"]) readParamsFixture === (defMessage :: ReadParamsResponse)

hprop_field_mask_apply_keeps_selected_leaf :: Property
hprop_field_mask_apply_keeps_selected_leaf = H.propertyOnce $ do
  applyFieldMask (resolve ["ledger_tip"]) readParamsFixture
    === (defMessage & U5c.ledgerTip .~ chainPointFixture :: ReadParamsResponse)

hprop_field_mask_apply_keeps_selected_nested_field :: Property
hprop_field_mask_apply_keeps_selected_nested_field = H.propertyOnce $ do
  let expected :: ReadParamsResponse
      expected =
        defMessage
          & U5c.values
            .~ ( defMessage
                   & U5c.cardano .~ (defMessage & U5c.maxTxSize .~ 16384)
               )
  applyFieldMask (resolve ["values.cardano.max_tx_size"]) readParamsFixture === expected

hprop_field_mask_apply_keeps_active_oneof_member :: Property
hprop_field_mask_apply_keeps_active_oneof_member = H.propertyOnce $ do
  let expected :: Sync.FollowTipResponse
      expected = defMessage & Sync.apply .~ blockFixture
  applyFieldMask (resolve ["apply"]) followApplyFixture === expected

-- | A mask naming an inactive member of the active member's oneof must not
-- wipe the active one.
hprop_field_mask_apply_inactive_oneof_member_keeps_active :: Property
hprop_field_mask_apply_inactive_oneof_member_keeps_active = H.propertyOnce $ do
  let expected :: Sync.FollowTipResponse
      expected = defMessage & Sync.apply .~ blockFixture
  applyFieldMask (resolve ["apply", "undo.native_bytes"]) followApplyFixture === expected
  applyFieldMask (resolve ["apply", "reset.slot"]) followApplyFixture === expected

hprop_field_mask_apply_clears_unselected_oneof :: Property
hprop_field_mask_apply_clears_unselected_oneof = H.propertyOnce $ do
  applyFieldMask (resolve ["undo"]) followApplyFixture === defMessage
  applyFieldMask (resolve ["undo.native_bytes"]) followApplyFixture === defMessage
  applyFieldMask (resolve ["tip"]) followApplyFixture
    === (defMessage & Sync.tip .~ blockRefFixture :: Sync.FollowTipResponse)

hprop_field_mask_apply_trims_each_repeated_element :: Property
hprop_field_mask_apply_trims_each_repeated_element = H.propertyOnce $ do
  let expected :: ReadUtxosResponse
      expected =
        defMessage
          & U5c.items
            .~ [ defMessage & U5c.nativeBytes .~ "first"
               , defMessage & U5c.nativeBytes .~ "second"
               ]
  applyFieldMask (resolve ["items.native_bytes"]) readUtxosFixture === expected
  applyFieldMask (resolve ["ledger_tip"]) readUtxosFixture
    === (defMessage & U5c.ledgerTip .~ chainPointFixture :: ReadUtxosResponse)

hprop_field_mask_apply_keeps_selected_map :: Property
hprop_field_mask_apply_keeps_selected_map = H.propertyOnce $ do
  let expected :: ReadGenesisResponse
      expected =
        defMessage
          & U5c.cardano .~ (defMessage & U5c.avvmDistr .~ avvmDistrFixture)
  applyFieldMask (resolve ["cardano.avvm_distr"]) readGenesisFixture === expected

hprop_field_mask_apply_empties_unselected_map :: Property
hprop_field_mask_apply_empties_unselected_map = H.propertyOnce $ do
  let expected :: ReadGenesisResponse
      expected = defMessage & U5c.caip2 .~ "cardano-mainnet"
  applyFieldMask (resolve ["caip2"]) readGenesisFixture === expected

-- | Trimming by a sub-mask after trimming by a mask that covers it is the same
-- as trimming by the sub-mask alone.
hprop_field_mask_apply_composes :: Property
hprop_field_mask_apply_composes = H.property $ do
  extra <- H.forAll $ Gen.list (Range.linear 0 4) genTreePath
  inner <- H.forAll $ Gen.list (Range.linear 1 4) (Gen.filter (/= "*") genTreePath)
  let trimmed = applyFieldMask (resolve @ReadParamsResponse (extra <> inner)) readParamsFixture
  applyFieldMask (resolve inner) trimmed === applyFieldMask (resolve inner) readParamsFixture

hprop_field_mask_apply_trims_message_inside_repeated_element :: Property
hprop_field_mask_apply_trims_message_inside_repeated_element = H.propertyOnce $ do
  let expected :: ReadUtxosResponse
      expected =
        defMessage
          & U5c.items
            .~ [ defMessage & U5c.txoRef .~ (defMessage & U5c.index .~ 0)
               , defMessage & U5c.txoRef .~ (defMessage & U5c.index .~ 1)
               ]
  applyFieldMask (resolve ["items.txo_ref.index"]) readUtxosFixture === expected

hprop_field_mask_apply_keeps_repeated_field_selected_as_leaf :: Property
hprop_field_mask_apply_keeps_repeated_field_selected_as_leaf = H.propertyOnce $ do
  let expected :: ReadUtxosResponse
      expected = defMessage & U5c.items .~ (readUtxosFixture ^. U5c.items)
  applyFieldMask (resolve ["items"]) readUtxosFixture === expected

hprop_field_mask_is_selected_everything_under_select_all :: Property
hprop_field_mask_is_selected_everything_under_select_all = H.propertyOnce $ do
  isSelected @"ledgerTip" (SelectAll :: FieldMaskTree ReadParamsResponse) === True
  isSelected @"values" (SelectAll :: FieldMaskTree ReadParamsResponse) === True

hprop_field_mask_is_selected_field_with_selected_descendant :: Property
hprop_field_mask_is_selected_field_with_selected_descendant = H.propertyOnce $ do
  let fieldMask = resolve @ReadParamsResponse ["values.cardano.max_tx_size"]
  isSelected @"values" fieldMask === True
  isSelected @"cardano" (subMask @"values" fieldMask) === True
  isSelected @"maxTxSize" (subMask @"cardano" $ subMask @"values" fieldMask) === True

hprop_field_mask_is_selected_rejects_other_fields :: Property
hprop_field_mask_is_selected_rejects_other_fields = H.propertyOnce $ do
  let fieldMask = resolve @ReadParamsResponse ["values.cardano.max_tx_size"]
  isSelected @"ledgerTip" fieldMask === False
  isSelected @"poolInfluence" (subMask @"cardano" $ subMask @"values" fieldMask) === False
  isSelected @"ledgerTip" (selectsNothing :: FieldMaskTree ReadParamsResponse) === False

hprop_field_mask_is_selected_below_a_leaf :: Property
hprop_field_mask_is_selected_below_a_leaf = H.propertyOnce $ do
  let fieldMask = resolve @ReadParamsResponse ["values"]
  isSelected @"cardano" (subMask @"values" fieldMask) === True
  isSelected @"maxTxSize" (subMask @"cardano" $ subMask @"values" fieldMask) === True

hprop_field_mask_is_selected_maybe_label_names_the_same_field :: Property
hprop_field_mask_is_selected_maybe_label_names_the_same_field = H.propertyOnce $ do
  isSelected @"maybe'ledgerTip" (resolve @ReadParamsResponse ["ledger_tip"]) === True
  isSelected @"maybe'ledgerTip" (resolve @ReadParamsResponse ["values"]) === False

-- | A oneof label names no field: it is selected only under 'SelectAll'.
hprop_field_mask_is_selected_label_naming_no_field :: Property
hprop_field_mask_is_selected_label_naming_no_field = H.propertyOnce $ do
  isSelected @"maybe'parsedState" (resolve @U5c.AnyUtxoData ["cardano"]) === False
  isSelected @"maybe'parsedState" (SelectAll :: FieldMaskTree U5c.AnyUtxoData) === True
  subMask @"maybe'parsedState" (resolve @U5c.AnyUtxoData ["cardano"]) === selectsNothing

hprop_field_mask_lens_label_of_real_names :: Property
hprop_field_mask_lens_label_of_real_names = H.propertyOnce $ do
  lensLabel "items" === "items"
  lensLabel "ledger_tip" === "ledgerTip"
  lensLabel "native_bytes" === "nativeBytes"
  lensLabel "plutus_v1" === "plutusV1"
  lensLabel "max_tx_size" === "maxTxSize"
  lensLabel "coins_per_utxo_byte" === "coinsPerUtxoByte"

-- | No two fields of a message reachable from a masked response share a lens
-- label, so a label names at most one field.
hprop_field_mask_lens_labels_are_unique_per_message :: Property
hprop_field_mask_lens_labels_are_unique_per_message = H.propertyOnce $ do
  let messages = reachableFieldNames maskedResponses
  forM_ (toList messages) $ \(name, fieldNames) ->
    H.assertWith (name, map lensLabel fieldNames) $ hasNoDuplicates . snd

hprop_field_mask_masked_set_skips_unselected_value :: Property
hprop_field_mask_masked_set_skips_unselected_value = H.propertyOnce $ do
  let fieldMask = resolve ["ledger_tip"]
      built :: ReadParamsResponse
      built =
        defMessage
          & maskedSet @"ledgerTip" fieldMask chainPointFixture
          & maskedSet @"values" fieldMask (error "forced")
  _ <- H.evalNF built
  built === (defMessage & U5c.ledgerTip .~ chainPointFixture)

hprop_field_mask_masked_set_stores_value_under_select_all :: Property
hprop_field_mask_masked_set_stores_value_under_select_all = H.propertyOnce $ do
  let built :: ReadParamsResponse
      built = defMessage & maskedSet @"ledgerTip" SelectAll chainPointFixture
  built === (defMessage & U5c.ledgerTip .~ chainPointFixture)

hprop_field_mask_masked_set_through_maybe_label :: Property
hprop_field_mask_masked_set_through_maybe_label = H.propertyOnce $ do
  let built :: ReadParamsResponse
      built =
        defMessage & maskedSet @"maybe'ledgerTip" (resolve ["ledger_tip"]) (Just chainPointFixture)
  built === (defMessage & U5c.ledgerTip .~ chainPointFixture)

hprop_field_mask_when_selected_runs_only_selected_action :: Property
hprop_field_mask_when_selected_runs_only_selected_action = H.propertyOnce $ do
  let fieldMask = resolve @ReadParamsResponse ["ledger_tip"]
  ran <- H.evalIO $ newIORef (0 :: Int)
  let action = modifyIORef' ran (+ 1) $> "result"
  skipped <- H.evalIO $ whenSelected @"values" fieldMask action
  skipped === Nothing
  H.evalIO (readIORef ran) >>= (=== 0)
  performed <- H.evalIO $ whenSelected @"ledgerTip" fieldMask action
  performed === Just ("result" :: Text)
  H.evalIO (readIORef ran) >>= (=== 1)

hprop_field_mask_when_selected_runs_action_under_select_all :: Property
hprop_field_mask_when_selected_runs_action_under_select_all = H.propertyOnce $ do
  performed <-
    H.evalIO $
      whenSelected @"values" (SelectAll :: FieldMaskTree ReadParamsResponse) (pure ("result" :: Text))
  performed === Just "result"

hprop_field_mask_when_selected_runs_action_for_selected_descendant :: Property
hprop_field_mask_when_selected_runs_action_for_selected_descendant = H.propertyOnce $ do
  let fieldMask = resolve @ReadParamsResponse ["values.cardano.max_tx_size"]
  performed <- H.evalIO $ whenSelected @"values" fieldMask (pure ("result" :: Text))
  performed === Just "result"

hprop_field_mask_sub_mask_select_all :: Property
hprop_field_mask_sub_mask_select_all = H.propertyOnce $ do
  subMask @"values" (SelectAll :: FieldMaskTree ReadParamsResponse) === SelectAll
  subMask @"cardano" (subMask @"values" (SelectAll :: FieldMaskTree ReadParamsResponse))
    === SelectAll

hprop_field_mask_sub_mask_of_selected_leaf_selects_all :: Property
hprop_field_mask_sub_mask_of_selected_leaf_selects_all = H.propertyOnce $ do
  let fieldMask = resolve @ReadParamsResponse ["values"]
  subMask @"values" fieldMask === SelectAll
  subMask @"cardano" (subMask @"values" fieldMask) === SelectAll

hprop_field_mask_sub_mask_of_unselected_field_selects_nothing :: Property
hprop_field_mask_sub_mask_of_unselected_field_selects_nothing = H.propertyOnce $ do
  subMask @"values" (resolve @ReadParamsResponse ["ledger_tip"]) === selectsNothing
  let fieldMask = resolve @ReadParamsResponse ["values.cardano.max_tx_size"]
  subMask @"poolInfluence" (subMask @"cardano" $ subMask @"values" fieldMask) === selectsNothing

hprop_field_mask_sub_mask_keeps_nested_paths :: Property
hprop_field_mask_sub_mask_keeps_nested_paths = H.propertyOnce $ do
  let fieldMask =
        resolve @ReadParamsResponse
          ["values.cardano.max_tx_size", "values.cardano.pool_influence.numerator", "ledger_tip"]
      valuesMask = subMask @"values" fieldMask
  valuesMask === Select (normalise ["cardano.max_tx_size", "cardano.pool_influence.numerator"])
  subMask @"cardano" valuesMask === Select (normalise ["max_tx_size", "pool_influence.numerator"])
  subMask @"poolInfluence" (subMask @"cardano" valuesMask) === Select (normalise ["numerator"])

hprop_field_mask_sub_mask_each_select_all :: Property
hprop_field_mask_sub_mask_each_select_all = H.propertyOnce $ do
  let itemMask = subMaskEach @"items" (SelectAll :: FieldMaskTree ReadUtxosResponse)
  itemMask === SelectAll
  subMask @"blockRef" itemMask === SelectAll

hprop_field_mask_sub_mask_each_selects_field_below_repeated_field :: Property
hprop_field_mask_sub_mask_each_selects_field_below_repeated_field = H.propertyOnce $ do
  let fieldMask = resolve @ReadUtxosResponse ["items.txo_ref"]
  subMaskEach @"items" fieldMask === Select (normalise ["txo_ref"])

hprop_field_mask_sub_mask_each_of_selected_leaf_selects_all :: Property
hprop_field_mask_sub_mask_each_of_selected_leaf_selects_all = H.propertyOnce $ do
  subMaskEach @"items" (resolve @ReadUtxosResponse ["items"]) === SelectAll
  subMask @"blockRef" (subMaskEach @"items" $ resolve @ReadUtxosResponse ["items"]) === SelectAll
  subMask @"blockRef" (subMaskEach @"items" $ resolve @ReadUtxosResponse ["items.block_ref"])
    === SelectAll

hprop_field_mask_sub_mask_each_of_unselected_field_selects_nothing :: Property
hprop_field_mask_sub_mask_each_of_unselected_field_selects_nothing = H.propertyOnce $ do
  subMaskEach @"items" (resolve @ReadUtxosResponse ["ledger_tip"]) === selectsNothing
  subMask @"txoRef" (subMaskEach @"items" $ resolve @ReadUtxosResponse ["items.native_bytes"])
    === selectsNothing

hprop_field_mask_sub_mask_each_keeps_nested_paths :: Property
hprop_field_mask_sub_mask_each_keeps_nested_paths = H.propertyOnce $ do
  let fieldMask = resolve @ReadUtxosResponse ["items.cardano.coin", "items.cardano.address", "items.txo_ref"]
      itemMask = subMaskEach @"items" fieldMask
  itemMask === Select (normalise ["cardano.coin", "cardano.address", "txo_ref"])
  subMask @"cardano" itemMask === Select (normalise ["coin", "address"])
  subMask @"txoRef" itemMask === SelectAll

-- | A mask needs a field exactly when it has no paths, or one of its paths is a
-- segment prefix of the field's path or starts with it.
hprop_field_mask_is_selected_matches_path_prefix_oracle :: Property
hprop_field_mask_is_selected_matches_path_prefix_oracle = H.property $ do
  inputs <- H.forAll . Gen.list (Range.linear 0 4) $ Gen.element utxoMaskPaths
  let fieldMask = resolve @ReadUtxosResponse inputs
      itemMask = subMaskEach @"items" fieldMask
      isExpected path =
        null inputs || any (\input -> isSegmentPrefix input path || isSegmentPrefix path input) inputs
  isSelected @"items" fieldMask === isExpected "items"
  isSelected @"ledgerTip" fieldMask === isExpected "ledger_tip"
  isSelected @"txoRef" itemMask === isExpected "items.txo_ref"
  isSelected @"cardano" itemMask === isExpected "items.cardano"
  isSelected @"blockRef" itemMask === isExpected "items.block_ref"
  isSelected @"nativeBytes" itemMask === isExpected "items.native_bytes"

-- | Trimming each element of a repeated field by its sub-mask is the same as
-- trimming the whole response, once the response mask selects the field.
hprop_field_mask_sub_mask_trims_elements_like_whole_message :: Property
hprop_field_mask_sub_mask_trims_elements_like_whole_message = H.property $ do
  inputs <- H.forAll . Gen.list (Range.linear 0 4) $ Gen.element utxoMaskPaths
  let fieldMask = resolve @ReadUtxosResponse inputs
      trimmedItems = applyFieldMask fieldMask readUtxosFixture ^. U5c.items
  H.cover 30 "items selected" $ isSelected @"items" fieldMask
  when (isSelected @"items" fieldMask) $
    trimmedItems
      === map (applyFieldMask (subMaskEach @"items" fieldMask)) (readUtxosFixture ^. U5c.items)

chainPointFixture :: U5c.ChainPoint
chainPointFixture =
  defMessage
    & U5c.slot .~ 5
    & U5c.hash .~ "point-hash"
    & U5c.height .~ 3
    & U5c.timestamp .~ 9

blockRefFixture :: Sync.BlockRef
blockRefFixture =
  defMessage
    & Sync.slot .~ 7
    & Sync.hash .~ "block-hash"
    & Sync.height .~ 4
    & Sync.timestamp .~ 11

blockFixture :: Sync.AnyChainBlock
blockFixture = defMessage & Sync.nativeBytes .~ "block"

readParamsFixture :: ReadParamsResponse
readParamsFixture =
  defMessage
    & U5c.ledgerTip .~ chainPointFixture
    & U5c.values
      .~ ( defMessage
             & U5c.cardano
               .~ ( defMessage
                      & U5c.maxTxSize .~ 16384
                      & U5c.maxBlockBodySize .~ 90112
                      & U5c.poolInfluence
                        .~ (defMessage & U5c.numerator .~ 3 & U5c.denominator .~ 10)
                      & U5c.protocolVersion
                        .~ (defMessage & U5c.major .~ 10 & U5c.minor .~ 1)
                  )
         )

followApplyFixture :: Sync.FollowTipResponse
followApplyFixture =
  defMessage
    & Sync.apply .~ blockFixture
    & Sync.tip .~ blockRefFixture

readUtxosFixture :: ReadUtxosResponse
readUtxosFixture =
  defMessage
    & U5c.ledgerTip .~ chainPointFixture
    & U5c.items
      .~ [ utxo "first" "tx-a" 0
         , utxo "second" "tx-b" 1
         ]
 where
  utxo :: ByteString -> ByteString -> Word32 -> U5c.AnyUtxoData
  utxo bytes txHash index =
    defMessage
      & U5c.nativeBytes .~ bytes
      & U5c.txoRef .~ (defMessage & U5c.hash .~ txHash & U5c.index .~ index)
      & U5c.blockRef .~ chainPointFixture

avvmDistrFixture :: Map Text Text
avvmDistrFixture = fromList [("key-a", "1"), ("key-b", "2")]

readGenesisFixture :: ReadGenesisResponse
readGenesisFixture =
  defMessage
    & U5c.genesis .~ "genesis-hash"
    & U5c.caip2 .~ "cardano-mainnet"
    & U5c.cardano
      .~ ( defMessage
             & U5c.avvmDistr .~ avvmDistrFixture
             & U5c.epochLength .~ 432000
             & U5c.networkMagic .~ 764824073
         )

-- | Resolving against nothing that exists keeps no paths, unlike 'SelectAll'.
selectsNothing :: FieldMaskTree msg
selectsNothing = Select $ PathTree mempty

-- | The responses of the methods whose requests carry a field mask.
maskedResponses :: [SomeMessage]
maskedResponses =
  [ SomeMessage $ Proxy @U5c.ReadParamsResponse
  , SomeMessage $ Proxy @U5c.ReadGenesisResponse
  , SomeMessage $ Proxy @U5c.ReadEraSummaryResponse
  , SomeMessage $ Proxy @U5c.ReadStateResponse
  , SomeMessage $ Proxy @U5c.ReadUtxosResponse
  , SomeMessage $ Proxy @U5c.SearchUtxosResponse
  , SomeMessage $ Proxy @U5c.ReadDataResponse
  , SomeMessage $ Proxy @U5c.ReadTxResponse
  , SomeMessage $ Proxy @Submit.WatchMempoolResponse
  , SomeMessage $ Proxy @Sync.FetchBlockResponse
  , SomeMessage $ Proxy @Sync.DumpHistoryResponse
  , SomeMessage $ Proxy @Sync.FollowTipResponse
  ]

data SomeMessage = forall msg. Message msg => SomeMessage (Proxy msg)

-- | The field names of every message reachable from the roots through message
-- fields, keyed by message name.
reachableFieldNames :: [SomeMessage] -> Map Text [Text]
reachableFieldNames = go mempty
 where
  go seen = \case
    [] -> seen
    SomeMessage (proxy :: Proxy msg) : rest
      | Map.member (messageName proxy) seen -> go seen rest
      | otherwise -> do
          let descriptors = toList $ fieldsByTextFormatName @msg
          go
            (Map.insert (messageName proxy) (map (T.pack . fst) descriptors) seen)
            (mapMaybe (messageFieldType . snd) descriptors <> rest)

-- | The message type of a message field.
messageFieldType :: FieldDescriptor msg -> Maybe SomeMessage
messageFieldType (FieldDescriptor _ fieldType _) = case fieldType of
  MessageField _ -> Just . SomeMessage $ proxyOf fieldType
  _ -> Nothing

proxyOf :: FieldTypeDescriptor value -> Proxy value
proxyOf _ = Proxy

hasNoDuplicates :: Eq a => [a] -> Bool
hasNoDuplicates xs = length (nub xs) == length xs

-- | A field's type in a hand-written description of a message: scalars and
-- unlisted message fields have no children.
newtype Shape = Shape [(Text, Shape)]

readParamsShape :: Shape
readParamsShape =
  Shape
    [ ("ledger_tip", Shape [(name, leaf) | name <- ["slot", "hash", "height", "timestamp"]])
    ,
      ( "values"
      , Shape
          [
            ( "cardano"
            , Shape
                [ ("max_tx_size", leaf)
                , ("pool_influence", rational)
                , ("protocol_version", Shape [("major", leaf), ("minor", leaf)])
                , ("prices", Shape [("steps", rational), ("memory", rational)])
                , ("cost_models", Shape [("plutus_v1", Shape [("values", leaf)])])
                ]
            )
          ]
      )
    ]
 where
  leaf = Shape []
  rational = Shape [("numerator", leaf), ("denominator", leaf)]

-- | Whether the segments follow the description from the root.
isValidPath :: Shape -> [Text] -> Bool
isValidPath _ [] = True
isValidPath (Shape fields) (segment : rest) = case lookup segment fields of
  Nothing -> False
  Just child@(Shape grandchildren) -> null rest || (not (null grandchildren) && isValidPath child rest)

-- | A prefix of a path through the description, sometimes followed by a segment
-- that names no field, which may sit below a scalar.
genTreePath :: Gen Text
genTreePath = do
  depth <- Gen.int (Range.linear 1 4)
  walked <- walk depth readParamsShape
  junk <- Gen.element [[], [], ["zzz"], ["x"]]
  pure . T.intercalate "." $ walked <> junk
 where
  walk :: Int -> Shape -> Gen [Text]
  walk 0 _ = pure []
  walk _ (Shape []) = pure []
  walk remaining (Shape fields) = do
    (name, child) <- Gen.element fields
    (name :) <$> walk (remaining - 1) child

-- | The paths a mask resolves to, sorted; a placeholder for 'SelectAll'.
resolved :: forall msg. Message msg => [Text] -> [Text]
resolved input = case resolve @msg input of
  SelectAll -> ["<all>"]
  Select tree -> toPaths tree

-- | Dotted paths whose segments are real field names at that depth or junk.
genResolvePaths :: Gen [Text]
genResolvePaths = Gen.list (Range.linear 0 8) $ do
  depths <- Gen.list (Range.linear 1 4) (pure ())
  T.intercalate "." <$> traverse genSegmentAt (zip [0 :: Int ..] depths)
 where
  genSegmentAt (depth, ()) =
    Gen.frequency
      [ (1, Gen.element ["zzz", "x", ""])
      , (5, Gen.element $ realSegments depth)
      ]
  realSegments = \case
    0 -> ["values", "ledger_tip"]
    1 -> ["cardano", "slot", "hash", "height"]
    2 -> ["max_tx_size", "pool_influence", "cost_models", "protocol_version"]
    _ -> ["numerator", "denominator", "major", "plutus_v1"]

-- | Real paths below 'ReadUtxosResponse', at several depths.
utxoMaskPaths :: [Text]
utxoMaskPaths =
  [ "items"
  , "items.native_bytes"
  , "items.txo_ref"
  , "items.txo_ref.hash"
  , "items.cardano"
  , "items.cardano.coin"
  , "items.block_ref.slot"
  , "ledger_tip"
  ]

paths :: [Text] -> [Text]
paths = toPaths . normalise

segments :: Text -> [Text]
segments = T.splitOn "."

isSegmentPrefix :: Text -> Text -> Bool
isSegmentPrefix prefix path = segments prefix `isPrefixOf` segments path

-- | Dotted paths of up to four segments over a tiny alphabet, so prefixes
-- collide often. Segments are occasionally empty to exercise the dropping.
genPaths :: Gen [Text]
genPaths = Gen.list (Range.linear 0 12) genPath

genPath :: Gen Text
genPath = T.intercalate "." <$> Gen.list (Range.linear 1 4) genSegment

genSegment :: Gen Text
genSegment = Gen.frequency [(1, pure ""), (12, Gen.element ["a", "b", "c", "a-"])]
