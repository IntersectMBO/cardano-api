{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tests for how @ReadUtxos@ builds its response from a field mask.
module Test.Cardano.Rpc.ReadUtxos where

import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as U5c
import Cardano.Rpc.Server.Internal.FieldMask
import Cardano.Rpc.Server.Internal.UtxoRpc.Query
  ( readUtxosFill
  )
import Cardano.Rpc.Server.Internal.UtxoRpc.Type

import RIO

import Data.ProtoLens (defMessage)

import Test.Cardano.Rpc.RecordedQuery

import Hedgehog as H
import Hedgehog.Extras qualified as H

hprop_field_mask_read_utxos_unselected_item_fields_are_not_evaluated :: Property
hprop_field_mask_read_utxos_unselected_item_fields_are_not_evaluated = H.propertyOnce $ do
  let lazyFields =
        [ UtxoFields (utxoFieldsRef f) (error "forced") (error "forced")
        | f <- realFields
        ]
  (response, ran) <- H.evalNF $ build ["items.txo_ref"] lazyFields
  ran === ["session", "items"]
  map (view U5c.txoRef) (response ^. U5c.items) === map utxoFieldsRef realFields
  H.assertWith response $ all (isNothing . view U5c.maybe'cardano) . view U5c.items
  H.assertWith response $ all ((== mempty) . view U5c.nativeBytes) . view U5c.items
  H.assertWith response $ isNothing . view U5c.maybe'ledgerTip

hprop_field_mask_read_utxos_cardano_only_forces_no_other_item_field :: Property
hprop_field_mask_read_utxos_cardano_only_forces_no_other_item_field = H.propertyOnce $ do
  let lazyFields =
        [ UtxoFields (error "forced") (error "forced") (utxoFieldsCardano f)
        | f <- realFields
        ]
  (response, _) <- H.evalNF $ build ["items.cardano"] lazyFields
  map (view U5c.cardano) (response ^. U5c.items) === map utxoFieldsCardano realFields
  H.assertWith response $ all (isNothing . view U5c.maybe'txoRef) . view U5c.items
  H.assertWith response $ all ((== mempty) . view U5c.nativeBytes) . view U5c.items

hprop_field_mask_read_utxos_native_bytes_only_forces_no_other_item_field :: Property
hprop_field_mask_read_utxos_native_bytes_only_forces_no_other_item_field = H.propertyOnce $ do
  let lazyFields =
        [ UtxoFields (error "forced") (utxoFieldsNativeBytes f) (error "forced")
        | f <- realFields
        ]
  (response, _) <- H.evalNF $ build ["items.native_bytes"] lazyFields
  map (view U5c.nativeBytes) (response ^. U5c.items) === map utxoFieldsNativeBytes realFields
  H.assertWith response $ all (isNothing . view U5c.maybe'txoRef) . view U5c.items
  H.assertWith response $ all (isNothing . view U5c.maybe'cardano) . view U5c.items

hprop_field_mask_read_utxos_ledger_tip_runs_only_the_tip_query :: Property
hprop_field_mask_read_utxos_ledger_tip_runs_only_the_tip_query = H.propertyOnce $ do
  let (response, ran) = build ["ledger_tip"] realFields
  ran === ["session", "ledgerTip"]
  response === (defMessage & U5c.ledgerTip .~ realTip)

hprop_field_mask_read_utxos_items_runs_only_the_utxo_query :: Property
hprop_field_mask_read_utxos_items_runs_only_the_utxo_query = H.propertyOnce $ do
  let (response, ran) = build ["items"] realFields
  ran === ["session", "items"]
  H.assertWith response $ isNothing . view U5c.maybe'ledgerTip
  length (response ^. U5c.items) === 2

hprop_field_mask_read_utxos_nothing_selected_runs_no_query :: Property
hprop_field_mask_read_utxos_nothing_selected_runs_no_query = H.propertyOnce $ do
  build ["bogus"] realFields === (defMessage, [])

-- | Each item is kept when the mask selects only item fields the handler does not set.
hprop_field_mask_read_utxos_unset_item_field_keeps_every_item :: Property
hprop_field_mask_read_utxos_unset_item_field_keeps_every_item = H.propertyOnce $ do
  fst (build ["items.block_ref"] realFields) ^. U5c.items === [defMessage, defMessage]

hprop_field_mask_read_utxos_gated_paths_set_their_field :: Property
hprop_field_mask_read_utxos_gated_paths_set_their_field = H.propertyOnce $ do
  responseFor ["ledger_tip"] ^. U5c.maybe'ledgerTip === Just realTip
  H.assertWith (responseFor ["ledger_tip"]) $ null . view U5c.items
  map (view U5c.txoRef) (responseFor ["items.txo_ref"] ^. U5c.items) === map utxoFieldsRef realFields
  map (view U5c.nativeBytes) (responseFor ["items.native_bytes"] ^. U5c.items)
    === map utxoFieldsNativeBytes realFields
  map (view U5c.cardano) (responseFor ["items.cardano"] ^. U5c.items)
    === map utxoFieldsCardano realFields
  -- The whole items field selects every item field.
  map (view U5c.nativeBytes) (responseFor ["items"] ^. U5c.items)
    === map utxoFieldsNativeBytes realFields

hprop_field_mask_read_utxos_select_all_sets_every_field :: Property
hprop_field_mask_read_utxos_select_all_sets_every_field = H.propertyOnce $ do
  let (fullResponse, ran) = build [] realFields
      expectedItems =
        [ defMessage
            & U5c.txoRef .~ utxoFieldsRef f
            & U5c.nativeBytes .~ utxoFieldsNativeBytes f
            & U5c.cardano .~ utxoFieldsCardano f
        | f <- realFields
        ]
  ran === ["session", "items", "ledgerTip"]
  fullResponse ^. U5c.items === expectedItems
  fullResponse ^. U5c.maybe'ledgerTip === Just realTip

hprop_field_mask_read_utxos_nested_path_trims_cardano :: Property
hprop_field_mask_read_utxos_nested_path_trims_cardano = H.propertyOnce $ do
  let trimmed = responseFor ["items.cardano.address"]
      expectedItems =
        [ defMessage & U5c.cardano . U5c.address .~ utxoFieldsCardano f ^. U5c.address
        | f <- realFields
        ]
  trimmed ^. U5c.items === expectedItems
  H.assertWith trimmed $ isNothing . view U5c.maybe'ledgerTip

-- | The response for the mask paths from 'realFields'.
responseFor
  :: [FieldPath]
  -- ^ Field mask paths, rooted at the response
  -> U5c.ReadUtxosResponse
responseFor paths = fst $ build paths realFields

-- | Run the response fill for the mask paths, recording the session and each query it runs.
build
  :: [FieldPath]
  -- ^ Field mask paths, rooted at the response
  -> [UtxoFields]
  -- ^ The UTxO entries that the recorded UTxO query returns
  -> (U5c.ReadUtxosResponse, [Text])
build paths items =
  runRecordedFill paths $ readUtxosFill (recorded "items" items) (recorded "ledgerTip" realTip)
