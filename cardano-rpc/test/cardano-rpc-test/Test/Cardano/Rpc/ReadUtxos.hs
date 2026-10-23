{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for how @ReadUtxos@ builds its response from a field mask.
module Test.Cardano.Rpc.ReadUtxos where

import Cardano.Api (ChainPoint (..))
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as U5c
import Cardano.Rpc.Server.Internal.FieldMask
import Cardano.Rpc.Server.Internal.UtxoRpc.Query
  ( mkReadUtxosResponse
  , readUtxosFieldMask
  )
import Cardano.Rpc.Server.Internal.UtxoRpc.Type

import Cardano.Slotting.Slot (WithOrigin (..))

import RIO

import Data.ProtoLens (defMessage)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Network.GRPC.Spec (getProto)

import Hedgehog as H
import Hedgehog.Extras qualified as H

import Proto.Google.Protobuf.FieldMask_Fields qualified as FieldMask (paths)

maskOf :: [Text] -> FieldMaskTree U5c.ReadUtxosResponse
maskOf = resolve

realTip :: U5c.ChainPoint
realTip = getProto $ mkChainPointMsg ChainPointAtGenesis Origin (posixSecondsToUTCTime 1)

realFields :: [UtxoFields]
realFields =
  [ UtxoFields
      { utxoFieldsRef = defMessage & U5c.hash .~ "tx-1" & U5c.index .~ 0
      , utxoFieldsNativeBytes = "native-1"
      , utxoFieldsCardano = defMessage & U5c.address .~ "address-1"
      }
  , UtxoFields
      { utxoFieldsRef = defMessage & U5c.hash .~ "tx-2" & U5c.index .~ 1
      , utxoFieldsNativeBytes = "native-2"
      , utxoFieldsCardano = defMessage & U5c.address .~ "address-2"
      }
  ]

build :: [Text] -> U5c.ReadUtxosResponse
build paths = mkReadUtxosResponse (maskOf paths) (Just realFields) (Just realTip)

hprop_field_mask_read_utxos_unselected_item_fields_are_not_evaluated :: Property
hprop_field_mask_read_utxos_unselected_item_fields_are_not_evaluated = H.propertyOnce $ do
  let lazyFields =
        [ UtxoFields (utxoFieldsRef f) (error "forced") (error "forced")
        | f <- realFields
        ]
  response <-
    H.evalNF $
      mkReadUtxosResponse (maskOf ["items.txo_ref"]) (Just lazyFields) (Just realTip)
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
  response <-
    H.evalNF $
      mkReadUtxosResponse (maskOf ["items.cardano"]) (Just lazyFields) (Just realTip)
  map (view U5c.cardano) (response ^. U5c.items) === map utxoFieldsCardano realFields
  H.assertWith response $ all (isNothing . view U5c.maybe'txoRef) . view U5c.items
  H.assertWith response $ all ((== mempty) . view U5c.nativeBytes) . view U5c.items

hprop_field_mask_read_utxos_native_bytes_only_forces_no_other_item_field :: Property
hprop_field_mask_read_utxos_native_bytes_only_forces_no_other_item_field = H.propertyOnce $ do
  let lazyFields =
        [ UtxoFields (error "forced") (utxoFieldsNativeBytes f) (error "forced")
        | f <- realFields
        ]
  response <-
    H.evalNF $
      mkReadUtxosResponse (maskOf ["items.native_bytes"]) (Just lazyFields) (Just realTip)
  map (view U5c.nativeBytes) (response ^. U5c.items) === map utxoFieldsNativeBytes realFields
  H.assertWith response $ all (isNothing . view U5c.maybe'txoRef) . view U5c.items
  H.assertWith response $ all (isNothing . view U5c.maybe'cardano) . view U5c.items

hprop_field_mask_read_utxos_unselected_items_are_not_evaluated :: Property
hprop_field_mask_read_utxos_unselected_items_are_not_evaluated = H.propertyOnce $ do
  -- The handler passes 'Nothing' for unselected items; 'Just' shows the builder never forces them.
  response <-
    H.evalNF $ mkReadUtxosResponse (maskOf ["ledger_tip"]) (Just $ error "forced") (Just realTip)
  H.assertWith response $ isJust . view U5c.maybe'ledgerTip
  H.assertWith response $ null . view U5c.items

hprop_field_mask_read_utxos_unselected_tip_is_not_evaluated :: Property
hprop_field_mask_read_utxos_unselected_tip_is_not_evaluated = H.propertyOnce $ do
  -- The handler passes 'Nothing' for an unselected tip; 'Just' shows the builder never forces it.
  response <-
    H.evalNF $ mkReadUtxosResponse (maskOf ["items"]) (Just realFields) (Just $ error "forced")
  H.assertWith response $ isNothing . view U5c.maybe'ledgerTip
  length (response ^. U5c.items) === 2

hprop_field_mask_read_utxos_gated_paths_set_their_field :: Property
hprop_field_mask_read_utxos_gated_paths_set_their_field = H.propertyOnce $ do
  build ["ledger_tip"] ^. U5c.maybe'ledgerTip === Just realTip
  H.assertWith (build ["ledger_tip"]) $ null . view U5c.items
  map (view U5c.txoRef) (build ["items.txo_ref"] ^. U5c.items) === map utxoFieldsRef realFields
  map (view U5c.nativeBytes) (build ["items.native_bytes"] ^. U5c.items)
    === map utxoFieldsNativeBytes realFields
  map (view U5c.cardano) (build ["items.cardano"] ^. U5c.items) === map utxoFieldsCardano realFields
  -- The whole items field selects every item field.
  map (view U5c.nativeBytes) (build ["items"] ^. U5c.items) === map utxoFieldsNativeBytes realFields

hprop_field_mask_read_utxos_select_all_sets_every_field :: Property
hprop_field_mask_read_utxos_select_all_sets_every_field = H.propertyOnce $ do
  let response = build []
      expectedItems =
        [ defMessage
            & U5c.txoRef .~ utxoFieldsRef f
            & U5c.nativeBytes .~ utxoFieldsNativeBytes f
            & U5c.cardano .~ utxoFieldsCardano f
        | f <- realFields
        ]
  response ^. U5c.items === expectedItems
  response ^. U5c.maybe'ledgerTip === Just realTip

hprop_field_mask_read_utxos_nested_path_trims_cardano :: Property
hprop_field_mask_read_utxos_nested_path_trims_cardano = H.propertyOnce $ do
  let response = build ["items.cardano.address"]
      expectedItems =
        [ defMessage & U5c.cardano . U5c.address .~ utxoFieldsCardano f ^. U5c.address
        | f <- realFields
        ]
  response ^. U5c.items === expectedItems
  H.assertWith response $ isNothing . view U5c.maybe'ledgerTip

-- | The handler's own mask wiring: request paths select, an empty request selects all.
hprop_field_mask_read_utxos_request_mask_selects_requested_paths :: Property
hprop_field_mask_read_utxos_request_mask_selects_requested_paths = H.propertyOnce $ do
  let request = defMessage & U5c.fieldMask . FieldMask.paths .~ ["ledger_tip"]
      fieldMask = readUtxosFieldMask request
  H.assertWith fieldMask $ isSelected @"ledgerTip"
  H.assertWith fieldMask $ not . isSelected @"items"
  readUtxosFieldMask defMessage === SelectAll
