{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for how @SearchUtxos@ builds its response from a field mask.
module Test.Cardano.Rpc.SearchUtxos where

import Cardano.Api (ChainPoint (..), TxIn)
import Cardano.Api.Parser.Text qualified as P
import Cardano.Api.Tx (parseTxIn)
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as U5c
import Cardano.Rpc.Server.Internal.FieldMask
import Cardano.Rpc.Server.Internal.UtxoRpc.Query
  ( mkSearchUtxosResponse
  , paginateByTxIn
  , searchUtxosFieldMask
  )
import Cardano.Rpc.Server.Internal.UtxoRpc.Type

import Cardano.Slotting.Slot (WithOrigin (..))

import RIO

import Data.Map.Strict qualified as Map
import Data.ProtoLens (defMessage)
import Data.Text qualified as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Network.GRPC.Spec (getProto)

import Hedgehog as H
import Hedgehog.Extras qualified as H

import Proto.Google.Protobuf.FieldMask_Fields qualified as FieldMask (paths)

maskOf :: [Text] -> FieldMaskTree U5c.SearchUtxosResponse
maskOf = resolve

realTip :: U5c.ChainPoint
realTip = getProto $ mkChainPointMsg ChainPointAtGenesis Origin (posixSecondsToUTCTime 1)

realToken :: Maybe Text
realToken = Just "next-token"

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

build :: [Text] -> U5c.SearchUtxosResponse
build paths =
  mkSearchUtxosResponse (maskOf paths) (Just realFields) (Just realToken) (Just realTip)

hprop_field_mask_search_utxos_unselected_item_fields_are_not_evaluated :: Property
hprop_field_mask_search_utxos_unselected_item_fields_are_not_evaluated = H.propertyOnce $ do
  let lazyFields =
        [ UtxoFields (utxoFieldsRef f) (error "forced") (error "forced")
        | f <- realFields
        ]
  response <-
    H.evalNF $
      mkSearchUtxosResponse
        (maskOf ["items.txo_ref"])
        (Just lazyFields)
        (Just $ error "forced")
        (Just $ error "forced")
  map (view U5c.txoRef) (response ^. U5c.items) === map utxoFieldsRef realFields
  H.assertWith response $ all (isNothing . view U5c.maybe'cardano) . view U5c.items
  H.assertWith response $ all ((== mempty) . view U5c.nativeBytes) . view U5c.items
  H.assertWith response $ isNothing . view U5c.maybe'ledgerTip
  H.assertWith response $ isNothing . view U5c.maybe'nextToken

hprop_field_mask_search_utxos_unselected_items_and_token_are_not_evaluated :: Property
hprop_field_mask_search_utxos_unselected_items_and_token_are_not_evaluated = H.propertyOnce $ do
  response <-
    H.evalNF $
      mkSearchUtxosResponse
        (maskOf ["ledger_tip"])
        (Just $ error "forced")
        (Just $ error "forced")
        (Just realTip)
  response ^. U5c.maybe'ledgerTip === Just realTip
  H.assertWith response $ null . view U5c.items
  H.assertWith response $ isNothing . view U5c.maybe'nextToken

hprop_field_mask_search_utxos_unselected_items_are_not_evaluated :: Property
hprop_field_mask_search_utxos_unselected_items_are_not_evaluated = H.propertyOnce $ do
  response <-
    H.evalNF $
      mkSearchUtxosResponse
        (maskOf ["next_token"])
        (Just $ error "forced")
        (Just realToken)
        (Just $ error "forced")
  response ^. U5c.maybe'nextToken === realToken
  H.assertWith response $ null . view U5c.items
  H.assertWith response $ isNothing . view U5c.maybe'ledgerTip

hprop_field_mask_search_utxos_gated_paths_set_their_field :: Property
hprop_field_mask_search_utxos_gated_paths_set_their_field = H.propertyOnce $ do
  build ["ledger_tip"] ^. U5c.maybe'ledgerTip === Just realTip
  H.assertWith (build ["ledger_tip"]) $ null . view U5c.items
  build ["next_token"] ^. U5c.maybe'nextToken === realToken
  H.assertWith (build ["next_token"]) $ null . view U5c.items
  map (view U5c.txoRef) (build ["items.txo_ref"] ^. U5c.items) === map utxoFieldsRef realFields
  map (view U5c.nativeBytes) (build ["items.native_bytes"] ^. U5c.items)
    === map utxoFieldsNativeBytes realFields
  map (view U5c.cardano) (build ["items.cardano"] ^. U5c.items) === map utxoFieldsCardano realFields
  -- The whole items field selects every item field.
  map (view U5c.nativeBytes) (build ["items"] ^. U5c.items) === map utxoFieldsNativeBytes realFields

hprop_field_mask_search_utxos_select_all_sets_every_field :: Property
hprop_field_mask_search_utxos_select_all_sets_every_field = H.propertyOnce $ do
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
  response ^. U5c.maybe'nextToken === realToken

hprop_field_mask_search_utxos_nested_path_trims_cardano :: Property
hprop_field_mask_search_utxos_nested_path_trims_cardano = H.propertyOnce $ do
  let response = build ["items.cardano.address"]
      expectedItems =
        [ defMessage & U5c.cardano . U5c.address .~ utxoFieldsCardano f ^. U5c.address
        | f <- realFields
        ]
  response ^. U5c.items === expectedItems
  H.assertWith response $ isNothing . view U5c.maybe'ledgerTip
  H.assertWith response $ isNothing . view U5c.maybe'nextToken

-- | The handler's own mask wiring: request paths select, an empty request selects all.
hprop_field_mask_search_utxos_request_mask_selects_requested_paths :: Property
hprop_field_mask_search_utxos_request_mask_selects_requested_paths = H.propertyOnce $ do
  let request = defMessage & U5c.fieldMask . FieldMask.paths .~ ["next_token"]
      fieldMask = searchUtxosFieldMask request
  H.assertWith fieldMask $ isSelected @"nextToken"
  H.assertWith fieldMask $ not . isSelected @"items"
  H.assertWith fieldMask $ not . isSelected @"ledgerTip"
  searchUtxosFieldMask defMessage === SelectAll

-- | The builder passes the pagination token through unchanged whenever the mask selects it.
hprop_field_mask_search_utxos_next_token_is_the_same_under_any_mask :: Property
hprop_field_mask_search_utxos_next_token_is_the_same_under_any_mask = H.propertyOnce $ do
  let entries = Map.fromList [mkEntry i | i <- [0 .. 4 :: Int]]
      (page, token) = paginateByTxIn (const True) entries Nothing 2
      items = map snd page
      respond paths =
        mkSearchUtxosResponse (maskOf paths) (Just items) (Just token) (Just realTip)
  length page === 2
  H.assertWith token isJust
  respond ["next_token"] ^. U5c.maybe'nextToken === token
  respond [] ^. U5c.maybe'nextToken === token
  length (respond ["items"] ^. U5c.items) === 2
 where
  mkEntry i =
    ( mkTxIn $ T.replicate 64 "1" <> "#" <> T.pack (show i)
    , UtxoFields
        { utxoFieldsRef = defMessage & U5c.index .~ fromIntegral i
        , utxoFieldsNativeBytes = "native"
        , utxoFieldsCardano = defMessage & U5c.address .~ "address"
        }
    )

mkTxIn :: Text -> TxIn
mkTxIn = either error id . P.runParser parseTxIn
