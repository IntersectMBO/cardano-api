{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tests for how @SearchUtxos@ builds its response from a field mask.
module Test.Cardano.Rpc.SearchUtxos where

import Cardano.Api (TxIn)
import Cardano.Api.Parser.Text qualified as P
import Cardano.Api.Tx (parseTxIn)
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as U5c
import Cardano.Rpc.Server.Internal.FieldMask
import Cardano.Rpc.Server.Internal.UtxoRpc.Query
  ( paginateByTxIn
  , searchUtxosFill
  )
import Cardano.Rpc.Server.Internal.UtxoRpc.Type

import RIO

import Data.Map.Strict qualified as Map
import Data.ProtoLens (defMessage)
import Data.Text qualified as T

import Test.Cardano.Rpc.RecordedQuery

import Hedgehog as H
import Hedgehog.Extras qualified as H

hprop_field_mask_search_utxos_unselected_item_fields_are_not_evaluated :: Property
hprop_field_mask_search_utxos_unselected_item_fields_are_not_evaluated = H.propertyOnce $ do
  let lazyFields =
        [ UtxoFields (utxoFieldsRef f) (error "forced") (error "forced")
        | f <- realFields
        ]
  (response, ran) <- H.evalNF $ build ["items.txo_ref"] lazyFields realToken
  ran === ["session", "page"]
  map (view U5c.txoRef) (response ^. U5c.items) === map utxoFieldsRef realFields
  H.assertWith response $ all (isNothing . view U5c.maybe'cardano) . view U5c.items
  H.assertWith response $ all ((== mempty) . view U5c.nativeBytes) . view U5c.items
  H.assertWith response $ isNothing . view U5c.maybe'ledgerTip
  H.assertWith response $ isNothing . view U5c.maybe'nextToken

hprop_field_mask_search_utxos_ledger_tip_runs_only_the_tip_query :: Property
hprop_field_mask_search_utxos_ledger_tip_runs_only_the_tip_query = H.propertyOnce $ do
  let (response, ran) = build ["ledger_tip"] realFields realToken
  ran === ["session", "ledgerTip"]
  response === (defMessage & U5c.ledgerTip .~ realTip)

hprop_field_mask_search_utxos_next_token_runs_only_the_page_query :: Property
hprop_field_mask_search_utxos_next_token_runs_only_the_page_query = H.propertyOnce $ do
  (response, ran) <- H.evalNF $ build ["next_token"] (error "forced") realToken
  ran === ["session", "page"]
  response ^. U5c.maybe'nextToken === realToken
  H.assertWith response $ null . view U5c.items
  H.assertWith response $ isNothing . view U5c.maybe'ledgerTip

hprop_field_mask_search_utxos_items_and_next_token_share_one_page_query :: Property
hprop_field_mask_search_utxos_items_and_next_token_share_one_page_query = H.propertyOnce $ do
  let (response, ran) = build ["items", "next_token"] realFields realToken
  ran === ["session", "page"]
  length (response ^. U5c.items) === 2
  response ^. U5c.maybe'nextToken === realToken

hprop_field_mask_search_utxos_nothing_selected_runs_no_query :: Property
hprop_field_mask_search_utxos_nothing_selected_runs_no_query = H.propertyOnce $ do
  build ["bogus"] realFields realToken === (defMessage, [])

hprop_field_mask_search_utxos_gated_paths_set_their_field :: Property
hprop_field_mask_search_utxos_gated_paths_set_their_field = H.propertyOnce $ do
  responseFor ["ledger_tip"] ^. U5c.maybe'ledgerTip === Just realTip
  H.assertWith (responseFor ["ledger_tip"]) $ null . view U5c.items
  responseFor ["next_token"] ^. U5c.maybe'nextToken === realToken
  H.assertWith (responseFor ["next_token"]) $ null . view U5c.items
  map (view U5c.txoRef) (responseFor ["items.txo_ref"] ^. U5c.items) === map utxoFieldsRef realFields
  map (view U5c.nativeBytes) (responseFor ["items.native_bytes"] ^. U5c.items)
    === map utxoFieldsNativeBytes realFields
  map (view U5c.cardano) (responseFor ["items.cardano"] ^. U5c.items)
    === map utxoFieldsCardano realFields
  -- The whole items field selects every item field.
  map (view U5c.nativeBytes) (responseFor ["items"] ^. U5c.items)
    === map utxoFieldsNativeBytes realFields

hprop_field_mask_search_utxos_select_all_sets_every_field :: Property
hprop_field_mask_search_utxos_select_all_sets_every_field = H.propertyOnce $ do
  let (fullResponse, ran) = build [] realFields realToken
      expectedItems =
        [ defMessage
            & U5c.txoRef .~ utxoFieldsRef f
            & U5c.nativeBytes .~ utxoFieldsNativeBytes f
            & U5c.cardano .~ utxoFieldsCardano f
        | f <- realFields
        ]
  ran === ["session", "page", "ledgerTip"]
  fullResponse ^. U5c.items === expectedItems
  fullResponse ^. U5c.maybe'ledgerTip === Just realTip
  fullResponse ^. U5c.maybe'nextToken === realToken

hprop_field_mask_search_utxos_nested_path_trims_cardano :: Property
hprop_field_mask_search_utxos_nested_path_trims_cardano = H.propertyOnce $ do
  let trimmed = responseFor ["items.cardano.address"]
      expectedItems =
        [ defMessage & U5c.cardano . U5c.address .~ utxoFieldsCardano f ^. U5c.address
        | f <- realFields
        ]
  trimmed ^. U5c.items === expectedItems
  H.assertWith trimmed $ isNothing . view U5c.maybe'ledgerTip
  H.assertWith trimmed $ isNothing . view U5c.maybe'nextToken

-- | The fill passes the pagination token through unchanged whenever the mask selects it.
hprop_field_mask_search_utxos_next_token_is_the_same_under_any_mask :: Property
hprop_field_mask_search_utxos_next_token_is_the_same_under_any_mask = H.propertyOnce $ do
  let entries = Map.fromList [mkEntry i | i <- [0 .. 4 :: Int]]
      (page, token) = paginateByTxIn (const True) entries Nothing 2
      respond paths = fst $ build paths (map snd page) token
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

-- | The response for the mask paths from 'realFields' and 'realToken'.
responseFor
  :: [FieldPath]
  -- ^ Field mask paths, rooted at the response
  -> U5c.SearchUtxosResponse
responseFor paths = fst $ build paths realFields realToken

-- | Run the response fill for the mask paths, recording the session and each query it runs.
build
  :: [FieldPath]
  -- ^ Field mask paths, rooted at the response
  -> [UtxoFields]
  -- ^ The UTxO entries of the page that the recorded page query returns
  -> Maybe Text
  -- ^ The next page token that the recorded page query returns
  -> (U5c.SearchUtxosResponse, [Text])
build paths items nextToken =
  runRecordedFill paths $
    searchUtxosFill (recorded "page" (items, nextToken)) (recorded "ledgerTip" realTip)

mkTxIn :: Text -> TxIn
mkTxIn = either error id . P.runParser parseTxIn

realToken :: Maybe Text
realToken = Just "next-token"
