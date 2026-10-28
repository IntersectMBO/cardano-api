{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tests for how @ReadParams@ builds its response from a field mask.
module Test.Cardano.Rpc.ReadParams where

import Cardano.Api.Experimental.Era
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as U5c
import Cardano.Rpc.Server.Internal.FieldMask
import Cardano.Rpc.Server.Internal.UtxoRpc.Query
  ( readParamsFill
  )
import Cardano.Rpc.Server.Internal.UtxoRpc.Type

import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.Coin qualified as LC (CompactForm (..))

import RIO

import Data.ProtoLens (defMessage)
import Network.GRPC.Spec (getProto)

import Test.Cardano.Rpc.RecordedQuery

import Hedgehog as H
import Hedgehog.Extras qualified as H

hprop_field_mask_read_params_ledger_tip_runs_only_the_tip_query :: Property
hprop_field_mask_read_params_ledger_tip_runs_only_the_tip_query = H.propertyOnce $ do
  let (response, ran) = build ["ledger_tip"]
  ran === ["session", "ledgerTip"]
  response === (defMessage & U5c.ledgerTip .~ realTip)

hprop_field_mask_read_params_values_runs_only_the_values_query :: Property
hprop_field_mask_read_params_values_runs_only_the_values_query = H.propertyOnce $ do
  let (response, ran) = build ["values"]
  ran === ["session", "values"]
  response === (defMessage & U5c.values . U5c.cardano .~ realValues)

hprop_field_mask_read_params_nothing_selected_runs_no_query :: Property
hprop_field_mask_read_params_nothing_selected_runs_no_query = H.propertyOnce $ do
  build ["bogus"] === (defMessage, [])

hprop_field_mask_read_params_gated_paths_set_their_field :: Property
hprop_field_mask_read_params_gated_paths_set_their_field = H.propertyOnce $ do
  fst (build ["values"]) ^. U5c.values . U5c.cardano === realValues
  fst (build ["ledger_tip"]) ^. U5c.maybe'ledgerTip === Just realTip

hprop_field_mask_read_params_select_all_runs_both_queries_in_one_session :: Property
hprop_field_mask_read_params_select_all_runs_both_queries_in_one_session = H.propertyOnce $ do
  let (response, ran) = build []
  ran === ["session", "values", "ledgerTip"]
  response ^. U5c.values . U5c.cardano === realValues
  response ^. U5c.maybe'ledgerTip === Just realTip

hprop_field_mask_read_params_nested_path_trims_values :: Property
hprop_field_mask_read_params_nested_path_trims_values = H.propertyOnce $ do
  let expected =
        defMessage
          & U5c.values . U5c.cardano . U5c.maxTxSize .~ realValues ^. U5c.maxTxSize
  fst (build ["values.cardano.max_tx_size"]) === expected

-- | Run the response fill for the mask paths, recording the session and each query it runs.
build
  :: [FieldPath]
  -- ^ Field mask paths, rooted at the response
  -> (U5c.ReadParamsResponse, [Text])
build paths =
  runRecordedFill paths $
    readParamsFill (recorded "values" realValues) (recorded "ledgerTip" realTip)

realValues :: U5c.PParams
realValues =
  getProto . obtainCommonConstraints ConwayEra $
    protocolParamsToUtxoRpcPParams ConwayEra $
      L.emptyPParams
        & L.ppMaxTxSizeL .~ 16384
        & L.ppTxFeePerByteL .~ L.CoinPerByte (LC.CompactCoin 44)
