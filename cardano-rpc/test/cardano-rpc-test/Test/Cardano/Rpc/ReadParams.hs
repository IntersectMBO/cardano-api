{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for how @ReadParams@ builds its response from a field mask.
module Test.Cardano.Rpc.ReadParams where

import Cardano.Api (ChainPoint (..))
import Cardano.Api.Experimental.Era
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as U5c
import Cardano.Rpc.Server.Internal.FieldMask
import Cardano.Rpc.Server.Internal.UtxoRpc.Query
  ( mkReadParamsResponse
  , readParamsFieldMask
  )
import Cardano.Rpc.Server.Internal.UtxoRpc.Type

import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.Coin qualified as LC (CompactForm (..))
import Cardano.Slotting.Slot (WithOrigin (..))

import RIO

import Data.ProtoLens (defMessage)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Network.GRPC.Spec (getProto)

import Hedgehog as H
import Hedgehog.Extras qualified as H

import Proto.Google.Protobuf.FieldMask_Fields qualified as FieldMask (paths)

maskOf :: [Text] -> FieldMaskTree U5c.ReadParamsResponse
maskOf = resolve

realValues :: U5c.PParams
realValues =
  getProto . obtainCommonConstraints ConwayEra $
    protocolParamsToUtxoRpcPParams ConwayEra $
      L.emptyPParams
        & L.ppMaxTxSizeL .~ 16384
        & L.ppTxFeePerByteL .~ L.CoinPerByte (LC.CompactCoin 44)

realTip :: U5c.ChainPoint
realTip = getProto $ mkChainPointMsg ChainPointAtGenesis Origin (posixSecondsToUTCTime 1)

hprop_field_mask_read_params_unselected_values_are_not_evaluated :: Property
hprop_field_mask_read_params_unselected_values_are_not_evaluated = H.propertyOnce $ do
  response <-
    H.evalNF $ mkReadParamsResponse (maskOf ["ledger_tip"]) (Just $ error "forced") (Just realTip)
  H.assertWith response $ isJust . view U5c.maybe'ledgerTip
  H.assertWith response $ isNothing . view U5c.maybe'values

hprop_field_mask_read_params_unselected_tip_is_not_evaluated :: Property
hprop_field_mask_read_params_unselected_tip_is_not_evaluated = H.propertyOnce $ do
  response <-
    H.evalNF $ mkReadParamsResponse (maskOf ["values"]) (Just realValues) (Just $ error "forced")
  H.assertWith response $ isJust . view U5c.maybe'values
  H.assertWith response $ isNothing . view U5c.maybe'ledgerTip

hprop_field_mask_read_params_gated_paths_set_their_field :: Property
hprop_field_mask_read_params_gated_paths_set_their_field = H.propertyOnce $ do
  let build m = mkReadParamsResponse (maskOf m) (Just realValues) (Just realTip)
  build ["values"] ^. U5c.values . U5c.cardano === realValues
  build ["ledger_tip"] ^. U5c.maybe'ledgerTip === Just realTip

hprop_field_mask_read_params_select_all_sets_both_fields :: Property
hprop_field_mask_read_params_select_all_sets_both_fields = H.propertyOnce $ do
  let response = mkReadParamsResponse (maskOf []) (Just realValues) (Just realTip)
  H.assertWith response $ isJust . view U5c.maybe'values
  H.assertWith response $ isJust . view U5c.maybe'ledgerTip

hprop_field_mask_read_params_nested_path_trims_values :: Property
hprop_field_mask_read_params_nested_path_trims_values = H.propertyOnce $ do
  let response =
        mkReadParamsResponse (maskOf ["values.cardano.max_tx_size"]) (Just realValues) (Just realTip)
      expected =
        defMessage
          & U5c.values . U5c.cardano . U5c.maxTxSize .~ realValues ^. U5c.maxTxSize
  response === expected

-- | The handler's own mask wiring: request paths select, an empty request selects all.
hprop_field_mask_read_params_request_mask_selects_requested_paths :: Property
hprop_field_mask_read_params_request_mask_selects_requested_paths = H.propertyOnce $ do
  let request = defMessage & U5c.fieldMask . FieldMask.paths .~ ["ledger_tip"]
      fieldMask = readParamsFieldMask request
  H.assertWith fieldMask $ isSelected @"ledgerTip"
  H.assertWith fieldMask $ not . isSelected @"values"
  readParamsFieldMask defMessage === SelectAll
