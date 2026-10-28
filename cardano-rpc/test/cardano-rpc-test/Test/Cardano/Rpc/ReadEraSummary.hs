{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tests for how @ReadEraSummary@ builds its response from a field mask.
module Test.Cardano.Rpc.ReadEraSummary where

import Cardano.Api (EpochNo (..), SlotNo (..), SystemStart (..))
import Cardano.Api.Consensus qualified as Consensus
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as U5c
import Cardano.Rpc.Server.Internal.FieldMask
import Cardano.Rpc.Server.Internal.UtxoRpc.Query
  ( readEraSummaryFill
  )
import Cardano.Rpc.Server.Internal.UtxoRpc.Type (eraSummariesToProto)

import Cardano.Slotting.Time (RelativeTime (..))
import Ouroboros.Consensus.Cardano.Block (CardanoEras)
import Ouroboros.Consensus.HardFork.History qualified as History

import RIO
import RIO.Writer (runWriter)

import Data.ProtoLens (defMessage)
import Data.SOP.NonEmpty (NonEmpty (..))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Network.GRPC.Spec (getProto)

import Test.Cardano.Rpc.EraSummary (mkBound, mkEraSummary)
import Test.Cardano.Rpc.RecordedQuery

import Hedgehog as H
import Hedgehog.Extras qualified as H

hprop_field_mask_read_era_summary_name_only_forces_no_system_start :: Property
hprop_field_mask_read_era_summary_name_only_forces_no_system_start = H.propertyOnce $ do
  (response, ran) <-
    H.evalNF $ build ["cardano.summaries.name"] (SystemStart $ error "forced") realSummary
  ran === ["summary"]
  map (view U5c.name) (response ^. U5c.cardano . U5c.summaries) === ["byron", "shelley"]
  H.assertWith response $ all (isNothing . view U5c.maybe'start) . view (U5c.cardano . U5c.summaries)
  H.assertWith response $ all (isNothing . view U5c.maybe'end) . view (U5c.cardano . U5c.summaries)

hprop_field_mask_read_era_summary_start_only_sets_no_name_and_no_end :: Property
hprop_field_mask_read_era_summary_start_only_sets_no_name_and_no_end = H.propertyOnce $ do
  (response, _) <- H.evalNF $ build ["cardano.summaries.start"] realSystemStart realSummary
  map (view U5c.start) (response ^. U5c.cardano . U5c.summaries)
    === map (view U5c.start) (expected ^. U5c.cardano . U5c.summaries)
  H.assertWith response $ all ((== mempty) . view U5c.name) . view (U5c.cardano . U5c.summaries)
  H.assertWith response $ all (isNothing . view U5c.maybe'end) . view (U5c.cardano . U5c.summaries)

hprop_field_mask_read_era_summary_end_only_sets_the_end_of_non_final_eras :: Property
hprop_field_mask_read_era_summary_end_only_sets_the_end_of_non_final_eras = H.propertyOnce $ do
  (response, _) <- H.evalNF $ build ["cardano.summaries.end"] realSystemStart realSummary
  map (view U5c.maybe'end) (response ^. U5c.cardano . U5c.summaries)
    === map (view U5c.maybe'end) (expected ^. U5c.cardano . U5c.summaries)
  H.assertWith response $ all ((== mempty) . view U5c.name) . view (U5c.cardano . U5c.summaries)
  H.assertWith response $ all (isNothing . view U5c.maybe'start) . view (U5c.cardano . U5c.summaries)

hprop_field_mask_read_era_summary_nothing_selected_runs_no_query :: Property
hprop_field_mask_read_era_summary_nothing_selected_runs_no_query = H.propertyOnce $ do
  build ["bogus"] (SystemStart $ error "forced") (error "forced") === (defMessage, [])

-- | The expected values are hand-written in "Test.Cardano.Rpc.EraSummary".
hprop_field_mask_read_era_summary_select_all_matches_the_whole_conversion :: Property
hprop_field_mask_read_era_summary_select_all_matches_the_whole_conversion = H.propertyOnce $ do
  let (response, ran) = build [] realSystemStart realSummary
  ran === ["summary"]
  response === expected
  length (response ^. U5c.cardano . U5c.summaries) === 2

hprop_field_mask_read_era_summary_nested_path_trims_boundary :: Property
hprop_field_mask_read_era_summary_nested_path_trims_boundary = H.propertyOnce $ do
  let trimmed =
        defMessage
          & U5c.cardano . U5c.summaries
            .~ [ defMessage & U5c.start . U5c.slot .~ 0
               , defMessage & U5c.start . U5c.slot .~ 21_600
               ]
  fst (build ["cardano.summaries.start.slot"] realSystemStart realSummary) === trimmed

-- | Run the response fill for the mask paths, recording the era history read.
build
  :: [FieldPath]
  -- ^ Field mask paths, rooted at the response
  -> SystemStart
  -- ^ The system start of the chain
  -> History.Summary (CardanoEras Consensus.StandardCrypto)
  -- ^ The era history that the recorded read returns
  -> (U5c.ReadEraSummaryResponse, [Text])
build = buildWith . resolve

-- | 'build' for a resolved field mask. The handler runs the fill without a session.
buildWith
  :: FieldMaskTree U5c.ReadEraSummaryResponse
  -- ^ The field mask, rooted at the response
  -> SystemStart
  -- ^ The system start of the chain
  -> History.Summary (CardanoEras Consensus.StandardCrypto)
  -- ^ The era history that the recorded read returns
  -> (U5c.ReadEraSummaryResponse, [Text])
buildWith fieldMask systemStart summary =
  runWriter . runFill id fieldMask $ readEraSummaryFill (recorded "summary" summary) systemStart

-- | The response of the whole conversion, which the unmasked fill must equal.
expected :: U5c.ReadEraSummaryResponse
expected =
  defMessage & U5c.cardano .~ getProto (eraSummariesToProto realSystemStart realSummary)

realSystemStart :: SystemStart
realSystemStart = SystemStart $ posixSecondsToUTCTime 1_000

-- | Two eras whose bounds differ from the defaults in every field.
realSummary :: History.Summary (CardanoEras Consensus.StandardCrypto)
realSummary =
  History.Summary
    ( NonEmptyCons
        (mkEraSummary byronStart $ History.EraEnd shelleyStart)
        ( NonEmptyOne . mkEraSummary shelleyStart . History.EraEnd $
            mkBound (SlotNo 43_200) (EpochNo 2) (RelativeTime 259_200)
        )
    )

byronStart, shelleyStart :: History.Bound
byronStart = mkBound (SlotNo 0) (EpochNo 0) (RelativeTime 0)
shelleyStart = mkBound (SlotNo 21_600) (EpochNo 1) (RelativeTime 172_800.5)
