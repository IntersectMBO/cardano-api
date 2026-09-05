{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Rpc.EraSummary where

import Cardano.Api (EpochNo (..), SlotNo (..), SystemStart (..))
import Cardano.Api.Consensus qualified as Consensus
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as U5c
import Cardano.Rpc.Server.Internal.UtxoRpc.Type (eraSummariesToProto)

import Cardano.Ledger.BaseTypes (knownNonZeroBounded)
import Cardano.Slotting.Time (RelativeTime (..))
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (slotLengthFromSec)
import Ouroboros.Consensus.Cardano.Block (CardanoEras)
import Ouroboros.Consensus.HardFork.History qualified as History

import RIO

import Data.SOP.NonEmpty (NonEmpty (..))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import Hedgehog as H
import Hedgehog.Extras qualified as H

-- | Placeholder era parameters: 'eraSummariesToProto' never reads them, but
-- an 'History.EraSummary' fixture still needs one.
dummyEraParams :: History.EraParams
dummyEraParams =
  History.defaultEraParams
    (Consensus.SecurityParam (knownNonZeroBounded @2160))
    (slotLengthFromSec 1)

mkBound :: SlotNo -> EpochNo -> RelativeTime -> History.Bound
mkBound slot epoch time =
  History.Bound
    { History.boundTime = time
    , History.boundSlot = slot
    , History.boundEpoch = epoch
    , History.boundPerasRound = History.NoPerasEnabled
    }

mkEraSummary :: History.Bound -> History.EraEnd -> History.EraSummary
mkEraSummary start end =
  History.EraSummary
    { History.eraStart = start
    , History.eraEnd = end
    , History.eraParams = dummyEraParams
    }

-- | Two eras: the boundary between them uses a fractional-second
-- 'RelativeTime' to prove the millisecond conversion is exact (rounded to
-- the nearest millisecond, never routed through 'Double'). The second era is
-- last and carries a real 'History.EraEnd' bound, but its end must still
-- come out unset.
hprop_era_summary_multi_era :: Property
hprop_era_summary_multi_era = H.propertyOnce $ do
  let systemStart = SystemStart (posixSecondsToUTCTime 0)

      byronStart = mkBound (SlotNo 0) (EpochNo 0) (RelativeTime 0)
      -- 172800.6789s proves the ms conversion is exact fixed-point via the
      -- shared 'utcTimeToMs' (nearest-ms rounding): .6789s -> 679ms. A
      -- Double-based path, or a floor instead of a round, would give 678.
      transition = mkBound (SlotNo 21600) (EpochNo 1) (RelativeTime 172800.6789)
      shelleyEnd = mkBound (SlotNo 43200) (EpochNo 2) (RelativeTime 259200)

      byronSummary = mkEraSummary byronStart (History.EraEnd transition)
      shelleySummary = mkEraSummary transition (History.EraEnd shelleyEnd)

      summary :: History.Summary (CardanoEras Consensus.StandardCrypto)
      summary = History.Summary (NonEmptyCons byronSummary (NonEmptyOne shelleySummary))

      proto = eraSummariesToProto systemStart summary
      entries = proto ^. U5c.summaries

  length entries === 2

  byronEntry <- H.nothingFail $ listToMaybe entries
  let shelleyEntry = entries !! 1

  byronEntry ^. U5c.name === "byron"
  byronEntry ^. U5c.start . U5c.time === 0
  byronEntry ^. U5c.start . U5c.slot === 0
  byronEntry ^. U5c.start . U5c.epoch === 0
  H.assertWith (byronEntry ^. U5c.maybe'end) isJust
  byronEntry ^. U5c.end . U5c.time === 172800679
  byronEntry ^. U5c.end . U5c.slot === 21600
  byronEntry ^. U5c.end . U5c.epoch === 1

  shelleyEntry ^. U5c.name === "shelley"
  shelleyEntry ^. U5c.start . U5c.time === 172800679
  shelleyEntry ^. U5c.start . U5c.slot === 21600
  shelleyEntry ^. U5c.start . U5c.epoch === 1
  -- Last era: end must be unset even though the fixture supplies a real bound.
  H.assertWith (shelleyEntry ^. U5c.maybe'end) isNothing

-- | A single-era summary (only Byron) has exactly one entry, and that entry
-- has no end, whether or not consensus reports the era as unbounded.
hprop_era_summary_single_era_no_end :: Property
hprop_era_summary_single_era_no_end = H.propertyOnce $ do
  let systemStart = SystemStart (posixSecondsToUTCTime 0)
      byronStart = mkBound (SlotNo 0) (EpochNo 0) (RelativeTime 0)
      byronSummary = mkEraSummary byronStart History.EraUnbounded

      summary :: History.Summary (CardanoEras Consensus.StandardCrypto)
      summary = History.Summary (NonEmptyOne byronSummary)

      proto = eraSummariesToProto systemStart summary
      entries = proto ^. U5c.summaries

  length entries === 1

  byronEntry <- H.nothingFail $ listToMaybe entries
  byronEntry ^. U5c.name === "byron"
  H.assertWith (byronEntry ^. U5c.maybe'end) isNothing
