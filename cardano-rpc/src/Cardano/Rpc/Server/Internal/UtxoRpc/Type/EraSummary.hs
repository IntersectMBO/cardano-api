{-# LANGUAGE LambdaCase #-}

-- | Conversion of the node's hard-fork era summary to the UTxO RPC
-- 'U5c.EraSummaries' message.
module Cardano.Rpc.Server.Internal.UtxoRpc.Type.EraSummary
  ( eraSummariesToProto
  )
where

import Cardano.Api (AnyCardanoEra (..), SystemStart, docToText, pretty, unEpochNo, unSlotNo)
import Cardano.Api.Consensus qualified as Consensus
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as U5c
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.ChainPoint (utcTimeToMs)

import Cardano.Slotting.Time (fromRelativeTime)
import Ouroboros.Consensus.Cardano.Block (CardanoEras)
import Ouroboros.Consensus.HardFork.History qualified as History

import RIO

import Data.ProtoLens (defMessage)
import Data.SOP.NonEmpty (nonEmptyToList)
import Data.Text qualified as Text
import Network.GRPC.Spec

-- | Convert the node's hard-fork era summary to the UTxO RPC
-- 'U5c.EraSummaries' message.
--
-- Every era except the last gets its 'U5c.maybe''end' populated from the
-- confirmed era transition. The last era's end is always left unset, even
-- when consensus already supplies a bound for it: consensus cannot
-- distinguish a confirmed transition from the safe-zone forecast horizon, so
-- the spec's "if the era has a well-defined ending" only ever holds for
-- non-final eras here. 'History.EraUnbounded' likewise maps to unset.
--
-- 'U5c.protocolParams' is left unset for every era: the node does not keep
-- historical per-era protocol parameters. Use @ReadParams@ for the current
-- era's parameters.
eraSummariesToProto
  :: SystemStart
  -> History.Summary (CardanoEras Consensus.StandardCrypto)
  -> Proto U5c.EraSummaries
eraSummariesToProto systemStart summary =
  defMessage & U5c.summaries .~ zipWith3 mkEraSummary eraNames isLastEra eraEntries
 where
  -- All eras in chronological order, i.e. the same order as the summary's
  -- entries: 'History.Summary' has no era name field, an entry's era is its
  -- position, so the names are zipped in positionally.
  eraNames :: [Text]
  eraNames =
    [ Text.toLower . docToText $ pretty era
    | AnyCardanoEra era <- [minBound .. maxBound]
    ]

  eraEntries :: [History.EraSummary]
  eraEntries = nonEmptyToList (History.getSummary summary)

  -- 'eraEntries' is always non-empty ('Summary' wraps a non-empty list), so
  -- this always ends in exactly one 'True'.
  isLastEra :: [Bool]
  isLastEra = replicate (length eraEntries - 1) False <> [True]

  mkEraSummary :: Text -> Bool -> History.EraSummary -> Proto U5c.EraSummary
  mkEraSummary name isLast entry =
    defMessage
      & U5c.name .~ name
      & U5c.start .~ boundToProto (History.eraStart entry)
      & U5c.maybe'end .~ if isLast then Nothing else endToProto (History.eraEnd entry)

  endToProto :: History.EraEnd -> Maybe (Proto U5c.EraBoundary)
  endToProto = \case
    History.EraEnd bound -> Just (boundToProto bound)
    History.EraUnbounded -> Nothing

  boundToProto :: History.Bound -> Proto U5c.EraBoundary
  boundToProto bound =
    defMessage
      & U5c.time .~ boundTimeMs bound
      & U5c.slot .~ unSlotNo (History.boundSlot bound)
      & U5c.epoch .~ unEpochNo (History.boundEpoch bound)

  -- Reuses 'utcTimeToMs', the same millisecond conversion 'mkChainPointMsg'
  -- and 'mkTipBlockRef' use for their proto timestamps, for consistency
  -- across the API. 'fromRelativeTime' adds the boundary's 'RelativeTime' to
  -- the system start with 'Pico'-precision arithmetic throughout.
  boundTimeMs :: History.Bound -> Word64
  boundTimeMs bound = utcTimeToMs (fromRelativeTime systemStart (History.boundTime bound))
