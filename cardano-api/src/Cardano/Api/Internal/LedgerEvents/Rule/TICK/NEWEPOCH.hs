{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Api.Internal.LedgerEvents.Rule.TICK.NEWEPOCH
  ( LatestTickEventConstraints
  , handleShelleyNEWEPOCHEvents
  , handleLedgerTICKEvents
  , handleConwayNEWEPOCHEvents
  )
where

import Cardano.Api.Internal.Address (fromShelleyStakeCredential)
import Cardano.Api.Internal.LedgerEvents.LedgerEvent
import Cardano.Api.Internal.LedgerEvents.Rule.TICK.RUPD
import Cardano.Api.Internal.ReexposeLedger

import Cardano.Ledger.Conway.Rules (ConwayNewEpochEvent)
import Cardano.Ledger.Conway.Rules qualified as Conway
import Cardano.Ledger.Core qualified as Core
import Cardano.Ledger.Shelley.Rules
import Cardano.Ledger.Shelley.Rules qualified as Shelley

import Data.Map.Strict qualified as Map

type LatestTickEventConstraints ledgerera =
  ( Event (Core.EraRule "TICK" ledgerera) ~ ShelleyTickEvent ledgerera
  , Event (Core.EraRule "RUPD" ledgerera) ~ RupdEvent StandardCrypto
  , Event (Core.EraRule "NEWEPOCH" ledgerera) ~ ShelleyNewEpochEvent ledgerera
  , Event (Core.EraRule "EPOCH" ledgerera) ~ Shelley.ShelleyEpochEvent ledgerera
  , Event (Core.EraRule "POOLREAP" ledgerera) ~ Shelley.ShelleyPoolreapEvent ledgerera
  )

handleLedgerTICKEvents
  :: EraCrypto ledgerera ~ StandardCrypto
  => LatestTickEventConstraints ledgerera
  => ShelleyTickEvent ledgerera -> Maybe LedgerEvent
handleLedgerTICKEvents (TickNewEpochEvent newEpochEvent) = handleShelleyNEWEPOCHEvents newEpochEvent
handleLedgerTICKEvents (TickRupdEvent rewardUpdate) = handleLedgerRUPDEvents rewardUpdate

handleShelleyNEWEPOCHEvents
  :: EraCrypto ledgerera ~ StandardCrypto
  => Event (Core.EraRule "EPOCH" ledgerera) ~ ShelleyEpochEvent ledgerera
  => Event (Core.EraRule "POOLREAP" ledgerera) ~ ShelleyPoolreapEvent ledgerera
  => ShelleyNewEpochEvent ledgerera -> Maybe LedgerEvent
handleShelleyNEWEPOCHEvents shelleyNewEpochEvent =
  case shelleyNewEpochEvent of
    Shelley.DeltaRewardEvent{} -> Nothing
    Shelley.RestrainedRewards{} -> Nothing
    Shelley.TotalRewardEvent epochNo rewardsMap ->
      Just $ RewardsDistribution epochNo (Map.mapKeys fromShelleyStakeCredential rewardsMap)
    Shelley.EpochEvent e -> handleEpochEvents e
    Shelley.MirEvent{} -> Nothing -- We no longer care about MIR events
    Shelley.TotalAdaPotsEvent{} -> Nothing

handleEpochEvents
  :: EraCrypto ledgerera ~ StandardCrypto
  => Event (Core.EraRule "POOLREAP" ledgerera) ~ ShelleyPoolreapEvent ledgerera
  => ShelleyEpochEvent ledgerera -> Maybe LedgerEvent
handleEpochEvents (PoolReapEvent e) =
  case e of
    RetiredPools{refundPools, unclaimedPools, epochNo} ->
      Just . PoolReap $
        PoolReapDetails
          epochNo
          (convertRetiredPoolsMap refundPools)
          (convertRetiredPoolsMap unclaimedPools)
handleEpochEvents (SnapEvent{}) = Nothing
handleEpochEvents (UpecEvent{}) = Nothing

handleConwayNEWEPOCHEvents
  :: EraCrypto ledgerera ~ StandardCrypto
  => Core.EraPParams ledgerera
  => Event (Core.EraRule "EPOCH" ledgerera) ~ Conway.ConwayEpochEvent ledgerera
  => Event (Core.EraRule "POOLREAP" ledgerera) ~ ShelleyPoolreapEvent ledgerera
  => Event (Core.EraRule "RUPD" ledgerera) ~ RupdEvent StandardCrypto
  => ConwayNewEpochEvent ledgerera -> Maybe LedgerEvent
handleConwayNEWEPOCHEvents conwayNewEpochEvent =
  case conwayNewEpochEvent of
    Conway.DeltaRewardEvent rewardUpdate ->
      case rewardUpdate of
        RupdEvent epochNum rewards ->
          Just $ IncrementalRewardsDistribution epochNum (Map.mapKeys fromShelleyStakeCredential rewards)
    Conway.RestrainedRewards{} -> Nothing
    Conway.TotalRewardEvent epochNo rewardsMap ->
      Just $ RewardsDistribution epochNo (Map.mapKeys fromShelleyStakeCredential rewardsMap)
    Conway.EpochEvent epochEvent ->
      case epochEvent of
        Conway.EpochBoundaryRatifyState ratifyState ->
          Just $ EpochBoundaryRatificationState (AnyRatificationState ratifyState)
        Conway.PoolReapEvent poolReap ->
          case poolReap of
            RetiredPools{refundPools, unclaimedPools, epochNo} ->
              Just . PoolReap $
                PoolReapDetails
                  epochNo
                  (convertRetiredPoolsMap refundPools)
                  (convertRetiredPoolsMap unclaimedPools)
        Conway.SnapEvent _ -> Nothing
        Conway.GovInfoEvent{} -> Nothing
        Conway.HardForkEvent{} -> Nothing
    Conway.TotalAdaPotsEvent _ -> Nothing
