module Cardano.Api.LedgerState.Internal.Rule.TICK.RUPD
  ( handleLedgerRUPDEvents
  )
where

import Cardano.Api.Address (fromShelleyStakeCredential)
import Cardano.Api.LedgerState.Internal.LedgerEvent
  ( LedgerEvent (IncrementalRewardsDistribution)
  )

import Cardano.Ledger.Shelley.Rules

import Data.Map.Strict qualified as Map

handleLedgerRUPDEvents :: RupdEvent -> Maybe LedgerEvent
handleLedgerRUPDEvents (RupdEvent epochNum rewards) =
  Just $ IncrementalRewardsDistribution epochNum (Map.mapKeys fromShelleyStakeCredential rewards)
