module Cardano.Api.Internal.LedgerEvents.Rule.TICK.RUPD
  ( handleLedgerRUPDEvents
  )
where

import Cardano.Api.Internal.Address (fromShelleyStakeCredential)
import Cardano.Api.Internal.LedgerEvents.LedgerEvent
  ( LedgerEvent (IncrementalRewardsDistribution)
  )

import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Shelley.Rules

import Data.Map.Strict qualified as Map

handleLedgerRUPDEvents :: RupdEvent StandardCrypto -> Maybe LedgerEvent
handleLedgerRUPDEvents (RupdEvent epochNum rewards) =
  Just $ IncrementalRewardsDistribution epochNum (Map.mapKeys fromShelleyStakeCredential rewards)
