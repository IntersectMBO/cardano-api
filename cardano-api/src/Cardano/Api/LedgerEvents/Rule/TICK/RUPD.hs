module Cardano.Api.LedgerEvents.Rule.TICK.RUPD
  ( handleLedgerRUPDEvents
  )
where

import           Cardano.Api.Address (fromShelleyStakeCredential)
import           Cardano.Api.LedgerEvents.LedgerEvent (LedgerEvent (IncrementalRewardsDistribution))

import           Cardano.Ledger.Crypto (StandardCrypto)
import           Cardano.Ledger.Shelley.Rules

import qualified Data.Map.Strict as Map

handleLedgerRUPDEvents :: RupdEvent StandardCrypto -> Maybe LedgerEvent
handleLedgerRUPDEvents (RupdEvent epochNum rewards) =
  Just $ IncrementalRewardsDistribution epochNum (Map.mapKeys fromShelleyStakeCredential rewards)
