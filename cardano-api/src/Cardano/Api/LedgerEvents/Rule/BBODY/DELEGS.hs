{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.LedgerEvents.Rule.BBODY.DELEGS
  ( handleShelleyDELEGSEvent
  )
where

import           Cardano.Api.LedgerEvents.LedgerEvent

import qualified Cardano.Ledger.Shelley.Rules as Shelley

handleShelleyDELEGSEvent :: Shelley.ShelleyDelegsEvent ledgerera -> Maybe LedgerEvent
handleShelleyDELEGSEvent _ = Nothing
