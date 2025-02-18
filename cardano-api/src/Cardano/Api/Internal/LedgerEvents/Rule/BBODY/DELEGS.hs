{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.Internal.LedgerEvents.Rule.BBODY.DELEGS
  ( handleShelleyDELEGSEvent
  )
where

import Cardano.Api.Internal.LedgerEvents.LedgerEvent

import Cardano.Ledger.Shelley.Rules qualified as Shelley

handleShelleyDELEGSEvent :: Shelley.ShelleyDelegsEvent ledgerera -> Maybe LedgerEvent
handleShelleyDELEGSEvent _ = Nothing
