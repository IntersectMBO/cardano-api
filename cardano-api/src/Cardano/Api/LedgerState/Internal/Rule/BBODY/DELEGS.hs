{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.LedgerState.Internal.Rule.BBODY.DELEGS
  ( handleShelleyDELEGSEvent
  )
where

import Cardano.Api.LedgerState.Internal.LedgerEvent

import Cardano.Ledger.Shelley.Rules qualified as Shelley

handleShelleyDELEGSEvent :: Shelley.ShelleyDelegsEvent ledgerera -> Maybe LedgerEvent
handleShelleyDELEGSEvent _ = Nothing
