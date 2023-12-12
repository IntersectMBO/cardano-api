{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Api.LedgerEvents.Rule.BBODY.LEDGER
  ( LatestBBodyEventConstraints
  , handleShelleyLEDGEREvents
  ) where

import           Cardano.Api.LedgerEvents.LedgerEvent
import           Cardano.Api.LedgerEvents.Rule.BBODY.DELEGS
import           Cardano.Api.LedgerEvents.Rule.BBODY.UTXOW

import           Cardano.Ledger.Alonzo.Rules (AlonzoUtxoEvent (..), AlonzoUtxosEvent (..),
                   AlonzoUtxowEvent (..))
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo
import qualified Cardano.Ledger.Core as Ledger.Core
import qualified Cardano.Ledger.Shelley.Rules as Shelley

import           Control.State.Transition.Extended

type LatestBBodyEventConstraints ledgerera =
 ( Event (Ledger.Core.EraRule "BBODY" ledgerera) ~ Alonzo.AlonzoBbodyEvent ledgerera
 , Event (Ledger.Core.EraRule "LEDGERS" ledgerera) ~ Shelley.ShelleyLedgersEvent ledgerera
 , Event (Ledger.Core.EraRule "LEDGER" ledgerera) ~ Shelley.ShelleyLedgerEvent ledgerera
 , Event (Ledger.Core.EraRule "DELEGS" ledgerera) ~ Shelley.ShelleyDelegsEvent ledgerera
 , Event (Ledger.Core.EraRule "UTXOW" ledgerera) ~ AlonzoUtxowEvent ledgerera
 , Event (Ledger.Core.EraRule "UTXO" ledgerera) ~ AlonzoUtxoEvent ledgerera
 , Event (Ledger.Core.EraRule "UTXOS" ledgerera) ~ AlonzoUtxosEvent ledgerera
 )

handleShelleyLEDGEREvents
  :: Event (Ledger.Core.EraRule "UTXOW" ledgerera) ~ AlonzoUtxowEvent ledgerera
  => Event (Ledger.Core.EraRule "UTXO" ledgerera) ~ AlonzoUtxoEvent ledgerera
  => Event (Ledger.Core.EraRule "UTXOS" ledgerera) ~ AlonzoUtxosEvent ledgerera
  => Event (Ledger.Core.EraRule "DELEGS" ledgerera) ~ Shelley.ShelleyDelegsEvent ledgerera
  => Shelley.ShelleyLedgerEvent ledgerera -> Maybe LedgerEvent
handleShelleyLEDGEREvents ledgerEvent =
  case ledgerEvent of
   Shelley.UtxowEvent e -> handleAlonzoOnwardsUTxOWEvent e
   Shelley.DelegsEvent e -> handleShelleyDELEGSEvent e
