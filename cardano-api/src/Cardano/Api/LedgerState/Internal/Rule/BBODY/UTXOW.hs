{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Api.LedgerState.Internal.Rule.BBODY.UTXOW
  ( handleAlonzoUTxOWEvent
  , handleAllegraMaryUTxOWEvent
  , handleConwayUTxOWEvent
  , handlePreAlonzoUTxOWEvent
  )
where

import Cardano.Api.LedgerState.Internal.LedgerEvent

import Cardano.Ledger.Allegra.Rules qualified as Allegra
import Cardano.Ledger.Alonzo.Rules
  ( AlonzoUtxoEvent (..)
  , AlonzoUtxosEvent (..)
  , AlonzoUtxowEvent (..)
  )
import Cardano.Ledger.Alonzo.Rules qualified as Alonzo
import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.Rules qualified as Conway
import Cardano.Ledger.Core qualified as Ledger.Core
import Cardano.Ledger.Shelley.Rules qualified as Shelley

import Control.State.Transition.Extended

handleConwayUTxOWEvent
  :: AlonzoUtxowEvent ConwayEra -> Maybe LedgerEvent
handleConwayUTxOWEvent = \case
  (Alonzo.WrappedShelleyEraEvent (Shelley.UtxoEvent TotalDeposits{})) -> Nothing
  (Alonzo.WrappedShelleyEraEvent (Shelley.UtxoEvent TxUTxODiff{})) -> Nothing
  (Alonzo.WrappedShelleyEraEvent (Shelley.UtxoEvent (Alonzo.UtxosEvent conwayUTxOsEvent))) ->
    case conwayUTxOsEvent of
      Conway.SuccessfulPlutusScriptsEvent e -> Just $ SuccessfulPlutusScript e
      Conway.FailedPlutusScriptsEvent e -> Just $ FailedPlutusScript e

handleAlonzoUTxOWEvent
  :: Event (Ledger.Core.EraRule "UTXO" ledgerera) ~ AlonzoUtxoEvent ledgerera
  => Event (Ledger.Core.EraRule "UTXOS" ledgerera) ~ AlonzoUtxosEvent ledgerera
  => AlonzoUtxowEvent ledgerera -> Maybe LedgerEvent
handleAlonzoUTxOWEvent = \case
  (Alonzo.WrappedShelleyEraEvent (Shelley.UtxoEvent TotalDeposits{})) -> Nothing
  (Alonzo.WrappedShelleyEraEvent (Shelley.UtxoEvent TxUTxODiff{})) -> Nothing
  (WrappedShelleyEraEvent (Shelley.UtxoEvent (UtxosEvent utxoEvent))) ->
    case utxoEvent of
      Alonzo.AlonzoPpupToUtxosEvent{} -> Nothing
      Alonzo.SuccessfulPlutusScriptsEvent e -> Just $ SuccessfulPlutusScript e
      Alonzo.FailedPlutusScriptsEvent e -> Just $ FailedPlutusScript e

handlePreAlonzoUTxOWEvent
  :: Event (Ledger.Core.EraRule "UTXO" ledgerera) ~ Shelley.UtxoEvent ledgerera
  => Event (Ledger.Core.EraRule "PPUP" ledgerera) ~ Shelley.PpupEvent ledgerera
  => Shelley.ShelleyUtxowEvent ledgerera -> Maybe LedgerEvent
handlePreAlonzoUTxOWEvent (Shelley.UtxoEvent e) =
  case e of
    Shelley.TotalDeposits{} -> Nothing
    Shelley.UpdateEvent (Shelley.PpupNewEpoch _) -> Nothing
    Shelley.TxUTxODiff _ _ -> Nothing

handleAllegraMaryUTxOWEvent
  :: Event (Ledger.Core.EraRule "UTXO" ledgerera) ~ Allegra.AllegraUtxoEvent ledgerera
  => Event (Ledger.Core.EraRule "PPUP" ledgerera) ~ Shelley.PpupEvent ledgerera
  => Shelley.ShelleyUtxowEvent ledgerera -> Maybe LedgerEvent
handleAllegraMaryUTxOWEvent (Shelley.UtxoEvent e) =
  case e of
    Allegra.TotalDeposits{} -> Nothing
    Allegra.UpdateEvent (Shelley.PpupNewEpoch _) -> Nothing
    Allegra.TxUTxODiff _ _ -> Nothing
