{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Api.Internal.LedgerEvents.Rule.BBODY.UTXOW
  ( handleAlonzoUTxOWEvent
  , handleAllegraMaryUTxOWEvent
  , handleConwayUTxOWEvent
  , handlePreAlonzoUTxOWEvent
  )
where

import           Cardano.Api.Internal.LedgerEvents.LedgerEvent

import qualified Cardano.Ledger.Allegra.Rules as Allegra
import           Cardano.Ledger.Alonzo.Rules (AlonzoUtxoEvent (..), AlonzoUtxosEvent (..),
                   AlonzoUtxowEvent (..))
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo
import           Cardano.Ledger.Conway
import qualified Cardano.Ledger.Conway.Rules as Conway
import qualified Cardano.Ledger.Core as Ledger.Core
import qualified Cardano.Ledger.Crypto as Crypto
import qualified Cardano.Ledger.Shelley.Rules as Shelley

import           Control.State.Transition.Extended

handleConwayUTxOWEvent
  :: AlonzoUtxowEvent (ConwayEra Crypto.StandardCrypto) -> Maybe LedgerEvent
handleConwayUTxOWEvent (Alonzo.WrappedShelleyEraEvent (Shelley.UtxoEvent (Alonzo.UtxosEvent conwayUTxOsEvent))) =
  case conwayUTxOsEvent of
    Conway.TotalDeposits{} -> Nothing
    Conway.SuccessfulPlutusScriptsEvent e -> Just $ SuccessfulPlutusScript e
    Conway.FailedPlutusScriptsEvent e -> Just $ FailedPlutusScript e
    Conway.TxUTxODiff _ _ -> Nothing

handleAlonzoUTxOWEvent
  :: Event (Ledger.Core.EraRule "UTXO" ledgerera) ~ AlonzoUtxoEvent ledgerera
  => Event (Ledger.Core.EraRule "UTXOS" ledgerera) ~ AlonzoUtxosEvent ledgerera
  => Ledger.Core.EraCrypto ledgerera ~ Crypto.StandardCrypto
  => AlonzoUtxowEvent ledgerera -> Maybe LedgerEvent
handleAlonzoUTxOWEvent (WrappedShelleyEraEvent (Shelley.UtxoEvent (UtxosEvent utxoEvent))) =
  case utxoEvent of
    Alonzo.AlonzoPpupToUtxosEvent{} -> Nothing
    Alonzo.TotalDeposits{} -> Nothing
    Alonzo.SuccessfulPlutusScriptsEvent e -> Just $ SuccessfulPlutusScript e
    Alonzo.FailedPlutusScriptsEvent e -> Just $ FailedPlutusScript e
    Alonzo.TxUTxODiff _ _ -> Nothing

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
