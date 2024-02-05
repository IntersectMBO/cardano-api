{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Api.LedgerEvents.Rule.BBODY.UTXOW
  ( handleAlonzoOnwardsUTxOWEvent
  , handleAllegraMaryUTxOWEvent
  , handlePreAlonzoUTxOWEvent
  ) where

import           Cardano.Api.LedgerEvents.LedgerEvent

import qualified Cardano.Ledger.Allegra.Rules as Allegra
import           Cardano.Ledger.Alonzo.Rules (AlonzoUtxoEvent (..), AlonzoUtxosEvent (..),
                   AlonzoUtxowEvent (..))
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo
import qualified Cardano.Ledger.Core as Ledger.Core
import qualified Cardano.Ledger.Shelley.Rules as Shelley

import           Control.State.Transition.Extended



handleAlonzoOnwardsUTxOWEvent
  :: Event (Ledger.Core.EraRule "UTXO" ledgerera) ~ AlonzoUtxoEvent ledgerera
  => Event (Ledger.Core.EraRule "UTXOS" ledgerera) ~ AlonzoUtxosEvent ledgerera
  => AlonzoUtxowEvent ledgerera -> Maybe LedgerEvent
handleAlonzoOnwardsUTxOWEvent (WrappedShelleyEraEvent (Shelley.UtxoEvent (UtxosEvent utxoEvent))) =
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
handlePreAlonzoUTxOWEvent (Shelley.UtxoEvent e)=
  case e of
    Shelley.TotalDeposits{} -> Nothing
    Shelley.UpdateEvent (Shelley.NewEpoch _) -> Nothing
    Shelley.TxUTxODiff _ _ -> Nothing


handleAllegraMaryUTxOWEvent
  :: Event (Ledger.Core.EraRule "UTXO" ledgerera) ~ Allegra.AllegraUtxoEvent ledgerera
  => Event (Ledger.Core.EraRule "PPUP" ledgerera) ~ Shelley.PpupEvent ledgerera
  => Shelley.ShelleyUtxowEvent ledgerera -> Maybe LedgerEvent
handleAllegraMaryUTxOWEvent (Shelley.UtxoEvent e)=
  case e of
    Allegra.TotalDeposits{} -> Nothing
    Allegra.UpdateEvent (Shelley.NewEpoch _) -> Nothing
    Allegra.TxUTxODiff _ _ -> Nothing
