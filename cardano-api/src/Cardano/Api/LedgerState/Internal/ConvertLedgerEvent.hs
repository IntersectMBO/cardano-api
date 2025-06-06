{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Cardano.Api.LedgerState.Internal.ConvertLedgerEvent
  ( LedgerEvent (..)
  , toLedgerEvent
  )
where

import Cardano.Api.LedgerState.Internal.LedgerEvent
import Cardano.Api.LedgerState.Internal.Rule.BBODY.DELEGS
import Cardano.Api.LedgerState.Internal.Rule.BBODY.LEDGER
import Cardano.Api.LedgerState.Internal.Rule.BBODY.UTXOW
import Cardano.Api.LedgerState.Internal.Rule.TICK.NEWEPOCH
import Cardano.Api.LedgerState.Internal.Rule.TICK.RUPD

import Cardano.Ledger.Allegra.Rules qualified as Allegra
import Cardano.Ledger.Alonzo.Rules (AlonzoBbodyEvent (..))
import Cardano.Ledger.Api.Era
  ( AllegraEra
  , AlonzoEra
  , BabbageEra
  , ConwayEra
  , MaryEra
  , ShelleyEra
  )
import Cardano.Ledger.Conway.Rules qualified as Conway
import Cardano.Ledger.Core
import Cardano.Ledger.Core qualified as Ledger.Core
import Cardano.Ledger.Shelley.Rules
  ( RupdEvent (..)
  , ShelleyBbodyEvent (LedgersEvent)
  , ShelleyNewEpochEvent (..)
  , ShelleyTickEvent (TickNewEpochEvent, TickRupdEvent)
  , ShelleyUtxowEvent (..)
  )
import Cardano.Ledger.Shelley.Rules qualified as Shelley
import Cardano.Protocol.Crypto (StandardCrypto)
import Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import Ouroboros.Consensus.Cardano.Block (HardForkBlock)
import Ouroboros.Consensus.Cardano.Block qualified as Consensus
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (getOneEraLedgerEvent)
import Ouroboros.Consensus.Protocol.Praos qualified as Consensus
import Ouroboros.Consensus.Protocol.TPraos qualified as Consensus
import Ouroboros.Consensus.Shelley.Ledger
  ( ShelleyBlock
  , ShelleyLedgerEvent (ShelleyLedgerEventBBODY, ShelleyLedgerEventTICK)
  )
import Ouroboros.Consensus.TypeFamilyWrappers (WrapLedgerEvent (unwrapLedgerEvent))

import Control.State.Transition (Event)
import Data.SOP.Strict

class ConvertLedgerEvent blk where
  toLedgerEvent :: WrapLedgerEvent blk -> Maybe LedgerEvent

instance ConvertLedgerEvent ByronBlock where
  toLedgerEvent _ = Nothing

instance ConvertLedgerEvent (ShelleyBlock protocol ShelleyEra) where
  toLedgerEvent = toLedgerEventShelley

toLedgerEventShelley
  :: Event (Ledger.Core.EraRule "TICK" ledgerera) ~ ShelleyTickEvent ledgerera
  => Event (Ledger.Core.EraRule "NEWEPOCH" ledgerera) ~ ShelleyNewEpochEvent ledgerera
  => Event (Ledger.Core.EraRule "POOLREAP" ledgerera) ~ Shelley.ShelleyPoolreapEvent ledgerera
  => Event (Ledger.Core.EraRule "EPOCH" ledgerera) ~ Shelley.ShelleyEpochEvent ledgerera
  => Event (Ledger.Core.EraRule "RUPD" ledgerera) ~ RupdEvent
  => Event (Ledger.Core.EraRule "BBODY" ledgerera) ~ ShelleyBbodyEvent ledgerera
  => Event (Ledger.Core.EraRule "LEDGERS" ledgerera) ~ Shelley.ShelleyLedgersEvent ledgerera
  => Event (Ledger.Core.EraRule "LEDGER" ledgerera) ~ Shelley.ShelleyLedgerEvent ledgerera
  => Event (Ledger.Core.EraRule "UTXOW" ledgerera) ~ ShelleyUtxowEvent ledgerera
  => Event (Ledger.Core.EraRule "UTXO" ledgerera) ~ Shelley.UtxoEvent ledgerera
  => Event (Ledger.Core.EraRule "PPUP" ledgerera) ~ Shelley.PpupEvent ledgerera
  => Event (EraRule "DELEGS" ledgerera) ~ Shelley.ShelleyDelegsEvent ledgerera
  => WrapLedgerEvent (ShelleyBlock protocol ledgerera) -> Maybe LedgerEvent
toLedgerEventShelley evt = case unwrapLedgerEvent evt of
  ShelleyLedgerEventTICK e -> handleLedgerTICKEvents e
  ShelleyLedgerEventBBODY e -> handleShelleyLedgerBBODYEvents e

handleShelleyLedgerBBODYEvents
  :: Event (Ledger.Core.EraRule "LEDGERS" ledgerera) ~ Shelley.ShelleyLedgersEvent ledgerera
  => Event (Ledger.Core.EraRule "LEDGER" ledgerera) ~ Shelley.ShelleyLedgerEvent ledgerera
  => Event (Ledger.Core.EraRule "UTXOW" ledgerera) ~ ShelleyUtxowEvent ledgerera
  => Event (Ledger.Core.EraRule "UTXO" ledgerera) ~ Shelley.UtxoEvent ledgerera
  => Event (Ledger.Core.EraRule "PPUP" ledgerera) ~ Shelley.PpupEvent ledgerera
  => Event (EraRule "DELEGS" ledgerera) ~ Shelley.ShelleyDelegsEvent ledgerera
  => ShelleyBbodyEvent ledgerera -> Maybe LedgerEvent
handleShelleyLedgerBBODYEvents (LedgersEvent (Shelley.LedgerEvent e)) =
  case e of
    Shelley.UtxowEvent ev -> handlePreAlonzoUTxOWEvent ev
    Shelley.DelegsEvent ev -> handleShelleyDELEGSEvent ev

instance ConvertLedgerEvent (ShelleyBlock protocol MaryEra) where
  toLedgerEvent = toLedgerEventAllegraMary

instance ConvertLedgerEvent (ShelleyBlock protocol AllegraEra) where
  toLedgerEvent = toLedgerEventAllegraMary

toLedgerEventAllegraMary
  :: Event (Ledger.Core.EraRule "TICK" ledgerera) ~ ShelleyTickEvent ledgerera
  => Event (Ledger.Core.EraRule "NEWEPOCH" ledgerera) ~ ShelleyNewEpochEvent ledgerera
  => Event (Ledger.Core.EraRule "POOLREAP" ledgerera) ~ Shelley.ShelleyPoolreapEvent ledgerera
  => Event (Ledger.Core.EraRule "EPOCH" ledgerera) ~ Shelley.ShelleyEpochEvent ledgerera
  => Event (Ledger.Core.EraRule "RUPD" ledgerera) ~ RupdEvent
  => Event (Ledger.Core.EraRule "BBODY" ledgerera) ~ ShelleyBbodyEvent ledgerera
  => Event (Ledger.Core.EraRule "LEDGERS" ledgerera) ~ Shelley.ShelleyLedgersEvent ledgerera
  => Event (Ledger.Core.EraRule "LEDGER" ledgerera) ~ Shelley.ShelleyLedgerEvent ledgerera
  => Event (Ledger.Core.EraRule "UTXOW" ledgerera) ~ ShelleyUtxowEvent ledgerera
  => Event (Ledger.Core.EraRule "UTXO" ledgerera) ~ Allegra.AllegraUtxoEvent ledgerera
  => Event (Ledger.Core.EraRule "PPUP" ledgerera) ~ Shelley.PpupEvent ledgerera
  => Event (Ledger.Core.EraRule "DELEGS" ledgerera) ~ Shelley.ShelleyDelegsEvent ledgerera
  => WrapLedgerEvent (ShelleyBlock protocol ledgerera)
  -> Maybe LedgerEvent
toLedgerEventAllegraMary evt = case unwrapLedgerEvent evt of
  ShelleyLedgerEventTICK e -> handleLedgerTICKEvents e
  ShelleyLedgerEventBBODY e -> handleAllegraMaryLedgerBBODYEvents e

handleAllegraMaryLedgerBBODYEvents
  :: Event (Ledger.Core.EraRule "LEDGERS" ledgerera) ~ Shelley.ShelleyLedgersEvent ledgerera
  => Event (Ledger.Core.EraRule "LEDGER" ledgerera) ~ Shelley.ShelleyLedgerEvent ledgerera
  => Event (Ledger.Core.EraRule "UTXOW" ledgerera) ~ ShelleyUtxowEvent ledgerera
  => Event (Ledger.Core.EraRule "UTXO" ledgerera) ~ Allegra.AllegraUtxoEvent ledgerera
  => Event (Ledger.Core.EraRule "PPUP" ledgerera) ~ Shelley.PpupEvent ledgerera
  => Event (Ledger.Core.EraRule "DELEGS" ledgerera) ~ Shelley.ShelleyDelegsEvent ledgerera
  => ShelleyBbodyEvent ledgerera -> Maybe LedgerEvent
handleAllegraMaryLedgerBBODYEvents (LedgersEvent (Shelley.LedgerEvent e)) =
  case e of
    Shelley.UtxowEvent ev -> handleAllegraMaryUTxOWEvent ev
    Shelley.DelegsEvent ev -> handleShelleyDELEGSEvent ev

instance ConvertLedgerEvent (ShelleyBlock protocol AlonzoEra) where
  toLedgerEvent = toAlonzoOrBabbageLedgerEvents

instance ConvertLedgerEvent (ShelleyBlock protocol BabbageEra) where
  toLedgerEvent = toAlonzoOrBabbageLedgerEvents

toAlonzoOrBabbageLedgerEvents
  :: LatestTickEventConstraints ledgerera
  => LatestBBodyEventConstraints ledgerera
  => WrapLedgerEvent (ShelleyBlock protocol ledgerera) -> Maybe LedgerEvent
toAlonzoOrBabbageLedgerEvents e =
  case unwrapLedgerEvent e of
    ShelleyLedgerEventTICK tickEvent -> handleLedgerTICKEvents tickEvent
    ShelleyLedgerEventBBODY bbodyEvent -> handleAlonzoToBabbageLedgerBBODYEvents bbodyEvent

handleAlonzoToBabbageLedgerBBODYEvents
  :: LatestBBodyEventConstraints ledgerera
  => AlonzoBbodyEvent ledgerera -> Maybe LedgerEvent
handleAlonzoToBabbageLedgerBBODYEvents (ShelleyInAlonzoEvent (LedgersEvent (Shelley.LedgerEvent ledgerEvent))) =
  handleShelleyLEDGEREvents ledgerEvent

instance ConvertLedgerEvent (ShelleyBlock protocol ConwayEra) where
  toLedgerEvent = toLedgerEventConway

-- LEDGER rule is defined anew in Conway

toLedgerEventConway
  :: WrapLedgerEvent (ShelleyBlock protocol ConwayEra)
  -> Maybe LedgerEvent
toLedgerEventConway evt =
  case unwrapLedgerEvent evt of
    ShelleyLedgerEventTICK (TickNewEpochEvent newEpochEvent) -> handleConwayNEWEPOCHEvents newEpochEvent
    ShelleyLedgerEventTICK (TickRupdEvent rewardUpdate) -> handleLedgerRUPDEvents rewardUpdate
    ShelleyLedgerEventBBODY
      (ShelleyInAlonzoEvent (LedgersEvent (Shelley.LedgerEvent conwayLedgerEvent))) ->
        case conwayLedgerEvent of
          Conway.UtxowEvent utxowEvent -> handleConwayUTxOWEvent utxowEvent
          Conway.CertsEvent{} -> Nothing
          Conway.GovEvent govEvent ->
            case govEvent of
              Conway.GovNewProposals txid props ->
                Just $ NewGovernanceProposals txid (AnyProposals props)
              Conway.GovRemovedVotes txid replacedVotes unregisteredDReps ->
                Just $ RemovedGovernanceVotes txid replacedVotes unregisteredDReps

instance ConvertLedgerEvent (HardForkBlock (Consensus.CardanoEras StandardCrypto)) where
  toLedgerEvent wrappedLedgerEvent =
    case getOneEraLedgerEvent $ unwrapLedgerEvent wrappedLedgerEvent of
      ShelleyLedgerEvent ledgerEvent -> toLedgerEvent ledgerEvent
      AllegraLedgerEvent ledgerEvent -> toLedgerEvent ledgerEvent
      MaryLedgerEvent ledgerEvent -> toLedgerEvent ledgerEvent
      AlonzoLedgerEvent ledgerEvent -> toLedgerEvent ledgerEvent
      BabbageLedgerEvent ledgerEvent -> toLedgerEvent ledgerEvent
      ConwayLedgerEvent ledgerEvent -> toLedgerEvent ledgerEvent

{-# COMPLETE
  ShelleyLedgerEvent
  , AllegraLedgerEvent
  , MaryLedgerEvent
  , AlonzoLedgerEvent
  , BabbageLedgerEvent
  , ConwayLedgerEvent
  #-}

pattern ShelleyLedgerEvent
  :: WrapLedgerEvent (ShelleyBlock (Consensus.TPraos StandardCrypto) ShelleyEra)
  -> NS WrapLedgerEvent (Consensus.CardanoEras StandardCrypto)
pattern ShelleyLedgerEvent x = S (Z x)

pattern AllegraLedgerEvent
  :: WrapLedgerEvent (ShelleyBlock (Consensus.TPraos StandardCrypto) AllegraEra)
  -> NS WrapLedgerEvent (Consensus.CardanoEras StandardCrypto)
pattern AllegraLedgerEvent x = S (S (Z x))

pattern MaryLedgerEvent
  :: WrapLedgerEvent (ShelleyBlock (Consensus.TPraos StandardCrypto) MaryEra)
  -> NS WrapLedgerEvent (Consensus.CardanoEras StandardCrypto)
pattern MaryLedgerEvent x = S (S (S (Z x)))

pattern AlonzoLedgerEvent
  :: WrapLedgerEvent (ShelleyBlock (Consensus.TPraos StandardCrypto) AlonzoEra)
  -> NS WrapLedgerEvent (Consensus.CardanoEras StandardCrypto)
pattern AlonzoLedgerEvent x = S (S (S (S (Z x))))

pattern BabbageLedgerEvent
  :: WrapLedgerEvent (ShelleyBlock (Consensus.Praos StandardCrypto) BabbageEra)
  -> NS WrapLedgerEvent (Consensus.CardanoEras StandardCrypto)
pattern BabbageLedgerEvent x = S (S (S (S (S (Z x)))))

pattern ConwayLedgerEvent
  :: WrapLedgerEvent (ShelleyBlock (Consensus.Praos StandardCrypto) ConwayEra)
  -> NS WrapLedgerEvent (Consensus.CardanoEras StandardCrypto)
pattern ConwayLedgerEvent x = S (S (S (S (S (S (Z x))))))
