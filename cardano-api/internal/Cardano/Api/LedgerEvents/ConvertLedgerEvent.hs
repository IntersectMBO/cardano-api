{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Cardano.Api.LedgerEvents.ConvertLedgerEvent
  ( LedgerEvent (..)
  , toLedgerEvent
  )
where

import Cardano.Api.LedgerEvents.LedgerEvent
import Cardano.Api.LedgerEvents.Rule.BBODY.DELEGS
import Cardano.Api.LedgerEvents.Rule.BBODY.LEDGER
import Cardano.Api.LedgerEvents.Rule.BBODY.UTXOW
import Cardano.Api.LedgerEvents.Rule.TICK.NEWEPOCH
import Cardano.Api.LedgerEvents.Rule.TICK.RUPD
import qualified Cardano.Ledger.Allegra.Rules as Allegra
import Cardano.Ledger.Alonzo.Rules (AlonzoBbodyEvent (..))
import Cardano.Ledger.Api.Era
  ( AllegraEra
  , AlonzoEra
  , BabbageEra
  , ConwayEra
  , MaryEra
  , ShelleyEra
  )
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Core as Ledger.Core
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Shelley.Rules
  ( RupdEvent (..)
  , ShelleyBbodyEvent (LedgersEvent)
  , ShelleyNewEpochEvent (..)
  , ShelleyTickEvent (TickNewEpochEvent, TickRupdEvent)
  , ShelleyUtxowEvent (..)
  )
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Control.State.Transition (Event)
import Data.SOP.Strict
import Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import Ouroboros.Consensus.Cardano.Block (HardForkBlock)
import qualified Ouroboros.Consensus.Cardano.Block as Consensus
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (getOneEraLedgerEvent)
import qualified Ouroboros.Consensus.Protocol.Praos as Consensus
import qualified Ouroboros.Consensus.Protocol.TPraos as Consensus
import Ouroboros.Consensus.Shelley.Ledger
  ( ShelleyBlock
  , ShelleyLedgerEvent (ShelleyLedgerEventBBODY, ShelleyLedgerEventTICK)
  )
import Ouroboros.Consensus.TypeFamilyWrappers (WrapLedgerEvent (unwrapLedgerEvent))

class ConvertLedgerEvent blk where
  toLedgerEvent :: WrapLedgerEvent blk -> Maybe LedgerEvent

instance ConvertLedgerEvent ByronBlock where
  toLedgerEvent _ = Nothing

instance ConvertLedgerEvent (ShelleyBlock protocol (ShelleyEra StandardCrypto)) where
  toLedgerEvent = toLedgerEventShelley

toLedgerEventShelley
  :: EraCrypto ledgerera ~ StandardCrypto
  => Event (Ledger.Core.EraRule "TICK" ledgerera) ~ ShelleyTickEvent ledgerera
  => Event (Ledger.Core.EraRule "NEWEPOCH" ledgerera) ~ ShelleyNewEpochEvent ledgerera
  => Event (Ledger.Core.EraRule "POOLREAP" ledgerera) ~ Shelley.ShelleyPoolreapEvent ledgerera
  => Event (Ledger.Core.EraRule "EPOCH" ledgerera) ~ Shelley.ShelleyEpochEvent ledgerera
  => Event (Ledger.Core.EraRule "RUPD" ledgerera) ~ RupdEvent StandardCrypto
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

instance ConvertLedgerEvent (ShelleyBlock protocol (MaryEra StandardCrypto)) where
  toLedgerEvent = toLedgerEventAllegraMary

instance ConvertLedgerEvent (ShelleyBlock protocol (AllegraEra StandardCrypto)) where
  toLedgerEvent = toLedgerEventAllegraMary

toLedgerEventAllegraMary
  :: EraCrypto ledgerera ~ StandardCrypto
  => Event (Ledger.Core.EraRule "TICK" ledgerera) ~ ShelleyTickEvent ledgerera
  => Event (Ledger.Core.EraRule "NEWEPOCH" ledgerera) ~ ShelleyNewEpochEvent ledgerera
  => Event (Ledger.Core.EraRule "POOLREAP" ledgerera) ~ Shelley.ShelleyPoolreapEvent ledgerera
  => Event (Ledger.Core.EraRule "EPOCH" ledgerera) ~ Shelley.ShelleyEpochEvent ledgerera
  => Event (Ledger.Core.EraRule "RUPD" ledgerera) ~ RupdEvent StandardCrypto
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

instance ConvertLedgerEvent (ShelleyBlock protocol (AlonzoEra StandardCrypto)) where
  toLedgerEvent = toAlonzoOrBabbageLedgerEvents

instance ConvertLedgerEvent (ShelleyBlock protocol (BabbageEra StandardCrypto)) where
  toLedgerEvent = toAlonzoOrBabbageLedgerEvents

toAlonzoOrBabbageLedgerEvents
  :: LatestTickEventConstraints ledgerera
  => LatestBBodyEventConstraints ledgerera
  => EraCrypto ledgerera ~ StandardCrypto
  => WrapLedgerEvent (ShelleyBlock protocol ledgerera) -> Maybe LedgerEvent
toAlonzoOrBabbageLedgerEvents e =
  case unwrapLedgerEvent e of
    ShelleyLedgerEventTICK tickEvent -> handleLedgerTICKEvents tickEvent
    ShelleyLedgerEventBBODY bbodyEvent -> handleAlonzoToBabbageLedgerBBODYEvents bbodyEvent

handleAlonzoToBabbageLedgerBBODYEvents
  :: LatestBBodyEventConstraints ledgerera
  => EraCrypto ledgerera ~ StandardCrypto
  => AlonzoBbodyEvent ledgerera -> Maybe LedgerEvent
handleAlonzoToBabbageLedgerBBODYEvents (ShelleyInAlonzoEvent (LedgersEvent (Shelley.LedgerEvent ledgerEvent))) =
  handleShelleyLEDGEREvents ledgerEvent

instance ConvertLedgerEvent (ShelleyBlock protocol (ConwayEra StandardCrypto)) where
  toLedgerEvent = toLedgerEventConway

-- LEDGER rule is defined anew in Conway

toLedgerEventConway
  :: WrapLedgerEvent (ShelleyBlock protocol (ConwayEra StandardCrypto))
  -> Maybe LedgerEvent
toLedgerEventConway evt =
  case unwrapLedgerEvent evt of
    ShelleyLedgerEventTICK (TickNewEpochEvent newEpochEvent) -> handleConwayNEWEPOCHEvents newEpochEvent
    ShelleyLedgerEventTICK (TickRupdEvent rewardUpdate) -> handleLedgerRUPDEvents rewardUpdate
    ShelleyLedgerEventBBODY
      (ShelleyInAlonzoEvent (LedgersEvent (Shelley.LedgerEvent conwayLedgerEvent))) ->
        case conwayLedgerEvent of
          Conway.UtxowEvent {} -> Nothing
          Conway.CertsEvent {} -> Nothing
          Conway.GovEvent govEvent ->
            case govEvent of
              Conway.GovNewProposals txid props ->
                Just $ NewGovernanceProposals txid (AnyProposals props)

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
  :: WrapLedgerEvent (ShelleyBlock (Consensus.TPraos StandardCrypto) (ShelleyEra StandardCrypto))
  -> NS WrapLedgerEvent (Consensus.CardanoEras StandardCrypto)
pattern ShelleyLedgerEvent x = S (Z x)

pattern AllegraLedgerEvent
  :: WrapLedgerEvent (ShelleyBlock (Consensus.TPraos StandardCrypto) (AllegraEra StandardCrypto))
  -> NS WrapLedgerEvent (Consensus.CardanoEras StandardCrypto)
pattern AllegraLedgerEvent x = S (S (Z x))

pattern MaryLedgerEvent
  :: WrapLedgerEvent (ShelleyBlock (Consensus.TPraos StandardCrypto) (MaryEra StandardCrypto))
  -> NS WrapLedgerEvent (Consensus.CardanoEras StandardCrypto)
pattern MaryLedgerEvent x = S (S (S (Z x)))

pattern AlonzoLedgerEvent
  :: WrapLedgerEvent (ShelleyBlock (Consensus.TPraos StandardCrypto) (AlonzoEra StandardCrypto))
  -> NS WrapLedgerEvent (Consensus.CardanoEras StandardCrypto)
pattern AlonzoLedgerEvent x = S (S (S (S (Z x))))

pattern BabbageLedgerEvent
  :: WrapLedgerEvent (ShelleyBlock (Consensus.Praos StandardCrypto) (BabbageEra StandardCrypto))
  -> NS WrapLedgerEvent (Consensus.CardanoEras StandardCrypto)
pattern BabbageLedgerEvent x = S (S (S (S (S (Z x)))))

pattern ConwayLedgerEvent
  :: WrapLedgerEvent (ShelleyBlock (Consensus.Praos StandardCrypto) (ConwayEra StandardCrypto))
  -> NS WrapLedgerEvent (Consensus.CardanoEras StandardCrypto)
pattern ConwayLedgerEvent x = S (S (S (S (S (S (Z x))))))
