{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Api.Eras.Case
  ( -- Case on CardanoEra
    caseByronOrShelleyBasedEra
  , caseByronToAllegraOrMaryEraOnwards
  , caseByronToMaryOrAlonzoEraOnwards
  , caseByronToAlonzoOrBabbageEraOnwards

    -- Case on ShelleyBasedEra
  , caseShelleyToAllegraOrMaryEraOnwards
  , caseShelleyToMaryOrAlonzoEraOnwards
  , caseShelleyToAlonzoOrBabbageEraOnwards
  , caseShelleyToBabbageOrConwayEraOnwards

  , noByronEraInShelleyBasedEra

    -- Conversions
  , shelleyToAllegraEraToByronToAllegraEra
  ) where

import           Cardano.Api.Eon.AlonzoEraOnwards
import           Cardano.Api.Eon.BabbageEraOnwards
import           Cardano.Api.Eon.ByronEraOnly
import           Cardano.Api.Eon.ByronToAllegraEra
import           Cardano.Api.Eon.ByronToAlonzoEra
import           Cardano.Api.Eon.ByronToMaryEra
import           Cardano.Api.Eon.ConwayEraOnwards
import           Cardano.Api.Eon.MaryEraOnwards
import           Cardano.Api.Eon.ShelleyBasedEra
import           Cardano.Api.Eon.ShelleyToAllegraEra
import           Cardano.Api.Eon.ShelleyToAlonzoEra
import           Cardano.Api.Eon.ShelleyToBabbageEra
import           Cardano.Api.Eon.ShelleyToMaryEra
import           Cardano.Api.Eras.Constraints
import           Cardano.Api.Eras.Core

caseByronOrShelleyBasedEra :: ()
  => (ByronEraOnly era -> a)
  -> (ShelleyBasedEraConstraints era => ShelleyBasedEra era -> a)
  -> CardanoEra era
  -> a
caseByronOrShelleyBasedEra l r = \case
  ByronEra   -> l ByronEraOnlyByron
  ShelleyEra -> r ShelleyBasedEraShelley
  AllegraEra -> r ShelleyBasedEraAllegra
  MaryEra    -> r ShelleyBasedEraMary
  AlonzoEra  -> r ShelleyBasedEraAlonzo
  BabbageEra -> r ShelleyBasedEraBabbage
  ConwayEra  -> r ShelleyBasedEraConway

caseByronToAllegraOrMaryEraOnwards :: ()
  => (ByronToAllegraEraConstraints era => ByronToAllegraEra era -> a)
  -> (MaryEraOnwardsConstraints era => MaryEraOnwards era -> a)
  -> CardanoEra era
  -> a
caseByronToAllegraOrMaryEraOnwards l r = \case
  ByronEra   -> l ByronToAllegraEraByron
  ShelleyEra -> l ByronToAllegraEraShelley
  AllegraEra -> l ByronToAllegraEraAllegra
  MaryEra    -> r MaryEraOnwardsMary
  AlonzoEra  -> r MaryEraOnwardsAlonzo
  BabbageEra -> r MaryEraOnwardsBabbage
  ConwayEra  -> r MaryEraOnwardsConway

caseByronToMaryOrAlonzoEraOnwards :: ()
  => (ByronToMaryEraConstraints era => ByronToMaryEra era -> a)
  -> (AlonzoEraOnwardsConstraints era => AlonzoEraOnwards era -> a)
  -> CardanoEra era
  -> a
caseByronToMaryOrAlonzoEraOnwards l r = \case
  ByronEra   -> l ByronToMaryEraByron
  ShelleyEra -> l ByronToMaryEraShelley
  AllegraEra -> l ByronToMaryEraAllegra
  MaryEra    -> l ByronToMaryEraMary
  AlonzoEra  -> r AlonzoEraOnwardsAlonzo
  BabbageEra -> r AlonzoEraOnwardsBabbage
  ConwayEra  -> r AlonzoEraOnwardsConway

caseByronToAlonzoOrBabbageEraOnwards :: ()
  => (ByronToAlonzoEraConstraints era => ByronToAlonzoEra era -> a)
  -> (BabbageEraOnwardsConstraints era => BabbageEraOnwards era -> a)
  -> CardanoEra era
  -> a
caseByronToAlonzoOrBabbageEraOnwards l r = \case
  ByronEra   -> l ByronToAlonzoEraByron
  ShelleyEra -> l ByronToAlonzoEraShelley
  AllegraEra -> l ByronToAlonzoEraAllegra
  MaryEra    -> l ByronToAlonzoEraMary
  AlonzoEra  -> l ByronToAlonzoEraAlonzo
  BabbageEra -> r BabbageEraOnwardsBabbage
  ConwayEra  -> r BabbageEraOnwardsConway

caseShelleyToAllegraOrMaryEraOnwards :: ()
  => (ShelleyToAllegraEraConstraints era => ShelleyToAllegraEra era -> a)
  -> (MaryEraOnwardsConstraints era => MaryEraOnwards era -> a)
  -> ShelleyBasedEra era
  -> a
caseShelleyToAllegraOrMaryEraOnwards l r = \case
  ShelleyBasedEraShelley  -> l ShelleyToAllegraEraShelley
  ShelleyBasedEraAllegra  -> l ShelleyToAllegraEraAllegra
  ShelleyBasedEraMary     -> r MaryEraOnwardsMary
  ShelleyBasedEraAlonzo   -> r MaryEraOnwardsAlonzo
  ShelleyBasedEraBabbage  -> r MaryEraOnwardsBabbage
  ShelleyBasedEraConway   -> r MaryEraOnwardsConway

caseShelleyToMaryOrAlonzoEraOnwards :: ()
  => (ShelleyToMaryEraConstraints era => ShelleyToMaryEra era -> a)
  -> (AlonzoEraOnwardsConstraints era => AlonzoEraOnwards era -> a)
  -> ShelleyBasedEra era
  -> a
caseShelleyToMaryOrAlonzoEraOnwards l r = \case
  ShelleyBasedEraShelley  -> l ShelleyToMaryEraShelley
  ShelleyBasedEraAllegra  -> l ShelleyToMaryEraAllegra
  ShelleyBasedEraMary     -> l ShelleyToMaryEraMary
  ShelleyBasedEraAlonzo   -> r AlonzoEraOnwardsAlonzo
  ShelleyBasedEraBabbage  -> r AlonzoEraOnwardsBabbage
  ShelleyBasedEraConway   -> r AlonzoEraOnwardsConway

caseShelleyToAlonzoOrBabbageEraOnwards :: ()
  => (ShelleyToAlonzoEraConstraints era => ShelleyToAlonzoEra era -> a)
  -> (BabbageEraOnwardsConstraints  era => BabbageEraOnwards era  -> a)
  -> ShelleyBasedEra era
  -> a
caseShelleyToAlonzoOrBabbageEraOnwards l r = \case
  ShelleyBasedEraShelley -> l ShelleyToAlonzoEraShelley
  ShelleyBasedEraAllegra -> l ShelleyToAlonzoEraAllegra
  ShelleyBasedEraMary    -> l ShelleyToAlonzoEraMary
  ShelleyBasedEraAlonzo  -> l ShelleyToAlonzoEraAlonzo
  ShelleyBasedEraBabbage -> r BabbageEraOnwardsBabbage
  ShelleyBasedEraConway  -> r BabbageEraOnwardsConway

caseShelleyToBabbageOrConwayEraOnwards :: ()
  => (ShelleyToBabbageEraConstraints  era => ShelleyToBabbageEra era  -> a)
  -> (ConwayEraOnwardsConstraints     era => ConwayEraOnwards era     -> a)
  -> ShelleyBasedEra era
  -> a
caseShelleyToBabbageOrConwayEraOnwards l r = \case
  ShelleyBasedEraShelley -> l ShelleyToBabbageEraShelley
  ShelleyBasedEraAllegra -> l ShelleyToBabbageEraAllegra
  ShelleyBasedEraMary    -> l ShelleyToBabbageEraMary
  ShelleyBasedEraAlonzo  -> l ShelleyToBabbageEraAlonzo
  ShelleyBasedEraBabbage -> l ShelleyToBabbageEraBabbage
  ShelleyBasedEraConway  -> r ConwayEraOnwardsConway

noByronEraInShelleyBasedEra :: ShelleyBasedEra era -> ByronEraOnly era -> a
noByronEraInShelleyBasedEra sbe ByronEraOnlyByron = case sbe of {}

shelleyToAllegraEraToByronToAllegraEra :: ShelleyToAllegraEra era -> ByronToAllegraEra era
shelleyToAllegraEraToByronToAllegraEra = \case
  ShelleyToAllegraEraShelley -> ByronToAllegraEraShelley
  ShelleyToAllegraEraAllegra -> ByronToAllegraEraAllegra
