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
  , caseShelleyEraOnlyOrAllegraEraOnwards
  , caseShelleyToAllegraOrMaryEraOnwards
  , caseShelleyToMaryOrAlonzoEraOnwards
  , caseShelleyToAlonzoOrBabbageEraOnwards
  , caseShelleyToBabbageOrConwayEraOnwards

    -- Case on MaryEraOnwards
  , caseMaryEraOnlyOrAlonzoEraOnwards

    -- Proofs
  , noByronEraInShelleyBasedEra
  , disjointByronEraOnlyAndShelleyBasedEra

    -- Conversions
  , shelleyToAllegraEraToByronToAllegraEra
  , shelleyToAlonzoEraToShelleyToBabbageEra
  , alonzoEraOnwardsToMaryEraOnwards
  , babbageEraOnwardsToMaryEraOnwards
  , babbageEraOnwardsToAlonzoEraOnwards
  ) where

import           Cardano.Api.Eon.AllegraEraOnwards
import           Cardano.Api.Eon.AlonzoEraOnwards
import           Cardano.Api.Eon.BabbageEraOnwards
import           Cardano.Api.Eon.ByronEraOnly
import           Cardano.Api.Eon.ByronToAllegraEra
import           Cardano.Api.Eon.ByronToAlonzoEra
import           Cardano.Api.Eon.ByronToMaryEra
import           Cardano.Api.Eon.ConwayEraOnwards
import           Cardano.Api.Eon.MaryEraOnly
import           Cardano.Api.Eon.MaryEraOnwards
import           Cardano.Api.Eon.ShelleyBasedEra
import           Cardano.Api.Eon.ShelleyEraOnly
import           Cardano.Api.Eon.ShelleyToAllegraEra
import           Cardano.Api.Eon.ShelleyToAlonzoEra
import           Cardano.Api.Eon.ShelleyToBabbageEra
import           Cardano.Api.Eon.ShelleyToMaryEra
import           Cardano.Api.Eras.Core

-- | @caseByronOrShelleyBasedEra f g era@ applies @f@ to byron and @g@ to other eras.
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

-- | @caseByronToAllegraOrMaryEraOnwards f g era@ applies @f@ to byron, shelley, and allegra;
-- and @g@ to mary and later eras.
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

-- | @caseByronToMaryOrAlonzoEraOnwards f g era@ applies @f@ to byron, shelley, allegra, and mary;
-- and @g@ to alonzo and later eras.
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

-- | @caseByronToAlonzoOrBabbageEraOnwards f g era@ applies @f@ to byron, shelley, allegra, mary, and alonzo;
-- and @g@ to babbage and later eras.
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

-- | @caseShelleyEraOnlyOrAllegraEraOnwards f g era@ applies @f@ to shelley;
-- and applies @g@ to allegra and later eras.
caseShelleyEraOnlyOrAllegraEraOnwards :: ()
  => (ShelleyEraOnlyConstraints era => ShelleyEraOnly era -> a)
  -> (AllegraEraOnwardsConstraints era => AllegraEraOnwards era -> a)
  -> ShelleyBasedEra era
  -> a
caseShelleyEraOnlyOrAllegraEraOnwards l r = \case
  ShelleyBasedEraShelley  -> l ShelleyEraOnlyShelley
  ShelleyBasedEraAllegra  -> r AllegraEraOnwardsAllegra
  ShelleyBasedEraMary     -> r AllegraEraOnwardsMary
  ShelleyBasedEraAlonzo   -> r AllegraEraOnwardsAlonzo
  ShelleyBasedEraBabbage  -> r AllegraEraOnwardsBabbage
  ShelleyBasedEraConway   -> r AllegraEraOnwardsConway

-- | @caseShelleyToAllegraOrMaryEraOnwards f g era@ applies @f@ to shelley and allegra;
-- and applies @g@ to mary and later eras.
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

-- | @caseShelleyToMaryOrAlonzoEraOnwards f g era@ applies @f@ to shelley, allegra, and mary;
-- and applies @g@ to alonzo and later eras.
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

-- | @caseShelleyToAlonzoOrBabbageEraOnwards f g era@ applies @f@ to shelley, allegra, mary, and alonzo;
-- and applies @g@ to babbage and later eras.
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

-- | @caseShelleyToBabbageOrConwayEraOnwards f g era@ applies @f@ to eras before conway;
-- and applies @g@ to conway and later eras.
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

-- | @caseMaryEraOnlyOrAlonzoEraOnwards f g era@ applies @f@ to mary;
-- and applies @g@ to alonzo and later eras.
caseMaryEraOnlyOrAlonzoEraOnwards :: ()
  => (MaryEraOnly era -> a)
  -> (AlonzoEraOnwards era -> a)
  -> MaryEraOnwards era
  -> a
caseMaryEraOnlyOrAlonzoEraOnwards l r = \case
  MaryEraOnwardsMary    -> l MaryEraOnlyMary
  MaryEraOnwardsAlonzo  -> r AlonzoEraOnwardsAlonzo
  MaryEraOnwardsBabbage -> r AlonzoEraOnwardsBabbage
  MaryEraOnwardsConway  -> r AlonzoEraOnwardsConway

{-# DEPRECATED noByronEraInShelleyBasedEra "Use disjointByronEraOnlyAndShelleyBasedEra instead" #-}
noByronEraInShelleyBasedEra :: ShelleyBasedEra era -> ByronEraOnly era -> a
noByronEraInShelleyBasedEra = flip disjointByronEraOnlyAndShelleyBasedEra

disjointByronEraOnlyAndShelleyBasedEra :: ByronEraOnly era -> ShelleyBasedEra era -> a
disjointByronEraOnlyAndShelleyBasedEra ByronEraOnlyByron sbe = case sbe of {}

shelleyToAllegraEraToByronToAllegraEra :: ShelleyToAllegraEra era -> ByronToAllegraEra era
shelleyToAllegraEraToByronToAllegraEra = \case
  ShelleyToAllegraEraShelley -> ByronToAllegraEraShelley
  ShelleyToAllegraEraAllegra -> ByronToAllegraEraAllegra

shelleyToAlonzoEraToShelleyToBabbageEra :: ()
  => ShelleyToAlonzoEra era
  -> ShelleyToBabbageEra era
shelleyToAlonzoEraToShelleyToBabbageEra = \case
  ShelleyToAlonzoEraShelley -> ShelleyToBabbageEraShelley
  ShelleyToAlonzoEraAllegra -> ShelleyToBabbageEraAllegra
  ShelleyToAlonzoEraMary -> ShelleyToBabbageEraMary
  ShelleyToAlonzoEraAlonzo -> ShelleyToBabbageEraAlonzo

alonzoEraOnwardsToMaryEraOnwards :: ()
  => AlonzoEraOnwards era
  -> MaryEraOnwards era
alonzoEraOnwardsToMaryEraOnwards = \case
  AlonzoEraOnwardsAlonzo  -> MaryEraOnwardsAlonzo
  AlonzoEraOnwardsBabbage -> MaryEraOnwardsBabbage
  AlonzoEraOnwardsConway  -> MaryEraOnwardsConway

babbageEraOnwardsToMaryEraOnwards :: ()
  => BabbageEraOnwards era
  -> MaryEraOnwards era
babbageEraOnwardsToMaryEraOnwards = \case
  BabbageEraOnwardsBabbage -> MaryEraOnwardsBabbage
  BabbageEraOnwardsConway  -> MaryEraOnwardsConway

babbageEraOnwardsToAlonzoEraOnwards :: ()
  => BabbageEraOnwards era
  -> AlonzoEraOnwards era
babbageEraOnwardsToAlonzoEraOnwards = \case
  BabbageEraOnwardsBabbage  -> AlonzoEraOnwardsBabbage
  BabbageEraOnwardsConway   -> AlonzoEraOnwardsConway
