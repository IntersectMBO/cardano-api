{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Api.Eras.Case
  ( -- Case on CardanoEra
    caseByronOrShelleyBasedEra

    -- Case on ShelleyBasedEra
  , caseShelleyToMaryOrAlonzoEraOnwards
  , caseShelleyToAlonzoOrBabbageEraOnwards
  , caseShelleyToBabbageOrConwayEraOnwards
  ) where

import           Cardano.Api.Eras.Constraints
import           Cardano.Api.Eras.Core
import           Cardano.Api.Feature.AlonzoEraOnwards
import           Cardano.Api.Feature.BabbageEraOnwards
import           Cardano.Api.Feature.ConwayEraOnwards
import           Cardano.Api.Feature.ShelleyToAlonzoEra
import           Cardano.Api.Feature.ShelleyToBabbageEra
import           Cardano.Api.Feature.ShelleyToMaryEra

caseByronOrShelleyBasedEra :: ()
  => (CardanoEra ByronEra -> a)
  -> (ShelleyBasedEraConstraints era => ShelleyBasedEra era -> a)
  -> CardanoEra era
  -> a
caseByronOrShelleyBasedEra l r = \case
  ByronEra   -> l ByronEra
  ShelleyEra -> r ShelleyBasedEraShelley
  AllegraEra -> r ShelleyBasedEraAllegra
  MaryEra    -> r ShelleyBasedEraMary
  AlonzoEra  -> r ShelleyBasedEraAlonzo
  BabbageEra -> r ShelleyBasedEraBabbage
  ConwayEra  -> r ShelleyBasedEraConway

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
