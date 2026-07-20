{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Api.Era.Internal.Case
  ( -- Case on CardanoEra
    caseByronOrShelleyBasedEra
  -- Case on ShelleyBasedEra
  , caseShelleyEraOnlyOrAllegraEraOnwards
  , caseShelleyToBabbageOrConwayEraOnwards
  )
where

import Cardano.Api.Era.Internal.Core
import Cardano.Api.Era.Internal.Eon.AllegraEraOnwards
import Cardano.Api.Era.Internal.Eon.ConwayEraOnwards
import Cardano.Api.Era.Internal.Eon.ShelleyBasedEra
import Cardano.Api.Era.Internal.Eon.ShelleyEraOnly
import Cardano.Api.Era.Internal.Eon.ShelleyToBabbageEra

-- | @caseByronOrShelleyBasedEra f g era@ returns @f@ in Byron and applies @g@ to Shelley-based eras.
caseByronOrShelleyBasedEra
  :: ()
  => a
  -> (ShelleyBasedEraConstraints era => ShelleyBasedEra era -> a)
  -> CardanoEra era
  -> a
caseByronOrShelleyBasedEra l r = \case
  ByronEra -> l -- We no longer provide the witness because Byron is isolated.
  -- This function will be deleted shortly after build-raw --byron-era is
  -- deprecated in cardano-cli
  ShelleyEra -> r ShelleyBasedEraShelley
  AllegraEra -> r ShelleyBasedEraAllegra
  MaryEra -> r ShelleyBasedEraMary
  AlonzoEra -> r ShelleyBasedEraAlonzo
  BabbageEra -> r ShelleyBasedEraBabbage
  ConwayEra -> r ShelleyBasedEraConway
  DijkstraEra -> error "TODO Dijkstra: caseByronOrShelleyBasedEra: era not supported"

-- | @caseShelleyEraOnlyOrAllegraEraOnwards f g era@ applies @f@ to shelley;
-- and applies @g@ to allegra and later eras.
caseShelleyEraOnlyOrAllegraEraOnwards
  :: ()
  => (ShelleyEraOnlyConstraints era => ShelleyEraOnly era -> a)
  -> (AllegraEraOnwardsConstraints era => AllegraEraOnwards era -> a)
  -> ShelleyBasedEra era
  -> a
caseShelleyEraOnlyOrAllegraEraOnwards l r = \case
  ShelleyBasedEraShelley -> l ShelleyEraOnlyShelley
  ShelleyBasedEraAllegra -> r AllegraEraOnwardsAllegra
  ShelleyBasedEraMary -> r AllegraEraOnwardsMary
  ShelleyBasedEraAlonzo -> r AllegraEraOnwardsAlonzo
  ShelleyBasedEraBabbage -> r AllegraEraOnwardsBabbage
  ShelleyBasedEraConway -> r AllegraEraOnwardsConway
  ShelleyBasedEraDijkstra -> error "TODO Dijkstra: caseShelleyEraOnlyOrAllegraEraOnwards: era not supported"

-- | @caseShelleyToBabbageOrConwayEraOnwards f g era@ applies @f@ to eras before conway;
-- and applies @g@ to conway and later eras.
caseShelleyToBabbageOrConwayEraOnwards
  :: ()
  => (ShelleyToBabbageEraConstraints era => ShelleyToBabbageEra era -> a)
  -> (ConwayEraOnwardsConstraints era => ConwayEraOnwards era -> a)
  -> ShelleyBasedEra era
  -> a
caseShelleyToBabbageOrConwayEraOnwards l r = \case
  ShelleyBasedEraShelley -> l ShelleyToBabbageEraShelley
  ShelleyBasedEraAllegra -> l ShelleyToBabbageEraAllegra
  ShelleyBasedEraMary -> l ShelleyToBabbageEraMary
  ShelleyBasedEraAlonzo -> l ShelleyToBabbageEraAlonzo
  ShelleyBasedEraBabbage -> l ShelleyToBabbageEraBabbage
  ShelleyBasedEraConway -> r ConwayEraOnwardsConway
  ShelleyBasedEraDijkstra -> error "TODO Dijkstra: caseShelleyToBabbageOrConwayEraOnwards: era not supported"
