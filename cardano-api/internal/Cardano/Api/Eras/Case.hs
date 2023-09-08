{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.Api.Eras.Case
  ( caseShelleyToBabbageAndConwayEraOnwards
  ) where

import           Cardano.Api.Eras.Core
import           Cardano.Api.Feature.ConwayEraOnwards
import           Cardano.Api.Feature.ShelleyToBabbageEra

caseShelleyToBabbageAndConwayEraOnwards :: ()
  => (ShelleyToBabbageEra era -> a)
  -> (ConwayEraOnwards era -> a)
  -> ShelleyBasedEra era
  -> a
caseShelleyToBabbageAndConwayEraOnwards l r = \case
  ShelleyBasedEraShelley -> l ShelleyToBabbageEraShelley
  ShelleyBasedEraAllegra -> l ShelleyToBabbageEraAllegra
  ShelleyBasedEraMary    -> l ShelleyToBabbageEraMary
  ShelleyBasedEraAlonzo  -> l ShelleyToBabbageEraAlonzo
  ShelleyBasedEraBabbage -> l ShelleyToBabbageEraBabbage
  ShelleyBasedEraConway  -> r ConwayEraOnwardsConway
