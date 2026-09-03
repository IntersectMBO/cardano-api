{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Api.Era.Internal.Case
  ( caseShelleyToBabbageOrConwayEraOnwards
  )
where

import Cardano.Api.Era.Internal.Eon.ConwayEraOnwards
import Cardano.Api.Era.Internal.Eon.ShelleyBasedEra
import Cardano.Api.Era.Internal.Eon.ShelleyToBabbageEra

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
  ShelleyBasedEraDijkstra -> r ConwayEraOnwardsDijkstra
