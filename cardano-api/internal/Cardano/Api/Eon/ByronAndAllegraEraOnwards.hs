{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.Eon.ByronAndAllegraEraOnwards
  ( ByronAndAllegraEraOnwards(..)
  , byronAndAllegraEraOnwardsConstraints
  , byronAndAllegraEraOnwardsToCardanoEra

  , ByronAndAllegraEraOnwardsConstraints
  ) where

import           Cardano.Api.Eras.Core

import           Data.Typeable (Typeable)

data ByronAndAllegraEraOnwards era where
  ByronAndAllegraEraOnwardsByron   :: ByronAndAllegraEraOnwards ByronEra
  ByronAndAllegraEraOnwardsAllegra :: ByronAndAllegraEraOnwards AllegraEra
  ByronAndAllegraEraOnwardsMary    :: ByronAndAllegraEraOnwards MaryEra
  ByronAndAllegraEraOnwardsAlonzo  :: ByronAndAllegraEraOnwards AlonzoEra
  ByronAndAllegraEraOnwardsBabbage :: ByronAndAllegraEraOnwards BabbageEra
  ByronAndAllegraEraOnwardsConway  :: ByronAndAllegraEraOnwards ConwayEra

deriving instance Show (ByronAndAllegraEraOnwards era)
deriving instance Eq (ByronAndAllegraEraOnwards era)

instance Eon ByronAndAllegraEraOnwards where
  inEonForEra no yes = \case
    ByronEra    -> yes ByronAndAllegraEraOnwardsByron
    ShelleyEra  -> no
    AllegraEra  -> yes ByronAndAllegraEraOnwardsAllegra
    MaryEra     -> yes ByronAndAllegraEraOnwardsMary
    AlonzoEra   -> yes ByronAndAllegraEraOnwardsAlonzo
    BabbageEra  -> yes ByronAndAllegraEraOnwardsBabbage
    ConwayEra   -> yes ByronAndAllegraEraOnwardsConway

instance ToCardanoEra ByronAndAllegraEraOnwards where
  toCardanoEra = \case
    ByronAndAllegraEraOnwardsByron   -> ByronEra
    ByronAndAllegraEraOnwardsAllegra -> AllegraEra
    ByronAndAllegraEraOnwardsMary    -> MaryEra
    ByronAndAllegraEraOnwardsAlonzo  -> AlonzoEra
    ByronAndAllegraEraOnwardsBabbage -> BabbageEra
    ByronAndAllegraEraOnwardsConway  -> ConwayEra

type ByronAndAllegraEraOnwardsConstraints era =
  ( IsCardanoEra era
  , Typeable era
  )

byronAndAllegraEraOnwardsConstraints :: ()
  => ByronAndAllegraEraOnwards era
  -> (ByronAndAllegraEraOnwardsConstraints era => a)
  -> a
byronAndAllegraEraOnwardsConstraints = \case
  ByronAndAllegraEraOnwardsByron    -> id
  ByronAndAllegraEraOnwardsAllegra  -> id
  ByronAndAllegraEraOnwardsMary     -> id
  ByronAndAllegraEraOnwardsAlonzo   -> id
  ByronAndAllegraEraOnwardsBabbage  -> id
  ByronAndAllegraEraOnwardsConway   -> id

byronAndAllegraEraOnwardsToCardanoEra :: ByronAndAllegraEraOnwards era -> CardanoEra era
byronAndAllegraEraOnwardsToCardanoEra = \case
  ByronAndAllegraEraOnwardsByron    -> ByronEra
  ByronAndAllegraEraOnwardsAllegra  -> AllegraEra
  ByronAndAllegraEraOnwardsMary     -> MaryEra
  ByronAndAllegraEraOnwardsAlonzo   -> AlonzoEra
  ByronAndAllegraEraOnwardsBabbage  -> BabbageEra
  ByronAndAllegraEraOnwardsConway   -> ConwayEra
