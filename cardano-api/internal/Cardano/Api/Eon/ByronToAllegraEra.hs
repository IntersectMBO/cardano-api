{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.Eon.ByronToAllegraEra
  ( ByronToAllegraEra(..)
  , IsByronToAllegraEra(..)
  , AnyByronToAllegraEra(..)
  , byronToAllegraEraConstraints
  , byronToAllegraEraToCardanoEra

  , ByronToAllegraEraConstraints
  ) where

import           Cardano.Api.Eras.Core

import           Data.Typeable (Typeable)

class IsByronToAllegraEra era where
  byronToAllegraEra :: ByronToAllegraEra era

data ByronToAllegraEra era where
  ByronToAllegraEraByron    :: ByronToAllegraEra ByronEra
  ByronToAllegraEraShelley  :: ByronToAllegraEra ShelleyEra
  ByronToAllegraEraAllegra  :: ByronToAllegraEra AllegraEra

deriving instance Show (ByronToAllegraEra era)
deriving instance Eq (ByronToAllegraEra era)

instance IsByronToAllegraEra ByronEra where
  byronToAllegraEra = ByronToAllegraEraByron

instance IsByronToAllegraEra ShelleyEra where
  byronToAllegraEra = ByronToAllegraEraShelley

instance IsByronToAllegraEra AllegraEra where
  byronToAllegraEra = ByronToAllegraEraAllegra

instance Eon ByronToAllegraEra where
  inEonForEra no yes = \case
    ByronEra    -> yes ByronToAllegraEraByron
    ShelleyEra  -> yes ByronToAllegraEraShelley
    AllegraEra  -> yes ByronToAllegraEraAllegra
    MaryEra     -> no
    AlonzoEra   -> no
    BabbageEra  -> no
    ConwayEra   -> no

instance ToCardanoEra ByronToAllegraEra where
  toCardanoEra = \case
    ByronToAllegraEraByron    -> ByronEra
    ByronToAllegraEraShelley  -> ShelleyEra
    ByronToAllegraEraAllegra  -> AllegraEra

type ByronToAllegraEraConstraints era =
  ( IsCardanoEra era
  , IsByronToAllegraEra era
  , Typeable era
  )

data AnyByronToAllegraEra where
  AnyByronToAllegraEra :: ByronToAllegraEra era -> AnyByronToAllegraEra

deriving instance Show AnyByronToAllegraEra

byronToAllegraEraConstraints :: ()
  => ByronToAllegraEra era
  -> (ByronToAllegraEraConstraints era => a)
  -> a
byronToAllegraEraConstraints = \case
  ByronToAllegraEraByron    -> id
  ByronToAllegraEraShelley  -> id
  ByronToAllegraEraAllegra  -> id

byronToAllegraEraToCardanoEra :: ByronToAllegraEra era -> CardanoEra era
byronToAllegraEraToCardanoEra = \case
  ByronToAllegraEraByron    -> ByronEra
  ByronToAllegraEraShelley  -> ShelleyEra
  ByronToAllegraEraAllegra  -> AllegraEra