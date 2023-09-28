{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.Eon.ByronToAlonzoEra
  ( ByronToAlonzoEra(..)
  , IsByronToAlonzoEra(..)
  , AnyByronToAlonzoEra(..)
  , byronToAlonzoEraConstraints
  , byronToAlonzoEraToCardanoEra

  , ByronToAlonzoEraConstraints
  ) where

import           Cardano.Api.Eras.Core

import           Data.Typeable (Typeable)

class IsByronToAlonzoEra era where
  byronToAlonzoEra :: ByronToAlonzoEra era

data ByronToAlonzoEra era where
  ByronToAlonzoEraByron   :: ByronToAlonzoEra ByronEra
  ByronToAlonzoEraShelley :: ByronToAlonzoEra ShelleyEra
  ByronToAlonzoEraAllegra :: ByronToAlonzoEra AllegraEra
  ByronToAlonzoEraMary    :: ByronToAlonzoEra MaryEra
  ByronToAlonzoEraAlonzo  :: ByronToAlonzoEra AlonzoEra

deriving instance Show (ByronToAlonzoEra era)
deriving instance Eq (ByronToAlonzoEra era)

instance IsByronToAlonzoEra ByronEra where
  byronToAlonzoEra = ByronToAlonzoEraByron

instance IsByronToAlonzoEra ShelleyEra where
  byronToAlonzoEra = ByronToAlonzoEraShelley

instance IsByronToAlonzoEra AllegraEra where
  byronToAlonzoEra = ByronToAlonzoEraAllegra

instance IsByronToAlonzoEra MaryEra where
  byronToAlonzoEra = ByronToAlonzoEraMary

instance IsByronToAlonzoEra AlonzoEra where
  byronToAlonzoEra = ByronToAlonzoEraAlonzo

instance Eon ByronToAlonzoEra where
  inEonForEra no yes = \case
    ByronEra    -> yes ByronToAlonzoEraByron
    ShelleyEra  -> yes ByronToAlonzoEraShelley
    AllegraEra  -> yes ByronToAlonzoEraAllegra
    MaryEra     -> yes ByronToAlonzoEraMary
    AlonzoEra   -> yes ByronToAlonzoEraAlonzo
    BabbageEra  -> no
    ConwayEra   -> no

instance ToCardanoEra ByronToAlonzoEra where
  toCardanoEra = \case
    ByronToAlonzoEraByron   -> ByronEra
    ByronToAlonzoEraShelley -> ShelleyEra
    ByronToAlonzoEraAllegra -> AllegraEra
    ByronToAlonzoEraMary    -> MaryEra
    ByronToAlonzoEraAlonzo  -> AlonzoEra

type ByronToAlonzoEraConstraints era =
  ( IsCardanoEra era
  , IsByronToAlonzoEra era
  , Typeable era
  )

data AnyByronToAlonzoEra where
  AnyByronToAlonzoEra :: ByronToAlonzoEra era -> AnyByronToAlonzoEra

deriving instance Show AnyByronToAlonzoEra

byronToAlonzoEraConstraints :: ()
  => ByronToAlonzoEra era
  -> (ByronToAlonzoEraConstraints era => a)
  -> a
byronToAlonzoEraConstraints = \case
  ByronToAlonzoEraByron    -> id
  ByronToAlonzoEraShelley  -> id
  ByronToAlonzoEraAllegra  -> id
  ByronToAlonzoEraMary     -> id
  ByronToAlonzoEraAlonzo   -> id

byronToAlonzoEraToCardanoEra :: ByronToAlonzoEra era -> CardanoEra era
byronToAlonzoEraToCardanoEra = \case
  ByronToAlonzoEraByron    -> ByronEra
  ByronToAlonzoEraShelley  -> ShelleyEra
  ByronToAlonzoEraAllegra  -> AllegraEra
  ByronToAlonzoEraMary     -> MaryEra
  ByronToAlonzoEraAlonzo   -> AlonzoEra