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
  , byronToAllegraEraConstraints
  , byronToAllegraEraToCardanoEra

  , ByronToAllegraEraConstraints
  ) where

import           Cardano.Api.Eras.Core

import           Data.Typeable (Typeable)

data ByronToAllegraEra era where
  ByronToAllegraEraByron    :: ByronToAllegraEra ByronEra
  ByronToAllegraEraShelley  :: ByronToAllegraEra ShelleyEra
  ByronToAllegraEraAllegra  :: ByronToAllegraEra AllegraEra

deriving instance Show (ByronToAllegraEra era)
deriving instance Eq (ByronToAllegraEra era)

instance ToCardanoEra ByronToAllegraEra where
  toCardanoEra = \case
    ByronToAllegraEraByron    -> ByronEra
    ByronToAllegraEraShelley  -> ShelleyEra
    ByronToAllegraEraAllegra  -> AllegraEra

type ByronToAllegraEraConstraints era =
  ( IsCardanoEra era
  , Typeable era
  )

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
