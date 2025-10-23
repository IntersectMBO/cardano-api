{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.Era.Internal.Eon.ByronToAlonzoEra
  ( ByronToAlonzoEra (..)
  , byronToAlonzoEraConstraints
  , ByronToAlonzoEraConstraints
  )
where

import Cardano.Api.Era.Internal.Core
import Cardano.Api.Era.Internal.Eon.Convert

import Data.Typeable (Typeable)

data ByronToAlonzoEra era where
  ByronToAlonzoEraByron :: ByronToAlonzoEra ByronEra
  ByronToAlonzoEraShelley :: ByronToAlonzoEra ShelleyEra
  ByronToAlonzoEraAllegra :: ByronToAlonzoEra AllegraEra
  ByronToAlonzoEraMary :: ByronToAlonzoEra MaryEra
  ByronToAlonzoEraAlonzo :: ByronToAlonzoEra AlonzoEra

deriving instance Show (ByronToAlonzoEra era)

deriving instance Eq (ByronToAlonzoEra era)

instance Eon ByronToAlonzoEra where
  inEonForEra no yes = \case
    ByronEra -> yes ByronToAlonzoEraByron
    ShelleyEra -> yes ByronToAlonzoEraShelley
    AllegraEra -> yes ByronToAlonzoEraAllegra
    MaryEra -> yes ByronToAlonzoEraMary
    AlonzoEra -> yes ByronToAlonzoEraAlonzo
    BabbageEra -> no
    ConwayEra -> no
    DijkstraEra -> no

instance ToCardanoEra ByronToAlonzoEra where
  toCardanoEra = \case
    ByronToAlonzoEraByron -> ByronEra
    ByronToAlonzoEraShelley -> ShelleyEra
    ByronToAlonzoEraAllegra -> AllegraEra
    ByronToAlonzoEraMary -> MaryEra
    ByronToAlonzoEraAlonzo -> AlonzoEra

instance Convert ByronToAlonzoEra CardanoEra where
  convert = toCardanoEra

type ByronToAlonzoEraConstraints era =
  ( IsCardanoEra era
  , Typeable era
  )

byronToAlonzoEraConstraints
  :: ()
  => ByronToAlonzoEra era
  -> (ByronToAlonzoEraConstraints era => a)
  -> a
byronToAlonzoEraConstraints = \case
  ByronToAlonzoEraByron -> id
  ByronToAlonzoEraShelley -> id
  ByronToAlonzoEraAllegra -> id
  ByronToAlonzoEraMary -> id
  ByronToAlonzoEraAlonzo -> id
