{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Cardano.Api.Eras.Constraints
  ( cardanoEraConstraints

  , CardanoEraConstraints
  ) where

import           Cardano.Api.Eras.Core

import           Data.Typeable (Typeable)

type CardanoEraConstraints era =
  ( Typeable era
  , IsCardanoEra era
  )

cardanoEraConstraints :: ()
  => CardanoEra era
  -> (CardanoEraConstraints era => a)
  -> a
cardanoEraConstraints = \case
  ByronEra   -> id
  ShelleyEra -> id
  AllegraEra -> id
  MaryEra    -> id
  AlonzoEra  -> id
  BabbageEra -> id
  ConwayEra  -> id
