{-# LANGUAGE GADTs #-}

module Cardano.Api.Query.New.ShelleyBased.Query where

import           Cardano.Api.IPC
import           Cardano.Api.Query.New.EraIndependent.Expr

import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)

data ShelleyBasedQueryError
  = ShelleyBasedQueryEraMismatch EraMismatch
  | ShelleyBasedEquerySimpleError IndependentEraQueryError
  deriving Show


data QueryShelleyBasedEra era result where
  QueryShelleyBasedEra
    :: QueryInShelleyBasedEra era result
    -> QueryShelleyBasedEra era (Either EraMismatch result)
