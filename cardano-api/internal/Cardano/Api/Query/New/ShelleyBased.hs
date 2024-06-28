{-# LANGUAGE GADTs #-}

module Cardano.Api.Query.New.ShelleyBased where

import           Cardano.Api.IPC

import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)


data QueryShelleyBasedEra era result where
  QueryShelleyBasedEra
    :: QueryInShelleyBasedEra era result
    -> QueryShelleyBasedEra era (Either EraMismatch result)
