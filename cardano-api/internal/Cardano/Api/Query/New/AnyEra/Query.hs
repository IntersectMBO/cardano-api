{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}

{-|
This module is intended to be a replacement for the existing Query
related types in cardano-api.
|-}

module Cardano.Api.Query.New.AnyEra.Query where

import           Cardano.Api.IPC.Monad
import           Cardano.Api.Query
import           Cardano.Api.Query.New.ShelleyBased.Query

import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)

import           Control.Monad.Trans.Except (ExceptT)


-- 'QueryInShelleyBasedEra' queries return 'Either EraMismatch'.
-- This means we have an additional failure in the query of the result, that does
-- not exist in other `QueryInMode` queries. Because of this we make this distinction
-- so that we are not forced to handle errors that cannot occur.
data AnyEraQuery result where
  AnyEraQuery :: QueryInMode result -> AnyEraQuery result
  AnyEraQueryShelleyBasedEra :: QueryShelleyBasedEra era result -> AnyEraQuery result

pattern AnyQuerySbe
  :: QueryInShelleyBasedEra era result
  -> AnyEraQuery (Either EraMismatch result)
pattern AnyQuerySbe q = AnyEraQueryShelleyBasedEra (QueryShelleyBasedEra q)

{-# COMPLETE AnyEraQuery, AnyQuerySbe #-}
