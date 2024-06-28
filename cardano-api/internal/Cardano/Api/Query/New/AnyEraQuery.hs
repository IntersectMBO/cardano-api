{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}

{-|
This module is intended to be a replacement for the existing Query
related types in cardano-api.
|-}

module Cardano.Api.Query.New.AnyEraQuery where

import           Cardano.Api.IPC.Monad
import           Cardano.Api.Query
import           Cardano.Api.Query.New.ShelleyBased

import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)

import           Control.Monad.Trans.Except (ExceptT)




-- We introduce this type because currently the result type of
-- `LocalStateQueryExpr` can contain an error. We want to separate
-- the errors out of the result type `a` and into the error type `e`.
-- This will come about by a redefining of the query related data types.
type LocalStateQueryExprWithError e block point query r m a
  = ExceptT e (LocalStateQueryExpr block point query r m) a


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
