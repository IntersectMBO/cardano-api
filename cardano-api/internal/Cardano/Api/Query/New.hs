module Cardano.Api.Query.New where

import           Cardano.Api.IPC.Monad

{-
This module is intended to be a replacement for the existing Query
related types in cardano-api.
-}



-- We introduce this type because currently the result type of
-- `LocalStateQueryExpr` can contain an error. We want to separate
-- the errors out of the result type `a` and into the error type `e`.
-- This will come about by a redefining of the query related data types.
type LocalStateQueryExprWithError e block point query r m a
  = ExceptT e (LocalStateQueryExpr block point query r m) a
