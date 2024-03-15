{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | This module serves purpose as a single source of abstractions used in handling 'MonadError' and
'ExceptT' through 'cardano-api'.
-}

module Cardano.Api.Monad.Error
  ( MonadTransError
  , MonadIOTransError
  , liftExceptT
  , modifyError
  , handleIOExceptionsWith
  , handleIOExceptionsLiftWith
  , hoistIOEither

  , module Control.Monad.Except
  , module Control.Monad.IO.Class
  , module Control.Monad.Trans.Class
  , module Control.Monad.Trans.Except
  , module Control.Monad.Trans.Except.Extra
  ) where

import           Control.Exception.Safe
import           Control.Monad.Except (ExceptT (..), MonadError (..), catchError, liftEither,
                   mapExcept, mapExceptT, runExcept, runExceptT, withExcept)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Except.Extra
import           Data.Bifunctor (first)

-- | Convenience alias
type MonadTransError e t m = (Monad m, MonadTrans t, MonadError e (t m))
--
-- | Same as 'MonadTransError', but with also 'MonadIO' constraint
type MonadIOTransError e t m = (MonadIO m, MonadIO (t m), MonadCatch m, MonadTrans t, MonadError e (t m))

-- | Modify an 'ExceptT' error and lift it to 'MonadError' transformer stack.
--
-- This implementation avoids nesting problem of @modifyError@ from 'mtl'. The issue with @modifyError@ (from
-- 'mtl') is that when you use it on a function, you actually end up with @ExceptT e1 (ExceptT e2 m a)@:
--
-- > modifyError (f :: e2 -> e1) (foo :: ExceptT e2 (ExceptT e1 m) a) :: ExceptT e1 m a
--
-- and if you use @modifyError@ ('mtl') again, the more nested you get e.g.
-- @ExceptT e1 (ExceptT e2 (ExceptT e3 m a))@. With a deeper monad stack you pay the overhead with every
-- use of '>>='.
--
-- This function avoids that, but at the cost of limiting its application to transformers.
modifyError
  :: MonadTransError e' t m
  => (e -> e') -- ^ mapping function
  -> ExceptT e m a -- ^ value
  -> t m a -- ^ result with modified error
modifyError f m = lift (runExceptT m) >>= either (throwError . f) pure

-- | Wrap an exception and lift it into 'MonadError'.
handleIOExceptionsWith
  :: MonadError e' m
  => MonadCatch m
  => Exception e
  => (e -> e') -- ^ mapping function
  -> m a -- ^ action that can throw
  -> m a -- ^ result with caught exception
handleIOExceptionsWith f act = liftEither . first f =<< try act

-- | Wrap an exception and lift it into 'MonadError' stack.
handleIOExceptionsLiftWith
  :: MonadIOTransError e' t m
  => Exception e
  => (e -> e') -- ^ mapping function
  -> m a -- ^ action that can throw
  -> t m a -- ^ action with caught error lifted into 'MonadError' stack
handleIOExceptionsLiftWith f act = liftEither =<< lift (first f <$> try act)

-- | Lift 'ExceptT' into 'MonadTransError'
liftExceptT :: MonadTransError e t m
            => ExceptT e m a
            -> t m a
liftExceptT = modifyError id


-- | Lift an 'IO' action that returns 'Either' into 'MonadIOTransError'
hoistIOEither :: MonadIOTransError e t m
              => IO (Either e a)
              -> t m a
hoistIOEither = liftExceptT . ExceptT . liftIO
