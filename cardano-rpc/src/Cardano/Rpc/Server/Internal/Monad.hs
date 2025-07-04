{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Rpc.Server.Internal.Monad
  ( Has (..)
  , MonadRpc
  , grab
  , putTrace
  )
where

import Cardano.Api
import Cardano.Rpc.Server.Internal.Env

import RIO

import Control.Tracer (Tracer, traceWith)

-- | Provides a value of type 'field' from the value 'env'
-- Used in conjunction with 'MonadReader env m' allows to easily access fields from the environment.
class Has field env where
  obtain :: env -> field

instance Has a a where
  obtain = id

instance Has LocalNodeConnectInfo RpcEnv where
  obtain RpcEnv{rpcLocalNodeConnectInfo} = rpcLocalNodeConnectInfo

instance MonadIO m => Has (Tracer m String) RpcEnv where
  obtain RpcEnv{tracer} = tracer

-- | Obtain the field from the environment
grab
  :: forall field env m
   . (Has field env, MonadReader env m)
  => m field
grab = asks $ obtain @field
{-# INLINE grab #-}

-- | Using tracer from the environment, print the trace
putTrace :: (Has (Tracer m t) e, MonadReader e m) => t -> m ()
putTrace t = grab >>= (`traceWith` t)

type MonadRpc e m =
  ( Has (Tracer m String) e
  , Has LocalNodeConnectInfo e
  , HasCallStack
  , MonadReader e m
  , MonadUnliftIO m
  )
