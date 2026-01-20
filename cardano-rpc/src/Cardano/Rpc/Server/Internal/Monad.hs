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
{-# LANGUAGE TypeOperators #-}

module Cardano.Rpc.Server.Internal.Monad
  ( Has (..)
  , MonadRpc
  , grab
  , putTrace
  , wrapInSpan
  )
where

import Cardano.Api
import Cardano.Rpc.Server.Internal.Env
import Cardano.Rpc.Server.Internal.Tracing

import RIO

import Control.Tracer (Tracer, traceWith)
import System.Random.Stateful (globalStdGen, uniformM)

-- | Provides a value of type 'field' from the value 'env'
-- Used in conjunction with 'MonadReader env m' allows to easily access fields from the environment.
class Has field env where
  obtain :: env -> field

instance Has a a where
  obtain = id

instance Has LocalNodeConnectInfo RpcEnv where
  obtain RpcEnv{rpcLocalNodeConnectInfo} = rpcLocalNodeConnectInfo

instance MonadIO m => Has (Tracer m TraceRpc) RpcEnv where
  obtain RpcEnv{tracer} = tracer

-- | Obtain the field from the environment
grab
  :: forall field env m
   . (Has field env, MonadReader env m)
  => m field
grab = asks $ obtain @field
{-# INLINE grab #-}

-- | Using tracer from the environment, print the trace
putTrace
  :: forall t' t e m
   . t ~ TraceRpc
  => Inject t' t
  => Has (Tracer m t) e
  => MonadReader e m
  => t'
  -- ^ the traced value
  -> m ()
putTrace t' = grab @(Tracer m t) >>= (`traceWith` inject t')
{-# INLINE putTrace #-}

-- | Wrap the action in span begin and end events
wrapInSpan
  :: forall t' t e m a
   . t ~ TraceRpc
  => Inject t' t
  => NFData a
  => Has (Tracer m t) e
  => MonadReader e m
  => MonadUnliftIO m
  => (TraceSpanEvent -> t')
  -- ^ Trace constructor accepting 'TraceSpanEvent'
  -> m a
  -- ^ action to be wrapped in begin and end events
  -> m a
wrapInSpan spanConstructor act = do
  spanId <- newSpanId
  putTrace $ spanConstructor (SpanBegin spanId)
  (act >>= (evaluate . force)) `finally` putTrace (spanConstructor $ SpanEnd spanId)
 where
  -- generate random span id
  newSpanId :: m SpanId
  newSpanId = UsingRawBytesHex <$> uniformM globalStdGen
{-# INLINE wrapInSpan #-}

type MonadRpc e m =
  ( Has (Tracer m TraceRpc) e
  , Has LocalNodeConnectInfo e
  , HasCallStack
  , MonadReader e m
  , MonadUnliftIO m
  )
