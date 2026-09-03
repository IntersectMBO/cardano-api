{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Rpc.Server.Internal.Error
  ( throwEither
  , throwExceptT
  , throwGrpcErrorWithMessage
  , RpcException (..)
  , renderRpcExceptionForClient
  )
where

import Cardano.Api

import RIO

import GHC.Stack
import Network.GRPC.Spec (GrpcError, GrpcException (..))

throwEither :: (Error e, HasCallStack, MonadIO m, Show e, Typeable e) => Either e a -> m a
throwEither = withFrozenCallStack $ either (throwIO . RpcException) pure

throwExceptT :: (Error e, HasCallStack, MonadIO m, Show e, Typeable e) => ExceptT e m a -> m a
throwExceptT = withFrozenCallStack $ throwEither <=< runExceptT

data RpcException where
  RpcException :: (Error err, HasCallStack, Show err, Typeable err) => err -> RpcException

deriving instance Show RpcException

instance Exception RpcException where
  displayException (RpcException e) =
    unlines
      [ show (prettyError e)
      , prettyCallStack callStack
      ]

-- | Render an 'RpcException' for an RPC client, without the call stack.
-- The call stack is internal detail (module names, source lines) and must stay server-side only.
renderRpcExceptionForClient :: RpcException -> Text
renderRpcExceptionForClient (RpcException e) = tshow (prettyError e)

-- | Throw a 'GrpcException' with the given error code and message.
-- grapesy converts this to proper gRPC trailers before it reaches 'serverTopLevel'.
throwGrpcErrorWithMessage :: MonadIO m => GrpcError -> Text -> m a
throwGrpcErrorWithMessage err message =
  throwIO
    GrpcException
      { grpcError = err
      , grpcErrorMessage = Just message
      , grpcErrorDetails = Nothing
      , grpcErrorMetadata = []
      }
