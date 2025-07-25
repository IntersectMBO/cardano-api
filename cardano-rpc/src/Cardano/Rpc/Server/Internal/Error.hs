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
  , RpcException (..)
  )
where

import Cardano.Api

import RIO

import GHC.Stack

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
