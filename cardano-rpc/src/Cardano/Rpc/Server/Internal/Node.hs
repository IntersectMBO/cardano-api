{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Rpc.Server.Internal.Node
  ( getEraMethod
  , getProtocolParamsJsonMethod
  )
where

import Cardano.Api
import Cardano.Api.Experimental.Era
import Cardano.Rpc.Proto.Api.Node qualified as Rpc
import Cardano.Rpc.Server.Internal.Error
import Cardano.Rpc.Server.Internal.Monad
import Cardano.Rpc.Server.Internal.Orphans ()

import RIO hiding (toList)

import Data.Aeson qualified as A
import Data.ByteString.Lazy qualified as BL
import Data.Default
import Data.ProtoLens (defMessage)
import Network.GRPC.Spec

import Proto.Google.Protobuf.Empty

getEraMethod :: MonadRpc e m => Proto Empty -> m (Proto Rpc.CurrentEra)
getEraMethod _ = pure . Proto $ defMessage & #era .~ Rpc.Conway

getProtocolParamsJsonMethod :: MonadRpc e m => Proto Empty -> m (Proto Rpc.ProtocolParamsJson)
getProtocolParamsJsonMethod _ = do
  nodeConnInfo <- grab
  AnyCardanoEra era <- liftIO . throwExceptT $ determineEra nodeConnInfo
  eon <- forEraInEon @Era era (error "getProtocolParamsJsonMethod: Minimum Conway era required") pure
  let sbe = convert eon

  let target = VolatileTip
  pparams <-
    liftIO . (throwEither =<<) $
      executeLocalStateQueryExpr nodeConnInfo target $
        throwEither =<< throwEither =<< queryProtocolParameters sbe

  let pparamsJson = obtainCommonConstraints eon $ A.encode pparams

  pure $
    def
      & #json .~ BL.toStrict pparamsJson
