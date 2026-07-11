{-# LANGUAGE RankNTypes #-}

module Cardano.Rpc.Server.Internal.UtxoRpc.Type.BigInt
  ( utxoRpcBigIntToInteger
  )
where

import Cardano.Api.Error
import Cardano.Api.HasTypeProxy
import Cardano.Api.Serialise.Raw
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as U5c
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as UtxoRpc

import RIO

import Network.GRPC.Spec

utxoRpcBigIntToInteger
  :: forall m
   . HasCallStack
  => MonadThrow m
  => Proto UtxoRpc.BigInt
  -> m Integer
utxoRpcBigIntToInteger bigInt
  | Just int <- bigInt ^. U5c.maybe'int = pure $ fromIntegral int
  | Just bytes <- bigInt ^. U5c.maybe'bigNInt = do
      n <- fmap fromIntegral . liftEitherError $ deserialiseFromRawBytes AsNatural bytes
      pure $ -n - 1
  | Just bytes <- bigInt ^. U5c.maybe'bigUInt =
      fmap fromIntegral . liftEitherError $ deserialiseFromRawBytes AsNatural bytes
  | otherwise = pure 0 -- assume default value
