{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Rpc.Server.Internal.UtxoRpc.Submit
  ( submitTxMethod
  )
where

import Cardano.Api
import Cardano.Api.Network.IPC qualified as Net.Tx
import Cardano.Rpc.Proto.Api.UtxoRpc.Submit qualified as UtxoRpc
import Cardano.Rpc.Server.Internal.Error
import Cardano.Rpc.Server.Internal.Monad
import Cardano.Rpc.Server.Internal.Orphans ()
import Cardano.Rpc.Server.Internal.Tracing

import RIO hiding (toList)

import Data.Default
import GHC.Stack
import Network.GRPC.Spec

-- | Submit a CBOR-serialised list of transactions to the node
submitTxMethod
  :: MonadRpc e m
  => Proto UtxoRpc.SubmitTxRequest
  -> m (Proto UtxoRpc.SubmitTxResponse)
submitTxMethod req = do
  nodeConnInfo <- grab
  AnyCardanoEra era <- liftIO . throwExceptT $ determineEra nodeConnInfo
  eon <- forEraInEon era (error "Minimum Shelley era required") pure

  tx <-
    putTraceThrowEither
      . first TraceRpcSubmitTxDecodingError
      . deserialiseTx eon
      $ req ^. #tx . #raw

  txId' <- submitTx eon tx

  pure $ def & #ref .~ serialiseToRawBytes txId'
 where
  deserialiseTx :: ShelleyBasedEra era -> ByteString -> Either DecoderError (Tx era)
  deserialiseTx sbe = shelleyBasedEraConstraints sbe $ deserialiseFromCBOR asType

  submitTx
    :: MonadRpc e m
    => ShelleyBasedEra era
    -> Tx era
    -> m TxId
  submitTx sbe tx = do
    nodeConnInfo <- grab
    putTraceThrowEither . join . first TraceRpcSubmitN2cConnectionError
      =<< tryAny
        ( submitTxToNodeLocal nodeConnInfo (TxInMode sbe tx) >>= \case
            Net.Tx.SubmitFail reason -> pure . Left $ TraceRpcSubmitTxValidationError reason
            Net.Tx.SubmitSuccess -> pure . Right $ getTxId $ getTxBody tx
        )

  putTraceThrowEither v = withFrozenCallStack $ do
    -- See Cardano.Node.Tracing.Tracers.Rpc in cardano-node for details how this is logged
    either putTrace (const $ pure ()) v
    throwEither v
