{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Rpc.Server.Internal.UtxoRpc.Submit
  ( submitTxMethod
  )
where

import Cardano.Api
import Cardano.Rpc.Proto.Api.UtxoRpc.Submit qualified as U5c
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
      $ req ^. U5c.tx . U5c.raw

  txId' <- submitTx eon tx

  pure $ def & U5c.ref .~ serialiseToRawBytes txId'
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
    result <-
      submitTxToNodeLocal nodeConnInfo (TxInMode sbe tx) <&> \case
        TxSubmitError e -> Left $ TraceRpcSubmitN2cConnectionError e
        TxSubmitFail reason -> Left $ TraceRpcSubmitTxValidationError reason
        TxSubmitSuccess -> Right $ getTxId $ getTxBody tx
    putTraceThrowEither result

  putTraceThrowEither v = withFrozenCallStack $ do
    -- See Cardano.Node.Tracing.Tracers.Rpc in cardano-node for details how this is logged
    either putTrace (const $ pure ()) v
    throwEither v
