{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Provides datatypes used in tracing
module Cardano.Rpc.Server.Internal.Tracing where

import Cardano.Api.Consensus (TxValidationErrorInCardanoMode)
import Cardano.Api.Era (Inject (..))
import Cardano.Api.Error
import Cardano.Api.Pretty
import Cardano.Api.Serialise.Cbor (DecoderError)
import Cardano.Api.Serialise.SerialiseUsing

import Control.Exception
import Data.Word (Word64)

-- | A sum type representing all possible traces
data TraceRpc
  = TraceRpcQuery TraceRpcQuery
  | TraceRpcSubmit TraceRpcSubmit
  | TraceRpcError SomeException
  | TraceRpcFatalError SomeException

-- | Traces used in Query service
data TraceRpcQuery
  = -- | Span trace marking ReadParams query
    TraceRpcQueryParamsSpan TraceSpanEvent
  | -- | Span trace marking ReadUtxos query
    TraceRpcQueryReadUtxosSpan TraceSpanEvent
  deriving Show

instance Pretty TraceRpc where
  pretty = \case
    TraceRpcQuery t -> pretty t
    TraceRpcSubmit t -> pretty t
    TraceRpcError e -> "Exception when processing RPC request:\n" <> prettyException e
    TraceRpcFatalError e -> "RPC server fatal error: " <> prettyException e

-- | Span type
data TraceSpanEvent
  = -- | Opening span trace
    SpanBegin SpanId
  | -- | Ending span trace
    SpanEnd SpanId
  deriving Show

-- | 8-byte span ID, serialised in hex.
type SpanId = UsingRawBytesHex Word64

instance Pretty TraceRpcQuery where
  pretty = \case
    TraceRpcQueryParamsSpan (SpanBegin _) -> "Started query params method"
    TraceRpcQueryParamsSpan (SpanEnd _) -> "Finished query params method"
    TraceRpcQueryReadUtxosSpan (SpanBegin _) -> "Started query read UTXO method"
    TraceRpcQueryReadUtxosSpan (SpanEnd _) -> "Finished query read UTXO method"

instance Error TraceRpcQuery where
  prettyError = pretty

-- | Traces used in SubmitTx service
data TraceRpcSubmit
  = -- | Node-to-client exception
    TraceRpcSubmitN2cConnectionError SomeException
  | -- | Transaction deserialisation error
    TraceRpcSubmitTxDecodingError DecoderError
  | -- | Transaction submission error
    TraceRpcSubmitTxValidationError TxValidationErrorInCardanoMode
  | -- | Transaction submission span
    TraceRpcSubmitSpan TraceSpanEvent
  deriving Show

instance Pretty TraceRpcSubmit where
  pretty = \case
    TraceRpcSubmitSpan (SpanBegin _) -> "Started submit method"
    TraceRpcSubmitSpan (SpanEnd _) -> "Finished submit method"
    TraceRpcSubmitN2cConnectionError e -> "N2C connection error while trying to submit a transaction: " <> prettyException e
    TraceRpcSubmitTxDecodingError e -> "Failed to decode transaction: " <> pshow e
    TraceRpcSubmitTxValidationError e -> "Failed to submit transaction: " <> pshow e

instance Error TraceRpcSubmit where
  prettyError = pretty

instance Inject TraceRpcSubmit TraceRpc where
  inject = TraceRpcSubmit

instance Inject TraceRpcQuery TraceRpc where
  inject = TraceRpcQuery
