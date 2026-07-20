{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Provides datatypes used in tracing
module Cardano.Rpc.Server.Internal.Tracing where

import Cardano.Api (SlotNo)
import Cardano.Api.Consensus (TxValidationErrorInCardanoMode)
import Cardano.Api.Era (Inject (..))
import Cardano.Api.Error
import Cardano.Api.Pretty
import Cardano.Api.Serialise.Cbor (DecoderError)
import Cardano.Api.Serialise.Raw (SerialiseAsRawBytesError)
import Cardano.Api.Serialise.SerialiseUsing

import Control.Exception
import Data.Word (Word64)

-- | A sum type representing all possible traces
data TraceRpc
  = TraceRpcQuery TraceRpcQuery
  | TraceRpcSubmit TraceRpcSubmit
  | TraceRpcSync TraceRpcSync
  | TraceRpcError SomeException
  | TraceRpcFatalError SomeException

-- | Traces used in Query service
data TraceRpcQuery
  = -- | Span trace marking ReadParams query
    TraceRpcQueryParamsSpan TraceSpanEvent
  | -- | Span trace marking ReadUtxos query
    TraceRpcQueryReadUtxosSpan TraceSpanEvent
  | -- | Span trace marking SearchUtxos query
    TraceRpcQuerySearchUtxosSpan TraceSpanEvent
  deriving Show

instance Pretty TraceRpc where
  pretty = \case
    TraceRpcQuery t -> pretty t
    TraceRpcSubmit t -> pretty t
    TraceRpcSync t -> pretty t
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
    TraceRpcQuerySearchUtxosSpan (SpanBegin _) -> "Started query search UTXO method"
    TraceRpcQuerySearchUtxosSpan (SpanEnd _) -> "Finished query search UTXO method"

instance Error TraceRpcQuery where
  prettyError = pretty

-- | Traces used in SubmitTx service
data TraceRpcSubmit
  = -- | Node-to-client connection error during submission
    TraceRpcSubmitN2cConnectionError SomeException
  | -- | Transaction deserialisation error
    TraceRpcSubmitTxDecodingError DecoderError
  | -- | Transaction submission error
    TraceRpcSubmitTxValidationError TxValidationErrorInCardanoMode
  | -- | Transaction submission span
    TraceRpcSubmitSpan TraceSpanEvent
  | -- | Transaction evaluation deserialisation error
    TraceRpcEvalTxDecodingError SerialiseAsRawBytesError
  | -- | Transaction evaluation span
    TraceRpcEvalTxSpan TraceSpanEvent
  deriving Show

instance Pretty TraceRpcSubmit where
  pretty = \case
    TraceRpcSubmitSpan (SpanBegin _) -> "Started submit method"
    TraceRpcSubmitSpan (SpanEnd _) -> "Finished submit method"
    TraceRpcEvalTxSpan (SpanBegin _) -> "Started eval tx method"
    TraceRpcEvalTxSpan (SpanEnd _) -> "Finished eval tx method"
    TraceRpcEvalTxDecodingError e -> "Failed to decode transaction for evaluation: " <> pshow e
    TraceRpcSubmitN2cConnectionError e -> "N2C connection error while trying to submit a transaction: " <> prettyException e
    TraceRpcSubmitTxDecodingError e -> "Failed to decode transaction: " <> pshow e
    TraceRpcSubmitTxValidationError e -> "Failed to submit transaction: " <> pshow e

instance Error TraceRpcSubmit where
  prettyError = pretty

-- | Traces used in SyncService (FetchBlock, ReadTip, FollowTip)
data TraceRpcSync
  = -- | FetchBlock span
    TraceRpcFetchBlockSpan TraceSpanEvent
  | -- | ReadTip span
    TraceRpcReadTipSpan TraceSpanEvent
  | -- | Requested block was not found
    TraceRpcFetchBlockNotFound SlotNo
  | -- | Node kernel access is not yet available
    TraceRpcNodeKernelAccessUnavailable
  | -- | Ledger forker error
    TraceRpcForkerError String
  deriving Show

instance Pretty TraceRpcSync where
  pretty = \case
    TraceRpcFetchBlockSpan (SpanBegin _) -> "Started FetchBlock method"
    TraceRpcFetchBlockSpan (SpanEnd _) -> "Finished FetchBlock method"
    TraceRpcReadTipSpan (SpanBegin _) -> "Started ReadTip method"
    TraceRpcReadTipSpan (SpanEnd _) -> "Finished ReadTip method"
    TraceRpcFetchBlockNotFound slot -> "Block not found at slot " <> pshow slot
    TraceRpcNodeKernelAccessUnavailable -> "Node kernel access not yet initialised"
    TraceRpcForkerError e -> "Ledger forker error: " <> pretty e

instance Error TraceRpcSync where
  prettyError = pretty

instance Inject TraceRpcSubmit TraceRpc where
  inject = TraceRpcSubmit

instance Inject TraceRpcQuery TraceRpc where
  inject = TraceRpcQuery

instance Inject TraceRpcSync TraceRpc where
  inject = TraceRpcSync
