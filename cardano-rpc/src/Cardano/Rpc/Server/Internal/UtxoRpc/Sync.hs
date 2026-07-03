{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

-- | Handlers for the UTxO RPC @SyncService@ - synchronising chain data
-- (fetching blocks, dumping history, following the tip).
module Cardano.Rpc.Server.Internal.UtxoRpc.Sync
  ( fetchBlockMethod
  )
where

import Cardano.Api
import Cardano.Rpc.Proto.Api.UtxoRpc.Sync qualified as U5c
import Cardano.Rpc.Server.Internal.Error
import Cardano.Rpc.Server.Internal.Monad
import Cardano.Rpc.Server.Internal.Tracing ()
import Cardano.Rpc.Server.NodeKernelAccess

import RIO

import Data.ByteString qualified as BS
import Data.ProtoLens (defMessage)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Network.GRPC.Spec (GrpcError (GrpcInternal, GrpcInvalidArgument, GrpcNotFound), Proto)

-- | Handle the @FetchBlock@ SyncService RPC method.
-- Fetches a block from ChainDB by slot and header hash.
-- Returns @NOT_FOUND@ if the requested block is missing.
-- Returns @INVALID_ARGUMENT@ if the block reference has an invalid hash.
fetchBlockMethod
  :: MonadRpc e m
  => Proto U5c.FetchBlockRequest
  -- ^ Request containing a block reference (slot + hash)
  -> m (Proto U5c.FetchBlockResponse)
  -- ^ Response containing the fetched block with raw CBOR and cardano header
fetchBlockMethod request = do
  nodeKernelAccess@NodeKernelAccess{systemStart, readEraHistory} <- grabNodeKernelAccess
  let blockRef = request ^. U5c.ref
      slot = SlotNo $ blockRef ^. U5c.slot
      hashBytes = blockRef ^. U5c.hash
      throwInvalidHash =
        throwGrpcErrorWithMessage GrpcInvalidArgument $
          "invalid block header hash (" <> tshow (BS.length hashBytes) <> " bytes)"
      throwNotFound =
        throwGrpcErrorWithMessage GrpcNotFound $
          "block not found at slot " <> tshow (unSlotNo slot)
      throwPastHorizon =
        throwGrpcErrorWithMessage GrpcInternal $
          "past horizon converting slot " <> tshow (unSlotNo slot) <> " to timestamp"
  headerHash <-
    deserialiseFromRawBytes (proxyToAsType (Proxy @(Hash BlockHeader))) hashBytes
      & either (const throwInvalidHash) pure
  (rawBytes, BlockNo height) <-
    fetchBlock nodeKernelAccess slot headerHash >>= maybe throwNotFound pure
  eraHistory <- readEraHistory
  timestampMs <-
    slotToUTCTime systemStart eraHistory slot
      & either (const throwPastHorizon) (pure . round . (* 1000) . utcTimeToPOSIXSeconds)
  let blockHeader =
        defMessage
          & U5c.slot .~ unSlotNo slot
          & U5c.hash .~ hashBytes
          & U5c.height .~ height
  pure $
    defMessage
      & U5c.block
        .~ ( defMessage
               & U5c.nativeBytes .~ rawBytes
               & U5c.cardano . U5c.header .~ blockHeader
               & U5c.cardano . U5c.timestamp .~ timestampMs
           )

-- TODO: cardano.body.tx - needs full block deserialisation + UTxO RPC tx mapping
