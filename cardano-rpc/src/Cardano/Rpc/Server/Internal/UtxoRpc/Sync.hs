{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
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
import Cardano.Rpc.Server.Internal.UtxoRpc.Type (anyEraTxConstraints, txToUtxoRpcTx)
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.Byron (byronBlockTxs)
import Cardano.Rpc.Server.NodeKernelAccess

import RIO

import Data.ByteString qualified as BS
import Data.ProtoLens (defMessage)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Network.GRPC.Spec (GrpcError (GrpcInternal, GrpcInvalidArgument, GrpcNotFound), Proto)

-- | Handle the @FetchBlock@ SyncService RPC method.
-- Fetches a block from ChainDB by slot and header hash.
-- Byron-era transactions carry no fee: Byron fees are implicit (inputs minus
-- outputs) and computing them needs UTxO lookups this handler does not do.
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
          "cannot convert slot "
            <> tshow (unSlotNo slot)
            <> " to timestamp: the slot is past the era history horizon;"
            <> " check that the requested slot is correct and that the node is fully in sync"
  headerHash <-
    deserialiseFromRawBytes (proxyToAsType (Proxy @(Hash BlockHeader))) hashBytes
      & either (const throwInvalidHash) pure
  (rawBytes, BlockInMode _ block) <-
    fetchBlock nodeKernelAccess slot headerHash >>= maybe throwNotFound pure
  eraHistory <- readEraHistory
  timestampMs <-
    slotToUTCTime systemStart eraHistory slot
      & either (const throwPastHorizon) (pure . round . (* 1000) . utcTimeToPOSIXSeconds)
  let BlockHeader _ _ (BlockNo height) = getBlockHeader block
      -- Byron transactions are not representable as cardano-api's 'Tx era',
      -- so they are converted straight from the Byron ledger types
      txs = case block of
        ByronBlock consensusBlock ->
          byronBlockTxs (byronBlockRaw consensusBlock)
        ShelleyBlock sbe _ ->
          anyEraTxConstraints sbe $
            getBlockTxs block <&> \(ShelleyTx _ ledgerTx) -> txToUtxoRpcTx ledgerTx
      blockHeader =
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
               & U5c.cardano . U5c.body . U5c.tx .~ txs
               & U5c.cardano . U5c.timestamp .~ timestampMs
           )
