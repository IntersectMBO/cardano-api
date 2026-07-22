{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

-- | Handlers for the UTxO RPC @SyncService@ - synchronising chain data
-- (fetching blocks, dumping history, following the tip).
module Cardano.Rpc.Server.Internal.UtxoRpc.Sync
  ( fetchBlockMethod
  , readTipMethod
  )
where

import Cardano.Api
import Cardano.Api.Consensus qualified as Consensus
import Cardano.Rpc.Proto.Api.UtxoRpc.Sync qualified as U5c
import Cardano.Rpc.Server.Internal.Error
import Cardano.Rpc.Server.Internal.Monad
import Cardano.Rpc.Server.Internal.Tracing ()
import Cardano.Rpc.Server.Internal.UtxoRpc.Type (anyEraTxConstraints, txToUtxoRpcTx)
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.Byron (byronBlockTxs)
import Cardano.Rpc.Server.NodeKernelAccess

import RIO

import Data.ByteString qualified as BS
import Data.ByteString.Short qualified as SBS
import Data.ProtoLens (defMessage)
import Data.Time.Clock (UTCTime)
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
  (rawBytes, blockInMode) <-
    fetchBlock nodeKernelAccess slot headerHash >>= maybe throwNotFound pure
  eraHistory <- readEraHistory
  timestamp <-
    slotToUTCTime systemStart eraHistory slot
      & either (const throwPastHorizon) pure
  pure $ defMessage & U5c.block .~ mkAnyChainBlock rawBytes blockInMode timestamp

-- | Handle the @ReadTip@ SyncService RPC method.
-- Reads the current chain tip from ChainDB and returns it as slot, block
-- header hash, block height and slot timestamp.
-- When the chain is at origin, the tip field is left unset.
readTipMethod
  :: MonadRpc e m
  => Proto U5c.ReadTipRequest
  -> m (Proto U5c.ReadTipResponse)
readTipMethod _request = do
  NodeKernelAccess{chainDb, systemStart, readEraHistory} <- grabNodeKernelAccess
  tipHeader <- liftIO $ Consensus.getTipHeader chainDb
  tip <- forM tipHeader $ \header -> do
    let slot = Consensus.blockSlot header
        throwPastHorizon =
          throwGrpcErrorWithMessage GrpcInternal $
            "cannot convert tip slot "
              <> tshow (unSlotNo slot)
              <> " to timestamp: the slot is past the era history horizon"
    eraHistory <- readEraHistory
    timestamp <-
      slotToUTCTime systemStart eraHistory slot
        & either (const throwPastHorizon) pure
    pure $ mkTipBlockRef header timestamp
  pure $ defMessage & U5c.maybe'tip .~ tip

-- | Assemble the @AnyChainBlock@ proto message: raw CBOR bytes, the cardano
-- header (slot, hash, height - derived from the block itself) and the parsed
-- transactions (all eras), plus the given slot timestamp.
mkAnyChainBlock
  :: ByteString
  -> BlockInMode
  -> UTCTime
  -- ^ Slot wall-clock time; encoded as milliseconds since the Unix epoch
  -> Proto U5c.AnyChainBlock
mkAnyChainBlock rawBytes (BlockInMode _ block) timestamp =
  let BlockHeader slot headerHash (BlockNo height) = getBlockHeader block
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
          & U5c.hash .~ serialiseToRawBytes headerHash
          & U5c.height .~ height
   in defMessage
        & U5c.nativeBytes .~ rawBytes
        & U5c.cardano . U5c.header .~ blockHeader
        & U5c.cardano . U5c.body . U5c.tx .~ txs
        & U5c.cardano . U5c.timestamp .~ utcTimeToMs timestamp

-- | Project a ChainDB header and a slot timestamp into a @BlockRef@: slot,
-- header hash, block height and timestamp.
mkTipBlockRef
  :: Consensus.Header (Consensus.CardanoBlock Consensus.StandardCrypto)
  -> UTCTime
  -- ^ Slot wall-clock time; encoded as milliseconds since the Unix epoch
  -> Proto U5c.BlockRef
mkTipBlockRef header timestamp =
  let slot = Consensus.blockSlot header
      Consensus.OneEraHash tipHash = Consensus.blockHash header
      BlockNo height = Consensus.blockNo header
   in defMessage
        & U5c.slot .~ unSlotNo slot
        & U5c.hash .~ SBS.fromShort tipHash
        & U5c.height .~ height
        & U5c.timestamp .~ utcTimeToMs timestamp

-- | Milliseconds since the Unix epoch, the timestamp encoding UTxO RPC uses.
utcTimeToMs :: UTCTime -> Word64
utcTimeToMs = round . (* 1000) . utcTimeToPOSIXSeconds
