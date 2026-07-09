{-# LANGUAGE RankNTypes #-}

module Cardano.Rpc.Server.Internal.UtxoRpc.Type.ChainPoint
  ( mkChainPointMsg
  , utxoRpcChainPointMsgToChainPoint
  )
where

import Cardano.Api.Block
import Cardano.Api.Error
import Cardano.Api.Hash
import Cardano.Api.Serialise.Raw
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as U5c
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as UtxoRpc

import Cardano.Ledger.BaseTypes (WithOrigin (..))

import RIO

import Data.ByteString.Short qualified as SBS
import Data.ProtoLens (defMessage)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Network.GRPC.Spec

mkChainPointMsg
  :: ChainPoint
  -> WithOrigin BlockNo
  -> UTCTime
  -> Proto UtxoRpc.ChainPoint
mkChainPointMsg chainPoint blockNo timestamp = do
  let (slotNo, blockHash) = case chainPoint of
        ChainPointAtGenesis -> (0, mempty)
        ChainPoint (SlotNo slot) (HeaderHash hash) -> (slot, SBS.fromShort hash)
      blockHeight = case blockNo of
        Origin -> 0
        At (BlockNo h) -> h
      timestampMs = round . (* 1000) . utcTimeToPOSIXSeconds $ timestamp
  defMessage
    & U5c.slot .~ slotNo
    & U5c.hash .~ blockHash
    & U5c.height .~ blockHeight
    & U5c.timestamp .~ timestampMs

-- | Inverse of 'mkChainPointMsg'. Note: @Origin@ and @At (BlockNo 0)@ both
-- encode to @height=0@, so the decode always maps @0@ back to @Origin@.
utxoRpcChainPointMsgToChainPoint
  :: HasCallStack
  => MonadThrow m
  => Proto UtxoRpc.ChainPoint
  -> m (ChainPoint, WithOrigin BlockNo, UTCTime)
utxoRpcChainPointMsgToChainPoint msg = do
  let slot = msg ^. U5c.slot
      blockHash = msg ^. U5c.hash
      blockHeight = msg ^. U5c.height
      timestamp = posixSecondsToUTCTime . (/ 1000) . fromIntegral $ msg ^. U5c.timestamp
  chainPoint <-
    if slot == 0 && blockHash == mempty
      then pure ChainPointAtGenesis
      else do
        headerHash <- liftEitherError $ deserialiseFromRawBytes (AsHash asType) blockHash
        pure $ ChainPoint (SlotNo slot) headerHash
  let blockNo
        | blockHeight == 0 = Origin
        | otherwise = At (BlockNo blockHeight)
  pure (chainPoint, blockNo, timestamp)
