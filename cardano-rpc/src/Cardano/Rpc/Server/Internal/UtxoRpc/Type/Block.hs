{-# LANGUAGE GADTs #-}

-- | Conversion of a fetched or streamed chain block to the UTxO RPC
-- @AnyChainBlock@ message.
module Cardano.Rpc.Server.Internal.UtxoRpc.Type.Block
  ( mkAnyChainBlock
  )
where

import Cardano.Api.Block
import Cardano.Api.Consensus (byronBlockRaw)
import Cardano.Api.Serialise.Raw
import Cardano.Api.Tx
import Cardano.Rpc.Proto.Api.UtxoRpc.Sync qualified as U5c
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.Byron (byronBlockTxs)
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.ChainPoint (utcTimeToMs)
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.Tx (anyEraTxConstraints, txToUtxoRpcTx)

import RIO

import Data.ProtoLens (defMessage)
import Data.Time.Clock (UTCTime)
import Network.GRPC.Spec

-- | Assemble the @AnyChainBlock@ proto message: raw CBOR bytes, the cardano
-- header (slot, hash, height - derived from the block itself) and the parsed
-- transactions (all eras), plus the given slot timestamp.
mkAnyChainBlock
  :: ByteString
  -- ^ Raw CBOR bytes of the block, exactly as stored on chain
  -> BlockInMode
  -- ^ The same block, parsed and placed in its era context
  -> UTCTime
  -- ^ Slot wall-clock time; encoded as milliseconds since the Unix epoch
  -> Proto U5c.AnyChainBlock
  -- ^ Message carrying the native bytes, the parsed cardano block and the timestamp
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
