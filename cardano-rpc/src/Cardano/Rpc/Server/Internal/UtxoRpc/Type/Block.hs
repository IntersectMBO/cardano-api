{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

-- | Conversion of a fetched or streamed chain block to the UTxO RPC
-- @AnyChainBlock@ message.
module Cardano.Rpc.Server.Internal.UtxoRpc.Type.Block
  ( CardanoBlockComponent
  , anyChainBlockRead
  , mkAnyChainBlock
  )
where

import Cardano.Api.Block
import Cardano.Api.Consensus (byronBlockRaw)
import Cardano.Api.Consensus qualified as Consensus
import Cardano.Api.Serialise.Raw
import Cardano.Api.Tx
import Cardano.Rpc.Proto.Api.UtxoRpc.Sync qualified as U5c
import Cardano.Rpc.Server.Internal.FieldMask
  ( FieldFill
  , FieldMaskTree (SelectAll)
  , fill
  , fillShared
  , fillSub
  , isSelected
  , runFill
  , runFillPure
  , subMask
  )
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.Byron (byronBlockTxs)
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.ChainPoint (utcTimeToMs)
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.Tx (anyEraTxConstraints, txToUtxoRpcTx)

import RIO

import Data.Functor.Compose (Compose (..))
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
mkAnyChainBlock rawBytes block timestamp =
  Proto . runFillPure SelectAll $
    fill @"nativeBytes" (pure rawBytes)
      <> fillSub @"cardano"
        ( fill @"header" (pure . blockHeaderToUtxoRpc $ blockInModeHeader block)
            <> fillSub @"body" (fill @"tx" . pure $ blockTxsToUtxoRpc block)
            <> fill @"timestamp" (pure $ utcTimeToMs timestamp)
        )

-- | Components of a Cardano block that ChainDB reads together, for example the
-- raw bytes and the parsed block.
type CardanoBlockComponent =
  Consensus.BlockComponent (Consensus.CardanoBlock Consensus.StandardCrypto)

-- | The block components that the @AnyChainBlock@ fields a mask selects need, and
-- the action that builds the message from them. With no field selected, ChainDB only
-- checks that the block exists.
anyChainBlockRead
  :: Applicative m
  => (SlotNo -> m UTCTime)
  -- ^ Convert a slot to its wall-clock timestamp
  -> FieldMaskTree U5c.AnyChainBlock
  -- ^ Selects the fields to fill
  -> CardanoBlockComponent (m U5c.AnyChainBlock)
anyChainBlockRead doSlotTimestamp fieldMask =
  getCompose . runFill id fieldMask $ anyChainBlockFill isBodySelected doSlotTimestamp
 where
  isBodySelected = isSelected @"body" $ subMask @"cardano" fieldMask

-- | Fill the fields of an @AnyChainBlock@ that a mask selects, each from the block
-- component that holds it: @native_bytes@ from the raw block, @cardano.body@ from the
-- parsed block, @cardano.header@ from the header and @cardano.timestamp@ from the slot.
anyChainBlockFill
  :: Applicative m
  => Bool
  -- ^ Whether the mask selects @cardano.body@
  -> (SlotNo -> m UTCTime)
  -- ^ Convert a slot to its wall-clock timestamp
  -> FieldFill (Compose CardanoBlockComponent m) U5c.AnyChainBlock
anyChainBlockFill isBodySelected doSlotTimestamp =
  fill @"nativeBytes" (readComponent $ toStrictBytes <$> Consensus.GetRawBlock)
    <> fillSub @"cardano" (contentFill <> timestampFill)
 where
  -- ChainDB reads each component on its own, so the header comes from the parsed
  -- block when the body needs it anyway, and from the smaller header otherwise.
  contentFill
    | isBodySelected =
        fillShared (readComponent $ fromConsensusBlock <$> Consensus.GetBlock) $
          fill @"header" (blockHeaderToUtxoRpc . blockInModeHeader)
            <> fillSub @"body" (fill @"tx" blockTxsToUtxoRpc)
    | otherwise =
        fill @"header" . readComponent $
          blockHeaderToUtxoRpc . consensusHeaderToBlockHeader <$> Consensus.GetHeader
  timestampFill =
    fill @"timestamp" . Compose $ fmap utcTimeToMs . doSlotTimestamp <$> Consensus.GetSlot

-- | Read a block component that needs no further effect to fill its field.
readComponent :: Applicative m => CardanoBlockComponent a -> Compose CardanoBlockComponent m a
readComponent = Compose . fmap pure

-- | The UTxO RPC header of a block: its slot, header hash and height.
blockHeaderToUtxoRpc :: BlockHeader -> U5c.BlockHeader
blockHeaderToUtxoRpc (BlockHeader slot headerHash (BlockNo height)) =
  defMessage
    & U5c.slot .~ unSlotNo slot
    & U5c.hash .~ serialiseToRawBytes headerHash
    & U5c.height .~ height

-- | The header of a parsed block.
blockInModeHeader :: BlockInMode -> BlockHeader
blockInModeHeader (BlockInMode _ block) = getBlockHeader block

-- | The header that ChainDB reads without the block body, as the header of a parsed block.
consensusHeaderToBlockHeader
  :: Consensus.Header (Consensus.CardanoBlock Consensus.StandardCrypto) -> BlockHeader
consensusHeaderToBlockHeader header =
  BlockHeader
    (Consensus.blockSlot header)
    (HeaderHash . Consensus.getOneEraHash $ Consensus.blockHash header)
    (Consensus.blockNo header)

-- | The transactions of a parsed block. Byron transactions are not representable
-- as cardano-api's 'Tx era', so they are converted straight from the Byron ledger types.
blockTxsToUtxoRpc :: BlockInMode -> [U5c.Tx]
blockTxsToUtxoRpc (BlockInMode _ block) =
  map getProto $ case block of
    ByronBlock consensusBlock -> byronBlockTxs $ byronBlockRaw consensusBlock
    ShelleyBlock shelleyBasedEra _ ->
      anyEraTxConstraints shelleyBasedEra $
        getBlockTxs block <&> \(ShelleyTx _ ledgerTx) -> txToUtxoRpcTx ledgerTx
