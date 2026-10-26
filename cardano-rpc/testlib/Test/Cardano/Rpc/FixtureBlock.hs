{-# LANGUAGE GADTs #-}

-- | Genuine Byron blocks, a fixed slot timestamp and the @AnyChainBlock@ built
-- from them before field masks, shared by the block handler tests.
module Test.Cardano.Rpc.FixtureBlock where

import Cardano.Api
import Cardano.Rpc.Proto.Api.UtxoRpc.Sync qualified as U5c
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.Byron (byronBlockTxs)
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.ChainPoint (utcTimeToMs)
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.Tx (anyEraTxConstraints, txToUtxoRpcTx)

import Cardano.Chain.Block qualified as Byron (ABlockOrBoundary, decCBORABlockOrBoundary)
import Cardano.Chain.Epoch.File (mainnetEpochSlots)
import Cardano.Ledger.Binary (byronProtVer, decodeFullDecoder, slice)
import Ouroboros.Consensus.Byron.Ledger (mkByronBlock)

import RIO

import Codec.Compression.GZip qualified as GZip
import Data.ByteString.Lazy qualified as LBS
import Data.ProtoLens (defMessage)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Network.GRPC.Spec (Proto)

import Hedgehog as H
import Hedgehog.Extras qualified as H

-- | The @AnyChainBlock@ that FetchBlock and FollowTip built before field masks, the
-- oracle for their responses under an absent mask, independent of the code under test: raw CBOR bytes, the cardano
-- header, the parsed transactions and the given slot timestamp.
oldAnyChainBlock
  :: ByteString
  -- ^ Raw CBOR bytes of the block, exactly as stored on chain
  -> BlockInMode
  -- ^ The same block, parsed and placed in its era context
  -> UTCTime
  -- ^ Slot wall-clock time; encoded as milliseconds since the Unix epoch
  -> Proto U5c.AnyChainBlock
oldAnyChainBlock rawBytes (BlockInMode _ block) timestamp = do
  let BlockHeader slot headerHash (BlockNo height) = getBlockHeader block
      txs = case block of
        ByronBlock consensusBlock -> byronBlockTxs $ byronBlockRaw consensusBlock
        ShelleyBlock blockEra _ ->
          anyEraTxConstraints blockEra $
            getBlockTxs block <&> \(ShelleyTx _ ledgerTx) -> txToUtxoRpcTx ledgerTx
      blockHeader =
        defMessage
          & U5c.slot .~ unSlotNo slot
          & U5c.hash .~ serialiseToRawBytes headerHash
          & U5c.height .~ height
  defMessage
    & U5c.nativeBytes .~ rawBytes
    & U5c.cardano . U5c.header .~ blockHeader
    & U5c.cardano . U5c.body . U5c.tx .~ txs
    & U5c.cardano . U5c.timestamp .~ utcTimeToMs timestamp

-- | A genuine Byron 'BlockInMode' decoded from the mainnet main-block golden
-- fixture also used by 'Test.Cardano.Rpc.ByronTx'. The block handlers read
-- scripted blocks through their real block reads, so the blocks must be real,
-- valid blocks rather than placeholders; decoding an existing fixture through the
-- same 'mkByronBlock' smart constructor the node's ChainDB uses is the cheapest
-- way to get one, and reuses a fixture already checked in for this purpose.
mainBlockInMode :: HasCallStack => MonadTest m => MonadIO m => m (ByteString, BlockInMode)
mainBlockInMode = do
  rawBytes <-
    liftIO $ LBS.toStrict <$> LBS.readFile "test/cardano-rpc-test/files/golden/byron-main-block.cbor"
  blockOrBoundary <- decodeBlockOrBoundaryFixture (LBS.fromStrict rawBytes)
  pure (rawBytes, BlockInMode ByronEra (ByronBlock (mkByronBlock mainnetEpochSlots blockOrBoundary)))

-- | A genuine Byron 'BlockInMode' decoded from the mainnet epoch-boundary
-- golden fixture also used by 'Test.Cardano.Rpc.ByronTx'; see
-- 'mainBlockInMode'. Distinct from it (a boundary block instead of a main
-- block, at a different slot and hash), so a two-block script has two
-- genuinely different blocks to distinguish, for free.
boundaryBlockInMode :: HasCallStack => MonadTest m => MonadIO m => m (ByteString, BlockInMode)
boundaryBlockInMode = do
  compressedBytes <- liftIO $ LBS.readFile "test/cardano-rpc-test/files/golden/byron-ebb.cbor.gz"
  let rawBytes = LBS.toStrict $ GZip.decompress compressedBytes
  blockOrBoundary <- decodeBlockOrBoundaryFixture (LBS.fromStrict rawBytes)
  pure (rawBytes, BlockInMode ByronEra (ByronBlock (mkByronBlock mainnetEpochSlots blockOrBoundary)))

-- | A fixed stub slot timestamp, returned regardless of the slot asked for.
stubTimestamp :: UTCTime
stubTimestamp = posixSecondsToUTCTime 1700000000

-- | Decode a 'Byron.ABlockOrBoundary' fixture from the wire encoding the
-- node's ImmutableDB stores, annotating the result with the original bytes,
-- exactly as a block fetched from the node would carry them.
decodeBlockOrBoundaryFixture
  :: HasCallStack
  => MonadTest m
  => LBS.ByteString
  -> m (Byron.ABlockOrBoundary ByteString)
decodeBlockOrBoundaryFixture blockBytes = do
  blockOrBoundary <-
    H.leftFail $
      decodeFullDecoder
        byronProtVer
        "ABlockOrBoundary"
        (Byron.decCBORABlockOrBoundary mainnetEpochSlots)
        blockBytes
  pure $ LBS.toStrict . slice blockBytes <$> blockOrBoundary
