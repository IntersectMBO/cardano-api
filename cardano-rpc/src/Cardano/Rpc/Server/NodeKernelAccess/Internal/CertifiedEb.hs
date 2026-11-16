{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A CertRB has no transactions of its own. They are in the EB that the
-- block before it announced. Consensus's
-- 'Ouroboros.Consensus.MiniProtocol.ChainSync.Server.inlineCertifiedEbTxs'
-- puts them into the CertRB. This module adds 'announcedBefore' and
-- 'decodeBlock', which consensus doesn't export.
module Cardano.Rpc.Server.NodeKernelAccess.Internal.CertifiedEb
  ( announcedBefore
  , decodeBlock
  )
where

import Cardano.Binary (DecoderError)
import Ouroboros.Consensus.Block
  ( ChainHash (..)
  , CodecConfig
  , GetPrevHash
  , Header
  , HeaderHash
  , RealPoint (..)
  , SlotNo (..)
  , blockPrevHash
  , blockSlot
  )
import Ouroboros.Consensus.Storage.LedgerDB.Forker (ResolveLeiosBlock (..))
import Ouroboros.Consensus.Storage.Serialisation (DecodeDisk (..))

import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Data.ByteString.Lazy qualified as BSL

import LeiosDemoTypes (LeiosPoint)

-- | The EB that the block before this one announced, if any.
--
-- To find a block in the ChainDB, we need its slot and its hash. A block
-- stores the hash of the block before it, but not its slot. So we try one
-- slot back, then two slots back, and so on, until we find it.
announcedBefore
  :: forall blk
   . (GetPrevHash blk, ResolveLeiosBlock blk)
  => (RealPoint blk -> IO (Maybe (Header blk)))
  -- ^ Look up a header in the ChainDB
  -> blk
  -> IO (Maybe LeiosPoint)
announcedBefore lookupHeader blk = case blockPrevHash blk of
  GenesisHash -> pure Nothing
  BlockHash prevHash -> (announcedBy =<<) <$> predecessor prevHash (blockSlot blk)
 where
  predecessor :: HeaderHash blk -> SlotNo -> IO (Maybe (Header blk))
  predecessor _ (SlotNo 0) = pure Nothing
  predecessor prevHash (SlotNo slot) =
    lookupHeader (RealPoint (SlotNo (slot - 1)) prevHash) >>= \case
      Nothing -> predecessor prevHash (SlotNo (slot - 1))
      found -> pure found

  announcedBy :: Header blk -> Maybe LeiosPoint
  announcedBy = fmap fst . headerLeiosAnnouncement

-- | Decode a block from its bytes, for example the bytes that
-- 'Ouroboros.Consensus.MiniProtocol.ChainSync.Server.inlineCertifiedEbTxs'
-- returns. Consensus has the same decoder, but doesn't export it.
decodeBlock
  :: forall blk
   . DecodeDisk blk (BSL.ByteString -> Either DecoderError blk)
  => CodecConfig blk
  -> BSL.ByteString
  -> Either String blk
decodeBlock ccfg bytes = case CBOR.deserialiseFromBytes decoder bytes of
  Left e -> Left (show e)
  Right (_, applyBytes) -> either (Left . show) Right (applyBytes bytes)
 where
  decoder :: forall s. CBOR.Decoder s (BSL.ByteString -> Either DecoderError blk)
  decoder = decodeDisk ccfg
