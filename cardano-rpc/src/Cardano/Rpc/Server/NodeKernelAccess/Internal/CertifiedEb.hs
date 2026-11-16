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
import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as BSL

import LeiosDemoTypes (LeiosPoint)

-- | The EB that the block before this one announced, if any.
--
-- A block stores only the hash of the block before it, but the ChainDB also
-- needs a slot. So we try each earlier slot, down to the given one (e.g. the
-- start of the Leios era).
announcedBefore
  :: forall blk
   . (GetPrevHash blk, ResolveLeiosBlock blk)
  => (RealPoint blk -> IO (Maybe (Header blk)))
  -- ^ Look up a header in the ChainDB
  -> SlotNo
  -- ^ The earliest slot to look in
  -> blk
  -> IO (Maybe LeiosPoint)
announcedBefore lookupHeader earliest blk = case blockPrevHash blk of
  GenesisHash -> pure Nothing
  BlockHash prevHash -> (announcedBy =<<) <$> predecessor prevHash (blockSlot blk)
 where
  -- Look in the slot before the given one.
  predecessor :: HeaderHash blk -> SlotNo -> IO (Maybe (Header blk))
  predecessor prevHash (SlotNo slot)
    | SlotNo slot <= earliest = pure Nothing
    | otherwise =
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
  Right (_, applyBytes) -> first show (applyBytes bytes)
 where
  decoder :: forall s. CBOR.Decoder s (BSL.ByteString -> Either DecoderError blk)
  decoder = decodeDisk ccfg
