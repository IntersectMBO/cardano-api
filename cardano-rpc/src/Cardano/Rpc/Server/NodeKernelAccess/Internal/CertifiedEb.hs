{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A CertRB has no transactions of its own. They are in the EB that the
-- block before it announced. 'withCertifiedEbTxs' adds them, with consensus's
-- 'inlineCertifiedEbTxs' and two helpers consensus doesn't export:
-- 'announcedBefore' and 'decodeBlock'.
module Cardano.Rpc.Server.NodeKernelAccess.Internal.CertifiedEb
  ( withCertifiedEbTxs
  , announcedBefore
  , decodeBlock
  )
where

import Cardano.Api.Network (Serialised (..))

import Cardano.Binary (DecoderError)
import Ouroboros.Consensus.Block
  ( ChainHash (..)
  , CodecConfig
  , GetHeader (..)
  , GetPrevHash
  , Header
  , HeaderHash
  , RealPoint (..)
  , SlotNo (..)
  , blockPoint
  , blockPrevHash
  , blockSlot
  )
import Ouroboros.Consensus.MiniProtocol.ChainSync.Server
  ( CertRbWithoutEbTxs (..)
  , CertRbWithoutEbTxsReason (Undecodable)
  , inlineCertifiedEbTxs
  )
import Ouroboros.Consensus.Storage.LedgerDB.Forker (ResolveLeiosBlock (..))
import Ouroboros.Consensus.Storage.LeiosDB.API (LeiosDbHandle, withReader)
import Ouroboros.Consensus.Storage.Serialisation (DecodeDisk (..), EncodeDisk)

import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Control.Exception (throwIO)
import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as BSL

import LeiosDemoTypes (LeiosPoint)

-- | Add the EB's transactions to a CertRB, using consensus's
-- 'inlineCertifiedEbTxs'. Other blocks are returned unchanged. Throws
-- 'CertRbWithoutEbTxs' if the transactions can't be added.
withCertifiedEbTxs
  :: forall blk
   . ( GetPrevHash blk
     , ResolveLeiosBlock blk
     , DecodeDisk blk (BSL.ByteString -> Either DecoderError blk)
     , EncodeDisk blk blk
     )
  => CodecConfig blk
  -> LeiosDbHandle IO
  -> (RealPoint blk -> IO (Maybe (Header blk)))
  -- ^ Look up a header in the ChainDB
  -> IO SlotNo
  -- ^ The earliest slot to look for the block before in. Only read for a
  -- CertRB.
  -> BSL.ByteString
  -- ^ The block as stored
  -> blk
  -> IO blk
withCertifiedEbTxs ccfg leiosDb lookupHeader readEarliest rawBlock block
  | not (headerContainsLeiosCert header) = pure block
  | otherwise = do
      earliest <- readEarliest
      prevAnnouncement <- announcedBefore lookupHeader earliest block
      Serialised withTxs <-
        withReader leiosDb $ \reader ->
          inlineCertifiedEbTxs ccfg reader prevAnnouncement pt header (Serialised rawBlock)
      -- Consensus just encoded this block, so failing to decode it is a bug.
      either (throwIO . CertRbWithoutEbTxs pt . Undecodable) pure (decodeBlock ccfg withTxs)
 where
  header = getHeader block
  pt = blockPoint block

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
