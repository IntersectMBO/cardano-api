-- | Tests for what the server needs around consensus's 'inlineCertifiedEbTxs':
-- finding what the block before announced, and decoding a block.
--
-- Uses consensus's small test block ('LeiosTestBlock'), so these tests need no
-- real Cardano blocks and no running node.
module Test.Cardano.Rpc.CertifiedEb where

import Cardano.Rpc.Server.NodeKernelAccess.Internal.CertifiedEb
  ( announcedBefore
  , decodeBlock
  )

import Ouroboros.Consensus.Block
  ( RealPoint
  , SlotNo
  , blockRealPoint
  , blockSlot
  , getHeader
  )
import Ouroboros.Consensus.Storage.Serialisation (EncodeDisk (..))

import Codec.CBOR.Write qualified as CBOR
import Data.ByteString.Lazy qualified as BSL
import Data.Either (isLeft)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map

import Hedgehog (Property, (===))
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H
import Test.Util.LeiosTestBlock

import LeiosDemoTypes (BytesSize, LeiosEb, LeiosPoint)

type Blk = LeiosTestBlock

{-------------------------------------------------------------------------------
  The chain

  rb1 announces eb1 and rb3 announces eb2. b2 and b4 announce nothing.
-------------------------------------------------------------------------------}

rb1, b2, rb3, b4 :: Blk
rb1 = announcing eb1Point eb1Size (firstLeiosBlock 0)
b2 = successorLeiosBlock rb1
rb3 = announcing eb2Point eb2Size (successorLeiosBlock b2)
b4 = successorLeiosBlock rb3

chain :: [Blk]
chain = [rb1, b2, rb3, b4]

eb1Point, eb2Point :: LeiosPoint
eb1Point = leiosTestEbPoint (blockSlot (firstLeiosBlock 0)) eb1
eb2Point = leiosTestEbPoint (blockSlot (successorLeiosBlock b2)) eb2

eb1, eb2 :: LeiosEb
eb1Size, eb2Size :: BytesSize
(eb1, _, eb1Size) = mkLeiosTestEb [tx 1, tx 2]
(eb2, _, eb2Size) = mkLeiosTestEb [tx 3]

tx :: Int -> LeiosTestTx
tx k = LeiosTestTx{ltxAsserts = IntMap.empty, ltxWrites = IntMap.singleton k 'a'}

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

-- | Finds what the block before announced, by looking up its header.
hprop_announced_before_reads_predecessor :: Property
hprop_announced_before_reads_predecessor = H.propertyOnce $ do
  announced <- H.evalIO $ traverse (announcedBefore lookupHeader) chain
  -- rb1 comes after genesis, and b2 announced nothing.
  announced === [Nothing, Just eb1Point, Nothing, Just eb2Point]

-- | Finds the block before, even with empty slots in between.
-- Here b2 is moved from slot 2 to slot 5. Slots 4, 3 and 2 are empty, so
-- rb1 is found in slot 1.
hprop_announced_before_skips_empty_slots :: Property
hprop_announced_before_skips_empty_slots = H.propertyOnce $ do
  let later = atSlot 5 b2
  announced <- H.evalIO $ announcedBefore lookupHeader later
  announced === Just eb1Point

-- | If the block before isn't in the ChainDB, stops at slot 0 with 'Nothing'.
hprop_announced_before_missing_predecessor :: Property
hprop_announced_before_missing_predecessor = H.propertyOnce $ do
  announced <- H.evalIO $ announcedBefore (const (pure Nothing)) rb3
  announced === Nothing

-- | A block decodes back to itself.
hprop_decode_block_round_trips :: Property
hprop_decode_block_round_trips =
  H.propertyOnce $
    map (decodeBlock LeiosTestCodecConfig . encodeBlock) chain === map Right chain

-- | Bytes that aren't a block don't decode.
hprop_decode_block_rejects_garbage :: Property
hprop_decode_block_rejects_garbage =
  H.propertyOnce $
    H.assert $
      isLeft (decodeBlock LeiosTestCodecConfig (BSL.pack [0xff]) :: Either String Blk)

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

-- | Look up a header on 'chain', as the ChainDB does: by slot and hash.
lookupHeader :: RealPoint Blk -> IO (Maybe (Header Blk))
lookupHeader point = pure $ Map.lookup point headers
 where
  headers = Map.fromList [(blockRealPoint blk, getHeader blk) | blk <- chain]

-- | The same block, in a later slot.
atSlot :: SlotNo -> Blk -> Blk
atSlot slot blk = blk{ltbHeader = (ltbHeader blk){lthSlot = slot}}

encodeBlock :: Blk -> BSL.ByteString
encodeBlock = CBOR.toLazyByteString . encodeDisk LeiosTestCodecConfig
