{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for filling a CertRB with its EB's transactions, and for the helpers
-- that needs: finding what the block before announced, and decoding a block.
--
-- Uses consensus's small test block ('LeiosTestBlock') and its in-memory
-- LeiosDB, so these tests need no real Cardano blocks and no running node.
module Test.Cardano.Rpc.CertifiedEb where

import Cardano.Rpc.Server.NodeKernelAccess.Internal.CertifiedEb
  ( announcedBefore
  , decodeBlock
  , withCertifiedEbTxs
  )

import Ouroboros.Consensus.Block
  ( RealPoint
  , SlotNo
  , blockRealPoint
  , blockSlot
  , getHeader
  , realPointSlot
  )
import Ouroboros.Consensus.MiniProtocol.ChainSync.Server
  ( CertRbWithoutEbTxs (..)
  , CertRbWithoutEbTxsReason (..)
  )
import Ouroboros.Consensus.Storage.LeiosDB
  ( LeiosDbHandle
  , LeiosDbWriter (..)
  , Promise (..)
  , newLeiosDBInMemory
  , withWriter
  )
import Ouroboros.Consensus.Storage.Serialisation (EncodeDisk (..))

import Codec.CBOR.Write qualified as CBOR
import Control.Exception (try)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Either (isLeft)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import GHC.Stack (HasCallStack)

import Hedgehog (Property, (===))
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H
import Test.Util.LeiosTestBlock

import LeiosDemoTypes (BytesSize, LeiosEb, LeiosPoint, TxHash)

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

eb1Txs :: [LeiosTestTx]
eb1Txs = [tx 1, tx 2]

eb1, eb2 :: LeiosEb
eb1Size, eb2Size :: BytesSize
eb1Closure :: [(TxHash, ByteString)]
(eb1, eb1Closure, eb1Size) = mkLeiosTestEb eb1Txs
(eb2, _, eb2Size) = mkLeiosTestEb [tx 3]

tx :: Int -> LeiosTestTx
tx k = LeiosTestTx{ltxAsserts = IntMap.empty, ltxWrites = IntMap.singleton k 'a'}

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

-- | Finds what the block before announced, by looking up its header.
hprop_announced_before_reads_predecessor :: Property
hprop_announced_before_reads_predecessor = H.propertyOnce $ do
  announced <- H.evalIO $ traverse (announcedBefore lookupHeader 0) chain
  -- rb1 comes after genesis, and b2 announced nothing.
  announced === [Nothing, Just eb1Point, Nothing, Just eb2Point]

-- | Finds the block before, even with empty slots in between.
-- Here b2 is moved from slot 2 to slot 5. Slots 4, 3 and 2 are empty, so
-- rb1 is found in slot 1.
hprop_announced_before_skips_empty_slots :: Property
hprop_announced_before_skips_empty_slots = H.propertyOnce $ do
  announced <- H.evalIO $ announcedBefore lookupHeader 0 (atSlot 5 b2)
  announced === Just eb1Point

-- | If the block before is missing, returns 'Nothing' without looking below
-- the earliest slot.
-- rb3 is in slot 3 and the earliest slot is 1, so only slots 2 and 1 are tried.
hprop_announced_before_missing_predecessor :: Property
hprop_announced_before_missing_predecessor = H.propertyOnce $ do
  looked <- H.evalIO $ newIORef []
  let lookupNothing point = Nothing <$ modifyIORef looked (realPointSlot point :)
  announced <- H.evalIO $ announcedBefore lookupNothing 1 rb3
  announced === Nothing
  slots <- H.evalIO $ readIORef looked
  reverse slots === [2, 1]

-- | Doesn't look below the earliest slot.
-- b2 (moved to slot 5) comes after rb1 (slot 1). With 2 as the earliest slot,
-- rb1 isn't found.
hprop_announced_before_stops_at_earliest :: Property
hprop_announced_before_stops_at_earliest = H.propertyOnce $ do
  announced <- H.evalIO $ announcedBefore lookupHeader 2 (atSlot 5 b2)
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
  Filling a CertRB
-------------------------------------------------------------------------------}

-- | A block that isn't a CertRB is returned as it is. The earliest slot isn't
-- even read.
hprop_with_certified_eb_txs_leaves_other_blocks :: Property
hprop_with_certified_eb_txs_leaves_other_blocks = H.propertyOnce $ do
  db <- H.evalIO newLeiosDBInMemory
  filled <- H.evalIO $ fill db (fail "read the earliest slot") b2
  filled === b2

-- | A CertRB gets the transactions of the EB the block before announced.
-- rb1 announced eb1, so a CertRB after it gets eb1's transactions.
hprop_with_certified_eb_txs_fills_certrb :: Property
hprop_with_certified_eb_txs_fills_certrb = H.propertyOnce $ do
  db <- H.evalIO newLeiosDbWithEb1
  filled <- H.evalIO $ fill db (pure 0) (asCertRb b2)
  ltbTxs (ltbBody filled) === eb1Txs

-- | Throws if the block before announced no EB.
-- b2 announced nothing, so a CertRB after it can't be filled.
hprop_with_certified_eb_txs_throws_without_announcement :: Property
hprop_with_certified_eb_txs_throws_without_announcement = H.propertyOnce $ do
  db <- H.evalIO newLeiosDbWithEb1
  throwsBecause (\case NoAnnouncement -> True; _ -> False) $
    fill db (pure 0) (asCertRb rb3)

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

-- | Fill a block from this LeiosDB, as the server does, looking up the block
-- before on 'chain'.
fill :: LeiosDbHandle IO -> IO SlotNo -> Blk -> IO Blk
fill db readEarliest blk =
  withCertifiedEbTxs LeiosTestCodecConfig db lookupHeader readEarliest (encodeBlock blk) blk

-- | The same block, marked as a CertRB.
-- 'withCertifiedEbTxs' only checks a flag in the header, so we set that flag.
-- No real certificate is needed.
asCertRb :: Blk -> Blk
asCertRb blk = blk{ltbHeader = (ltbHeader blk){lthContainsCert = ContainsCert}}

-- | A new in-memory LeiosDB holding eb1 and all its transactions.
newLeiosDbWithEb1 :: IO (LeiosDbHandle IO)
newLeiosDbWithEb1 = do
  db <- newLeiosDBInMemory
  withWriter db $ \writer -> do
    void $ await =<< writeEbPoint writer eb1Point eb1Size
    void $ await =<< writeEbBody writer eb1Point eb1 []
    void $ await =<< writeTxs writer eb1Point (zip [0 ..] (map snd eb1Closure))
  pure db

-- | The action throws 'CertRbWithoutEbTxs', for this reason.
throwsBecause
  :: (HasCallStack, H.MonadTest m, MonadIO m)
  => (CertRbWithoutEbTxsReason -> Bool) -> IO Blk -> m ()
throwsBecause expected act =
  H.evalIO (try act) >>= \case
    Left (CertRbWithoutEbTxs _ reason :: CertRbWithoutEbTxs Blk) -> do
      H.annotateShow reason
      H.assert (expected reason)
    Right blk -> do
      H.annotate "expected CertRbWithoutEbTxs, but the block was filled"
      H.annotateShow blk
      H.failure

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
