{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tests for how @FetchBlock@ builds its response from a field mask.
module Test.Cardano.Rpc.FetchBlock where

import Cardano.Api (BlockHeader, BlockInMode, Hash (HeaderHash), SlotNo (..), fromConsensusBlock)
import Cardano.Rpc.Proto.Api.UtxoRpc.Sync qualified as U5c
import Cardano.Rpc.Server.Internal.FieldMask
import Cardano.Rpc.Server.Internal.UtxoRpc.Sync (fetchBlockResponse)
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.Block (anyChainBlockRead, mkAnyChainBlock)

import RIO
import RIO.Writer (runWriter)

import Data.ByteString.Short qualified as SBS
import Data.List (find)
import Data.ProtoLens (defMessage)
import GHC.Stack (withFrozenCallStack)
import Network.GRPC.Spec (GrpcError (GrpcNotFound), GrpcException (..), Proto, getProto)

import Test.Cardano.Rpc.FixtureBlock
  ( boundaryBlockInMode
  , mainBlockInMode
  , oldAnyChainBlock
  , stubTimestamp
  )
import Test.Cardano.Rpc.RecordedQuery

import Hedgehog as H
import Hedgehog.Extras qualified as H
import Hedgehog.Gen.QuickCheck qualified as Gen
import Test.Consensus.Cardano.Generators ()

import Proto.Google.Protobuf.FieldMask_Fields qualified as FieldMask (paths)

-- | An absent mask reads the raw and the parsed block, but no separate header,
-- and builds the response FetchBlock built before field masks.
hprop_field_mask_fetch_block_select_all_equals_unmasked_response :: Property
hprop_field_mask_fetch_block_select_all_equals_unmasked_response = H.propertyOnce $ do
  blocks <- fixtureBlocks
  (response, ran) <- fetchAll [] $ map toStoredBlock blocks
  ran
    === concat (replicate 2 ["lookup", "GetRawBlock", "GetBlock", "GetSlot"]) <> replicate 2 "slotTimestamp"
  response === (defMessage & U5c.block .~ map unmaskedBlock blocks)

-- | 'mkAnyChainBlock' still assembles the message it assembled before field masks.
hprop_field_mask_fetch_block_mk_any_chain_block_keeps_its_result :: Property
hprop_field_mask_fetch_block_mk_any_chain_block_keeps_its_result = H.propertyOnce $ do
  blocks <- fixtureBlocks
  for_ blocks $ \(rawBytes, blockInMode) ->
    mkAnyChainBlock rawBytes blockInMode stubTimestamp
      === oldAnyChainBlock rawBytes blockInMode stubTimestamp

-- | The native bytes alone read only the raw block and never evaluate the parsed one.
hprop_field_mask_fetch_block_native_bytes_reads_only_the_raw_block :: Property
hprop_field_mask_fetch_block_native_bytes_reads_only_the_raw_block = H.propertyOnce $ do
  blocks <- fixtureBlocks
  let unparsed =
        [stored{storedBlockInMode = error "forced"} | stored <- map toStoredBlock blocks]
  (response, ran) <- H.evalNF =<< fetchAll ["block.native_bytes"] unparsed
  ran === concat (replicate 2 ["lookup", "GetRawBlock"])
  response
    === ( defMessage
            & U5c.block .~ [defMessage & U5c.nativeBytes .~ rawBytes | (rawBytes, _) <- blocks]
        )

-- | The header alone reads only the header, and gives the header the parsed block has.
hprop_field_mask_fetch_block_header_reads_only_the_header :: Property
hprop_field_mask_fetch_block_header_reads_only_the_header = H.propertyOnce $ do
  blocks <- fixtureBlocks
  (response, ran) <- fetchAll ["block.cardano.header"] $ map toStoredBlock blocks
  ran === concat (replicate 2 ["lookup", "GetHeader"])
  let expectedBlock :: U5c.AnyChainBlock -> U5c.AnyChainBlock
      expectedBlock unmasked =
        defMessage & U5c.cardano . U5c.header .~ unmasked ^. U5c.cardano . U5c.header
  response === (defMessage & U5c.block .~ map (expectedBlock . unmaskedBlock) blocks)

-- | The body alone reads only the parsed block, and never the raw one.
hprop_field_mask_fetch_block_body_reads_only_the_parsed_block :: Property
hprop_field_mask_fetch_block_body_reads_only_the_parsed_block = H.propertyOnce $ do
  blocks <- fixtureBlocks
  let unread =
        [stored{storedBlockRawBytes = error "forced"} | stored <- map toStoredBlock blocks]
  (response, ran) <- H.evalNF =<< fetchAll ["block.cardano.body"] unread
  ran === concat (replicate 2 ["lookup", "GetBlock"])
  let expectedBlock :: U5c.AnyChainBlock -> U5c.AnyChainBlock
      expectedBlock unmasked =
        defMessage & U5c.cardano . U5c.body .~ unmasked ^. U5c.cardano . U5c.body
  response === (defMessage & U5c.block .~ map (expectedBlock . unmaskedBlock) blocks)
  H.assertWith response $ not . all (null . view (U5c.cardano . U5c.body . U5c.tx)) . view U5c.block

-- | The timestamp alone reads only the slot of each block, then converts the slots.
hprop_field_mask_fetch_block_timestamp_reads_only_the_slot :: Property
hprop_field_mask_fetch_block_timestamp_reads_only_the_slot = H.propertyOnce $ do
  blocks <- fixtureBlocks
  (response, ran) <- fetchAll ["block.cardano.timestamp"] $ map toStoredBlock blocks
  ran === concat (replicate 2 ["lookup", "GetSlot"]) <> replicate 2 "slotTimestamp"
  let expectedBlock :: U5c.AnyChainBlock -> U5c.AnyChainBlock
      expectedBlock unmasked =
        defMessage & U5c.cardano . U5c.timestamp .~ unmasked ^. U5c.cardano . U5c.timestamp
  response === (defMessage & U5c.block .~ map (expectedBlock . unmaskedBlock) blocks)

-- | Paths below the header and the transactions keep only those fields, from one
-- read of the parsed block.
hprop_field_mask_fetch_block_nested_paths_trim_each_block :: Property
hprop_field_mask_fetch_block_nested_paths_trim_each_block = H.propertyOnce $ do
  blocks <- fixtureBlocks
  (response, ran) <-
    fetchAll ["block.cardano.header.height", "block.cardano.body.tx.hash"] $
      map toStoredBlock blocks
  ran === concat (replicate 2 ["lookup", "GetBlock"])
  let expectedBlock :: U5c.AnyChainBlock -> U5c.AnyChainBlock
      expectedBlock unmasked =
        defMessage
          & U5c.cardano . U5c.header . U5c.height .~ unmasked ^. U5c.cardano . U5c.header . U5c.height
          & U5c.cardano . U5c.body . U5c.tx
            .~ [ defMessage & U5c.hash .~ tx ^. U5c.hash
               | tx <- unmasked ^. U5c.cardano . U5c.body . U5c.tx
               ]
  response === (defMessage & U5c.block .~ map (expectedBlock . unmaskedBlock) blocks)

-- | The header read without the block body is the header of the parsed block, in every era.
hprop_field_mask_fetch_block_header_read_equals_parsed_block_header :: Property
hprop_field_mask_fetch_block_header_read_equals_parsed_block_header = H.property $ do
  consensusBlock <- H.forAll Gen.arbitrary
  let stored = toStoredBlock (mempty, fromConsensusBlock consensusBlock)
      headerFrom paths = readAnyChainBlock paths stored ^. U5c.cardano . U5c.header
  headerFrom ["block.cardano.header"]
    === headerFrom ["block.cardano.header", "block.cardano.body"]

-- | A mask that selects no block field still reports a missing block, after only the lookup.
hprop_field_mask_fetch_block_nothing_selected_missing_block_is_not_found :: Property
hprop_field_mask_fetch_block_nothing_selected_missing_block_is_not_found = H.propertyOnce $ do
  (outcome, ran) <- runFetch ["bogus"] [] [missingPoint]
  ran === ["lookup"]
  (grpcError <$> either fromException (const Nothing) outcome) === Just GrpcNotFound

-- | A mask that selects no block field only checks that each block exists, and gives an empty response.
hprop_field_mask_fetch_block_nothing_selected_checks_existence_only :: Property
hprop_field_mask_fetch_block_nothing_selected_checks_existence_only = H.propertyOnce $ do
  blocks <- fixtureBlocks
  (response, ran) <- fetchAll ["bogus"] $ map toStoredBlock blocks
  ran === replicate 2 "lookup"
  response === defMessage

-- | A selected field still reports a missing block, before any timestamp conversion.
hprop_field_mask_fetch_block_selected_missing_block_is_not_found :: Property
hprop_field_mask_fetch_block_selected_missing_block_is_not_found = H.propertyOnce $ do
  (outcome, ran) <- runFetch ["block.cardano.timestamp"] [] [missingPoint]
  ran === ["lookup"]
  (grpcError <$> either fromException (const Nothing) outcome) === Just GrpcNotFound

-- | Run 'fetchBlockResponse' for a request with the mask paths, fetching every
-- stored block, and fail the test if it throws.
fetchAll
  :: HasCallStack
  => MonadIO m
  => MonadTest m
  => [FieldPath]
  -- ^ Field mask paths, rooted at the response
  -> [StoredBlock]
  -- ^ The blocks ChainDB holds, all of them requested
  -> m (U5c.FetchBlockResponse, [Text])
fetchAll paths storedBlocks = withFrozenCallStack $ do
  (outcome, ran) <- runFetch paths storedBlocks $ map storedBlockPoint storedBlocks
  response <- H.leftFail outcome
  pure (response, ran)

-- | Run 'fetchBlockResponse' for a request with the mask paths, recording each
-- ChainDB lookup, each block component read and each timestamp conversion.
runFetch
  :: MonadIO m
  => [FieldPath]
  -- ^ Field mask paths, rooted at the response
  -> [StoredBlock]
  -- ^ The blocks ChainDB holds
  -> [(SlotNo, Hash BlockHeader)]
  -- ^ The requested blocks
  -> m (Either SomeException U5c.FetchBlockResponse, [Text])
runFetch paths storedBlocks points = liftIO $ do
  logRef <- newIORef []
  let fieldMask = resolve $ getProto (requestWithMask paths) ^. U5c.fieldMask . FieldMask.paths
      doReadBlock component slot headerHash = do
        logRecorded logRef $ recorded "lookup" ()
        forM (find ((== (slot, headerHash)) . storedBlockPoint) storedBlocks) $
          logRecorded logRef . (`readRecordedComponent` component)
      doSlotTimestamp = const . logRecorded logRef $ recorded "slotTimestamp" stubTimestamp
  outcome <-
    try $
      fetchBlockResponse
        doReadBlock
        doSlotTimestamp
        fieldMask
        points
  ran <- readIORef logRef
  pure (outcome, ran)

-- | The @AnyChainBlock@ that the block reads of a mask build from a stored block,
-- with the stub timestamp.
readAnyChainBlock
  :: [FieldPath]
  -- ^ Field mask paths, rooted at the block
  -> StoredBlock
  -- ^ The block ChainDB holds
  -> U5c.AnyChainBlock
readAnyChainBlock paths stored =
  fst . runWriter . join . readRecordedComponent stored $
    anyChainBlockRead (const . pure $ stubTimestamp) (resolve paths)

-- | A @FetchBlock@ request carrying the mask paths.
requestWithMask
  :: [FieldPath]
  -- ^ Field mask paths, rooted at the response
  -> Proto U5c.FetchBlockRequest
requestWithMask paths = defMessage & U5c.fieldMask . FieldMask.paths .~ paths

-- | The block the response held before field masks, with the stub timestamp.
unmaskedBlock :: (ByteString, BlockInMode) -> U5c.AnyChainBlock
unmaskedBlock (rawBytes, blockInMode) = getProto $ oldAnyChainBlock rawBytes blockInMode stubTimestamp

-- | The two Byron fixture blocks: a main block with transactions and a boundary block.
fixtureBlocks :: HasCallStack => MonadTest m => MonadIO m => m [(ByteString, BlockInMode)]
fixtureBlocks = sequence [mainBlockInMode, boundaryBlockInMode]

-- | A point no stored block has.
missingPoint :: (SlotNo, Hash BlockHeader)
missingPoint = (SlotNo 42, HeaderHash . SBS.replicate 32 $ 0xAB)
