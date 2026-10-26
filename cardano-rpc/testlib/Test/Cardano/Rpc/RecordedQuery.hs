{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Actions that record their name when they run, and the values they return,
-- for testing which queries and block reads a field fill runs.
module Test.Cardano.Rpc.RecordedQuery where

import Cardano.Api
  ( BlockHeader (..)
  , BlockInMode (..)
  , ChainPoint (..)
  , Hash (HeaderHash)
  , SlotNo
  , getBlockHeader
  , toConsensusBlock
  )
import Cardano.Api.Consensus qualified as Consensus
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as U5c
import Cardano.Rpc.Server.Internal.FieldMask (FieldFill, FieldPath, resolve, runFill)
import Cardano.Rpc.Server.Internal.UtxoRpc.Type (UtxoFields (..), mkChainPointMsg)

import Cardano.Slotting.Slot (WithOrigin (..))
import Ouroboros.Consensus.Block (getHeader)

import RIO
import RIO.Writer (Writer, runWriter, tell)

import Data.ProtoLens (Message, defMessage)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Network.GRPC.Spec (getProto)

-- | Run a fill for the mask paths, recording the session and each query it runs.
runRecordedFill
  :: Message msg
  => [FieldPath]
  -- ^ Field mask paths, rooted at the response
  -> FieldFill (Writer [Text]) msg
  -- ^ Fills the response from recorded actions
  -> (msg, [Text])
runRecordedFill paths = runWriter . runFill session (resolve paths)

-- | Read block components from a stored block as ChainDB would, recording each
-- component read.
readRecordedComponent
  :: StoredBlock
  -- ^ The block to read from
  -> Consensus.BlockComponent (Consensus.CardanoBlock Consensus.StandardCrypto) a
  -- ^ The components to read
  -> Writer [Text] a
readRecordedComponent stored = \case
  Consensus.GetRawBlock -> recorded "GetRawBlock" . fromStrictBytes $ storedBlockRawBytes stored
  Consensus.GetBlock -> recorded "GetBlock" consensusBlock
  Consensus.GetHeader -> recorded "GetHeader" $ getHeader consensusBlock
  Consensus.GetSlot -> recorded "GetSlot" . fst $ storedBlockPoint stored
  Consensus.GetHash -> recorded "GetHash" . toOneEraHash . snd $ storedBlockPoint stored
  Consensus.GetPure value -> pure value
  Consensus.GetApply doRead component ->
    readRecordedComponent stored doRead <*> readRecordedComponent stored component
  _ -> error "readRecordedComponent: no handler reads this block component"
 where
  consensusBlock = toConsensusBlock $ storedBlockInMode stored
  toOneEraHash (HeaderHash bytes) = Consensus.OneEraHash bytes

-- | Run recorded actions in 'IO', appending what they record to the log.
logRecorded
  :: IORef [Text]
  -- ^ The log
  -> Writer [Text] a
  -- ^ The recorded actions
  -> IO a
logRecorded logRef doRecorded = do
  let (value, names) = runWriter doRecorded
  modifyIORef' logRef (<> names)
  pure value

-- | An action that records its name when it runs.
recorded
  :: Text
  -- ^ The name to record
  -> a
  -- ^ The result of the action
  -> Writer [Text] a
recorded name value = tell [name] $> value

-- | A runner that records each session it opens.
session
  :: Writer [Text] a
  -- ^ The actions to run in the session
  -> Writer [Text] a
session = (tell ["session"] *>)

-- | A block as ChainDB stores it: its point, which ChainDB keeps in its index,
-- its raw bytes and the parsed block. The fields are lazy, so a test can leave
-- out a part that must not be read.
data StoredBlock = StoredBlock
  { storedBlockPoint :: (SlotNo, Hash BlockHeader)
  , storedBlockRawBytes :: ByteString
  , storedBlockInMode :: BlockInMode
  }

-- | Store a raw block and its parsed form at the point its header names.
toStoredBlock
  :: (ByteString, BlockInMode)
  -- ^ The raw block and the same block parsed
  -> StoredBlock
toStoredBlock (rawBytes, blockInMode@(BlockInMode _ block)) = do
  let BlockHeader slot headerHash _ = getBlockHeader block
  StoredBlock (slot, headerHash) rawBytes blockInMode

-- | The chain tip that the recorded tip queries return.
realTip :: U5c.ChainPoint
realTip = getProto $ mkChainPointMsg ChainPointAtGenesis Origin (posixSecondsToUTCTime 1)

-- | The UTxO entries that the recorded UTxO queries return.
realFields :: [UtxoFields]
realFields =
  [ UtxoFields
      { utxoFieldsRef = defMessage & U5c.hash .~ "tx-1" & U5c.index .~ 0
      , utxoFieldsNativeBytes = "native-1"
      , utxoFieldsCardano = defMessage & U5c.address .~ "address-1"
      }
  , UtxoFields
      { utxoFieldsRef = defMessage & U5c.hash .~ "tx-2" & U5c.index .~ 1
      , utxoFieldsNativeBytes = "native-2"
      , utxoFieldsCardano = defMessage & U5c.address .~ "address-2"
      }
  ]
