{-# LANGUAGE OverloadedStrings #-}

-- | Actions that record their name when they run, and the values they return,
-- for testing which queries a field fill runs.
module Test.Cardano.Rpc.RecordedQuery where

import Cardano.Api (ChainPoint (..))
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as U5c
import Cardano.Rpc.Server.Internal.FieldMask (FieldFill, FieldPath, resolve, runFill)
import Cardano.Rpc.Server.Internal.UtxoRpc.Type (UtxoFields (..), mkChainPointMsg)

import Cardano.Slotting.Slot (WithOrigin (..))

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
