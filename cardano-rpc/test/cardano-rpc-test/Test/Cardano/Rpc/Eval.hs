{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Rpc.Eval where

import Cardano.Api
  ( ExecutionUnits (..)
  , ScriptData (..)
  , ScriptWitnessIndex (..)
  )
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Tx (ScriptExecutionError (..))
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as U5c
import Cardano.Rpc.Server.Internal.UtxoRpc.Type

import RIO

import GHC.Stack (withFrozenCallStack)
import Network.GRPC.Spec (Proto (..), getProto)

import Hedgehog as H
import Hedgehog.Extras qualified as H

-- | All six 'ScriptWitnessIndex' constructors map to the correct
-- 'RedeemerPurpose' and preserve the index.
hprop_scriptWitnessIndex_to_redeemerPurpose :: Property
hprop_scriptWitnessIndex_to_redeemerPurpose = H.propertyOnce $ do
  verify (ScriptWitnessIndexTxIn 3) U5c.REDEEMER_PURPOSE_SPEND 3
  verify (ScriptWitnessIndexMint 0) U5c.REDEEMER_PURPOSE_MINT 0
  verify (ScriptWitnessIndexCertificate 1) U5c.REDEEMER_PURPOSE_CERT 1
  verify (ScriptWitnessIndexWithdrawal 2) U5c.REDEEMER_PURPOSE_REWARD 2
  verify (ScriptWitnessIndexVoting 5) U5c.REDEEMER_PURPOSE_VOTE 5
  verify (ScriptWitnessIndexProposing 4) U5c.REDEEMER_PURPOSE_PROPOSE 4
 where
  verify :: (HasCallStack, MonadTest m) => ScriptWitnessIndex -> U5c.RedeemerPurpose -> Word32 -> m ()
  verify swi expectedPurpose expectedIndex = withFrozenCallStack $ do
    let (purpose, idx) = scriptWitnessIndexToRedeemerPurpose swi
    H.annotate $ "ScriptWitnessIndex: " <> show swi
    purpose === Proto expectedPurpose
    idx === expectedIndex

-- | 'mkProtoRedeemer' assembles a proto Redeemer with correct fields.
hprop_mkProtoRedeemer :: Property
hprop_mkProtoRedeemer = H.propertyOnce $ do
  let swi = ScriptWitnessIndexMint 2
      exUnits = ExecutionUnits{executionSteps = 100, executionMemory = 200}
      payload = ScriptDataNumber 42
      redeemer = getProto $ mkProtoRedeemer swi exUnits (Just (payload, mempty))

  redeemer ^. U5c.purpose === U5c.REDEEMER_PURPOSE_MINT
  redeemer ^. U5c.index === 2
  redeemer ^. U5c.exUnits . U5c.steps === 100
  redeemer ^. U5c.exUnits . U5c.memory === 200

-- | Successful evaluation with two redeemers produces correct aggregate
-- execution units, fee, redeemers list, empty errors, and traces.
hprop_mkProtoTxEval_success :: Property
hprop_mkProtoTxEval_success = H.propertyOnce $ do
  let fee = L.Coin 200000
      evalMap =
        [ (ScriptWitnessIndexTxIn 0, Right (["trace line 1"], ExecutionUnits 500 300))
        , (ScriptWitnessIndexMint 0, Right (["trace line 2", "trace line 3"], ExecutionUnits 100 200))
        ]
      redeemerData =
        [ (ScriptWitnessIndexTxIn 0, (ScriptDataNumber 1, mempty))
        , (ScriptWitnessIndexMint 0, (ScriptDataNumber 2, mempty))
        ]
      txEval = getProto $ mkProtoTxEval fee evalMap redeemerData

  H.note_ "Fee should be 200000"
  txEval ^. U5c.fee . U5c.int === 200000

  H.note_ "Aggregate execution units should be sum of both redeemers"
  txEval ^. U5c.exUnits . U5c.steps === 600
  txEval ^. U5c.exUnits . U5c.memory === 500

  H.note_ "Two redeemers"
  length (txEval ^. U5c.redeemers) === 2

  H.note_ "No errors"
  length (txEval ^. U5c.errors) === 0

  H.note_ "Three trace lines total"
  length (txEval ^. U5c.traces) === 3

-- | Mixed success and failure: only successful redeemers contribute to
-- aggregate execution units and appear in the redeemers list; failures
-- appear in errors.
hprop_mkProtoTxEval_with_errors :: Property
hprop_mkProtoTxEval_with_errors = H.propertyOnce $ do
  let fee = L.Coin 180000
      evalMap =
        [ (ScriptWitnessIndexTxIn 0, Right (["ok trace"], ExecutionUnits 500 300))
        , (ScriptWitnessIndexMint 0, Left ScriptErrorExecutionUnitsOverflow)
        ]
      redeemerData =
        [ (ScriptWitnessIndexTxIn 0, (ScriptDataNumber 1, mempty))
        , (ScriptWitnessIndexMint 0, (ScriptDataNumber 2, mempty))
        ]
      txEval = getProto $ mkProtoTxEval fee evalMap redeemerData

  H.note_ "Only the successful redeemer contributes to aggregate"
  txEval ^. U5c.exUnits . U5c.steps === 500
  txEval ^. U5c.exUnits . U5c.memory === 300

  H.note_ "One redeemer in response"
  length (txEval ^. U5c.redeemers) === 1

  H.note_ "One error"
  length (txEval ^. U5c.errors) === 1

  H.note_ "Only the success trace"
  length (txEval ^. U5c.traces) === 1

-- | 'scriptExecutionErrorToEvalReport' produces non-empty message strings and
-- carries the correct redeemer purpose and index.
hprop_scriptExecutionError_to_evalReport :: Property
hprop_scriptExecutionError_to_evalReport = H.propertyOnce $ do
  verifyError
    (ScriptWitnessIndexTxIn 0)
    ScriptErrorExecutionUnitsOverflow
    U5c.REDEEMER_PURPOSE_SPEND
    0
  verifyError
    (ScriptWitnessIndexMint 3)
    (ScriptErrorRedeemerPointsToUnknownScriptHash $ ScriptWitnessIndexTxIn 0)
    U5c.REDEEMER_PURPOSE_MINT
    3
 where
  verifyError
    :: (HasCallStack, MonadTest m)
    => ScriptWitnessIndex -> ScriptExecutionError -> U5c.RedeemerPurpose -> Word32 -> m ()
  verifyError swi err expectedPurpose expectedIndex = withFrozenCallStack $ do
    let evalReport = getProto $ scriptExecutionErrorToEvalReport swi err
    H.annotate $ "Error: " <> show err
    H.assertWith (evalReport ^. U5c.msg) (/= "")
    evalReport ^. U5c.purpose === expectedPurpose
    evalReport ^. U5c.index === expectedIndex
