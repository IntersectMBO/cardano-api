{-# LANGUAGE LambdaCase #-}

module Cardano.Rpc.Server.Internal.UtxoRpc.Type.TxEval
  ( mkProtoTxEval
  , mkProtoRedeemer
  , scriptExecutionErrorToEvalReport
  , scriptWitnessIndexToRedeemerPurpose
  )
where

import Cardano.Api.Era
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Plutus
import Cardano.Api.Tx
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as U5c
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as UtxoRpc
import Cardano.Rpc.Server.Internal.Orphans ()
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.PlutusData

import RIO hiding (toList)

import Data.Map.Strict qualified as M
import Data.ProtoLens (defMessage)
import GHC.IsList
import Network.GRPC.Spec

-- | Assemble a proto 'TxEval' response from the computed fee, per-redeemer
-- evaluation results, and redeemer Plutus data from the transaction witness set.
mkProtoTxEval
  :: L.Coin
  -- ^ Computed minimum fee
  -> Map ScriptWitnessIndex (Either ScriptExecutionError ([Text], ExecutionUnits))
  -- ^ Per-redeemer evaluation results: script traces and execution units on
  -- success, or a script execution error on failure
  -> Map ScriptWitnessIndex (ScriptData, ByteString)
  -- ^ Redeemer Plutus data extracted from the transaction witness set, keyed
  -- by script witness index, with decoded payload and CBOR bytes.
  -> Proto UtxoRpc.TxEval
mkProtoTxEval fee evalMap redeemerLookup = do
  let (failures, successes) = M.mapEither id evalMap
      totalSteps = sum [executionSteps units | (_, units) <- M.elems successes]
      totalMemory = sum [executionMemory units | (_, units) <- M.elems successes]
      protoExecutionUnits =
        defMessage
          & U5c.steps .~ fromIntegral totalSteps
          & U5c.memory .~ fromIntegral totalMemory
      protoRedeemers =
        [ mkProtoRedeemer witness units $ M.lookup witness redeemerLookup
        | (witness, (_, units)) <- toList successes
        ]
      protoErrors =
        [ scriptExecutionErrorToEvalReport witness err
        | (witness, err) <- toList failures
        ]
      protoTraces =
        [ do
            let (purpose, idx) = scriptWitnessIndexToRedeemerPurpose witness
            defMessage
              & U5c.msg .~ traceLine
              & U5c.purpose .~ purpose
              & U5c.index .~ idx
        | (witness, (traces, _)) <- toList successes
        , traceLine <- traces
        ]
  defMessage
    & U5c.fee .~ inject fee
    & U5c.exUnits .~ protoExecutionUnits
    & U5c.redeemers .~ protoRedeemers
    & U5c.errors .~ protoErrors
    & U5c.traces .~ protoTraces

-- | Assemble a proto 'Redeemer' from evaluation results and transaction
-- witness data.
mkProtoRedeemer
  :: ScriptWitnessIndex
  -- ^ Redeemer purpose and index
  -> ExecutionUnits
  -- ^ Execution units consumed
  -> Maybe (ScriptData, ByteString)
  -- ^ Plutus data payload and its CBOR bytes
  -> Proto UtxoRpc.Redeemer
mkProtoRedeemer swi exUnits redeemerDatum = do
  let (purpose, idx) = scriptWitnessIndexToRedeemerPurpose swi
      protoExUnits =
        defMessage
          & U5c.steps .~ fromIntegral (executionSteps exUnits)
          & U5c.memory .~ fromIntegral (executionMemory exUnits)
  defMessage
    & U5c.purpose .~ purpose
    & U5c.index .~ idx
    & U5c.exUnits .~ protoExUnits
    & U5c.maybe'payload .~ fmap (scriptDataToUtxoRpcPlutusData . fst) redeemerDatum
    & U5c.originalCbor .~ maybe mempty snd redeemerDatum

-- | Convert a 'ScriptExecutionError' into a proto 'EvalReport' with the
-- redeemer purpose and 0-based index that produced the error.
scriptExecutionErrorToEvalReport
  :: ScriptWitnessIndex -> ScriptExecutionError -> Proto UtxoRpc.EvalReport
scriptExecutionErrorToEvalReport swi err = do
  let (purpose, idx) = scriptWitnessIndexToRedeemerPurpose swi
  defMessage
    & U5c.msg .~ tshow err
    & U5c.purpose .~ purpose
    & U5c.index .~ idx

-- | Map a 'ScriptWitnessIndex' to the corresponding proto 'RedeemerPurpose'
-- and the numeric index within that purpose.
scriptWitnessIndexToRedeemerPurpose
  :: ScriptWitnessIndex -> (Proto UtxoRpc.RedeemerPurpose, Word32)
scriptWitnessIndexToRedeemerPurpose = \case
  ScriptWitnessIndexTxIn i -> (Proto UtxoRpc.REDEEMER_PURPOSE_SPEND, i)
  ScriptWitnessIndexMint i -> (Proto UtxoRpc.REDEEMER_PURPOSE_MINT, i)
  ScriptWitnessIndexCertificate i -> (Proto UtxoRpc.REDEEMER_PURPOSE_CERT, i)
  ScriptWitnessIndexWithdrawal i -> (Proto UtxoRpc.REDEEMER_PURPOSE_REWARD, i)
  ScriptWitnessIndexVoting i -> (Proto UtxoRpc.REDEEMER_PURPOSE_VOTE, i)
  ScriptWitnessIndexProposing i -> (Proto UtxoRpc.REDEEMER_PURPOSE_PROPOSE, i)
