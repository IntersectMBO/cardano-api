{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Rpc.Server.Internal.UtxoRpc.Eval
  ( evalTxMethod
  )
where

import Cardano.Api
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Experimental.Era
import Cardano.Api.Ledger qualified as L
import Cardano.Rpc.Proto.Api.UtxoRpc.Submit qualified as U5c
import Cardano.Rpc.Proto.Api.UtxoRpc.Submit qualified as UtxoRpc
import Cardano.Rpc.Server.Internal.Error
import Cardano.Rpc.Server.Internal.Monad
import Cardano.Rpc.Server.Internal.Orphans ()
import Cardano.Rpc.Server.Internal.Tracing
import Cardano.Rpc.Server.Internal.UtxoRpc.Type

import Cardano.Ledger.Api qualified as L

import RIO hiding (toList)

import Data.ByteString qualified as BS
import Data.Default
import Data.Map.Strict qualified as Map
import Data.ProtoLens (defMessage)
import Data.Set qualified as Set
import GHC.IsList
import GHC.Stack
import Network.GRPC.Spec

import Proto.Utxorpc.V1beta.Cardano.Cardano_Fields qualified as Proto

-- | Evaluate a CBOR-serialised transaction against the current ledger state,
-- returning per-redeemer execution units, computed minimum fee, script traces,
-- balance check results, and evaluation errors, without submitting.
evalTxMethod
  :: MonadRpc e m
  => Proto UtxoRpc.EvalTxRequest
  -- ^ The evaluation request containing raw transaction CBOR
  -> m (Proto UtxoRpc.EvalTxResponse)
evalTxMethod request = do
  nodeConnInfo <- grab
  AnyCardanoEra (era :: CardanoEra era) <- liftIO . throwExceptT $ determineEra nodeConnInfo
  (eon :: Era era) <- forEraInEon @Era era (error "Minimum Conway era required") pure

  let rawTx = request ^. U5c.tx . U5c.raw

  -- A tx's inputs live inside its raw bytes, so this also bounds the input set the
  -- query batch below resolves. The exact protocol limit is enforced further down,
  -- once protocol parameters are available.
  when (BS.length rawTx > maxEvalTxSizeBytes) $
    throwGrpcErrorWithMessage GrpcInvalidArgument $
      "transaction size "
        <> tshow (BS.length rawTx)
        <> " exceeds the evaluation limit "
        <> tshow maxEvalTxSizeBytes

  (Exp.SignedTx ledgerTx :: Exp.SignedTx era) <-
    putTraceThrowEither
      . first TraceRpcEvalTxDecodingError
      . obtainCommonConstraints eon (deserialiseFromRawBytes asType)
      $ rawTx

  -- Each redeemer gets the full per-tx execution budget during evaluation, so an
  -- unbounded redeemer count lets an unauthenticated caller multiply evaluation cost.
  let redeemerCount =
        obtainCommonConstraints eon $
          Map.size . L.unRedeemers $
            ledgerTx ^. L.witsTxL . L.rdmrsTxWitsL
  when (redeemerCount > maxEvalRedeemers) $
    throwGrpcErrorWithMessage GrpcInvalidArgument $
      "too many redeemers: " <> tshow redeemerCount <> ", maximum " <> tshow maxEvalRedeemers

  let allInputs =
        obtainCommonConstraints eon $
          Set.map fromShelleyTxIn $
            ledgerTx ^. L.bodyTxL . L.allInputsTxBodyF

  let (unregStakeCreds, _unregDRepCreds, regPoolIds) =
        extractBalanceCheckCreds eon $ ledgerTx ^. L.bodyTxL
      apiStakeCreds = Set.map fromShelleyStakeCredential unregStakeCreds
      apiPoolIds = Set.map StakePoolKeyHash regPoolIds

  let target = VolatileTip
  (protocolParams, utxo, systemStart, eraHistory, stakeDelegDeposits, registeredPools) <-
    liftIO . (throwEither =<<) $
      executeLocalStateQueryExpr nodeConnInfo target $ do
        protocolParams <- throwEither =<< throwEither =<< queryProtocolParameters (convert eon)
        utxo <- throwEither =<< throwEither =<< queryUtxo (convert eon) (QueryUTxOByTxIn allInputs)
        systemStart <- throwEither =<< querySystemStart
        eraHistory <- throwEither =<< queryEraHistory
        stakeDelegDeposits <-
          throwEither =<< throwEither =<< queryStakeDelegDeposits (convert eon) apiStakeCreds
        registeredPools <- throwEither =<< throwEither =<< queryStakePoolParameters (convert eon) apiPoolIds
        pure
          (protocolParams, utxo, systemStart, eraHistory, stakeDelegDeposits, registeredPools)

  -- A tx that only needs to decode, not be valid, could otherwise be submitted for
  -- evaluation regardless of size; bound it to what could plausibly be submitted.
  let maxTxSize = fromIntegral $ protocolParams ^. L.ppMaxTxSizeL
  when (BS.length rawTx > maxTxSize) $
    throwGrpcErrorWithMessage GrpcInvalidArgument $
      "transaction size "
        <> tshow (BS.length rawTx)
        <> " exceeds the protocol maximum "
        <> tshow maxTxSize

  obtainCommonConstraints eon $ do
    let ledgerUtxo = toLedgerUTxO (convert eon) utxo
        epochInfo = toLedgerEpochInfo eraHistory
        poolIdSet = Map.keysSet registeredPools
        Exp.TxEvaluationResult fee evalUnits balance =
          Exp.evaluateTransaction
            systemStart
            epochInfo
            protocolParams
            poolIdSet
            stakeDelegDeposits
            ledgerUtxo
            ledgerTx
        redeemerData =
          Map.fromList
            [ ( toScriptIndex (convert eon) purpose
              ,
                ( fromPlutusData $ L.getPlutusData datum
                , L.serialize' (Exp.eraProtVerHigh eon) datum
                )
              )
            | (purpose, (datum, _exUnits)) <-
                toList . L.unRedeemers $ ledgerTx ^. L.witsTxL . L.rdmrsTxWitsL
            ]
        txEval = mkProtoTxEval fee evalUnits redeemerData
        balanceErrors
          | balance == mempty = []
          | otherwise =
              [ defMessage
                  & Proto.msg
                    .~ "Transaction is not balanced. Remaining balance (consumed - produced): "
                      <> tshow balance
              ]
        finalTxEval = txEval & Proto.errors %~ (<> balanceErrors)

    pure $ def & U5c.report . U5c.cardano .~ finalTxEval
 where
  putTraceThrowEither value = withFrozenCallStack $ do
    either putTrace (const $ pure ()) value
    throwEither value

-- | Coarse pre-decode bound (~4x the current mainnet maxTxSize): caps decode and
-- UTxO-query work for oversized requests. The exact protocol limit is enforced
-- after protocol parameters are fetched.
maxEvalTxSizeBytes :: Int
maxEvalTxSizeBytes = 65536

-- | Bounds attacker-controlled script evaluations per request: each redeemer may
-- consume the full per-tx execution budget.
maxEvalRedeemers :: Int
maxEvalRedeemers = 100

-- | Extract the credentials and pool IDs needed for balance check queries from
-- the transaction body certificates.
extractBalanceCheckCreds
  :: Era era
  -> L.TxBody l (ShelleyLedgerEra era)
  -> ( Set.Set (L.Credential L.Staking)
     , Set.Set (L.Credential L.DRepRole)
     , Set.Set (L.KeyHash L.StakePool)
     )
extractBalanceCheckCreds eon txBody =
  obtainCommonConstraints eon $ do
    let certs = toList $ txBody ^. L.certsTxBodyL
        unregStakeCreds =
          fromList $ mapMaybe L.lookupUnRegStakeTxCert certs
        unregDRepCreds =
          fromList $ map fst $ mapMaybe L.getUnRegDRepTxCert certs
        regPoolIds =
          fromList $ map L.sppId $ mapMaybe L.getRegPoolTxCert certs
    (unregStakeCreds, unregDRepCreds, regPoolIds)
