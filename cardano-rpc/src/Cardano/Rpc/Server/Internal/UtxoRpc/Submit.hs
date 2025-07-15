{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Rpc.Server.Internal.UtxoRpc.Submit
  ( submitTxMethod
  )
where

import Cardano.Api
import Cardano.Api.Network.IPC qualified as Net.Tx
import Cardano.Rpc.Proto.Api.UtxoRpc.Submit qualified as UtxoRpc
import Cardano.Rpc.Server.Internal.Error
import Cardano.Rpc.Server.Internal.Monad
import Cardano.Rpc.Server.Internal.Orphans ()

import RIO hiding (toList)

import Data.ProtoLens (defMessage)
import Network.GRPC.Spec

submitTxMethod
  :: forall e m
   . MonadRpc e m
  => Proto UtxoRpc.SubmitTxRequest
  -> m (Proto UtxoRpc.SubmitTxResponse)
submitTxMethod req = do
  let serialisedTxs = zip @Int [0 ..] $ req ^.. #tx . traverse . #raw

  nodeConnInfo <- grab
  AnyCardanoEra era <- liftIO . throwExceptT $ determineEra nodeConnInfo
  eon <- forEraInEon era (error "Minimum Shelley era required") pure

  let (failedTxs, txs) = partitionEithers $ serialisedTxs <&> \(i, tx) -> bimap (i,) (i,) $ deserialiseTx eon tx

  -- TODO failures need to be included in the returned type, and not dumped to node logs
  forM_ failedTxs $ \(i, err) -> do
    putTrace $ "Failed to decode transaction with index: " <> show i <> " / " <> err

  (failedSubmissionTxs, txIds) <- fmap partitionEithers . forM txs $ \(i, tx) -> bimap (i,) (i,) <$> submitTx eon tx

  -- TODO failures need to be included in the returned type, and not dumped to node logs
  forM_ failedSubmissionTxs $ \(i, err) -> do
    putTrace $ "Failed to submit transaction with index: " <> show i <> " / " <> err

  -- TODO: so now, to check if the submission has succeeded, one has to check if the TxId is in the returned list.
  pure $ defMessage & #ref .~ map (serialiseToRawBytes . snd) txIds
 where
  deserialiseTx :: ShelleyBasedEra era -> ByteString -> Either String (Tx era)
  deserialiseTx sbe = shelleyBasedEraConstraints sbe $ first show . deserialiseFromCBOR asType

  submitTx
    :: ShelleyBasedEra era
    -> Tx era
    -> m (Either String TxId)
  submitTx sbe tx = do
    nodeConnInfo <- grab
    eRes <-
      tryAny $
        submitTxToNodeLocal nodeConnInfo (TxInMode sbe tx) >>= \case
          Net.Tx.SubmitFail reason -> pure . Left $ show reason
          Net.Tx.SubmitSuccess -> pure $ Right . getTxId $ getTxBody tx
    case eRes of
      Left err -> do
        let errString = displayException err
        putTrace $ "N2C connection error while trying to submit a transaction: " <> errString
        pure $ Left errString
      Right res -> pure res
