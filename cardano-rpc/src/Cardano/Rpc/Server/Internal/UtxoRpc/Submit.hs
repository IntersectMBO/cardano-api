{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
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

import Data.Default
import Network.GRPC.Spec

-- | Submit a CBOR-serialised list of transactions to the node
submitTxMethod
  :: MonadRpc e m
  => Proto UtxoRpc.SubmitTxRequest
  -> m (Proto UtxoRpc.SubmitTxResponse)
-- ^ A list of succeeded transaction ids or errors for failed ones
submitTxMethod req = do
  -- index transactions in the request
  let serialisedTxs = zip @Int [0 ..] $ req ^.. #tx . traverse . #raw

  nodeConnInfo <- grab
  AnyCardanoEra era <- liftIO . throwExceptT $ determineEra nodeConnInfo
  eon <- forEraInEon era (error "Minimum Shelley era required") pure

  -- try to submit each one consecutively
  submitResults <- forM serialisedTxs $ \(i, txBytes) -> do
    let eTx =
          first (("Failed to decode transaction with index " <> show i <> ": ") <>) $
            deserialiseTx eon txBytes

    eTxId <-
      fmap join . forM eTx $
        fmap
          ( first
              ( ("Failed to submit transaction with index " <> show i <> ": ")
                  <>
              )
          )
          . submitTx eon

    pure $ case eTxId of
      Left err -> def & #errorMessage .~ fromString err
      Right txId' -> def & #ref .~ serialiseToRawBytes txId'

  pure $ def & #results .~ submitResults
 where
  deserialiseTx :: ShelleyBasedEra era -> ByteString -> Either String (Tx era)
  deserialiseTx sbe = shelleyBasedEraConstraints sbe $ first show . deserialiseFromCBOR asType

  submitTx
    :: MonadRpc e m
    => ShelleyBasedEra era
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
