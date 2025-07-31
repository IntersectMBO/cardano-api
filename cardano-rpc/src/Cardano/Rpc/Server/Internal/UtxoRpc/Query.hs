{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Rpc.Server.Internal.UtxoRpc.Query
  ( readParamsMethod
  , readUtxosMethod
  )
where

import Cardano.Api
import Cardano.Api.Parser.Text qualified as P
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as UtxoRpc
import Cardano.Rpc.Server.Internal.Error
import Cardano.Rpc.Server.Internal.Monad
import Cardano.Rpc.Server.Internal.Orphans ()
import Cardano.Rpc.Server.Internal.UtxoRpc.Type

import RIO hiding (toList)

import Data.Default
import Data.ProtoLens (defMessage)
import Data.Text.Encoding qualified as T
import GHC.IsList
import Network.GRPC.Spec

readParamsMethod
  :: MonadRpc e m
  => Proto UtxoRpc.ReadParamsRequest
  -> m (Proto UtxoRpc.ReadParamsResponse)
readParamsMethod _req = do
  -- TODO: implement field masks - they are ignored for now
  -- they need to be normalised beforehand, see: https://github.com/protocolbuffers/protobuf/blob/main/java/util/src/main/java/com/google/protobuf/util/FieldMaskTree.java#L76
  -- let fieldMask :: [Text] = req ^. #fieldMask . #paths
  nodeConnInfo <- grab
  AnyCardanoEra era <- liftIO . throwExceptT $ determineEra nodeConnInfo
  eon <- forEraInEon era (error "Minimum Conway era required") pure
  let sbe = convert eon

  let target = VolatileTip
  (pparams, chainPoint, blockNo) <- liftIO . (throwEither =<<) $ executeLocalStateQueryExpr nodeConnInfo target $ do
    pparams <- throwEither =<< throwEither =<< queryProtocolParameters sbe
    chainPoint <- throwEither =<< queryChainPoint
    blockNo <- throwEither =<< queryChainBlockNo
    pure (pparams, chainPoint, blockNo)

  pure $
    def
      & #ledgerTip .~ mkChainPointMsg chainPoint blockNo
      & #values . #cardano .~ conwayEraOnwardsConstraints eon (inject pparams)

readUtxosMethod
  :: MonadRpc e m
  => Proto UtxoRpc.ReadUtxosRequest
  -> m (Proto UtxoRpc.ReadUtxosResponse)
readUtxosMethod req = do
  utxoFilter <-
    if
      | Just txoRefs <- req ^. #maybe'txoRefs ->
          QueryUTxOByTxIn . fromList <$> mapM txoRefToTxIn (txoRefs ^. #items)
      | Just addressesProto <- req ^. #maybe'addresses ->
          QueryUTxOByAddress . fromList <$> mapM readAddress (addressesProto ^. #items)
      | otherwise -> pure QueryUTxOWhole

  nodeConnInfo <- grab
  AnyCardanoEra era <- liftIO . throwExceptT $ determineEra nodeConnInfo
  eon <- forEraInEon era (error "Minimum Shelley era required") pure

  let target = VolatileTip
  (utxo, chainPoint, blockNo) <- liftIO . (throwEither =<<) $ executeLocalStateQueryExpr nodeConnInfo target $ do
    utxo <- throwEither =<< throwEither =<< queryUtxo eon utxoFilter
    chainPoint <- throwEither =<< queryChainPoint
    blockNo <- throwEither =<< queryChainBlockNo
    pure (utxo, chainPoint, blockNo)

  pure $
    defMessage
      & #ledgerTip .~ mkChainPointMsg chainPoint blockNo
      & #items .~ cardanoEraConstraints era (inject utxo)
 where
  txoRefToTxIn :: MonadRpc e m => Proto UtxoRpc.TxoRef -> m TxIn
  txoRefToTxIn r = do
    txId' <- throwEither $ deserialiseFromRawBytes AsTxId $ r ^. #hash
    pure $ TxIn txId' (TxIx . fromIntegral $ r ^. #index)

  readAddress :: MonadRpc e m => ByteString -> m AddressAny
  readAddress =
    throwEither . first stringException . P.runParser parseAddressAny <=< throwEither . T.decodeUtf8'
