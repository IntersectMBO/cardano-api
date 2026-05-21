{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Rpc.Server.Internal.UtxoRpc.Query
  ( readParamsMethod
  , readUtxosMethod
  , searchUtxosMethod
  , paginateByTxIn
  )
where

import Cardano.Api
import Cardano.Api.Experimental.Era
import Cardano.Api.Parser.Text qualified as P
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as U5c
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as UtxoRpc
import Cardano.Rpc.Server.Internal.Error
import Cardano.Rpc.Server.Internal.Monad
import Cardano.Rpc.Server.Internal.Orphans ()
import Cardano.Rpc.Server.Internal.UtxoRpc.Predicate
import Cardano.Rpc.Server.Internal.UtxoRpc.Type

import RIO hiding (toList)

import Control.Error.Util (hush)
import Data.Default
import Data.List (sortBy)
import Data.ProtoLens (defMessage)
import Data.Time.Clock (UTCTime)
import GHC.IsList
import Network.GRPC.Spec

-- | Handle the @ReadParams@ RPC method.
-- Queries the node for current protocol parameters and returns them
-- along with the ledger tip.
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
  eon <- forEraInEon @Era era (error "Minimum Conway era required") pure
  let sbe = convert eon

  let target = VolatileTip
  (pparams, chainPoint, blockNo, systemStart, eraHistory) <- liftIO . (throwEither =<<) $ executeLocalStateQueryExpr nodeConnInfo target $ do
    pparams <- throwEither =<< throwEither =<< queryProtocolParameters sbe
    chainPoint <- throwEither =<< queryChainPoint
    blockNo <- throwEither =<< queryChainBlockNo
    systemStart <- throwEither =<< querySystemStart
    eraHistory <- throwEither =<< queryEraHistory
    pure (pparams, chainPoint, blockNo, systemStart, eraHistory)

  timestamp <- slotToTimestamp systemStart eraHistory chainPoint

  pure $
    def
      & U5c.ledgerTip .~ mkChainPointMsg chainPoint blockNo timestamp
      & U5c.values . U5c.cardano .~ obtainCommonConstraints eon (protocolParamsToUtxoRpcPParams eon pparams)

-- | Handle the @ReadUtxos@ RPC method.
-- Looks up specific UTxO entries by their 'TxIn' keys and returns them
-- along with the ledger tip.
-- Returns an empty response when no keys are provided, matching other
-- UTxO RPC implementations (Dolos, cardano-node-api, Dingo).
readUtxosMethod
  :: MonadRpc e m
  => Proto UtxoRpc.ReadUtxosRequest
  -> m (Proto UtxoRpc.ReadUtxosResponse)
readUtxosMethod req
  | null $ req ^. U5c.keys = pure defMessage
  | otherwise = do
      utxoFilter <- QueryUTxOByTxIn . fromList <$> mapM txoRefToTxIn (req ^. U5c.keys)

      nodeConnInfo <- grab
      AnyCardanoEra era <- liftIO . throwExceptT $ determineEra nodeConnInfo
      eon <- forEraInEon @Era era (error "Minimum Conway era required") pure

      let target = VolatileTip
      (utxo, chainPoint, blockNo, systemStart, eraHistory) <- liftIO . (throwEither =<<) $ executeLocalStateQueryExpr nodeConnInfo target $ do
        utxo <- throwEither =<< throwEither =<< queryUtxo (convert eon) utxoFilter
        chainPoint <- throwEither =<< queryChainPoint
        blockNo <- throwEither =<< queryChainBlockNo
        systemStart <- throwEither =<< querySystemStart
        eraHistory <- throwEither =<< queryEraHistory
        pure (utxo, chainPoint, blockNo, systemStart, eraHistory)

      timestamp <- slotToTimestamp systemStart eraHistory chainPoint

      pure $
        defMessage
          & U5c.ledgerTip .~ mkChainPointMsg chainPoint blockNo timestamp
          & U5c.items .~ obtainCommonConstraints eon (utxoToUtxoRpcAnyUtxoData utxo)
 where
  txoRefToTxIn :: MonadRpc e m => Proto UtxoRpc.TxoRef -> m TxIn
  txoRefToTxIn r = do
    txId' <- throwEither $ deserialiseFromRawBytes AsTxId $ r ^. U5c.hash
    pure $ TxIn txId' (TxIx . fromIntegral $ r ^. U5c.index)

-- | Handle the @SearchUtxos@ RPC method.
-- Filters the UTxO set by a predicate and returns a paginated result.
-- The predicate must contain exact address matches so the query can be
-- narrowed; broad predicates are rejected with @INVALID_ARGUMENT@.
searchUtxosMethod
  :: MonadRpc e m
  => Proto UtxoRpc.SearchUtxosRequest
  -> m (Proto UtxoRpc.SearchUtxosResponse)
searchUtxosMethod req = do
  -- TODO: field masks are ignored for now (same as readParamsMethod)
  let mPredicate = fmap getProto $ req ^. U5c.maybe'predicate
      maxItems = req ^. U5c.maxItems
      startToken = req ^. U5c.maybe'startToken

  utxoFilter <- case mPredicate >>= extractAddressesFromPredicate of
    Just addrs -> pure $ QueryUTxOByAddress addrs
    Nothing ->
      throwGrpcErrorWithMessage
        GrpcInvalidArgument
        "predicate too broad: must contain exact address match to avoid fetching the entire UTxO set"

  nodeConnInfo <- grab
  AnyCardanoEra era <- liftIO . throwExceptT $ determineEra nodeConnInfo
  eon <- forEraInEon @Era era (error "Minimum Conway era required") pure

  let target = VolatileTip
  (utxo, chainPoint, blockNo, systemStart, eraHistory) <- liftIO . (throwEither =<<) $ executeLocalStateQueryExpr nodeConnInfo target $ do
    utxo <- throwEither =<< throwEither =<< queryUtxo (convert eon) utxoFilter
    chainPoint <- throwEither =<< queryChainPoint
    blockNo <- throwEither =<< queryChainBlockNo
    systemStart <- throwEither =<< querySystemStart
    eraHistory <- throwEither =<< queryEraHistory
    pure (utxo, chainPoint, blockNo, systemStart, eraHistory)

  timestamp <- slotToTimestamp systemStart eraHistory chainPoint

  obtainCommonConstraints eon $ do
    let filtered =
          maybe id (\p -> filter $ matchesUtxoPredicate p . snd) mPredicate $
            toList utxo

    let (page, nextTok) = paginateByTxIn filtered startToken maxItems

    pure $
      defMessage
        & U5c.ledgerTip .~ mkChainPointMsg chainPoint blockNo timestamp
        & U5c.items .~ map (uncurry txInTxOutToAnyUtxoData) page
        & U5c.maybe'nextToken .~ nextTok

-- | Paginate a list of UTxO entries using cursor-based pagination.
-- Items are sorted by 'TxIn'\'s 'Ord' instance (lexicographic on 'TxId', then numeric on 'TxIx').
-- The start token is the 'renderTxIn' of the last item on the previous page;
-- all items up to and including it are skipped, so the next page begins
-- immediately after that cursor.
paginateByTxIn
  :: [(TxIn, a)]
  -- ^ UTxO entries to paginate
  -> Maybe Text
  -- ^ start token: the 'renderTxIn' of the last 'TxIn' from the previous page,
  -- or 'Nothing' for the first page
  -> Int32
  -- ^ maximum number of items per page (0 defaults to 'defaultPageSize',
  -- capped at 'maxPageSize')
  -> ([(TxIn, a)], Maybe Text)
  -- ^ page of results and the next start token ('Nothing' when there are no more pages)
paginateByTxIn items startToken maxItems = (page, nextToken)
 where
  sorted = sortBy (compare `on` fst) items
  afterToken = maybe sorted dropAfterCursor $ hush . P.runParser parseTxIn =<< startToken
  dropAfterCursor cursor = dropWhile (\(txIn, _) -> txIn <= cursor) sorted
  limit = min (if maxItems > 0 then fromIntegral maxItems else defaultPageSize) maxPageSize
  page = take limit afterToken
  hasMore = not . null $ drop limit afterToken
  nextToken = do
    guard hasMore
    pure . renderTxIn . fst $ last page
  defaultPageSize = 100
  maxPageSize = 10_000

slotToTimestamp
  :: HasCallStack
  => MonadIO m
  => SystemStart -> EraHistory -> ChainPoint -> m UTCTime
slotToTimestamp systemStart eraHistory = \case
  ChainPointAtGenesis ->
    let SystemStart t = systemStart in pure t
  ChainPoint slotNo _ ->
    throwEither $ slotToUTCTime systemStart eraHistory slotNo
