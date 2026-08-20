{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Rpc.Server.Internal.UtxoRpc.Query
  ( readParamsMethod
  , readUtxosMethod
  , searchUtxosMethod
  , readGenesisMethod
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
import Cardano.Rpc.Server.Internal.TimedCache (readThroughCache)
import Cardano.Rpc.Server.Internal.UtxoRpc.Predicate
import Cardano.Rpc.Server.Internal.UtxoRpc.Type
import Cardano.Rpc.Server.NodeKernelAccess

import Cardano.Crypto.Hash.Class qualified as Crypto (hashToBytes)
import Cardano.Ledger.Shelley.Genesis qualified as L (ShelleyGenesis, sgNetworkMagic)

import RIO hiding (toList)

import Control.Error.Util (hush)
import Data.Default
import Data.List (sortBy)
import Data.ProtoLens (defMessage)
import Data.Text qualified as Text (pack)
import Data.Time.Clock (UTCTime)
import GHC.IsList
import Network.GRPC.Spec
import System.FS.API (MountPoint (..), SomeHasFS (..))
import System.FS.IO (ioHasFS)
import System.FilePath (takeDirectory)

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
  (pparams, chainPoint, chainBlockNo, systemStart, eraHistory) <- liftIO . (throwEither =<<) $ executeLocalStateQueryExpr nodeConnInfo target $ do
    pparams <- throwEither =<< throwEither =<< queryProtocolParameters sbe
    chainPoint <- throwEither =<< queryChainPoint
    chainBlockNo <- throwEither =<< queryChainBlockNo
    systemStart <- throwEither =<< querySystemStart
    eraHistory <- throwEither =<< queryEraHistory
    pure (pparams, chainPoint, chainBlockNo, systemStart, eraHistory)

  timestamp <- slotToTimestamp systemStart eraHistory chainPoint

  pure $
    def
      & U5c.ledgerTip .~ mkChainPointMsg chainPoint chainBlockNo timestamp
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
      (utxo, chainPoint, chainBlockNo, systemStart, eraHistory) <- liftIO . (throwEither =<<) $ executeLocalStateQueryExpr nodeConnInfo target $ do
        utxo <- throwEither =<< throwEither =<< queryUtxo (convert eon) utxoFilter
        chainPoint <- throwEither =<< queryChainPoint
        chainBlockNo <- throwEither =<< queryChainBlockNo
        systemStart <- throwEither =<< querySystemStart
        eraHistory <- throwEither =<< queryEraHistory
        pure (utxo, chainPoint, chainBlockNo, systemStart, eraHistory)

      timestamp <- slotToTimestamp systemStart eraHistory chainPoint

      pure $
        defMessage
          & U5c.ledgerTip .~ mkChainPointMsg chainPoint chainBlockNo timestamp
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
  let mPredicate = req ^. U5c.maybe'predicate
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
  (utxo, chainPoint, chainBlockNo, systemStart, eraHistory) <- liftIO . (throwEither =<<) $ executeLocalStateQueryExpr nodeConnInfo target $ do
    utxo <- throwEither =<< throwEither =<< queryUtxo (convert eon) utxoFilter
    chainPoint <- throwEither =<< queryChainPoint
    chainBlockNo <- throwEither =<< queryChainBlockNo
    systemStart <- throwEither =<< querySystemStart
    eraHistory <- throwEither =<< queryEraHistory
    pure (utxo, chainPoint, chainBlockNo, systemStart, eraHistory)

  timestamp <- slotToTimestamp systemStart eraHistory chainPoint

  obtainCommonConstraints eon $ do
    let filtered =
          maybe id (\p -> filter $ matchesUtxoPredicate p . snd) mPredicate $
            toList utxo

    let (page, nextTok) = paginateByTxIn filtered startToken maxItems

    pure $
      defMessage
        & U5c.ledgerTip .~ mkChainPointMsg chainPoint chainBlockNo timestamp
        & U5c.items .~ map (uncurry txInTxOutToAnyUtxoData) page
        & U5c.maybe'nextToken .~ nextTok

-- | Handle the @ReadGenesis@ RPC method.
-- Returns the chain's identity - the Shelley genesis hash and the CAIP-2 chain
-- identifier - together with the @cardano@ config, the Byron, Shelley, Alonzo
-- and Conway genesis parameters mapped by 'genesisBundleToProto'.
--
-- The whole Shelley genesis comes from the bundle's cache, so the file is only
-- read on a cache miss. The @FAILED_PRECONDITION@ that
-- 'readShelleyGenesisWithInitialFunds' raises for a genesis file that has
-- changed since the node started is therefore raised on cache misses only: a
-- file edited while the cache is warm goes unnoticed until the cache next
-- empties, which is at most five idle minutes later.
readGenesisMethod
  :: MonadRpc e m
  => Proto UtxoRpc.ReadGenesisRequest
  -> m (Proto UtxoRpc.ReadGenesisResponse)
readGenesisMethod _req = do
  -- TODO: field masks are ignored for now (same as readParamsMethod)
  NodeKernelAccess
    { genesisConfig =
      genesisBundle@GenesisBundle
        { shelleyGenesisHash
        , shelleyGenesis = (shelleyGenesisFile, shelleyGenesisCache)
        }
    } <-
    grabNodeKernelAccess
  shelleyGenesis <-
    readThroughCache shelleyGenesisCache $
      readShelleyGenesisWithInitialFunds shelleyGenesisFile shelleyGenesisHash
  pure $
    defMessage
      & U5c.genesis .~ Crypto.hashToBytes (unGenesisHashShelley shelleyGenesisHash)
      & U5c.caip2 .~ networkMagicToCaip2 (L.sgNetworkMagic shelleyGenesis)
      & U5c.cardano .~ genesisBundleToProto genesisBundle shelleyGenesis

-- | Re-read the Shelley genesis file to recover the network's initial funds.
--
-- The genesis consensus keeps in memory is compacted, with the initial funds
-- erased, so the file is the only place they can come from.
readShelleyGenesisWithInitialFunds
  :: forall e m
   . MonadRpc e m
  => ShelleyGenesisFile In
  -- ^ Path to the Shelley genesis file, as the node was configured with it
  -> GenesisHashShelley
  -- ^ Blake2b-256 hash the node computed over that file at startup
  -> m L.ShelleyGenesis
readShelleyGenesisWithInitialFunds shelleyGenesisFile@(File path) bootGenesisHash = do
  -- 'readShelleyGenesis' is the node's own boot-time path: it reads the bytes,
  -- hashes them and checks them against the hash we pass in, then decodes.
  -- Running it at IO because its 'MonadIOTransError' needs a 'MonadCatch' that
  -- 'MonadRpc' does not provide.
  ShelleyConfig bootGenesis _ <-
    either rejectGenesisFile pure
      =<< liftIO (runExceptT (readShelleyGenesis shelleyGenesisFile (Just bootGenesisHash)))
  -- An injection file is named relative to the genesis file's own directory,
  -- which is where consensus mounts it when it injects the funds itself.
  let genesisDirectory = SomeHasFS . ioHasFS . MountPoint $ takeDirectory path
  either (rejectInitialFunds . displayException) pure
    =<< tryAny (liftIO $ resolveShelleyInitialFunds genesisDirectory bootGenesis)
 where
  -- Both helpers carry explicit signatures because their result type is
  -- polymorphic, which MonoLocalBinds would otherwise refuse to generalise on
  -- GHC 9.6 and 9.10.
  rejectGenesisFile :: ShelleyGenesisError -> m a
  rejectGenesisFile = \case
    -- Deliberately not 'renderShelleyGenesisError' for this one: its wording
    -- blames the hash given in the node's configuration file, whereas the hash
    -- we compare against is the one the node itself computed at startup.
    ShelleyGenesisHashMismatch{} ->
      throwGrpcErrorWithMessage GrpcFailedPrecondition $
        "The Shelley genesis file "
          <> tshow path
          <> " has changed since the node started, so it no longer describes the genesis the node is running on."
    err -> throwGrpcErrorWithMessage GrpcInternal $ renderShelleyGenesisError err

  rejectInitialFunds :: String -> m a
  rejectInitialFunds reason =
    throwGrpcErrorWithMessage GrpcInternal $
      "Cannot resolve the initial funds of the Shelley genesis file "
        <> tshow path
        <> ": "
        <> Text.pack reason

-- | The CAIP-2 chain identifier for a Cardano network, keyed on the Shelley
-- network magic.
-- This follows Dolos, the reference UTxO RPC implementation: the three
-- well-known networks get their conventional names, and any other network is
-- identified by its magic.
networkMagicToCaip2 :: Word32 -> Text
networkMagicToCaip2 = \case
  764824073 -> "cardano:mainnet"
  1 -> "cardano:preprod"
  2 -> "cardano:preview"
  magic -> "cardano:" <> tshow magic

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
