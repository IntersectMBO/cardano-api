{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Api.IPC.Monad
  ( LocalStateQueryExpr
  , executeLocalStateQueryExpr
  , queryExpr
  ) where

import           Cardano.Api.Block
import           Cardano.Api.Eon.ShelleyBasedEra
import           Cardano.Api.Eras.Core
import           Cardano.Api.IPC
import           Cardano.Api.IPC.Version
import           Cardano.Api.Modes
import           Cardano.Api.Query (toConsensusQuery)
import qualified Cardano.Api.ReexposeLedger as L

import           Cardano.Ledger.Shelley.Scripts ()
import qualified Ouroboros.Consensus.Byron.Ledger.Block as Consensus
import qualified Ouroboros.Consensus.Cardano as Consensus
import qualified Ouroboros.Consensus.Cardano.Block as Consensus
import qualified Ouroboros.Consensus.HardFork.Combinator as Consensus
import qualified Ouroboros.Consensus.HardFork.Combinator.Serialisation as Consensus
import qualified Ouroboros.Consensus.Ledger.Query as Consensus
import qualified Ouroboros.Consensus.Node.NetworkProtocolVersion as Consensus
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyNodeToClientVersion)
import qualified Ouroboros.Consensus.Shelley.Ledger as Consensus
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Client as Net.Query
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Codec as Consensus

import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Cont
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Proxy
import           Data.SOP.Strict (NP (..))

{- HLINT ignore "Use const" -}
{- HLINT ignore "Use let" -}

-- | Monadic type for constructing local state query expressions.
--
-- Use 'queryExpr' in a do block to construct queries of this type and convert
-- the expression to a 'Net.Query.LocalStateQueryClient' with 'setupLocalStateQueryExpr'.
--
-- Some consideration was made to use Applicative instead of Monad as the abstraction in
-- order to support pipelining, but we actually have a fair amount of code where the next
-- query depends on the result of the former and therefore actually need Monad.
--
-- In order to make pipelining still possible we can explore the use of Selective Functors
-- which would allow us to straddle both worlds.

newtype LocalStateQueryExpr block point query r m a = LocalStateQueryExpr
  { runLocalStateQueryExpr :: ReaderT QueryConnectionVersions (ContT (Net.Query.ClientStAcquired block point query m r) m) a
  } deriving (Functor, Applicative, Monad, MonadReader QueryConnectionVersions, MonadIO)

data QueryConnectionVersions = QueryConnectionVersions
  { ntcVersion :: NodeToClientVersion
  , shelleyNtcVersion :: ShelleyNodeToClientVersion
  , nodeEra :: AnyShelleyBasedEra
  }

-- | Execute a local state query expression.
executeLocalStateQueryExpr :: ()
  => LocalNodeConnectInfo
  -> Maybe ChainPoint
  -> LocalStateQueryExpr BlockInMode ChainPoint QueryInMode () IO a
  -> IO (Either AcquiringFailure a)
executeLocalStateQueryExpr connectInfo mpoint f = do
  tmvResultLocalState <- newEmptyTMVarIO
  let waitResult = readTMVar tmvResultLocalState

  connectToLocalNodeWithVersion
    connectInfo
    (\ntcVersion ->
      LocalNodeClientProtocols
      { localChainSyncClient    = NoLocalChainSyncClient
      , localStateQueryClient   = Just $ setupLocalStateQueryExpr waitResult mpoint tmvResultLocalState ntcVersion f
      , localTxSubmissionClient = Nothing
      , localTxMonitoringClient = Nothing
      }
    )

  atomically waitResult

-- | Use 'queryExpr' in a do block to construct monadic local state queries.
setupLocalStateQueryExpr ::
     STM x
     -- ^ An STM expression that only returns when all protocols are complete.
     -- Protocols must wait until 'waitDone' returns because premature exit will
     -- cause other incomplete protocols to abort which may lead to deadlock.
  -> Maybe ChainPoint
  -> TMVar (Either AcquiringFailure a)
  -> NodeToClientVersion
  -> LocalStateQueryExpr BlockInMode ChainPoint QueryInMode () IO a
  -> Net.Query.LocalStateQueryClient BlockInMode ChainPoint QueryInMode IO ()
setupLocalStateQueryExpr waitDone mPointVar' resultVar' ntcVersion f = do
  -- stub the version and era before we query the node for the correct values
  let queryConnectionVersions = QueryConnectionVersions ntcVersion maxBound maxBound
  LocalStateQueryClient . pure . Net.Query.SendMsgAcquire mPointVar' $
    Net.Query.ClientStAcquiring
    { Net.Query.recvMsgAcquired = runContT (runReaderT (runLocalStateQueryExpr queryWrapped) queryConnectionVersions) $ \result -> do
        atomically $ putTMVar resultVar' (Right result)
        void $ atomically waitDone -- Wait for all protocols to complete before exiting.
        pure $ Net.Query.SendMsgRelease $ pure $ Net.Query.SendMsgDone ()

    , Net.Query.recvMsgFailure = \failure -> do
        atomically $ putTMVar resultVar' (Left (toAcquiringFailure failure))
        void $ atomically waitDone -- Wait for all protocols to complete before exiting.
        pure $ Net.Query.SendMsgDone ()
    }
  where
    -- retrieve era from the node and set correct versions in reader for the query
    queryWrapped = do
      nodeEra@(AnyShelleyBasedEra currentEra) <- queryExpr QueryCurrentEra >>= \case
        Right (AnyCardanoEra e) -> inEonForEra (error "byron not supported") (pure . AnyShelleyBasedEra) e
        Left _ -> error "Impossible! QueryCurrentEra is always supported."

      let shelleyNtcVersion = getSupportedShelleyNtcVersion currentEra ntcVersion supportedNodeToClientVersions

      -- run the queries with the correct version and era
      local (\r -> r {shelleyNtcVersion, nodeEra}) f

-- | Get the node server's Node-to-Client version.
getNtcVersion :: LocalStateQueryExpr block point QueryInMode r m NodeToClientVersion
getNtcVersion = asks ntcVersion

getShelleyNtcVersion :: LocalStateQueryExpr block point QueryInMode r m ShelleyNodeToClientVersion
getShelleyNtcVersion = asks shelleyNtcVersion

supportedNodeToClientVersions
  :: M.Map
      NodeToClientVersion
      (Consensus.HardForkNodeToClientVersion
        (Consensus.ByronBlock : Consensus.CardanoShelleyEras Consensus.StandardCrypto))
supportedNodeToClientVersions = Consensus.supportedNodeToClientVersions @(Consensus.CardanoBlock L.StandardCrypto) Proxy


-- | Use 'queryExpr' in a do block to construct monadic local state queries.
queryExpr :: QueryInMode a -> LocalStateQueryExpr block point QueryInMode r IO (Either UnsupportedNtcVersionError a)
queryExpr QueryCurrentEra = do
  -- Assuming an era cannot change during the single connection, use memoized value
  AnyShelleyBasedEra sbe <- asks nodeEra
  pure . pure $ AnyCardanoEra (shelleyBasedToCardanoEra sbe)

queryExpr q = do
  let minNtcVersion = nodeToClientVersionOf q
  ntcVersion <- getNtcVersion
  shelleyNtcVersion <- getShelleyNtcVersion

  case isQuerySupprted shelleyNtcVersion q of
    Left e -> pure $ Left e
    Right isShelleyNtcSupported
      | ntcVersion >= minNtcVersion && isShelleyNtcSupported ->
          fmap Right . LocalStateQueryExpr . ReaderT $ \_ -> ContT $ \f -> pure $
            Net.Query.SendMsgQuery q $
              Net.Query.ClientStQuerying
              { Net.Query.recvMsgResult = f
              }
      | otherwise -> pure . Left $ UnsupportedNtcVersionError minNtcVersion ntcVersion

-- | Check if query is supported in the current Shelley NTC
isQuerySupprted :: Consensus.ShelleyNodeToClientVersion
                -> QueryInMode a
                -> Either UnsupportedNtcVersionError Bool
isQuerySupprted shelleyVersion query =
  case toConsensusQuery query of
    Consensus.Some Consensus.GetSystemStart -> pure $ shelleyVersion >= Consensus.ShelleyNodeToClientVersion3
    Consensus.Some Consensus.GetChainBlockNo -> pure $ shelleyVersion >= Consensus.ShelleyNodeToClientVersion4
    Consensus.Some Consensus.GetChainPoint -> pure $ shelleyVersion >= Consensus.ShelleyNodeToClientVersion4
    Consensus.Some (Consensus.BlockQuery bq) ->
      case bq of
        Consensus.QueryHardFork Consensus.GetInterpreter -> pure True
        Consensus.QueryHardFork Consensus.GetCurrentEra -> pure True
        Consensus.QueryIfCurrentByron _ -> pure True
        Consensus.QueryIfCurrentShelley bsq -> pure $ Consensus.querySupportedVersion bsq shelleyVersion
        Consensus.QueryIfCurrentAllegra bsq -> pure $ Consensus.querySupportedVersion bsq shelleyVersion
        Consensus.QueryIfCurrentMary bsq -> pure $ Consensus.querySupportedVersion bsq shelleyVersion
        Consensus.QueryIfCurrentAlonzo bsq -> pure $ Consensus.querySupportedVersion bsq shelleyVersion
        Consensus.QueryIfCurrentBabbage bsq -> pure $ Consensus.querySupportedVersion bsq shelleyVersion
        Consensus.QueryIfCurrentConway bsq -> pure $ Consensus.querySupportedVersion bsq shelleyVersion
        -- this error will happen when a new era or a new type of block query is added
        -- in that case add a similar case to the ones above
        otherQuery -> Left $ UnsupportedBlockQuery (show otherQuery)

-- | Retrieve the supported shelley node-to-client version from the mapping
-- A node-to-client (NTC) version can be mapped to multiple values of Shelley NTC. It is
-- Shelley NTC which is used to check if the query can be used in the current era.
--
-- If the hard fork combinator is disabled, fall back to using 'maxBound' to allow all queries.
getSupportedShelleyNtcVersion
  :: ShelleyBasedEra era -- ^ node era
  -> NodeToClientVersion -- ^ protocol version
  -> M.Map
    NodeToClientVersion
    (Consensus.HardForkNodeToClientVersion
       (Consensus.ByronBlock
          : Consensus.CardanoShelleyEras Consensus.StandardCrypto))
          -- ^ mapping of NTC to Shelley NTC by era
  -> Consensus.ShelleyNodeToClientVersion
getSupportedShelleyNtcVersion sbe ntcVersion supportedVersions = fromMaybe maxBound $ do
  M.lookup ntcVersion supportedVersions >>= \case
    Consensus.HardForkNodeToClientDisabled _ -> Nothing
    Consensus.HardForkNodeToClientEnabled _ np -> getEraShelleyNtcVersion sbe np

-- | Get Shelley NTC version for an era
getEraShelleyNtcVersion
  :: ShelleyBasedEra era
  -> NP Consensus.EraNodeToClientVersion (Consensus.CardanoEras Consensus.StandardCrypto)
    -- ^ a product of all Shelley NTC versions for all eras
  -> Maybe Consensus.ShelleyNodeToClientVersion
getEraShelleyNtcVersion sbe (_byronV :* shelleyV :* allegraV :* maryV :* alonzoV :* babbageV :* conwayV :* Nil) =
  case sbe of
    ShelleyBasedEraShelley -> conv sbe shelleyV
    ShelleyBasedEraAllegra -> conv sbe allegraV
    ShelleyBasedEraMary -> conv sbe maryV
    ShelleyBasedEraAlonzo -> conv sbe alonzoV
    ShelleyBasedEraBabbage -> conv sbe babbageV
    ShelleyBasedEraConway -> conv sbe conwayV
  where
    conv :: ( Consensus.BlockNodeToClientVersion blk ~ Consensus.ShelleyNodeToClientVersion
            , blk ~ ConsensusBlockForEra era )
         => ShelleyBasedEra era
         -> Consensus.EraNodeToClientVersion blk
         -> Maybe Consensus.ShelleyNodeToClientVersion
    conv _ = \case
      Consensus.EraNodeToClientDisabled -> Nothing
      Consensus.EraNodeToClientEnabled x -> Just x

