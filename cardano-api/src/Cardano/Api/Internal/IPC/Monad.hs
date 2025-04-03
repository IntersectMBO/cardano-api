{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Cardano.Api.Internal.IPC.Monad
  ( LocalStateQueryExpr
  , executeLocalStateQueryExpr
  , queryExpr
  )
where

import Cardano.Api.Internal.Block
import Cardano.Api.Internal.IPC
import Cardano.Api.Internal.IPC.Version
import Cardano.Api.Internal.Query

import Cardano.Ledger.Shelley.Scripts ()
import Ouroboros.Network.Protocol.LocalStateQuery.Client qualified as Net.Query
import Ouroboros.Network.Protocol.LocalStateQuery.Type qualified as Net.Query

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Cont

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
  { runLocalStateQueryExpr
      :: ReaderT NodeToClientVersion (ContT (Net.Query.ClientStAcquired block point query m r) m) a
  }
  deriving (Functor, Applicative, Monad, MonadReader NodeToClientVersion, MonadIO)

-- | Execute a local state query expression.
executeLocalStateQueryExpr
  :: ()
  => LocalNodeConnectInfo
  -> Net.Query.Target ChainPoint
  -> LocalStateQueryExpr BlockInMode ChainPoint QueryInMode () IO a
  -> IO (Either AcquiringFailure a)
executeLocalStateQueryExpr connectInfo target f = do
  tmvResultLocalState <- newEmptyTMVarIO
  let waitResult = readTMVar tmvResultLocalState

  connectToLocalNodeWithVersion
    connectInfo
    ( \ntcVersion ->
        LocalNodeClientProtocols
          { localChainSyncClient = NoLocalChainSyncClient
          , localStateQueryClient =
              Just $ setupLocalStateQueryExpr waitResult target tmvResultLocalState ntcVersion f
          , localTxSubmissionClient = Nothing
          , localTxMonitoringClient = Nothing
          }
    )

  atomically waitResult

-- | Use 'queryExpr' in a do block to construct monadic local state queries.
setupLocalStateQueryExpr
  :: STM x
  -- ^ An STM expression that only returns when all protocols are complete.
  -- Protocols must wait until 'waitDone' returns because premature exit will
  -- cause other incomplete protocols to abort which may lead to deadlock.
  -> Net.Query.Target ChainPoint
  -> TMVar (Either AcquiringFailure a)
  -> NodeToClientVersion
  -> LocalStateQueryExpr BlockInMode ChainPoint QueryInMode () IO a
  -> Net.Query.LocalStateQueryClient BlockInMode ChainPoint QueryInMode IO ()
setupLocalStateQueryExpr waitDone mPointVar' resultVar' ntcVersion f =
  LocalStateQueryClient . pure . Net.Query.SendMsgAcquire mPointVar' $
    Net.Query.ClientStAcquiring
      { Net.Query.recvMsgAcquired = runContT (runReaderT (runLocalStateQueryExpr f) ntcVersion) $ \result -> do
          atomically $ putTMVar resultVar' (Right result)
          void $ atomically waitDone -- Wait for all protocols to complete before exiting.
          pure $ Net.Query.SendMsgRelease $ pure $ Net.Query.SendMsgDone ()
      , Net.Query.recvMsgFailure = \failure -> do
          atomically $ putTMVar resultVar' (Left (toAcquiringFailure failure))
          void $ atomically waitDone -- Wait for all protocols to complete before exiting.
          pure $ Net.Query.SendMsgDone ()
      }

-- | Get the node server's Node-to-Client version.
getNtcVersion :: LocalStateQueryExpr block point QueryInMode r IO NodeToClientVersion
getNtcVersion = LocalStateQueryExpr ask

-- | Use 'queryExpr' in a do block to construct monadic local state queries.
queryExpr
  :: QueryInMode a
  -> LocalStateQueryExpr block point QueryInMode r IO (Either UnsupportedNtcVersionError a)
queryExpr q = do
  ntcVersion <- getNtcVersion
  case isQuerySupportedInNtcVersion (toConsensusQuery q) ntcVersion of
    Right () ->
      fmap Right . LocalStateQueryExpr . ReaderT $ \_ -> ContT $ \f ->
        pure $
          Net.Query.SendMsgQuery q $
            Net.Query.ClientStQuerying
              { Net.Query.recvMsgResult = f
              }
    Left err -> pure $ Left err
