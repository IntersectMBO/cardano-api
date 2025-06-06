{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Cardano.Api.Network.IPC.Internal.Monad
  ( LocalStateQueryExpr
  , executeLocalStateQueryExpr
  , queryExpr
  )
where

import Cardano.Api.Block
import Cardano.Api.Network.IPC.Internal
import Cardano.Api.Network.IPC.Internal.Version
import Cardano.Api.Query.Internal.Type.QueryInMode

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
      { Net.Query.recvMsgAcquired =
          let allQueries = runReaderT (runLocalStateQueryExpr f) ntcVersion
           in runContT allQueries finalContinuation
      , Net.Query.recvMsgFailure = \failure -> do
          atomically $ putTMVar resultVar' (Left (toAcquiringFailure failure))
          void $ atomically waitDone -- Wait for all protocols to complete before exiting.
          pure $ Net.Query.SendMsgDone ()
      }
 where
  -- We wait for all queries to finish before exiting.
  finalContinuation result = do
    atomically $ putTMVar resultVar' (Right result)
    void $ atomically waitDone -- Wait for all protocols to complete before exiting.
    pure $ Net.Query.SendMsgRelease $ pure $ Net.Query.SendMsgDone ()

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
      fmap Right . LocalStateQueryExpr . ReaderT $ \_ -> constructQueryContinuation q
    Left err -> pure $ Left err

{-  The client sends a query with the following data constructor:

data ClientStAcquired block point query m a where
  SendMsgQuery     :: query result
                   -> ClientStQuerying block point query m a result
                   -> ClientStAcquired block point query m a

The client is then awaiting a result from the server which is represented by:

data ClientStQuerying block point query m a result = ClientStQuerying {
      recvMsgResult :: result -> m (ClientStAcquired block point query m a)
    }

When constructing the `ClientStQuerying` value we can send another query (`SendMsgQuery`) or
release (`SendMsgRelease`) and this recursion is nicely modelled with the `ContT` monad transformer.

The final continuation in our case is waiting for all the queries to be returned and then returning
`SendMsgRelease`.
-}
constructQueryContinuation
  :: Applicative m
  => QueryInMode result
  -> ContT
       (Net.Query.ClientStAcquired block point QueryInMode m a)
       m
       result
constructQueryContinuation q = do
  ContT $ \final ->
    pure $
      Net.Query.SendMsgQuery q $
        Net.Query.ClientStQuerying
          { Net.Query.recvMsgResult = final
          }
