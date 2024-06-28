{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.Query.New.ShelleyBased.Expr
  ( ShelleyBasedQueryError(..)
  , executeLocalStateQueryExprShelleyBased
  , queryExprShelleyBased
  ) where

import           Cardano.Api.Block
import           Cardano.Api.IPC
import           Cardano.Api.IPC.Monad
import           Cardano.Api.IPC.Version
import           Cardano.Api.Query
import           Cardano.Api.Query.New.EraIndependent.Query
import           Cardano.Api.Query.New.ShelleyBased.Query

import           Cardano.Ledger.Shelley.Scripts ()
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Client as Net.Query
import           Ouroboros.Network.Protocol.LocalStateQuery.Type (Target (..))

import           Control.Concurrent.STM
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Cont
import           Control.Monad.Trans.Except.Extra (left)
import           Data.Functor


{- HLINT ignore "Use const" -}
{- HLINT ignore "Use let" -}


-- We introduce this type because currently the result type of
-- `LocalStateQueryExpr` can contain an error. We want to separate
-- the errors out of the result type `a` and into the error type `e`.
-- This will come about by a redefining of the query related data types.
type LocalStateQueryExprWithError e block point query r m a
  = ExceptT e (LocalStateQueryExpr block point query r m) a



-- | Execute a local state query expression.
executeLocalStateQueryExprShelleyBased
  :: LocalNodeConnectInfo
  -> Target ChainPoint
  -> LocalStateQueryExprWithError ShelleyBasedQueryError BlockInMode ChainPoint (QueryShelleyBasedEra era) () IO a
  -> IO (Either ShelleyBasedQueryError a)
executeLocalStateQueryExprShelleyBased connectInfo target f = do
  tmvResultLocalState <- newEmptyTMVarIO
  let waitResult = readTMVar tmvResultLocalState

  connectToLocalNodeWithVersion
    connectInfo
    (\ntcVersion ->
      LocalNodeClientProtocols
      { localChainSyncClient    = NoLocalChainSyncClient
      , localStateQueryClient   = Just $ setupLocalStateQueryExpr waitResult target tmvResultLocalState ntcVersion f
      , localTxSubmissionClient = Nothing
      , localTxMonitoringClient = Nothing
      }
    )

  atomically waitResult


-- | Get the node server's Node-to-Client version.
getNtcVersion :: LocalStateQueryExpr block point (QueryShelleyBasedEra era) r IO NodeToClientVersion
getNtcVersion = LocalStateQueryExpr ask


-- | Use 'queryExprShelleyBased' in a do block to construct monadic local state queries.
queryExprShelleyBased
  :: QueryShelleyBasedEra era a
  -> LocalStateQueryExprWithError ShelleyBasedQueryError block point (QueryShelleyBasedEra era) r IO a
queryExprShelleyBased query = do
  let minNtcVersion = nodeToClientVersionOf query
  ntcVersion <- lift getNtcVersion
  if ntcVersion >= minNtcVersion
    then
      lift . LocalStateQueryExpr . ReaderT $ \_ -> ContT $ \f -> pure $
        Net.Query.SendMsgQuery query $
          Net.Query.ClientStQuerying
          { Net.Query.recvMsgResult = f
          }
    else left $ ShelleyBasedEquerySimpleError $ IndependentEraQueryUnsupportedVersion $ UnsupportedNtcVersionError minNtcVersion ntcVersion

-- | Use 'queryExprShelleyBased' in a do block to construct monadic local state queries.
setupLocalStateQueryExpr ::
     STM x
     -- ^ An STM expression that only returns when all protocols are complete.
     -- Protocols must wait until 'waitDone' returns because premature exit will
     -- cause other incomplete protocols to abort which may lead to deadlock.
  -> Target ChainPoint
  -> TMVar (Either ShelleyBasedQueryError a)
  -> NodeToClientVersion
  -> LocalStateQueryExprWithError ShelleyBasedQueryError BlockInMode ChainPoint (QueryShelleyBasedEra era) () IO a
  -> Net.Query.LocalStateQueryClient BlockInMode ChainPoint (QueryShelleyBasedEra era) IO ()
setupLocalStateQueryExpr waitDone target resultVar' ntcVersion f =
  LocalStateQueryClient . pure . Net.Query.SendMsgAcquire target $
    Net.Query.ClientStAcquiring
    { Net.Query.recvMsgAcquired =
      runContT (runReaderT (runLocalStateQueryExpr $ runExceptT f) ntcVersion) $ \result -> do
        atomically $ putTMVar resultVar' result
        void $ atomically waitDone -- Wait for all protocols to complete before exiting.
        pure $ Net.Query.SendMsgRelease $ pure $ Net.Query.SendMsgDone ()

    , Net.Query.recvMsgFailure = \failure -> do
        atomically $ putTMVar resultVar' (Left . ShelleyBasedEquerySimpleError . IndependentEraQueryAcquiringFail $ toAcquiringFailure failure)
        void $ atomically waitDone -- Wait for all protocols to complete before exiting.
        pure $ Net.Query.SendMsgDone ()
    }
