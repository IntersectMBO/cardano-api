{-# LANGUAGE FlexibleContexts #-}

-- | Client helpers for server-streaming gRPC calls.
module Cardano.Rpc.Client.Stream
  ( withServerStream
  )
where

import RIO
  ( MonadUnliftIO
  , Proxy
  , handle
  , liftIO
  , newIORef
  , readIORef
  , throwIO
  , unless
  , withRunInIO
  , writeIORef
  )

import Data.Default (Default, def)
import Network.GRPC.Client
import Network.GRPC.Spec
  ( GrpcError (..)
  , GrpcException (..)
  , Input
  , NextElem
  , Output
  , RequestMetadata
  , SupportsClientRpc
  )

-- | Run a server-streaming call and cancel it once the body returns.
--
-- Meant for streams that the server never ends, such as @followTip@ and @watchMempool@.
-- Since grapesy 1.2, leaving the call before the server's trailers arrive raises 'GrpcCancelled'
-- (<https://github.com/well-typed/grapesy/pull/372>).
-- That exception is ignored once the body has returned; a 'GrpcCancelled' raised earlier,
-- for example one sent by the server, is rethrown.
withServerStream
  :: (MonadUnliftIO m, SupportsClientRpc rpc, Default (RequestMetadata rpc))
  => Server
  -- ^ gRPC server to connect to
  -> Proxy rpc
  -- ^ RPC method, e.g. @Proxy \@(Protobuf SyncService "followTip")@
  -> Input rpc
  -- ^ request
  -> (m (NextElem (Output rpc)) -> m a)
  -- ^ body, reading as many stream elements as it needs
  -> m a
withServerStream server rpcProxy request body = do
  resultRef <- newIORef Nothing
  withRunInIO $ \runInIO ->
    handle (recoverResult resultRef) . withConnection def server $ \conn ->
      withRPC conn def rpcProxy $ \call -> do
        sendFinalInput call request
        result <- runInIO . body . liftIO $ recvNextOutputElem call
        writeIORef resultRef $ Just result
        pure result
 where
  -- The body has returned, so the cancellation of the still-open call is expected.
  recoverResult resultRef e = do
    unless (grpcError e == GrpcCancelled) $ throwIO e
    readIORef resultRef >>= either throwIO pure . maybe (Left e) Right
