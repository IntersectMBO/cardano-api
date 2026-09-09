{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Handler for the gRPC Server Reflection API
-- (<https://github.com/grpc/grpc/blob/master/doc/server-reflection.md>),
-- @grpc.reflection.v1@ and the older @grpc.reflection.v1alpha@. This lets
-- generic clients (e.g. @grpcurl@) discover and decode this server's proto
-- services without a local copy of the @.proto@ files.
module Cardano.Rpc.Server.Internal.Reflection
  ( serverReflectionInfoMethodV1
  , serverReflectionInfoMethodV1alpha
  , answerReflectionRequest
  , qualifiedServiceName
  )
where

import Cardano.Rpc.Proto.Api.Reflection.V1 qualified as V1
import Cardano.Rpc.Proto.Api.Reflection.V1alpha qualified as V1alpha
import Cardano.Rpc.Server.Internal.Error (throwGrpcErrorWithMessage)
import Cardano.Rpc.Server.Internal.Reflection.DescriptorTable

import RIO

import Data.ProtoLens (Message, decodeMessage, defMessage, encodeMessage)
import Data.ProtoLens.Service.Types (Service, ServiceName, ServicePackage)
import Data.Text qualified as Text
import GHC.TypeLits (symbolVal)
import Network.GRPC.Spec
  ( GrpcError (GrpcInternal, GrpcInvalidArgument, GrpcNotFound)
  , NextElem (NextElem, NoNextElem)
  , Proto (Proto)
  , fromGrpcError
  )

-- | Handle the @ServerReflectionInfo@ bidirectional stream for
-- @grpc.reflection.v1@: answer every request on the incoming stream in
-- turn, then forward the client's own terminal marker. A bidi handler that
-- returns without sending 'NoNextElem' itself has its stream cancelled
-- instead of closed with trailers, the same requirement as for
-- server-streaming handlers (both go through grapesy's identical
-- @sendOutput call . fromNextElem call@ path).
serverReflectionInfoMethodV1
  :: MonadIO m
  => [Text]
  -- ^ Fully qualified names of every service registered with this server,
  -- answered verbatim for @list_services@
  -> IO (NextElem (Proto V1.ServerReflectionRequest))
  -> (NextElem (Proto V1.ServerReflectionResponse) -> IO ())
  -> m ()
serverReflectionInfoMethodV1 serviceNames recv send = liftIO loop
 where
  loop =
    recv >>= \case
      NoNextElem -> send NoNextElem
      NextElem request -> do
        send . NextElem $ answerReflectionRequest descriptorTable serviceNames request
        loop

-- | Handle the same stream for the legacy @grpc.reflection.v1alpha@, by
-- bridging each message to and from @v1@ and answering with the one core
-- 'answerReflectionRequest'.
serverReflectionInfoMethodV1alpha
  :: MonadIO m
  => [Text]
  -- ^ Fully qualified names of every service registered with this server,
  -- answered verbatim for @list_services@
  -> IO (NextElem (Proto V1alpha.ServerReflectionRequest))
  -> (NextElem (Proto V1alpha.ServerReflectionResponse) -> IO ())
  -> m ()
serverReflectionInfoMethodV1alpha serviceNames recv send = liftIO loop
 where
  loop =
    recv >>= \case
      NoNextElem -> send NoNextElem
      NextElem request -> do
        v1Request <- bridgeMessage request
        v1alphaResponse <- bridgeMessage (answerReflectionRequest descriptorTable serviceNames v1Request)
        send $ NextElem v1alphaResponse
        loop

-- | Answer one @ServerReflectionRequest@, dispatching on its
-- @message_request@ oneof.
--
-- Lookup failures ('V1.FileByFilename', 'V1.FileContainingSymbol') are
-- reported in-stream as an @ErrorResponse@ with @NOT_FOUND@, never as a
-- gRPC error: the RPC itself stays OK for the life of the stream.
-- No proto file served here declares proto2 extensions, so
-- @file_containing_extension@ always answers @NOT_FOUND@, while
-- @all_extension_numbers_of_type@ answers an empty @ExtensionNumberResponse@
-- for a type the server knows and @NOT_FOUND@ for one it does not.
answerReflectionRequest
  :: DescriptorTable
  -> [Text]
  -> Proto V1.ServerReflectionRequest
  -> Proto V1.ServerReflectionResponse
answerReflectionRequest table serviceNames request =
  defMessage
    & V1.validHost .~ (request ^. V1.host)
    & V1.originalRequest .~ request
    & answer
 where
  answer :: Proto V1.ServerReflectionResponse -> Proto V1.ServerReflectionResponse
  answer = case request ^. V1.maybe'messageRequest of
    -- proto3 leaves message_request entirely unset when malformed by the
    -- client; there is no lookup to fail here, so this is INVALID_ARGUMENT
    -- rather than NOT_FOUND. Answering in-stream here matches grpc's
    -- canonical C++ implementation; Go instead terminates the RPC.
    Nothing ->
      V1.errorResponse .~ mkErrorResponse GrpcInvalidArgument "no message_request set"
    Just (Proto messageRequest) -> case messageRequest of
      V1.ServerReflectionRequest'FileByFilename fileName ->
        fileDescriptorAnswer fileName
      V1.ServerReflectionRequest'FileContainingSymbol symbolName ->
        case lookupSymbol table symbolName of
          Nothing -> V1.errorResponse .~ mkErrorResponse GrpcNotFound ("symbol not found: " <> symbolName)
          Just fileName -> fileDescriptorAnswer fileName
      V1.ServerReflectionRequest'FileContainingExtension extensionRequest ->
        V1.errorResponse
          .~ mkErrorResponse
            GrpcNotFound
            ( "no extensions are declared by this server (requested for type: "
                <> (Proto extensionRequest ^. V1.containingType)
                <> ")"
            )
      V1.ServerReflectionRequest'AllExtensionNumbersOfType typeName ->
        case lookupSymbol table typeName of
          Nothing -> V1.errorResponse .~ mkErrorResponse GrpcNotFound ("type not found: " <> typeName)
          Just _ -> V1.allExtensionNumbersResponse .~ (defMessage & V1.baseTypeName .~ typeName)
      V1.ServerReflectionRequest'ListServices _ ->
        V1.listServicesResponse
          .~ (defMessage & V1.service .~ map (\serviceName -> defMessage & V1.name .~ serviceName) serviceNames)

  fileDescriptorAnswer
    :: Text -> Proto V1.ServerReflectionResponse -> Proto V1.ServerReflectionResponse
  fileDescriptorAnswer fileName = case transitiveClosure table fileName of
    Nothing -> V1.errorResponse .~ mkErrorResponse GrpcNotFound ("file not found: " <> fileName)
    Just entries ->
      V1.fileDescriptorResponse .~ (defMessage & V1.fileDescriptorProto .~ map fileEntryBytes entries)

  mkErrorResponse :: GrpcError -> Text -> Proto V1.ErrorResponse
  mkErrorResponse grpcError message =
    defMessage
      & V1.errorCode .~ fromIntegral (fromGrpcError grpcError)
      & V1.errorMessage .~ message

-- | Re-encode a message as a wire-compatible message with different
-- generated Haskell types. Safe between schemas that agree on every field
-- number and wire type, which @v1@ and @v1alpha@ of the reflection protos
-- do (@v1alpha@ is @v1@ under its original package name); a future schema
-- divergence is reported as an @INTERNAL@ gRPC error rather than a panic.
bridgeMessage :: (Message a, Message b, MonadIO m) => Proto a -> m (Proto b)
bridgeMessage message =
  either (throwGrpcErrorWithMessage GrpcInternal . ("bridgeMessage: " <>) . Text.pack) pure $
    decodeMessage (encodeMessage message)

-- | The fully qualified name of a proto service, @\<package\>.\<Service\>@,
-- read off its own compiled-in descriptor via proto-lens's 'Service' class.
-- Deriving it this way, rather than writing out the string, means the name
-- paired with each service's handler in "Cardano.Rpc.Server" and the name
-- 'answerReflectionRequest' (above) advertises for @list_services@ can
-- never drift apart.
qualifiedServiceName :: forall s. Service s => Text
qualifiedServiceName =
  Text.pack (symbolVal (Proxy @(ServicePackage s)))
    <> "."
    <> Text.pack (symbolVal (Proxy @(ServiceName s)))
