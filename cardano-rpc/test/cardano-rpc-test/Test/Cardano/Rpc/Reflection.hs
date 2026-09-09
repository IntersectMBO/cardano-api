{-# LANGUAGE LambdaCase #-}

-- | Unit coverage for the gRPC Server Reflection handler
-- ('Cardano.Rpc.Server.Internal.Reflection'): request dispatch via
-- 'answerReflectionRequest', and the bidirectional stream's termination
-- behaviour and @v1@\/@v1alpha@ bridging.
module Test.Cardano.Rpc.Reflection where

import Cardano.Rpc.Proto.Api.Reflection.V1 qualified as V1
import Cardano.Rpc.Proto.Api.Reflection.V1alpha qualified as V1alpha
import Cardano.Rpc.Server (registeredServiceNames)
import Cardano.Rpc.Server.Internal.Reflection
import Cardano.Rpc.Server.Internal.Reflection.DescriptorTable
  ( descriptorTable
  , fileEntryBytes
  , lookupSymbol
  , transitiveClosure
  )

import RIO

import Data.ProtoLens (defMessage)
import Data.Text qualified as Text
import Network.GRPC.Spec (NextElem (NextElem, NoNextElem))

import Hedgehog as H
import Hedgehog.Extras qualified as H

-- | The server registers exactly six services, under exactly these names.
-- "Cardano.Rpc.Server" derives 'registeredServiceNames' from the same list
-- that supplies the registered handlers, so the two cannot drift apart by
-- construction; this pins the list's actual contents instead. Every name
-- must also resolve via the descriptor table, since @list_services@
-- answering a name whose file was never registered there would make the
-- reflection API self-inconsistent.
hprop_registered_service_names :: Property
hprop_registered_service_names = H.propertyOnce $ do
  registeredServiceNames
    === [ "cardano.rpc.Node"
        , "utxorpc.v1beta.query.QueryService"
        , "utxorpc.v1beta.submit.SubmitService"
        , "utxorpc.v1beta.sync.SyncService"
        , "grpc.reflection.v1.ServerReflection"
        , "grpc.reflection.v1alpha.ServerReflection"
        ]
  forM_ registeredServiceNames $ \serviceName ->
    H.assertWith serviceName (isJust . lookupSymbol descriptorTable)

-- | @list_services@ answers with exactly the given names, regardless of the
-- request's (unchecked, per spec) payload string.
hprop_list_services_returns_given_names :: Property
hprop_list_services_returns_given_names = H.propertyOnce $ do
  let request = defMessage & V1.host .~ "localhost" & V1.listServices .~ "ignored"
      response = answerReflectionRequest descriptorTable testServiceNames request
  map (^. V1.name) (response ^. V1.listServicesResponse . V1.service) === testServiceNames
  response ^. V1.originalRequest === request
  response ^. V1.validHost === "localhost"

-- | @file_by_filename@ for a known file answers with its full transitive
-- dependency closure, file first.
hprop_file_by_filename_returns_full_closure :: Property
hprop_file_by_filename_returns_full_closure = H.propertyOnce $ do
  let request = defMessage & V1.host .~ "localhost" & V1.fileByFilename .~ "cardano/rpc/node.proto"
      response = answerReflectionRequest descriptorTable testServiceNames request
  expectedEntries <- H.nothingFail $ transitiveClosure descriptorTable "cardano/rpc/node.proto"
  response ^. V1.fileDescriptorResponse . V1.fileDescriptorProto
    === map fileEntryBytes expectedEntries

-- | @file_by_filename@ for an unknown file answers in-stream with
-- @NOT_FOUND@ (error code 5), never a gRPC error.
hprop_file_by_filename_not_found :: Property
hprop_file_by_filename_not_found = H.propertyOnce $ do
  let request = defMessage & V1.host .~ "localhost" & V1.fileByFilename .~ "does/not/exist.proto"
      response = answerReflectionRequest descriptorTable testServiceNames request
  response ^. V1.originalRequest === request
  response ^. V1.errorResponse . V1.errorCode === 5
  H.assertWith
    (response ^. V1.errorResponse . V1.errorMessage)
    ("does/not/exist.proto" `Text.isInfixOf`)

-- | @file_containing_symbol@ for a known symbol answers with the closure of
-- the file that declares it.
hprop_file_containing_symbol_found :: Property
hprop_file_containing_symbol_found = H.propertyOnce $ do
  let request = defMessage & V1.host .~ "localhost" & V1.fileContainingSymbol .~ "cardano.rpc.Node"
      response = answerReflectionRequest descriptorTable testServiceNames request
  expectedEntries <- H.nothingFail $ transitiveClosure descriptorTable "cardano/rpc/node.proto"
  response ^. V1.fileDescriptorResponse . V1.fileDescriptorProto
    === map fileEntryBytes expectedEntries

-- | @file_containing_symbol@ for an unknown symbol answers @NOT_FOUND@.
hprop_file_containing_symbol_unknown :: Property
hprop_file_containing_symbol_unknown = H.propertyOnce $ do
  let request = defMessage & V1.host .~ "localhost" & V1.fileContainingSymbol .~ "no.such.Symbol"
      response = answerReflectionRequest descriptorTable testServiceNames request
  response ^. V1.errorResponse . V1.errorCode === 5

-- | @file_containing_extension@ always answers @NOT_FOUND@: none of the
-- proto files served here declare proto2 extensions.
hprop_file_containing_extension_is_not_found :: Property
hprop_file_containing_extension_is_not_found = H.propertyOnce $ do
  let extensionRequest = defMessage & V1.containingType .~ "some.Type" & V1.extensionNumber .~ 7
      request = defMessage & V1.host .~ "localhost" & V1.fileContainingExtension .~ extensionRequest
      response = answerReflectionRequest descriptorTable testServiceNames request
  response ^. V1.errorResponse . V1.errorCode === 5

-- | @all_extension_numbers_of_type@ answers @NOT_FOUND@ for a type this
-- server does not declare.
hprop_all_extension_numbers_of_unknown_type_is_not_found :: Property
hprop_all_extension_numbers_of_unknown_type_is_not_found = H.propertyOnce $ do
  let request = defMessage & V1.host .~ "localhost" & V1.allExtensionNumbersOfType .~ "some.Type"
      response = answerReflectionRequest descriptorTable testServiceNames request
  response ^. V1.errorResponse . V1.errorCode === 5

-- | For a type it does declare, the answer is an empty
-- @ExtensionNumberResponse@ naming that type rather than an error: the
-- lookup succeeded and found no extensions, every proto here being proto3.
hprop_all_extension_numbers_of_known_type_is_empty :: Property
hprop_all_extension_numbers_of_known_type_is_empty = H.propertyOnce $ do
  let request =
        defMessage
          & V1.host .~ "localhost"
          & V1.allExtensionNumbersOfType .~ "cardano.rpc.CurrentEra"
      response = answerReflectionRequest descriptorTable testServiceNames request
  response ^. V1.maybe'errorResponse === Nothing
  response ^. V1.allExtensionNumbersResponse . V1.baseTypeName === "cardano.rpc.CurrentEra"
  response ^. V1.allExtensionNumbersResponse . V1.extensionNumber === []

-- | A request with no @message_request@ set at all is a malformed request,
-- not a failed lookup, so it answers @INVALID_ARGUMENT@ (error code 3)
-- rather than @NOT_FOUND@.
hprop_unset_message_request_is_invalid_argument :: Property
hprop_unset_message_request_is_invalid_argument = H.propertyOnce $ do
  let request = defMessage & V1.host .~ "localhost"
      response = answerReflectionRequest descriptorTable testServiceNames request
  response ^. V1.errorResponse . V1.errorCode === 3

-- | The @v1@ handler sends the client's own terminal marker after
-- answering every request: one response per request, plus a final
-- 'NoNextElem'. A handler that omitted this would have its stream
-- cancelled instead of closed with trailers.
hprop_server_reflection_info_v1_sends_terminal_marker :: Property
hprop_server_reflection_info_v1_sends_terminal_marker = H.propertyOnce $ do
  let request = defMessage & V1.host .~ "localhost" & V1.listServices .~ "x"
  sent <- runScriptedBidi (serverReflectionInfoMethodV1 testServiceNames) [request]
  H.annotateShow sent
  length sent === 2
  lastElement <- H.nothingFail $ listToMaybe (reverse sent)
  lastElement === NoNextElem

-- | The @v1alpha@ handler answers using the same registry and service
-- names as @v1@, bridging each message across the wire-compatible schemas:
-- a @list_services@ round trip through it returns the same names.
hprop_server_reflection_info_v1alpha_bridges_to_v1 :: Property
hprop_server_reflection_info_v1alpha_bridges_to_v1 = H.propertyOnce $ do
  let request = defMessage & V1alpha.host .~ "localhost" & V1alpha.listServices .~ "x"
  sent <- runScriptedBidi (serverReflectionInfoMethodV1alpha testServiceNames) [request]
  response <- H.nothingFail $ listToMaybe [r | NextElem r <- sent]
  map (^. V1alpha.name) (response ^. V1alpha.listServicesResponse . V1alpha.service)
    === testServiceNames

-- | Drive a bidi handler with a fixed list of requests followed by the
-- client's own terminal marker, capturing every element it sends.
runScriptedBidi
  :: MonadIO m
  => (IO (NextElem req) -> (NextElem resp -> IO ()) -> IO ())
  -> [req]
  -> m [NextElem resp]
runScriptedBidi handler requests = liftIO $ do
  pending <- newIORef (map NextElem requests <> [NoNextElem])
  sentRef <- newIORef []
  let recv =
        readIORef pending >>= \case
          [] -> pure NoNextElem
          next : rest -> writeIORef pending rest $> next
      send nextElem = modifyIORef' sentRef (nextElem :)
  handler recv send
  reverse <$> readIORef sentRef

-- | A small, self-contained service list for tests that only care about
-- @list_services@ echoing back whatever it is given, independent of the
-- real production names asserted by 'hprop_registered_service_names'.
testServiceNames :: [Text]
testServiceNames = ["test.pkg.ServiceOne", "test.pkg.ServiceTwo"]
