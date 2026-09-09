-- | Unit coverage for 'Cardano.Rpc.Server.Internal.Reflection.DescriptorTable':
-- the compiled-in proto descriptor table and its symbol index, which back
-- the gRPC Server Reflection API.
module Test.Cardano.Rpc.Reflection.DescriptorTable where

import Cardano.Rpc.Server.Internal.Reflection.DescriptorTable

import RIO

import Data.ProtoLens (defMessage)
import Data.Set qualified as Set

import Hedgehog as H
import Hedgehog.Extras qualified as H

import Proto.Google.Protobuf.Descriptor
import Proto.Google.Protobuf.Descriptor_Fields qualified as Descriptor

-- | Every entry's own @dependency@ list only ever names other files in the
-- same table: the tripwire for a future proto addition whose import was
-- forgotten from the descriptor table's fixed entry list.
hprop_descriptor_table_closed_under_dependency :: Property
hprop_descriptor_table_closed_under_dependency = H.propertyOnce $ do
  let knownFiles = Set.fromList $ fileNames descriptorTable
  forM_ (fileNames descriptorTable) $ \fileName -> do
    entry <- H.nothingFail $ lookupFile descriptorTable fileName
    let dependencies = fileEntryDescriptor entry ^. Descriptor.dependency
    H.annotateShow (fileName, dependencies)
    H.assertWith dependencies (all (`Set.member` knownFiles))

-- | @cardano/rpc/node.proto@ imports exactly @google/protobuf/empty.proto@,
-- so its closure is itself plus that one file.
hprop_transitive_closure_single_dependency :: Property
hprop_transitive_closure_single_dependency = H.propertyOnce $ do
  entries <- H.nothingFail $ transitiveClosure descriptorTable "cardano/rpc/node.proto"
  map ((^. Descriptor.name) . fileEntryDescriptor) entries
    === ["cardano/rpc/node.proto", "google/protobuf/empty.proto"]

-- | @utxorpc/v1beta/query/query.proto@ imports both
-- @google/protobuf/field_mask.proto@ and
-- @utxorpc/v1beta/cardano/cardano.proto@; the closure is the file itself
-- plus both, deduplicated, file first.
hprop_transitive_closure_multiple_dependencies :: Property
hprop_transitive_closure_multiple_dependencies = H.propertyOnce $ do
  entries <- H.nothingFail $ transitiveClosure descriptorTable "utxorpc/v1beta/query/query.proto"
  let names = map ((^. Descriptor.name) . fileEntryDescriptor) entries
  H.annotateShow names
  listToMaybe names === Just "utxorpc/v1beta/query/query.proto"
  Set.fromList names
    === Set.fromList
      [ "utxorpc/v1beta/query/query.proto"
      , "google/protobuf/field_mask.proto"
      , "utxorpc/v1beta/cardano/cardano.proto"
      ]

-- | A file that is not in the table has no closure.
hprop_transitive_closure_unknown_file_is_nothing :: Property
hprop_transitive_closure_unknown_file_is_nothing = H.propertyOnce $ do
  transitiveClosure descriptorTable "does/not/exist.proto" === Nothing

-- | A service's own symbol resolves to the file that declares it.
hprop_lookup_symbol_service :: Property
hprop_lookup_symbol_service = H.propertyOnce $ do
  lookupSymbol descriptorTable "cardano.rpc.Node" === Just "cardano/rpc/node.proto"

-- | The reflection service can describe itself: its own symbol resolves to
-- its own file, the same as any other registered service.
hprop_lookup_symbol_reflection_service :: Property
hprop_lookup_symbol_reflection_service = H.propertyOnce $ do
  lookupSymbol descriptorTable "grpc.reflection.v1.ServerReflection"
    === Just "grpc/reflection/v1/reflection.proto"

-- | A method's symbol - @\<package\>.\<Service\>.\<Method\>@, using the
-- method's exact proto name (@GetEra@, not the Haskell binding's
-- @getEra@) - resolves to the same file as its service.
hprop_lookup_symbol_method :: Property
hprop_lookup_symbol_method = H.propertyOnce $ do
  lookupSymbol descriptorTable "cardano.rpc.Node.GetEra" === Just "cardano/rpc/node.proto"

-- | A top-level message symbol resolves to its file.
hprop_lookup_symbol_message :: Property
hprop_lookup_symbol_message = H.propertyOnce $ do
  lookupSymbol descriptorTable "cardano.rpc.CurrentEra" === Just "cardano/rpc/node.proto"
  lookupSymbol descriptorTable "grpc.reflection.v1.ServerReflectionRequest"
    === Just "grpc/reflection/v1/reflection.proto"

-- | A top-level enum symbol resolves to its file.
hprop_lookup_symbol_enum :: Property
hprop_lookup_symbol_enum = H.propertyOnce $ do
  lookupSymbol descriptorTable "cardano.rpc.Era" === Just "cardano/rpc/node.proto"

-- | An enum VALUE's symbol is scoped to the enum's own enclosing scope, not
-- to the enum type: @conway@ resolves as @cardano.rpc.conway@, a sibling of
-- @cardano.rpc.Era@, not @cardano.rpc.Era.conway@.
hprop_lookup_symbol_enum_value :: Property
hprop_lookup_symbol_enum_value = H.propertyOnce $ do
  lookupSymbol descriptorTable "cardano.rpc.conway" === Just "cardano/rpc/node.proto"
  lookupSymbol descriptorTable "cardano.rpc.Era.conway" === Nothing

-- | A field's symbol is scoped to its message, using the field's exact
-- proto name (@era@, snake_case as embedded in the descriptor).
hprop_lookup_symbol_field :: Property
hprop_lookup_symbol_field = H.propertyOnce $ do
  lookupSymbol descriptorTable "cardano.rpc.CurrentEra.era" === Just "cardano/rpc/node.proto"

-- | A oneof declaration's symbol is scoped to its message, same as a field.
hprop_lookup_symbol_oneof :: Property
hprop_lookup_symbol_oneof = H.propertyOnce $ do
  lookupSymbol descriptorTable "utxorpc.v1beta.cardano.GovernanceAction.governance_action"
    === Just "utxorpc/v1beta/cardano/cardano.proto"

-- | An unrecognised symbol resolves to 'Nothing'.
hprop_lookup_symbol_unknown_is_nothing :: Property
hprop_lookup_symbol_unknown_is_nothing = H.propertyOnce $ do
  lookupSymbol descriptorTable "no.such.Symbol" === Nothing

-- | 'fileSymbolNames' recurses into nested messages, qualifying each level
-- with a dot; into a nested enum's values, scoped to the CONTAINING MESSAGE
-- rather than the enum type (@test.nested.Outer.RED@, not
-- @test.nested.Outer.Color.RED@); and into a message's own fields and
-- oneof declarations, scoped to the message itself the same way. None of
-- the proto files actually served here declare a nested message or enum,
-- so this is exercised against a small hand-built descriptor instead of a
-- real compiled-in file.
hprop_file_symbol_names_recurses_into_nested_messages :: Property
hprop_file_symbol_names_recurses_into_nested_messages = H.propertyOnce $ do
  fileSymbolNames nestedFixtureFile
    === [ "test.nested.Outer"
        , "test.nested.Outer.Inner"
        , "test.nested.Outer.Color"
        , "test.nested.Outer.RED"
        , "test.nested.Outer.payload"
        , "test.nested.Outer.kind"
        ]
 where
  nestedFixtureFile :: FileDescriptorProto
  nestedFixtureFile =
    defMessage
      & Descriptor.name .~ "test/nested.proto"
      & Descriptor.package .~ "test.nested"
      & Descriptor.messageType
        .~ [ defMessage
               & Descriptor.name .~ "Outer"
               & Descriptor.nestedType .~ [defMessage & Descriptor.name .~ "Inner"]
               & Descriptor.enumType
                 .~ [ defMessage
                        & Descriptor.name .~ "Color"
                        & Descriptor.value .~ [defMessage & Descriptor.name .~ "RED"]
                    ]
               & Descriptor.field .~ [defMessage & Descriptor.name .~ "payload"]
               & Descriptor.oneofDecl .~ [defMessage & Descriptor.name .~ "kind"]
           ]
