{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | The proto file descriptors served by the gRPC Server Reflection API,
-- compiled into this binary, plus a package-qualified symbol index over
-- their contents (services, methods, messages, fields, oneofs, enums and
-- enum values).
--
-- Every proto file cardano-rpc serves is represented here by one
-- 'FileEntry', keyed by the file name embedded in its own descriptor (e.g.
-- @"cardano/rpc/node.proto"@), which is also how dependent files refer to
-- it in their own @dependency@ list.
module Cardano.Rpc.Server.Internal.Reflection.DescriptorTable
  ( FileEntry (..)
  , DescriptorTable
  , descriptorTable
  , fileNames
  , lookupFile
  , lookupSymbol
  , transitiveClosure
  , fileSymbolNames
  )
where

import RIO

import Data.Map.Strict qualified as Map
import Data.ProtoLens (Message, packedFileDescriptor)
import Data.ProtoLens.Descriptor (fileDescriptor)
import Data.Set qualified as Set
import Data.Text qualified as Text

import Proto.Cardano.Rpc.Node (CurrentEra)
import Proto.Google.Protobuf.Descriptor
import Proto.Google.Protobuf.Descriptor_Fields qualified as Descriptor
import Proto.Google.Protobuf.Empty (Empty)
import Proto.Google.Protobuf.FieldMask (FieldMask)
import Proto.Grpc.Reflection.V1.Reflection qualified as ReflectionV1
import Proto.Grpc.Reflection.V1alpha.Reflection qualified as ReflectionV1alpha
import Proto.Utxorpc.V1beta.Cardano.Cardano (TxInput)
import Proto.Utxorpc.V1beta.Query.Query (ReadParamsRequest)
import Proto.Utxorpc.V1beta.Submit.Submit (SubmitTxRequest)
import Proto.Utxorpc.V1beta.Sync.Sync (FollowTipRequest)

-- | The descriptor table for every proto file this server's gRPC API is
-- built from: the six services it registers, plus the well-known
-- @google.protobuf@ types and the reflection protos themselves.
descriptorTable :: DescriptorTable
descriptorTable =
  DescriptorTable
    { descriptorTableFileIndex =
        Map.fromList [(fileEntryDescriptor entry ^. Descriptor.name, entry) | entry <- entries]
    , descriptorTableSymbolIndex =
        Map.fromList
          [ (symbolName, fileEntryDescriptor entry ^. Descriptor.name)
          | entry <- entries
          , symbolName <- fileSymbolNames (fileEntryDescriptor entry)
          ]
    }
 where
  entries =
    [ mkFileEntry @CurrentEra
    , mkFileEntry @TxInput
    , mkFileEntry @ReadParamsRequest
    , mkFileEntry @SubmitTxRequest
    , mkFileEntry @FollowTipRequest
    , mkFileEntry @Empty
    , mkFileEntry @FieldMask
    , mkFileEntry @ReflectionV1.ServerReflectionRequest
    , mkFileEntry @ReflectionV1alpha.ServerReflectionRequest
    ]

-- | Resolve a fully qualified symbol - a service, method, message, nested
-- message, field, oneof, enum, or enum value name - to the file that
-- declares it.
lookupSymbol :: DescriptorTable -> Text -> Maybe Text
lookupSymbol table symbolName = Map.lookup symbolName (descriptorTableSymbolIndex table)

-- | The named file, plus every file it transitively depends on, in
-- traversal order with the file itself first, deduplicated. 'Nothing' if
-- the file itself is not in the table; a dependency that is missing from
-- the table (which should not happen - see the table's closure invariant)
-- is silently skipped rather than failing the whole lookup.
transitiveClosure :: DescriptorTable -> Text -> Maybe [FileEntry]
transitiveClosure table rootName = do
  rootEntry <- lookupFile table rootName
  pure $
    rootEntry : go (Set.singleton rootName) (fileEntryDescriptor rootEntry ^. Descriptor.dependency)
 where
  go _ [] = []
  go seen (depName : rest)
    | depName `Set.member` seen = go seen rest
    | otherwise = case lookupFile table depName of
        Nothing -> go (Set.insert depName seen) rest
        Just depEntry ->
          depEntry
            : go (Set.insert depName seen) (rest <> fileEntryDescriptor depEntry ^. Descriptor.dependency)

-- | Look up a file by name (e.g. @"cardano/rpc/node.proto"@).
lookupFile :: DescriptorTable -> Text -> Maybe FileEntry
lookupFile table fileName = Map.lookup fileName (descriptorTableFileIndex table)

-- | The name of every served proto file.
fileNames :: DescriptorTable -> [Text]
fileNames = Map.keys . descriptorTableFileIndex

-- | Build the 'FileEntry' for the proto file containing @msg@, using one
-- representative message type per file (any message declared in the file
-- works - 'packedFileDescriptor'\/'fileDescriptor' both resolve to the whole
-- containing file, not just @msg@ itself).
mkFileEntry :: forall msg. Message msg => FileEntry
mkFileEntry =
  FileEntry
    { fileEntryDescriptor = fileDescriptor @msg
    , fileEntryBytes = packedFileDescriptor (Proxy @msg)
    }

-- | Every package-qualified symbol a file declares: its services (and their
-- methods), top-level and nested messages (and their fields and oneofs),
-- and top-level and nested enums (and their values).
fileSymbolNames :: FileDescriptorProto -> [Text]
fileSymbolNames descriptor =
  concatMap (serviceSymbolNames packageName) (descriptor ^. Descriptor.service)
    <> concatMap (messageSymbolNames packageName) (descriptor ^. Descriptor.messageType)
    <> concatMap (enumSymbolNames packageName) (descriptor ^. Descriptor.enumType)
 where
  packageName = descriptor ^. Descriptor.package

-- | A service's own symbol, plus one per method it declares.
serviceSymbolNames :: Text -> ServiceDescriptorProto -> [Text]
serviceSymbolNames packageName service =
  serviceName : map (qualify serviceName . (^. Descriptor.name)) (service ^. Descriptor.method)
 where
  serviceName = qualify packageName (service ^. Descriptor.name)

-- | A message's own symbol, plus every symbol nested inside it, recursively:
-- its fields and oneof declarations (both scoped to the message itself,
-- like enum values are), its nested messages, and its nested enums.
messageSymbolNames :: Text -> DescriptorProto -> [Text]
messageSymbolNames enclosingName message =
  messageName
    : concatMap (messageSymbolNames messageName) (message ^. Descriptor.nestedType)
      <> concatMap (enumSymbolNames messageName) (message ^. Descriptor.enumType)
      <> map (qualify messageName . (^. Descriptor.name)) (message ^. Descriptor.field)
      <> map (qualify messageName . (^. Descriptor.name)) (message ^. Descriptor.oneofDecl)
 where
  messageName = qualify enclosingName (message ^. Descriptor.name)

-- | An enum's own symbol, plus one per value it declares. Protobuf scopes
-- an enum VALUE's fully qualified name to the enum's own enclosing scope
-- (the package, or the containing message), not to the enum type itself,
-- so a value's symbol is a SIBLING of its enum's symbol, not nested under
-- it - e.g. @cardano.rpc.conway@, not @cardano.rpc.Era.conway@.
enumSymbolNames :: Text -> EnumDescriptorProto -> [Text]
enumSymbolNames enclosingName enum =
  qualify enclosingName (enum ^. Descriptor.name)
    : map (qualify enclosingName . (^. Descriptor.name)) (enum ^. Descriptor.value)

-- | Join a package\/enclosing-message prefix and a name with a dot, or just
-- the name if the prefix is empty (a file with no @package@ declaration).
qualify :: Text -> Text -> Text
qualify prefix name
  | Text.null prefix = name
  | otherwise = prefix <> "." <> name

-- | One served proto file: its decoded descriptor, used to walk dependencies
-- and resolve symbols, and the packed bytes of that same descriptor, sent
-- verbatim in a @FileDescriptorResponse@.
data FileEntry = FileEntry
  { fileEntryDescriptor :: FileDescriptorProto
  , fileEntryBytes :: ByteString
  }
  deriving (Eq, Show)

data DescriptorTable = DescriptorTable
  { descriptorTableFileIndex :: Map Text FileEntry
  , descriptorTableSymbolIndex :: Map Text Text
  -- ^ Fully qualified symbol (service, method, message, field, oneof, enum,
  -- or enum value name) to the file name that declares it.
  }
