{- This file was auto-generated from grpc/reflection/v1alpha/reflection.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Grpc.Reflection.V1alpha.Reflection_Fields where
import qualified Data.ProtoLens.Runtime.Prelude as Prelude
import qualified Data.ProtoLens.Runtime.Data.Int as Data.Int
import qualified Data.ProtoLens.Runtime.Data.Monoid as Data.Monoid
import qualified Data.ProtoLens.Runtime.Data.Word as Data.Word
import qualified Data.ProtoLens.Runtime.Data.ProtoLens as Data.ProtoLens
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Bytes as Data.ProtoLens.Encoding.Bytes
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Growing as Data.ProtoLens.Encoding.Growing
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Parser.Unsafe as Data.ProtoLens.Encoding.Parser.Unsafe
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Wire as Data.ProtoLens.Encoding.Wire
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Field as Data.ProtoLens.Field
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Message.Enum as Data.ProtoLens.Message.Enum
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Service.Types as Data.ProtoLens.Service.Types
import qualified Data.ProtoLens.Runtime.Lens.Family2 as Lens.Family2
import qualified Data.ProtoLens.Runtime.Lens.Family2.Unchecked as Lens.Family2.Unchecked
import qualified Data.ProtoLens.Runtime.Data.Text as Data.Text
import qualified Data.ProtoLens.Runtime.Data.Map as Data.Map
import qualified Data.ProtoLens.Runtime.Data.ByteString as Data.ByteString
import qualified Data.ProtoLens.Runtime.Data.ByteString.Char8 as Data.ByteString.Char8
import qualified Data.ProtoLens.Runtime.Data.Text.Encoding as Data.Text.Encoding
import qualified Data.ProtoLens.Runtime.Data.Vector as Data.Vector
import qualified Data.ProtoLens.Runtime.Data.Vector.Generic as Data.Vector.Generic
import qualified Data.ProtoLens.Runtime.Data.Vector.Unboxed as Data.Vector.Unboxed
import qualified Data.ProtoLens.Runtime.Text.Read as Text.Read
allExtensionNumbersOfType ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "allExtensionNumbersOfType" a) =>
  Lens.Family2.LensLike' f s a
allExtensionNumbersOfType
  = Data.ProtoLens.Field.field @"allExtensionNumbersOfType"
allExtensionNumbersResponse ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "allExtensionNumbersResponse" a) =>
  Lens.Family2.LensLike' f s a
allExtensionNumbersResponse
  = Data.ProtoLens.Field.field @"allExtensionNumbersResponse"
baseTypeName ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "baseTypeName" a) =>
  Lens.Family2.LensLike' f s a
baseTypeName = Data.ProtoLens.Field.field @"baseTypeName"
containingType ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "containingType" a) =>
  Lens.Family2.LensLike' f s a
containingType = Data.ProtoLens.Field.field @"containingType"
errorCode ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "errorCode" a) =>
  Lens.Family2.LensLike' f s a
errorCode = Data.ProtoLens.Field.field @"errorCode"
errorMessage ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "errorMessage" a) =>
  Lens.Family2.LensLike' f s a
errorMessage = Data.ProtoLens.Field.field @"errorMessage"
errorResponse ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "errorResponse" a) =>
  Lens.Family2.LensLike' f s a
errorResponse = Data.ProtoLens.Field.field @"errorResponse"
extensionNumber ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "extensionNumber" a) =>
  Lens.Family2.LensLike' f s a
extensionNumber = Data.ProtoLens.Field.field @"extensionNumber"
fileByFilename ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "fileByFilename" a) =>
  Lens.Family2.LensLike' f s a
fileByFilename = Data.ProtoLens.Field.field @"fileByFilename"
fileContainingExtension ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "fileContainingExtension" a) =>
  Lens.Family2.LensLike' f s a
fileContainingExtension
  = Data.ProtoLens.Field.field @"fileContainingExtension"
fileContainingSymbol ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "fileContainingSymbol" a) =>
  Lens.Family2.LensLike' f s a
fileContainingSymbol
  = Data.ProtoLens.Field.field @"fileContainingSymbol"
fileDescriptorProto ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "fileDescriptorProto" a) =>
  Lens.Family2.LensLike' f s a
fileDescriptorProto
  = Data.ProtoLens.Field.field @"fileDescriptorProto"
fileDescriptorResponse ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "fileDescriptorResponse" a) =>
  Lens.Family2.LensLike' f s a
fileDescriptorResponse
  = Data.ProtoLens.Field.field @"fileDescriptorResponse"
host ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "host" a) =>
  Lens.Family2.LensLike' f s a
host = Data.ProtoLens.Field.field @"host"
listServices ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "listServices" a) =>
  Lens.Family2.LensLike' f s a
listServices = Data.ProtoLens.Field.field @"listServices"
listServicesResponse ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "listServicesResponse" a) =>
  Lens.Family2.LensLike' f s a
listServicesResponse
  = Data.ProtoLens.Field.field @"listServicesResponse"
maybe'allExtensionNumbersOfType ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'allExtensionNumbersOfType" a) =>
  Lens.Family2.LensLike' f s a
maybe'allExtensionNumbersOfType
  = Data.ProtoLens.Field.field @"maybe'allExtensionNumbersOfType"
maybe'allExtensionNumbersResponse ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'allExtensionNumbersResponse" a) =>
  Lens.Family2.LensLike' f s a
maybe'allExtensionNumbersResponse
  = Data.ProtoLens.Field.field @"maybe'allExtensionNumbersResponse"
maybe'errorResponse ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'errorResponse" a) =>
  Lens.Family2.LensLike' f s a
maybe'errorResponse
  = Data.ProtoLens.Field.field @"maybe'errorResponse"
maybe'fileByFilename ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'fileByFilename" a) =>
  Lens.Family2.LensLike' f s a
maybe'fileByFilename
  = Data.ProtoLens.Field.field @"maybe'fileByFilename"
maybe'fileContainingExtension ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'fileContainingExtension" a) =>
  Lens.Family2.LensLike' f s a
maybe'fileContainingExtension
  = Data.ProtoLens.Field.field @"maybe'fileContainingExtension"
maybe'fileContainingSymbol ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'fileContainingSymbol" a) =>
  Lens.Family2.LensLike' f s a
maybe'fileContainingSymbol
  = Data.ProtoLens.Field.field @"maybe'fileContainingSymbol"
maybe'fileDescriptorResponse ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'fileDescriptorResponse" a) =>
  Lens.Family2.LensLike' f s a
maybe'fileDescriptorResponse
  = Data.ProtoLens.Field.field @"maybe'fileDescriptorResponse"
maybe'listServices ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'listServices" a) =>
  Lens.Family2.LensLike' f s a
maybe'listServices
  = Data.ProtoLens.Field.field @"maybe'listServices"
maybe'listServicesResponse ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'listServicesResponse" a) =>
  Lens.Family2.LensLike' f s a
maybe'listServicesResponse
  = Data.ProtoLens.Field.field @"maybe'listServicesResponse"
maybe'messageRequest ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'messageRequest" a) =>
  Lens.Family2.LensLike' f s a
maybe'messageRequest
  = Data.ProtoLens.Field.field @"maybe'messageRequest"
maybe'messageResponse ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'messageResponse" a) =>
  Lens.Family2.LensLike' f s a
maybe'messageResponse
  = Data.ProtoLens.Field.field @"maybe'messageResponse"
maybe'originalRequest ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'originalRequest" a) =>
  Lens.Family2.LensLike' f s a
maybe'originalRequest
  = Data.ProtoLens.Field.field @"maybe'originalRequest"
name ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "name" a) =>
  Lens.Family2.LensLike' f s a
name = Data.ProtoLens.Field.field @"name"
originalRequest ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "originalRequest" a) =>
  Lens.Family2.LensLike' f s a
originalRequest = Data.ProtoLens.Field.field @"originalRequest"
service ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "service" a) =>
  Lens.Family2.LensLike' f s a
service = Data.ProtoLens.Field.field @"service"
validHost ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "validHost" a) =>
  Lens.Family2.LensLike' f s a
validHost = Data.ProtoLens.Field.field @"validHost"
vec'extensionNumber ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'extensionNumber" a) =>
  Lens.Family2.LensLike' f s a
vec'extensionNumber
  = Data.ProtoLens.Field.field @"vec'extensionNumber"
vec'fileDescriptorProto ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'fileDescriptorProto" a) =>
  Lens.Family2.LensLike' f s a
vec'fileDescriptorProto
  = Data.ProtoLens.Field.field @"vec'fileDescriptorProto"
vec'service ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'service" a) =>
  Lens.Family2.LensLike' f s a
vec'service = Data.ProtoLens.Field.field @"vec'service"