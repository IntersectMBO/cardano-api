{- This file was auto-generated from grpc/reflection/v1alpha/reflection.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Grpc.Reflection.V1alpha.Reflection (
        ServerReflection(..), ErrorResponse(), ExtensionNumberResponse(),
        ExtensionRequest(), FileDescriptorResponse(),
        ListServiceResponse(), ServerReflectionRequest(),
        ServerReflectionRequest'MessageRequest(..),
        _ServerReflectionRequest'FileByFilename,
        _ServerReflectionRequest'FileContainingSymbol,
        _ServerReflectionRequest'FileContainingExtension,
        _ServerReflectionRequest'AllExtensionNumbersOfType,
        _ServerReflectionRequest'ListServices, ServerReflectionResponse(),
        ServerReflectionResponse'MessageResponse(..),
        _ServerReflectionResponse'FileDescriptorResponse,
        _ServerReflectionResponse'AllExtensionNumbersResponse,
        _ServerReflectionResponse'ListServicesResponse,
        _ServerReflectionResponse'ErrorResponse, ServiceResponse()
    ) where
import qualified Data.ProtoLens.Runtime.Control.DeepSeq as Control.DeepSeq
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Prism as Data.ProtoLens.Prism
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
{- | Fields :
     
         * 'Proto.Grpc.Reflection.V1alpha.Reflection_Fields.errorCode' @:: Lens' ErrorResponse Data.Int.Int32@
         * 'Proto.Grpc.Reflection.V1alpha.Reflection_Fields.errorMessage' @:: Lens' ErrorResponse Data.Text.Text@ -}
data ErrorResponse
  = ErrorResponse'_constructor {_ErrorResponse'errorCode :: !Data.Int.Int32,
                                _ErrorResponse'errorMessage :: !Data.Text.Text,
                                _ErrorResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ErrorResponse where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ErrorResponse "errorCode" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ErrorResponse'errorCode
           (\ x__ y__ -> x__ {_ErrorResponse'errorCode = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ErrorResponse "errorMessage" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ErrorResponse'errorMessage
           (\ x__ y__ -> x__ {_ErrorResponse'errorMessage = y__}))
        Prelude.id
instance Data.ProtoLens.Message ErrorResponse where
  messageName _
    = Data.Text.pack "grpc.reflection.v1alpha.ErrorResponse"
  packedMessageDescriptor _
    = "\n\
      \\rErrorResponse\DC2\GS\n\
      \\n\
      \error_code\CAN\SOH \SOH(\ENQR\terrorCode\DC2#\n\
      \\rerror_message\CAN\STX \SOH(\tR\ferrorMessage"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        errorCode__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "error_code"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"errorCode")) ::
              Data.ProtoLens.FieldDescriptor ErrorResponse
        errorMessage__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "error_message"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"errorMessage")) ::
              Data.ProtoLens.FieldDescriptor ErrorResponse
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, errorCode__field_descriptor),
           (Data.ProtoLens.Tag 2, errorMessage__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ErrorResponse'_unknownFields
        (\ x__ y__ -> x__ {_ErrorResponse'_unknownFields = y__})
  defMessage
    = ErrorResponse'_constructor
        {_ErrorResponse'errorCode = Data.ProtoLens.fieldDefault,
         _ErrorResponse'errorMessage = Data.ProtoLens.fieldDefault,
         _ErrorResponse'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ErrorResponse -> Data.ProtoLens.Encoding.Bytes.Parser ErrorResponse
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        8 -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "error_code"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"errorCode") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "error_message"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"errorMessage") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "ErrorResponse"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"errorCode") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                      ((Prelude..)
                         Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
             ((Data.Monoid.<>)
                (let
                   _v
                     = Lens.Family2.view (Data.ProtoLens.Field.field @"errorMessage") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                         ((Prelude..)
                            (\ bs
                               -> (Data.Monoid.<>)
                                    (Data.ProtoLens.Encoding.Bytes.putVarInt
                                       (Prelude.fromIntegral (Data.ByteString.length bs)))
                                    (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                            Data.Text.Encoding.encodeUtf8 _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData ErrorResponse where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ErrorResponse'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ErrorResponse'errorCode x__)
                (Control.DeepSeq.deepseq (_ErrorResponse'errorMessage x__) ()))
{- | Fields :
     
         * 'Proto.Grpc.Reflection.V1alpha.Reflection_Fields.baseTypeName' @:: Lens' ExtensionNumberResponse Data.Text.Text@
         * 'Proto.Grpc.Reflection.V1alpha.Reflection_Fields.extensionNumber' @:: Lens' ExtensionNumberResponse [Data.Int.Int32]@
         * 'Proto.Grpc.Reflection.V1alpha.Reflection_Fields.vec'extensionNumber' @:: Lens' ExtensionNumberResponse (Data.Vector.Unboxed.Vector Data.Int.Int32)@ -}
data ExtensionNumberResponse
  = ExtensionNumberResponse'_constructor {_ExtensionNumberResponse'baseTypeName :: !Data.Text.Text,
                                          _ExtensionNumberResponse'extensionNumber :: !(Data.Vector.Unboxed.Vector Data.Int.Int32),
                                          _ExtensionNumberResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ExtensionNumberResponse where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ExtensionNumberResponse "baseTypeName" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExtensionNumberResponse'baseTypeName
           (\ x__ y__ -> x__ {_ExtensionNumberResponse'baseTypeName = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExtensionNumberResponse "extensionNumber" [Data.Int.Int32] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExtensionNumberResponse'extensionNumber
           (\ x__ y__
              -> x__ {_ExtensionNumberResponse'extensionNumber = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ExtensionNumberResponse "vec'extensionNumber" (Data.Vector.Unboxed.Vector Data.Int.Int32) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExtensionNumberResponse'extensionNumber
           (\ x__ y__
              -> x__ {_ExtensionNumberResponse'extensionNumber = y__}))
        Prelude.id
instance Data.ProtoLens.Message ExtensionNumberResponse where
  messageName _
    = Data.Text.pack "grpc.reflection.v1alpha.ExtensionNumberResponse"
  packedMessageDescriptor _
    = "\n\
      \\ETBExtensionNumberResponse\DC2$\n\
      \\SObase_type_name\CAN\SOH \SOH(\tR\fbaseTypeName\DC2)\n\
      \\DLEextension_number\CAN\STX \ETX(\ENQR\SIextensionNumber"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        baseTypeName__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "base_type_name"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"baseTypeName")) ::
              Data.ProtoLens.FieldDescriptor ExtensionNumberResponse
        extensionNumber__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "extension_number"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Packed
                 (Data.ProtoLens.Field.field @"extensionNumber")) ::
              Data.ProtoLens.FieldDescriptor ExtensionNumberResponse
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, baseTypeName__field_descriptor),
           (Data.ProtoLens.Tag 2, extensionNumber__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ExtensionNumberResponse'_unknownFields
        (\ x__ y__ -> x__ {_ExtensionNumberResponse'_unknownFields = y__})
  defMessage
    = ExtensionNumberResponse'_constructor
        {_ExtensionNumberResponse'baseTypeName = Data.ProtoLens.fieldDefault,
         _ExtensionNumberResponse'extensionNumber = Data.Vector.Generic.empty,
         _ExtensionNumberResponse'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ExtensionNumberResponse
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Unboxed.Vector Data.ProtoLens.Encoding.Growing.RealWorld Data.Int.Int32
             -> Data.ProtoLens.Encoding.Bytes.Parser ExtensionNumberResponse
        loop x mutable'extensionNumber
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'extensionNumber <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                  (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                     mutable'extensionNumber)
                      (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t)
                           (Lens.Family2.set
                              (Data.ProtoLens.Field.field @"vec'extensionNumber")
                              frozen'extensionNumber x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "base_type_name"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"baseTypeName") y x)
                                  mutable'extensionNumber
                        16
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (Prelude.fmap
                                           Prelude.fromIntegral
                                           Data.ProtoLens.Encoding.Bytes.getVarInt)
                                        "extension_number"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append
                                          mutable'extensionNumber y)
                                loop x v
                        18
                          -> do y <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                        Data.ProtoLens.Encoding.Bytes.isolate
                                          (Prelude.fromIntegral len)
                                          ((let
                                              ploop qs
                                                = do packedEnd <- Data.ProtoLens.Encoding.Bytes.atEnd
                                                     if packedEnd then
                                                         Prelude.return qs
                                                     else
                                                         do !q <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                    (Prelude.fmap
                                                                       Prelude.fromIntegral
                                                                       Data.ProtoLens.Encoding.Bytes.getVarInt)
                                                                    "extension_number"
                                                            qs' <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                                     (Data.ProtoLens.Encoding.Growing.append
                                                                        qs q)
                                                            ploop qs'
                                            in ploop)
                                             mutable'extensionNumber)
                                loop x y
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'extensionNumber
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'extensionNumber <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                           Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'extensionNumber)
          "ExtensionNumberResponse"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v
                  = Lens.Family2.view (Data.ProtoLens.Field.field @"baseTypeName") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8 _v))
             ((Data.Monoid.<>)
                (let
                   p = Lens.Family2.view
                         (Data.ProtoLens.Field.field @"vec'extensionNumber") _x
                 in
                   if Data.Vector.Generic.null p then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                         ((\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                            (Data.ProtoLens.Encoding.Bytes.runBuilder
                               (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                                  ((Prelude..)
                                     Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral)
                                  p))))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData ExtensionNumberResponse where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ExtensionNumberResponse'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ExtensionNumberResponse'baseTypeName x__)
                (Control.DeepSeq.deepseq
                   (_ExtensionNumberResponse'extensionNumber x__) ()))
{- | Fields :
     
         * 'Proto.Grpc.Reflection.V1alpha.Reflection_Fields.containingType' @:: Lens' ExtensionRequest Data.Text.Text@
         * 'Proto.Grpc.Reflection.V1alpha.Reflection_Fields.extensionNumber' @:: Lens' ExtensionRequest Data.Int.Int32@ -}
data ExtensionRequest
  = ExtensionRequest'_constructor {_ExtensionRequest'containingType :: !Data.Text.Text,
                                   _ExtensionRequest'extensionNumber :: !Data.Int.Int32,
                                   _ExtensionRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ExtensionRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ExtensionRequest "containingType" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExtensionRequest'containingType
           (\ x__ y__ -> x__ {_ExtensionRequest'containingType = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExtensionRequest "extensionNumber" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExtensionRequest'extensionNumber
           (\ x__ y__ -> x__ {_ExtensionRequest'extensionNumber = y__}))
        Prelude.id
instance Data.ProtoLens.Message ExtensionRequest where
  messageName _
    = Data.Text.pack "grpc.reflection.v1alpha.ExtensionRequest"
  packedMessageDescriptor _
    = "\n\
      \\DLEExtensionRequest\DC2'\n\
      \\SIcontaining_type\CAN\SOH \SOH(\tR\SOcontainingType\DC2)\n\
      \\DLEextension_number\CAN\STX \SOH(\ENQR\SIextensionNumber"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        containingType__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "containing_type"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"containingType")) ::
              Data.ProtoLens.FieldDescriptor ExtensionRequest
        extensionNumber__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "extension_number"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"extensionNumber")) ::
              Data.ProtoLens.FieldDescriptor ExtensionRequest
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, containingType__field_descriptor),
           (Data.ProtoLens.Tag 2, extensionNumber__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ExtensionRequest'_unknownFields
        (\ x__ y__ -> x__ {_ExtensionRequest'_unknownFields = y__})
  defMessage
    = ExtensionRequest'_constructor
        {_ExtensionRequest'containingType = Data.ProtoLens.fieldDefault,
         _ExtensionRequest'extensionNumber = Data.ProtoLens.fieldDefault,
         _ExtensionRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ExtensionRequest
          -> Data.ProtoLens.Encoding.Bytes.Parser ExtensionRequest
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "containing_type"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"containingType") y x)
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "extension_number"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"extensionNumber") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "ExtensionRequest"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v
                  = Lens.Family2.view
                      (Data.ProtoLens.Field.field @"containingType") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8 _v))
             ((Data.Monoid.<>)
                (let
                   _v
                     = Lens.Family2.view
                         (Data.ProtoLens.Field.field @"extensionNumber") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 16)
                         ((Prelude..)
                            Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData ExtensionRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ExtensionRequest'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ExtensionRequest'containingType x__)
                (Control.DeepSeq.deepseq
                   (_ExtensionRequest'extensionNumber x__) ()))
{- | Fields :
     
         * 'Proto.Grpc.Reflection.V1alpha.Reflection_Fields.fileDescriptorProto' @:: Lens' FileDescriptorResponse [Data.ByteString.ByteString]@
         * 'Proto.Grpc.Reflection.V1alpha.Reflection_Fields.vec'fileDescriptorProto' @:: Lens' FileDescriptorResponse (Data.Vector.Vector Data.ByteString.ByteString)@ -}
data FileDescriptorResponse
  = FileDescriptorResponse'_constructor {_FileDescriptorResponse'fileDescriptorProto :: !(Data.Vector.Vector Data.ByteString.ByteString),
                                         _FileDescriptorResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show FileDescriptorResponse where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField FileDescriptorResponse "fileDescriptorProto" [Data.ByteString.ByteString] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _FileDescriptorResponse'fileDescriptorProto
           (\ x__ y__
              -> x__ {_FileDescriptorResponse'fileDescriptorProto = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField FileDescriptorResponse "vec'fileDescriptorProto" (Data.Vector.Vector Data.ByteString.ByteString) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _FileDescriptorResponse'fileDescriptorProto
           (\ x__ y__
              -> x__ {_FileDescriptorResponse'fileDescriptorProto = y__}))
        Prelude.id
instance Data.ProtoLens.Message FileDescriptorResponse where
  messageName _
    = Data.Text.pack "grpc.reflection.v1alpha.FileDescriptorResponse"
  packedMessageDescriptor _
    = "\n\
      \\SYNFileDescriptorResponse\DC22\n\
      \\NAKfile_descriptor_proto\CAN\SOH \ETX(\fR\DC3fileDescriptorProto"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        fileDescriptorProto__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "file_descriptor_proto"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"fileDescriptorProto")) ::
              Data.ProtoLens.FieldDescriptor FileDescriptorResponse
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, fileDescriptorProto__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _FileDescriptorResponse'_unknownFields
        (\ x__ y__ -> x__ {_FileDescriptorResponse'_unknownFields = y__})
  defMessage
    = FileDescriptorResponse'_constructor
        {_FileDescriptorResponse'fileDescriptorProto = Data.Vector.Generic.empty,
         _FileDescriptorResponse'_unknownFields = []}
  parseMessage
    = let
        loop ::
          FileDescriptorResponse
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Data.ByteString.ByteString
             -> Data.ProtoLens.Encoding.Bytes.Parser FileDescriptorResponse
        loop x mutable'fileDescriptorProto
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'fileDescriptorProto <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                      (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                         mutable'fileDescriptorProto)
                      (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t)
                           (Lens.Family2.set
                              (Data.ProtoLens.Field.field @"vec'fileDescriptorProto")
                              frozen'fileDescriptorProto x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.getBytes
                                              (Prelude.fromIntegral len))
                                        "file_descriptor_proto"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append
                                          mutable'fileDescriptorProto y)
                                loop x v
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'fileDescriptorProto
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'fileDescriptorProto <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                               Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'fileDescriptorProto)
          "FileDescriptorResponse"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                (\ _v
                   -> (Data.Monoid.<>)
                        (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                        ((\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                           _v))
                (Lens.Family2.view
                   (Data.ProtoLens.Field.field @"vec'fileDescriptorProto") _x))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData FileDescriptorResponse where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_FileDescriptorResponse'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_FileDescriptorResponse'fileDescriptorProto x__) ())
{- | Fields :
     
         * 'Proto.Grpc.Reflection.V1alpha.Reflection_Fields.service' @:: Lens' ListServiceResponse [ServiceResponse]@
         * 'Proto.Grpc.Reflection.V1alpha.Reflection_Fields.vec'service' @:: Lens' ListServiceResponse (Data.Vector.Vector ServiceResponse)@ -}
data ListServiceResponse
  = ListServiceResponse'_constructor {_ListServiceResponse'service :: !(Data.Vector.Vector ServiceResponse),
                                      _ListServiceResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ListServiceResponse where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ListServiceResponse "service" [ServiceResponse] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ListServiceResponse'service
           (\ x__ y__ -> x__ {_ListServiceResponse'service = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ListServiceResponse "vec'service" (Data.Vector.Vector ServiceResponse) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ListServiceResponse'service
           (\ x__ y__ -> x__ {_ListServiceResponse'service = y__}))
        Prelude.id
instance Data.ProtoLens.Message ListServiceResponse where
  messageName _
    = Data.Text.pack "grpc.reflection.v1alpha.ListServiceResponse"
  packedMessageDescriptor _
    = "\n\
      \\DC3ListServiceResponse\DC2B\n\
      \\aservice\CAN\SOH \ETX(\v2(.grpc.reflection.v1alpha.ServiceResponseR\aservice"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        service__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "service"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ServiceResponse)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"service")) ::
              Data.ProtoLens.FieldDescriptor ListServiceResponse
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, service__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ListServiceResponse'_unknownFields
        (\ x__ y__ -> x__ {_ListServiceResponse'_unknownFields = y__})
  defMessage
    = ListServiceResponse'_constructor
        {_ListServiceResponse'service = Data.Vector.Generic.empty,
         _ListServiceResponse'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ListServiceResponse
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld ServiceResponse
             -> Data.ProtoLens.Encoding.Bytes.Parser ListServiceResponse
        loop x mutable'service
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'service <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                          (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                             mutable'service)
                      (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t)
                           (Lens.Family2.set
                              (Data.ProtoLens.Field.field @"vec'service") frozen'service x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "service"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'service y)
                                loop x v
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'service
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'service <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                   Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'service)
          "ListServiceResponse"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                (\ _v
                   -> (Data.Monoid.<>)
                        (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                        ((Prelude..)
                           (\ bs
                              -> (Data.Monoid.<>)
                                   (Data.ProtoLens.Encoding.Bytes.putVarInt
                                      (Prelude.fromIntegral (Data.ByteString.length bs)))
                                   (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                           Data.ProtoLens.encodeMessage _v))
                (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'service") _x))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData ListServiceResponse where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ListServiceResponse'_unknownFields x__)
             (Control.DeepSeq.deepseq (_ListServiceResponse'service x__) ())
{- | Fields :
     
         * 'Proto.Grpc.Reflection.V1alpha.Reflection_Fields.host' @:: Lens' ServerReflectionRequest Data.Text.Text@
         * 'Proto.Grpc.Reflection.V1alpha.Reflection_Fields.maybe'messageRequest' @:: Lens' ServerReflectionRequest (Prelude.Maybe ServerReflectionRequest'MessageRequest)@
         * 'Proto.Grpc.Reflection.V1alpha.Reflection_Fields.maybe'fileByFilename' @:: Lens' ServerReflectionRequest (Prelude.Maybe Data.Text.Text)@
         * 'Proto.Grpc.Reflection.V1alpha.Reflection_Fields.fileByFilename' @:: Lens' ServerReflectionRequest Data.Text.Text@
         * 'Proto.Grpc.Reflection.V1alpha.Reflection_Fields.maybe'fileContainingSymbol' @:: Lens' ServerReflectionRequest (Prelude.Maybe Data.Text.Text)@
         * 'Proto.Grpc.Reflection.V1alpha.Reflection_Fields.fileContainingSymbol' @:: Lens' ServerReflectionRequest Data.Text.Text@
         * 'Proto.Grpc.Reflection.V1alpha.Reflection_Fields.maybe'fileContainingExtension' @:: Lens' ServerReflectionRequest (Prelude.Maybe ExtensionRequest)@
         * 'Proto.Grpc.Reflection.V1alpha.Reflection_Fields.fileContainingExtension' @:: Lens' ServerReflectionRequest ExtensionRequest@
         * 'Proto.Grpc.Reflection.V1alpha.Reflection_Fields.maybe'allExtensionNumbersOfType' @:: Lens' ServerReflectionRequest (Prelude.Maybe Data.Text.Text)@
         * 'Proto.Grpc.Reflection.V1alpha.Reflection_Fields.allExtensionNumbersOfType' @:: Lens' ServerReflectionRequest Data.Text.Text@
         * 'Proto.Grpc.Reflection.V1alpha.Reflection_Fields.maybe'listServices' @:: Lens' ServerReflectionRequest (Prelude.Maybe Data.Text.Text)@
         * 'Proto.Grpc.Reflection.V1alpha.Reflection_Fields.listServices' @:: Lens' ServerReflectionRequest Data.Text.Text@ -}
data ServerReflectionRequest
  = ServerReflectionRequest'_constructor {_ServerReflectionRequest'host :: !Data.Text.Text,
                                          _ServerReflectionRequest'messageRequest :: !(Prelude.Maybe ServerReflectionRequest'MessageRequest),
                                          _ServerReflectionRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ServerReflectionRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
data ServerReflectionRequest'MessageRequest
  = ServerReflectionRequest'FileByFilename !Data.Text.Text |
    ServerReflectionRequest'FileContainingSymbol !Data.Text.Text |
    ServerReflectionRequest'FileContainingExtension !ExtensionRequest |
    ServerReflectionRequest'AllExtensionNumbersOfType !Data.Text.Text |
    ServerReflectionRequest'ListServices !Data.Text.Text
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.Field.HasField ServerReflectionRequest "host" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ServerReflectionRequest'host
           (\ x__ y__ -> x__ {_ServerReflectionRequest'host = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ServerReflectionRequest "maybe'messageRequest" (Prelude.Maybe ServerReflectionRequest'MessageRequest) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ServerReflectionRequest'messageRequest
           (\ x__ y__ -> x__ {_ServerReflectionRequest'messageRequest = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ServerReflectionRequest "maybe'fileByFilename" (Prelude.Maybe Data.Text.Text) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ServerReflectionRequest'messageRequest
           (\ x__ y__ -> x__ {_ServerReflectionRequest'messageRequest = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (ServerReflectionRequest'FileByFilename x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__
              -> Prelude.fmap ServerReflectionRequest'FileByFilename y__))
instance Data.ProtoLens.Field.HasField ServerReflectionRequest "fileByFilename" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ServerReflectionRequest'messageRequest
           (\ x__ y__ -> x__ {_ServerReflectionRequest'messageRequest = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (ServerReflectionRequest'FileByFilename x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__
                 -> Prelude.fmap ServerReflectionRequest'FileByFilename y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault))
instance Data.ProtoLens.Field.HasField ServerReflectionRequest "maybe'fileContainingSymbol" (Prelude.Maybe Data.Text.Text) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ServerReflectionRequest'messageRequest
           (\ x__ y__ -> x__ {_ServerReflectionRequest'messageRequest = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (ServerReflectionRequest'FileContainingSymbol x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__
              -> Prelude.fmap ServerReflectionRequest'FileContainingSymbol y__))
instance Data.ProtoLens.Field.HasField ServerReflectionRequest "fileContainingSymbol" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ServerReflectionRequest'messageRequest
           (\ x__ y__ -> x__ {_ServerReflectionRequest'messageRequest = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (ServerReflectionRequest'FileContainingSymbol x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__
                 -> Prelude.fmap ServerReflectionRequest'FileContainingSymbol y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault))
instance Data.ProtoLens.Field.HasField ServerReflectionRequest "maybe'fileContainingExtension" (Prelude.Maybe ExtensionRequest) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ServerReflectionRequest'messageRequest
           (\ x__ y__ -> x__ {_ServerReflectionRequest'messageRequest = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (ServerReflectionRequest'FileContainingExtension x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__
              -> Prelude.fmap
                   ServerReflectionRequest'FileContainingExtension y__))
instance Data.ProtoLens.Field.HasField ServerReflectionRequest "fileContainingExtension" ExtensionRequest where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ServerReflectionRequest'messageRequest
           (\ x__ y__ -> x__ {_ServerReflectionRequest'messageRequest = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (ServerReflectionRequest'FileContainingExtension x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__
                 -> Prelude.fmap
                      ServerReflectionRequest'FileContainingExtension y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Field.HasField ServerReflectionRequest "maybe'allExtensionNumbersOfType" (Prelude.Maybe Data.Text.Text) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ServerReflectionRequest'messageRequest
           (\ x__ y__ -> x__ {_ServerReflectionRequest'messageRequest = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (ServerReflectionRequest'AllExtensionNumbersOfType x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__
              -> Prelude.fmap
                   ServerReflectionRequest'AllExtensionNumbersOfType y__))
instance Data.ProtoLens.Field.HasField ServerReflectionRequest "allExtensionNumbersOfType" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ServerReflectionRequest'messageRequest
           (\ x__ y__ -> x__ {_ServerReflectionRequest'messageRequest = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (ServerReflectionRequest'AllExtensionNumbersOfType x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__
                 -> Prelude.fmap
                      ServerReflectionRequest'AllExtensionNumbersOfType y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault))
instance Data.ProtoLens.Field.HasField ServerReflectionRequest "maybe'listServices" (Prelude.Maybe Data.Text.Text) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ServerReflectionRequest'messageRequest
           (\ x__ y__ -> x__ {_ServerReflectionRequest'messageRequest = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (ServerReflectionRequest'ListServices x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap ServerReflectionRequest'ListServices y__))
instance Data.ProtoLens.Field.HasField ServerReflectionRequest "listServices" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ServerReflectionRequest'messageRequest
           (\ x__ y__ -> x__ {_ServerReflectionRequest'messageRequest = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (ServerReflectionRequest'ListServices x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap ServerReflectionRequest'ListServices y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault))
instance Data.ProtoLens.Message ServerReflectionRequest where
  messageName _
    = Data.Text.pack "grpc.reflection.v1alpha.ServerReflectionRequest"
  packedMessageDescriptor _
    = "\n\
      \\ETBServerReflectionRequest\DC2\DC2\n\
      \\EOThost\CAN\SOH \SOH(\tR\EOThost\DC2*\n\
      \\DLEfile_by_filename\CAN\ETX \SOH(\tH\NULR\SOfileByFilename\DC26\n\
      \\SYNfile_containing_symbol\CAN\EOT \SOH(\tH\NULR\DC4fileContainingSymbol\DC2g\n\
      \\EMfile_containing_extension\CAN\ENQ \SOH(\v2).grpc.reflection.v1alpha.ExtensionRequestH\NULR\ETBfileContainingExtension\DC2B\n\
      \\GSall_extension_numbers_of_type\CAN\ACK \SOH(\tH\NULR\EMallExtensionNumbersOfType\DC2%\n\
      \\rlist_services\CAN\a \SOH(\tH\NULR\flistServicesB\DC1\n\
      \\SImessage_request"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        host__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "host"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"host")) ::
              Data.ProtoLens.FieldDescriptor ServerReflectionRequest
        fileByFilename__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "file_by_filename"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'fileByFilename")) ::
              Data.ProtoLens.FieldDescriptor ServerReflectionRequest
        fileContainingSymbol__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "file_containing_symbol"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'fileContainingSymbol")) ::
              Data.ProtoLens.FieldDescriptor ServerReflectionRequest
        fileContainingExtension__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "file_containing_extension"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ExtensionRequest)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'fileContainingExtension")) ::
              Data.ProtoLens.FieldDescriptor ServerReflectionRequest
        allExtensionNumbersOfType__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "all_extension_numbers_of_type"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'allExtensionNumbersOfType")) ::
              Data.ProtoLens.FieldDescriptor ServerReflectionRequest
        listServices__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "list_services"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'listServices")) ::
              Data.ProtoLens.FieldDescriptor ServerReflectionRequest
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, host__field_descriptor),
           (Data.ProtoLens.Tag 3, fileByFilename__field_descriptor),
           (Data.ProtoLens.Tag 4, fileContainingSymbol__field_descriptor),
           (Data.ProtoLens.Tag 5, fileContainingExtension__field_descriptor),
           (Data.ProtoLens.Tag 6, 
            allExtensionNumbersOfType__field_descriptor),
           (Data.ProtoLens.Tag 7, listServices__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ServerReflectionRequest'_unknownFields
        (\ x__ y__ -> x__ {_ServerReflectionRequest'_unknownFields = y__})
  defMessage
    = ServerReflectionRequest'_constructor
        {_ServerReflectionRequest'host = Data.ProtoLens.fieldDefault,
         _ServerReflectionRequest'messageRequest = Prelude.Nothing,
         _ServerReflectionRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ServerReflectionRequest
          -> Data.ProtoLens.Encoding.Bytes.Parser ServerReflectionRequest
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "host"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"host") y x)
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "file_by_filename"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"fileByFilename") y x)
                        34
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "file_containing_symbol"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"fileContainingSymbol") y x)
                        42
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "file_containing_extension"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"fileContainingExtension") y x)
                        50
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "all_extension_numbers_of_type"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"allExtensionNumbersOfType") y x)
                        58
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "list_services"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"listServices") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "ServerReflectionRequest"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"host") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8 _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view
                       (Data.ProtoLens.Field.field @"maybe'messageRequest") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just (ServerReflectionRequest'FileByFilename v))
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.Text.Encoding.encodeUtf8 v)
                   (Prelude.Just (ServerReflectionRequest'FileContainingSymbol v))
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.Text.Encoding.encodeUtf8 v)
                   (Prelude.Just (ServerReflectionRequest'FileContainingExtension v))
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 42)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage v)
                   (Prelude.Just (ServerReflectionRequest'AllExtensionNumbersOfType v))
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 50)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.Text.Encoding.encodeUtf8 v)
                   (Prelude.Just (ServerReflectionRequest'ListServices v))
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 58)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.Text.Encoding.encodeUtf8 v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData ServerReflectionRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ServerReflectionRequest'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ServerReflectionRequest'host x__)
                (Control.DeepSeq.deepseq
                   (_ServerReflectionRequest'messageRequest x__) ()))
instance Control.DeepSeq.NFData ServerReflectionRequest'MessageRequest where
  rnf (ServerReflectionRequest'FileByFilename x__)
    = Control.DeepSeq.rnf x__
  rnf (ServerReflectionRequest'FileContainingSymbol x__)
    = Control.DeepSeq.rnf x__
  rnf (ServerReflectionRequest'FileContainingExtension x__)
    = Control.DeepSeq.rnf x__
  rnf (ServerReflectionRequest'AllExtensionNumbersOfType x__)
    = Control.DeepSeq.rnf x__
  rnf (ServerReflectionRequest'ListServices x__)
    = Control.DeepSeq.rnf x__
_ServerReflectionRequest'FileByFilename ::
  Data.ProtoLens.Prism.Prism' ServerReflectionRequest'MessageRequest Data.Text.Text
_ServerReflectionRequest'FileByFilename
  = Data.ProtoLens.Prism.prism'
      ServerReflectionRequest'FileByFilename
      (\ p__
         -> case p__ of
              (ServerReflectionRequest'FileByFilename p__val)
                -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_ServerReflectionRequest'FileContainingSymbol ::
  Data.ProtoLens.Prism.Prism' ServerReflectionRequest'MessageRequest Data.Text.Text
_ServerReflectionRequest'FileContainingSymbol
  = Data.ProtoLens.Prism.prism'
      ServerReflectionRequest'FileContainingSymbol
      (\ p__
         -> case p__ of
              (ServerReflectionRequest'FileContainingSymbol p__val)
                -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_ServerReflectionRequest'FileContainingExtension ::
  Data.ProtoLens.Prism.Prism' ServerReflectionRequest'MessageRequest ExtensionRequest
_ServerReflectionRequest'FileContainingExtension
  = Data.ProtoLens.Prism.prism'
      ServerReflectionRequest'FileContainingExtension
      (\ p__
         -> case p__ of
              (ServerReflectionRequest'FileContainingExtension p__val)
                -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_ServerReflectionRequest'AllExtensionNumbersOfType ::
  Data.ProtoLens.Prism.Prism' ServerReflectionRequest'MessageRequest Data.Text.Text
_ServerReflectionRequest'AllExtensionNumbersOfType
  = Data.ProtoLens.Prism.prism'
      ServerReflectionRequest'AllExtensionNumbersOfType
      (\ p__
         -> case p__ of
              (ServerReflectionRequest'AllExtensionNumbersOfType p__val)
                -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_ServerReflectionRequest'ListServices ::
  Data.ProtoLens.Prism.Prism' ServerReflectionRequest'MessageRequest Data.Text.Text
_ServerReflectionRequest'ListServices
  = Data.ProtoLens.Prism.prism'
      ServerReflectionRequest'ListServices
      (\ p__
         -> case p__ of
              (ServerReflectionRequest'ListServices p__val)
                -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
{- | Fields :
     
         * 'Proto.Grpc.Reflection.V1alpha.Reflection_Fields.validHost' @:: Lens' ServerReflectionResponse Data.Text.Text@
         * 'Proto.Grpc.Reflection.V1alpha.Reflection_Fields.originalRequest' @:: Lens' ServerReflectionResponse ServerReflectionRequest@
         * 'Proto.Grpc.Reflection.V1alpha.Reflection_Fields.maybe'originalRequest' @:: Lens' ServerReflectionResponse (Prelude.Maybe ServerReflectionRequest)@
         * 'Proto.Grpc.Reflection.V1alpha.Reflection_Fields.maybe'messageResponse' @:: Lens' ServerReflectionResponse (Prelude.Maybe ServerReflectionResponse'MessageResponse)@
         * 'Proto.Grpc.Reflection.V1alpha.Reflection_Fields.maybe'fileDescriptorResponse' @:: Lens' ServerReflectionResponse (Prelude.Maybe FileDescriptorResponse)@
         * 'Proto.Grpc.Reflection.V1alpha.Reflection_Fields.fileDescriptorResponse' @:: Lens' ServerReflectionResponse FileDescriptorResponse@
         * 'Proto.Grpc.Reflection.V1alpha.Reflection_Fields.maybe'allExtensionNumbersResponse' @:: Lens' ServerReflectionResponse (Prelude.Maybe ExtensionNumberResponse)@
         * 'Proto.Grpc.Reflection.V1alpha.Reflection_Fields.allExtensionNumbersResponse' @:: Lens' ServerReflectionResponse ExtensionNumberResponse@
         * 'Proto.Grpc.Reflection.V1alpha.Reflection_Fields.maybe'listServicesResponse' @:: Lens' ServerReflectionResponse (Prelude.Maybe ListServiceResponse)@
         * 'Proto.Grpc.Reflection.V1alpha.Reflection_Fields.listServicesResponse' @:: Lens' ServerReflectionResponse ListServiceResponse@
         * 'Proto.Grpc.Reflection.V1alpha.Reflection_Fields.maybe'errorResponse' @:: Lens' ServerReflectionResponse (Prelude.Maybe ErrorResponse)@
         * 'Proto.Grpc.Reflection.V1alpha.Reflection_Fields.errorResponse' @:: Lens' ServerReflectionResponse ErrorResponse@ -}
data ServerReflectionResponse
  = ServerReflectionResponse'_constructor {_ServerReflectionResponse'validHost :: !Data.Text.Text,
                                           _ServerReflectionResponse'originalRequest :: !(Prelude.Maybe ServerReflectionRequest),
                                           _ServerReflectionResponse'messageResponse :: !(Prelude.Maybe ServerReflectionResponse'MessageResponse),
                                           _ServerReflectionResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ServerReflectionResponse where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
data ServerReflectionResponse'MessageResponse
  = ServerReflectionResponse'FileDescriptorResponse !FileDescriptorResponse |
    ServerReflectionResponse'AllExtensionNumbersResponse !ExtensionNumberResponse |
    ServerReflectionResponse'ListServicesResponse !ListServiceResponse |
    ServerReflectionResponse'ErrorResponse !ErrorResponse
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.Field.HasField ServerReflectionResponse "validHost" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ServerReflectionResponse'validHost
           (\ x__ y__ -> x__ {_ServerReflectionResponse'validHost = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ServerReflectionResponse "originalRequest" ServerReflectionRequest where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ServerReflectionResponse'originalRequest
           (\ x__ y__
              -> x__ {_ServerReflectionResponse'originalRequest = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ServerReflectionResponse "maybe'originalRequest" (Prelude.Maybe ServerReflectionRequest) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ServerReflectionResponse'originalRequest
           (\ x__ y__
              -> x__ {_ServerReflectionResponse'originalRequest = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ServerReflectionResponse "maybe'messageResponse" (Prelude.Maybe ServerReflectionResponse'MessageResponse) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ServerReflectionResponse'messageResponse
           (\ x__ y__
              -> x__ {_ServerReflectionResponse'messageResponse = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ServerReflectionResponse "maybe'fileDescriptorResponse" (Prelude.Maybe FileDescriptorResponse) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ServerReflectionResponse'messageResponse
           (\ x__ y__
              -> x__ {_ServerReflectionResponse'messageResponse = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (ServerReflectionResponse'FileDescriptorResponse x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__
              -> Prelude.fmap
                   ServerReflectionResponse'FileDescriptorResponse y__))
instance Data.ProtoLens.Field.HasField ServerReflectionResponse "fileDescriptorResponse" FileDescriptorResponse where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ServerReflectionResponse'messageResponse
           (\ x__ y__
              -> x__ {_ServerReflectionResponse'messageResponse = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (ServerReflectionResponse'FileDescriptorResponse x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__
                 -> Prelude.fmap
                      ServerReflectionResponse'FileDescriptorResponse y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Field.HasField ServerReflectionResponse "maybe'allExtensionNumbersResponse" (Prelude.Maybe ExtensionNumberResponse) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ServerReflectionResponse'messageResponse
           (\ x__ y__
              -> x__ {_ServerReflectionResponse'messageResponse = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (ServerReflectionResponse'AllExtensionNumbersResponse x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__
              -> Prelude.fmap
                   ServerReflectionResponse'AllExtensionNumbersResponse y__))
instance Data.ProtoLens.Field.HasField ServerReflectionResponse "allExtensionNumbersResponse" ExtensionNumberResponse where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ServerReflectionResponse'messageResponse
           (\ x__ y__
              -> x__ {_ServerReflectionResponse'messageResponse = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (ServerReflectionResponse'AllExtensionNumbersResponse x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__
                 -> Prelude.fmap
                      ServerReflectionResponse'AllExtensionNumbersResponse y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Field.HasField ServerReflectionResponse "maybe'listServicesResponse" (Prelude.Maybe ListServiceResponse) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ServerReflectionResponse'messageResponse
           (\ x__ y__
              -> x__ {_ServerReflectionResponse'messageResponse = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (ServerReflectionResponse'ListServicesResponse x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__
              -> Prelude.fmap ServerReflectionResponse'ListServicesResponse y__))
instance Data.ProtoLens.Field.HasField ServerReflectionResponse "listServicesResponse" ListServiceResponse where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ServerReflectionResponse'messageResponse
           (\ x__ y__
              -> x__ {_ServerReflectionResponse'messageResponse = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (ServerReflectionResponse'ListServicesResponse x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__
                 -> Prelude.fmap ServerReflectionResponse'ListServicesResponse y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Field.HasField ServerReflectionResponse "maybe'errorResponse" (Prelude.Maybe ErrorResponse) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ServerReflectionResponse'messageResponse
           (\ x__ y__
              -> x__ {_ServerReflectionResponse'messageResponse = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (ServerReflectionResponse'ErrorResponse x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__
              -> Prelude.fmap ServerReflectionResponse'ErrorResponse y__))
instance Data.ProtoLens.Field.HasField ServerReflectionResponse "errorResponse" ErrorResponse where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ServerReflectionResponse'messageResponse
           (\ x__ y__
              -> x__ {_ServerReflectionResponse'messageResponse = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (ServerReflectionResponse'ErrorResponse x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__
                 -> Prelude.fmap ServerReflectionResponse'ErrorResponse y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Message ServerReflectionResponse where
  messageName _
    = Data.Text.pack "grpc.reflection.v1alpha.ServerReflectionResponse"
  packedMessageDescriptor _
    = "\n\
      \\CANServerReflectionResponse\DC2\GS\n\
      \\n\
      \valid_host\CAN\SOH \SOH(\tR\tvalidHost\DC2[\n\
      \\DLEoriginal_request\CAN\STX \SOH(\v20.grpc.reflection.v1alpha.ServerReflectionRequestR\SIoriginalRequest\DC2k\n\
      \\CANfile_descriptor_response\CAN\EOT \SOH(\v2/.grpc.reflection.v1alpha.FileDescriptorResponseH\NULR\SYNfileDescriptorResponse\DC2w\n\
      \\RSall_extension_numbers_response\CAN\ENQ \SOH(\v20.grpc.reflection.v1alpha.ExtensionNumberResponseH\NULR\ESCallExtensionNumbersResponse\DC2d\n\
      \\SYNlist_services_response\CAN\ACK \SOH(\v2,.grpc.reflection.v1alpha.ListServiceResponseH\NULR\DC4listServicesResponse\DC2O\n\
      \\SOerror_response\CAN\a \SOH(\v2&.grpc.reflection.v1alpha.ErrorResponseH\NULR\rerrorResponseB\DC2\n\
      \\DLEmessage_response"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        validHost__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "valid_host"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"validHost")) ::
              Data.ProtoLens.FieldDescriptor ServerReflectionResponse
        originalRequest__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "original_request"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ServerReflectionRequest)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'originalRequest")) ::
              Data.ProtoLens.FieldDescriptor ServerReflectionResponse
        fileDescriptorResponse__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "file_descriptor_response"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor FileDescriptorResponse)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'fileDescriptorResponse")) ::
              Data.ProtoLens.FieldDescriptor ServerReflectionResponse
        allExtensionNumbersResponse__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "all_extension_numbers_response"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ExtensionNumberResponse)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field
                    @"maybe'allExtensionNumbersResponse")) ::
              Data.ProtoLens.FieldDescriptor ServerReflectionResponse
        listServicesResponse__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "list_services_response"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ListServiceResponse)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'listServicesResponse")) ::
              Data.ProtoLens.FieldDescriptor ServerReflectionResponse
        errorResponse__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "error_response"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ErrorResponse)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'errorResponse")) ::
              Data.ProtoLens.FieldDescriptor ServerReflectionResponse
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, validHost__field_descriptor),
           (Data.ProtoLens.Tag 2, originalRequest__field_descriptor),
           (Data.ProtoLens.Tag 4, fileDescriptorResponse__field_descriptor),
           (Data.ProtoLens.Tag 5, 
            allExtensionNumbersResponse__field_descriptor),
           (Data.ProtoLens.Tag 6, listServicesResponse__field_descriptor),
           (Data.ProtoLens.Tag 7, errorResponse__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ServerReflectionResponse'_unknownFields
        (\ x__ y__ -> x__ {_ServerReflectionResponse'_unknownFields = y__})
  defMessage
    = ServerReflectionResponse'_constructor
        {_ServerReflectionResponse'validHost = Data.ProtoLens.fieldDefault,
         _ServerReflectionResponse'originalRequest = Prelude.Nothing,
         _ServerReflectionResponse'messageResponse = Prelude.Nothing,
         _ServerReflectionResponse'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ServerReflectionResponse
          -> Data.ProtoLens.Encoding.Bytes.Parser ServerReflectionResponse
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "valid_host"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"validHost") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "original_request"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"originalRequest") y x)
                        34
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "file_descriptor_response"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"fileDescriptorResponse") y x)
                        42
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "all_extension_numbers_response"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"allExtensionNumbersResponse") y
                                     x)
                        50
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "list_services_response"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"listServicesResponse") y x)
                        58
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "error_response"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"errorResponse") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "ServerReflectionResponse"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"validHost") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8 _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view
                       (Data.ProtoLens.Field.field @"maybe'originalRequest") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just _v)
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage _v))
                ((Data.Monoid.<>)
                   (case
                        Lens.Family2.view
                          (Data.ProtoLens.Field.field @"maybe'messageResponse") _x
                    of
                      Prelude.Nothing -> Data.Monoid.mempty
                      (Prelude.Just (ServerReflectionResponse'FileDescriptorResponse v))
                        -> (Data.Monoid.<>)
                             (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                             ((Prelude..)
                                (\ bs
                                   -> (Data.Monoid.<>)
                                        (Data.ProtoLens.Encoding.Bytes.putVarInt
                                           (Prelude.fromIntegral (Data.ByteString.length bs)))
                                        (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                Data.ProtoLens.encodeMessage v)
                      (Prelude.Just (ServerReflectionResponse'AllExtensionNumbersResponse v))
                        -> (Data.Monoid.<>)
                             (Data.ProtoLens.Encoding.Bytes.putVarInt 42)
                             ((Prelude..)
                                (\ bs
                                   -> (Data.Monoid.<>)
                                        (Data.ProtoLens.Encoding.Bytes.putVarInt
                                           (Prelude.fromIntegral (Data.ByteString.length bs)))
                                        (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                Data.ProtoLens.encodeMessage v)
                      (Prelude.Just (ServerReflectionResponse'ListServicesResponse v))
                        -> (Data.Monoid.<>)
                             (Data.ProtoLens.Encoding.Bytes.putVarInt 50)
                             ((Prelude..)
                                (\ bs
                                   -> (Data.Monoid.<>)
                                        (Data.ProtoLens.Encoding.Bytes.putVarInt
                                           (Prelude.fromIntegral (Data.ByteString.length bs)))
                                        (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                Data.ProtoLens.encodeMessage v)
                      (Prelude.Just (ServerReflectionResponse'ErrorResponse v))
                        -> (Data.Monoid.<>)
                             (Data.ProtoLens.Encoding.Bytes.putVarInt 58)
                             ((Prelude..)
                                (\ bs
                                   -> (Data.Monoid.<>)
                                        (Data.ProtoLens.Encoding.Bytes.putVarInt
                                           (Prelude.fromIntegral (Data.ByteString.length bs)))
                                        (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                Data.ProtoLens.encodeMessage v))
                   (Data.ProtoLens.Encoding.Wire.buildFieldSet
                      (Lens.Family2.view Data.ProtoLens.unknownFields _x))))
instance Control.DeepSeq.NFData ServerReflectionResponse where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ServerReflectionResponse'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ServerReflectionResponse'validHost x__)
                (Control.DeepSeq.deepseq
                   (_ServerReflectionResponse'originalRequest x__)
                   (Control.DeepSeq.deepseq
                      (_ServerReflectionResponse'messageResponse x__) ())))
instance Control.DeepSeq.NFData ServerReflectionResponse'MessageResponse where
  rnf (ServerReflectionResponse'FileDescriptorResponse x__)
    = Control.DeepSeq.rnf x__
  rnf (ServerReflectionResponse'AllExtensionNumbersResponse x__)
    = Control.DeepSeq.rnf x__
  rnf (ServerReflectionResponse'ListServicesResponse x__)
    = Control.DeepSeq.rnf x__
  rnf (ServerReflectionResponse'ErrorResponse x__)
    = Control.DeepSeq.rnf x__
_ServerReflectionResponse'FileDescriptorResponse ::
  Data.ProtoLens.Prism.Prism' ServerReflectionResponse'MessageResponse FileDescriptorResponse
_ServerReflectionResponse'FileDescriptorResponse
  = Data.ProtoLens.Prism.prism'
      ServerReflectionResponse'FileDescriptorResponse
      (\ p__
         -> case p__ of
              (ServerReflectionResponse'FileDescriptorResponse p__val)
                -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_ServerReflectionResponse'AllExtensionNumbersResponse ::
  Data.ProtoLens.Prism.Prism' ServerReflectionResponse'MessageResponse ExtensionNumberResponse
_ServerReflectionResponse'AllExtensionNumbersResponse
  = Data.ProtoLens.Prism.prism'
      ServerReflectionResponse'AllExtensionNumbersResponse
      (\ p__
         -> case p__ of
              (ServerReflectionResponse'AllExtensionNumbersResponse p__val)
                -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_ServerReflectionResponse'ListServicesResponse ::
  Data.ProtoLens.Prism.Prism' ServerReflectionResponse'MessageResponse ListServiceResponse
_ServerReflectionResponse'ListServicesResponse
  = Data.ProtoLens.Prism.prism'
      ServerReflectionResponse'ListServicesResponse
      (\ p__
         -> case p__ of
              (ServerReflectionResponse'ListServicesResponse p__val)
                -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_ServerReflectionResponse'ErrorResponse ::
  Data.ProtoLens.Prism.Prism' ServerReflectionResponse'MessageResponse ErrorResponse
_ServerReflectionResponse'ErrorResponse
  = Data.ProtoLens.Prism.prism'
      ServerReflectionResponse'ErrorResponse
      (\ p__
         -> case p__ of
              (ServerReflectionResponse'ErrorResponse p__val)
                -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
{- | Fields :
     
         * 'Proto.Grpc.Reflection.V1alpha.Reflection_Fields.name' @:: Lens' ServiceResponse Data.Text.Text@ -}
data ServiceResponse
  = ServiceResponse'_constructor {_ServiceResponse'name :: !Data.Text.Text,
                                  _ServiceResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ServiceResponse where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ServiceResponse "name" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ServiceResponse'name
           (\ x__ y__ -> x__ {_ServiceResponse'name = y__}))
        Prelude.id
instance Data.ProtoLens.Message ServiceResponse where
  messageName _
    = Data.Text.pack "grpc.reflection.v1alpha.ServiceResponse"
  packedMessageDescriptor _
    = "\n\
      \\SIServiceResponse\DC2\DC2\n\
      \\EOTname\CAN\SOH \SOH(\tR\EOTname"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        name__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "name"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"name")) ::
              Data.ProtoLens.FieldDescriptor ServiceResponse
      in
        Data.Map.fromList [(Data.ProtoLens.Tag 1, name__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ServiceResponse'_unknownFields
        (\ x__ y__ -> x__ {_ServiceResponse'_unknownFields = y__})
  defMessage
    = ServiceResponse'_constructor
        {_ServiceResponse'name = Data.ProtoLens.fieldDefault,
         _ServiceResponse'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ServiceResponse
          -> Data.ProtoLens.Encoding.Bytes.Parser ServiceResponse
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "name"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"name") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "ServiceResponse"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"name") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8 _v))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData ServiceResponse where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ServiceResponse'_unknownFields x__)
             (Control.DeepSeq.deepseq (_ServiceResponse'name x__) ())
data ServerReflection = ServerReflection {}
instance Data.ProtoLens.Service.Types.Service ServerReflection where
  type ServiceName ServerReflection = "ServerReflection"
  type ServicePackage ServerReflection = "grpc.reflection.v1alpha"
  type ServiceMethods ServerReflection = '["serverReflectionInfo"]
  packedServiceDescriptor _
    = "\n\
      \\DLEServerReflection\DC2\DEL\n\
      \\DC4ServerReflectionInfo\DC20.grpc.reflection.v1alpha.ServerReflectionRequest\SUB1.grpc.reflection.v1alpha.ServerReflectionResponse(\SOH0\SOH"
instance Data.ProtoLens.Service.Types.HasMethodImpl ServerReflection "serverReflectionInfo" where
  type MethodName ServerReflection "serverReflectionInfo" = "ServerReflectionInfo"
  type MethodInput ServerReflection "serverReflectionInfo" = ServerReflectionRequest
  type MethodOutput ServerReflection "serverReflectionInfo" = ServerReflectionResponse
  type MethodStreamingType ServerReflection "serverReflectionInfo" = 'Data.ProtoLens.Service.Types.BiDiStreaming
packedFileDescriptor :: Data.ByteString.ByteString
packedFileDescriptor
  = "\n\
    \(grpc/reflection/v1alpha/reflection.proto\DC2\ETBgrpc.reflection.v1alpha\"\248\STX\n\
    \\ETBServerReflectionRequest\DC2\DC2\n\
    \\EOThost\CAN\SOH \SOH(\tR\EOThost\DC2*\n\
    \\DLEfile_by_filename\CAN\ETX \SOH(\tH\NULR\SOfileByFilename\DC26\n\
    \\SYNfile_containing_symbol\CAN\EOT \SOH(\tH\NULR\DC4fileContainingSymbol\DC2g\n\
    \\EMfile_containing_extension\CAN\ENQ \SOH(\v2).grpc.reflection.v1alpha.ExtensionRequestH\NULR\ETBfileContainingExtension\DC2B\n\
    \\GSall_extension_numbers_of_type\CAN\ACK \SOH(\tH\NULR\EMallExtensionNumbersOfType\DC2%\n\
    \\rlist_services\CAN\a \SOH(\tH\NULR\flistServicesB\DC1\n\
    \\SImessage_request\"f\n\
    \\DLEExtensionRequest\DC2'\n\
    \\SIcontaining_type\CAN\SOH \SOH(\tR\SOcontainingType\DC2)\n\
    \\DLEextension_number\CAN\STX \SOH(\ENQR\SIextensionNumber\"\199\EOT\n\
    \\CANServerReflectionResponse\DC2\GS\n\
    \\n\
    \valid_host\CAN\SOH \SOH(\tR\tvalidHost\DC2[\n\
    \\DLEoriginal_request\CAN\STX \SOH(\v20.grpc.reflection.v1alpha.ServerReflectionRequestR\SIoriginalRequest\DC2k\n\
    \\CANfile_descriptor_response\CAN\EOT \SOH(\v2/.grpc.reflection.v1alpha.FileDescriptorResponseH\NULR\SYNfileDescriptorResponse\DC2w\n\
    \\RSall_extension_numbers_response\CAN\ENQ \SOH(\v20.grpc.reflection.v1alpha.ExtensionNumberResponseH\NULR\ESCallExtensionNumbersResponse\DC2d\n\
    \\SYNlist_services_response\CAN\ACK \SOH(\v2,.grpc.reflection.v1alpha.ListServiceResponseH\NULR\DC4listServicesResponse\DC2O\n\
    \\SOerror_response\CAN\a \SOH(\v2&.grpc.reflection.v1alpha.ErrorResponseH\NULR\rerrorResponseB\DC2\n\
    \\DLEmessage_response\"L\n\
    \\SYNFileDescriptorResponse\DC22\n\
    \\NAKfile_descriptor_proto\CAN\SOH \ETX(\fR\DC3fileDescriptorProto\"j\n\
    \\ETBExtensionNumberResponse\DC2$\n\
    \\SObase_type_name\CAN\SOH \SOH(\tR\fbaseTypeName\DC2)\n\
    \\DLEextension_number\CAN\STX \ETX(\ENQR\SIextensionNumber\"Y\n\
    \\DC3ListServiceResponse\DC2B\n\
    \\aservice\CAN\SOH \ETX(\v2(.grpc.reflection.v1alpha.ServiceResponseR\aservice\"%\n\
    \\SIServiceResponse\DC2\DC2\n\
    \\EOTname\CAN\SOH \SOH(\tR\EOTname\"S\n\
    \\rErrorResponse\DC2\GS\n\
    \\n\
    \error_code\CAN\SOH \SOH(\ENQR\terrorCode\DC2#\n\
    \\rerror_message\CAN\STX \SOH(\tR\ferrorMessage2\147\SOH\n\
    \\DLEServerReflection\DC2\DEL\n\
    \\DC4ServerReflectionInfo\DC20.grpc.reflection.v1alpha.ServerReflectionRequest\SUB1.grpc.reflection.v1alpha.ServerReflectionResponse(\SOH0\SOHB\172\SOH\n\
    \\ESCcom.grpc.reflection.v1alphaB\SIReflectionProtoP\SOH\162\STX\ETXGRX\170\STX\ETBGrpc.Reflection.V1alpha\202\STX\ETBGrpc\\Reflection\\V1alpha\226\STX#Grpc\\Reflection\\V1alpha\\GPBMetadata\234\STX\EMGrpc::Reflection::V1alphaJ\142+\n\
    \\a\DC2\ENQ\DLE\NUL\135\SOH\SOH\n\
    \\232\EOT\n\
    \\SOH\f\DC2\ETX\DLE\NUL\DC22\180\EOT Copyright 2016 gRPC authors.\n\
    \\n\
    \ Licensed under the Apache License, Version 2.0 (the \"License\");\n\
    \ you may not use this file except in compliance with the License.\n\
    \ You may obtain a copy of the License at\n\
    \\n\
    \     http://www.apache.org/licenses/LICENSE-2.0\n\
    \\n\
    \ Unless required by applicable law or agreed to in writing, software\n\
    \ distributed under the License is distributed on an \"AS IS\" BASIS,\n\
    \ WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.\n\
    \ See the License for the specific language governing permissions and\n\
    \ limitations under the License.\n\
    \2' Service exported by server reflection\n\
    \\n\
    \\b\n\
    \\SOH\STX\DC2\ETX\DC2\NUL \n\
    \\n\
    \\n\
    \\STX\ACK\NUL\DC2\EOT\DC4\NUL\EM\SOH\n\
    \\n\
    \\n\
    \\ETX\ACK\NUL\SOH\DC2\ETX\DC4\b\CAN\n\
    \\133\SOH\n\
    \\EOT\ACK\NUL\STX\NUL\DC2\EOT\ETB\STX\CAN0\SUBw The reflection service is structured as a bidirectional stream, ensuring\n\
    \ all related requests go to a single server.\n\
    \\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\SOH\DC2\ETX\ETB\ACK\SUB\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\ENQ\DC2\ETX\ETB\ESC!\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\STX\DC2\ETX\ETB\"9\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\ACK\DC2\ETX\CAN\SI\NAK\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\ETX\DC2\ETX\CAN\SYN.\n\
    \V\n\
    \\STX\EOT\NUL\DC2\EOT\FS\NUL<\SOH\SUBJ The message sent by the client when calling ServerReflectionInfo method.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\NUL\SOH\DC2\ETX\FS\b\US\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\NUL\DC2\ETX\GS\STX\DC2\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ENQ\DC2\ETX\GS\STX\b\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\SOH\DC2\ETX\GS\t\r\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ETX\DC2\ETX\GS\DLE\DC1\n\
    \\223\SOH\n\
    \\EOT\EOT\NUL\b\NUL\DC2\EOT!\STX;\ETX\SUB\208\SOH To use reflection service, the client should set one of the following\n\
    \ fields in message_request. The server distinguishes requests by their\n\
    \ defined field and then handles them using corresponding methods.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\b\NUL\SOH\DC2\ETX!\b\ETB\n\
    \2\n\
    \\EOT\EOT\NUL\STX\SOH\DC2\ETX#\EOT \SUB% Find a proto file by the file name.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\ENQ\DC2\ETX#\EOT\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\SOH\DC2\ETX#\v\ESC\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\ETX\DC2\ETX#\RS\US\n\
    \\200\SOH\n\
    \\EOT\EOT\NUL\STX\STX\DC2\ETX(\EOT&\SUB\186\SOH Find the proto file that declares the given fully-qualified symbol name.\n\
    \ This field should be a fully-qualified symbol name\n\
    \ (e.g. <package>.<service>[.<method>] or <package>.<type>).\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\STX\ENQ\DC2\ETX(\EOT\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\STX\SOH\DC2\ETX(\v!\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\STX\ETX\DC2\ETX($%\n\
    \|\n\
    \\EOT\EOT\NUL\STX\ETX\DC2\ETX,\EOT3\SUBo Find the proto file which defines an extension extending the given\n\
    \ message type with the given field number.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ETX\ACK\DC2\ETX,\EOT\DC4\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ETX\SOH\DC2\ETX,\NAK.\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ETX\ETX\DC2\ETX,12\n\
    \\238\ETX\n\
    \\EOT\EOT\NUL\STX\EOT\DC2\ETX6\EOT-\SUB\224\ETX Finds the tag numbers used by all known extensions of the given message\n\
    \ type, and appends them to ExtensionNumberResponse in an undefined order.\n\
    \ Its corresponding method is best-effort: it's not guaranteed that the\n\
    \ reflection service will implement this method, and it's not guaranteed\n\
    \ that this method will provide all extensions. Returns\n\
    \ StatusCode::UNIMPLEMENTED if it's not implemented.\n\
    \ This field should be a fully-qualified type name. The format is\n\
    \ <package>.<type>\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\EOT\ENQ\DC2\ETX6\EOT\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\EOT\SOH\DC2\ETX6\v(\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\EOT\ETX\DC2\ETX6+,\n\
    \\\\n\
    \\EOT\EOT\NUL\STX\ENQ\DC2\ETX:\EOT\GS\SUBO List the full names of registered services. The content will not be\n\
    \ checked.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ENQ\ENQ\DC2\ETX:\EOT\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ENQ\SOH\DC2\ETX:\v\CAN\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ENQ\ETX\DC2\ETX:\ESC\FS\n\
    \o\n\
    \\STX\EOT\SOH\DC2\EOT@\NULD\SOH\SUBc The type name and extension number sent by the client when requesting\n\
    \ file_containing_extension.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\SOH\SOH\DC2\ETX@\b\CAN\n\
    \O\n\
    \\EOT\EOT\SOH\STX\NUL\DC2\ETXB\STX\GS\SUBB Fully-qualified type name. The format should be <package>.<type>\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ENQ\DC2\ETXB\STX\b\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\SOH\DC2\ETXB\t\CAN\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ETX\DC2\ETXB\ESC\FS\n\
    \\v\n\
    \\EOT\EOT\SOH\STX\SOH\DC2\ETXC\STX\GS\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\ENQ\DC2\ETXC\STX\a\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\SOH\DC2\ETXC\b\CAN\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\ETX\DC2\ETXC\ESC\FS\n\
    \S\n\
    \\STX\EOT\STX\DC2\EOTG\NUL^\SOH\SUBG The message sent by the server to answer ServerReflectionInfo method.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\STX\SOH\DC2\ETXG\b \n\
    \\v\n\
    \\EOT\EOT\STX\STX\NUL\DC2\ETXH\STX\CAN\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ENQ\DC2\ETXH\STX\b\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\SOH\DC2\ETXH\t\DC3\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ETX\DC2\ETXH\SYN\ETB\n\
    \\v\n\
    \\EOT\EOT\STX\STX\SOH\DC2\ETXI\STX/\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\ACK\DC2\ETXI\STX\EM\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\SOH\DC2\ETXI\SUB*\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\ETX\DC2\ETXI-.\n\
    \l\n\
    \\EOT\EOT\STX\b\NUL\DC2\EOTL\STX]\ETX\SUB^ The server set one of the following fields accroding to the message_request\n\
    \ in the request.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\STX\b\NUL\SOH\DC2\ETXL\b\CAN\n\
    \\177\ETX\n\
    \\EOT\EOT\STX\STX\STX\DC2\ETXS\EOT8\SUB\163\ETX This message is used to answer file_by_filename, file_containing_symbol,\n\
    \ file_containing_extension requests with transitive dependencies. As\n\
    \ the repeated label is not allowed in oneof fields, we use a\n\
    \ FileDescriptorResponse message to encapsulate the repeated fields.\n\
    \ The reflection service is allowed to avoid sending FileDescriptorProtos\n\
    \ that were previously sent in response to earlier requests in the stream.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\STX\ACK\DC2\ETXS\EOT\SUB\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\STX\SOH\DC2\ETXS\ESC3\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\STX\ETX\DC2\ETXS67\n\
    \S\n\
    \\EOT\EOT\STX\STX\ETX\DC2\ETXV\EOT?\SUBF This message is used to answer all_extension_numbers_of_type requst.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\ETX\ACK\DC2\ETXV\EOT\ESC\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\ETX\SOH\DC2\ETXV\FS:\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\ETX\ETX\DC2\ETXV=>\n\
    \D\n\
    \\EOT\EOT\STX\STX\EOT\DC2\ETXY\EOT3\SUB7 This message is used to answer list_services request.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\EOT\ACK\DC2\ETXY\EOT\ETB\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\EOT\SOH\DC2\ETXY\CAN.\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\EOT\ETX\DC2\ETXY12\n\
    \9\n\
    \\EOT\EOT\STX\STX\ENQ\DC2\ETX\\\EOT%\SUB, This message is used when an error occurs.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\ENQ\ACK\DC2\ETX\\\EOT\DC1\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\ENQ\SOH\DC2\ETX\\\DC2 \n\
    \\f\n\
    \\ENQ\EOT\STX\STX\ENQ\ETX\DC2\ETX\\#$\n\
    \\167\SOH\n\
    \\STX\EOT\ETX\DC2\EOTc\NULh\SOH\SUB\154\SOH Serialized FileDescriptorProto messages sent by the server answering\n\
    \ a file_by_filename, file_containing_symbol, or file_containing_extension\n\
    \ request.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\ETX\SOH\DC2\ETXc\b\RS\n\
    \\178\SOH\n\
    \\EOT\EOT\ETX\STX\NUL\DC2\ETXg\STX+\SUB\164\SOH Serialized FileDescriptorProto messages. We avoid taking a dependency on\n\
    \ descriptor.proto, which uses proto2 only features, by making them opaque\n\
    \ bytes instead.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\EOT\DC2\ETXg\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\ENQ\DC2\ETXg\v\DLE\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\SOH\DC2\ETXg\DC1&\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\ETX\DC2\ETXg)*\n\
    \n\n\
    \\STX\EOT\EOT\DC2\EOTl\NULq\SOH\SUBb A list of extension numbers sent by the server answering\n\
    \ all_extension_numbers_of_type request.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\EOT\SOH\DC2\ETXl\b\US\n\
    \f\n\
    \\EOT\EOT\EOT\STX\NUL\DC2\ETXo\STX\FS\SUBY Full name of the base type, including the package name. The format\n\
    \ is <package>.<type>\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\ENQ\DC2\ETXo\STX\b\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\SOH\DC2\ETXo\t\ETB\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\ETX\DC2\ETXo\SUB\ESC\n\
    \\v\n\
    \\EOT\EOT\EOT\STX\SOH\DC2\ETXp\STX&\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\SOH\EOT\DC2\ETXp\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\SOH\ENQ\DC2\ETXp\v\DLE\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\SOH\SOH\DC2\ETXp\DC1!\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\SOH\ETX\DC2\ETXp$%\n\
    \[\n\
    \\STX\EOT\ENQ\DC2\EOTt\NULx\SOH\SUBO A list of ServiceResponse sent by the server answering list_services request.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\ENQ\SOH\DC2\ETXt\b\ESC\n\
    \\131\SOH\n\
    \\EOT\EOT\ENQ\STX\NUL\DC2\ETXw\STX'\SUBv The information of each service may be expanded in the future, so we use\n\
    \ ServiceResponse message to encapsulate it.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\NUL\EOT\DC2\ETXw\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\NUL\ACK\DC2\ETXw\v\SUB\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\NUL\SOH\DC2\ETXw\ESC\"\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\NUL\ETX\DC2\ETXw%&\n\
    \p\n\
    \\STX\EOT\ACK\DC2\ENQ|\NUL\128\SOH\SOH\SUBc The information of a single service used by ListServiceResponse to answer\n\
    \ list_services request.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\ACK\SOH\DC2\ETX|\b\ETB\n\
    \p\n\
    \\EOT\EOT\ACK\STX\NUL\DC2\ETX\DEL\STX\DC2\SUBc Full name of a registered service, including its package name. The format\n\
    \ is <package>.<service>\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\NUL\ENQ\DC2\ETX\DEL\STX\b\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\NUL\SOH\DC2\ETX\DEL\t\r\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\NUL\ETX\DC2\ETX\DEL\DLE\DC1\n\
    \Y\n\
    \\STX\EOT\a\DC2\ACK\131\SOH\NUL\135\SOH\SOH\SUBK The error code and error message sent by the server when an error occurs.\n\
    \\n\
    \\v\n\
    \\ETX\EOT\a\SOH\DC2\EOT\131\SOH\b\NAK\n\
    \L\n\
    \\EOT\EOT\a\STX\NUL\DC2\EOT\133\SOH\STX\ETB\SUB> This field uses the error codes defined in grpc::StatusCode.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\a\STX\NUL\ENQ\DC2\EOT\133\SOH\STX\a\n\
    \\r\n\
    \\ENQ\EOT\a\STX\NUL\SOH\DC2\EOT\133\SOH\b\DC2\n\
    \\r\n\
    \\ENQ\EOT\a\STX\NUL\ETX\DC2\EOT\133\SOH\NAK\SYN\n\
    \\f\n\
    \\EOT\EOT\a\STX\SOH\DC2\EOT\134\SOH\STX\ESC\n\
    \\r\n\
    \\ENQ\EOT\a\STX\SOH\ENQ\DC2\EOT\134\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\a\STX\SOH\SOH\DC2\EOT\134\SOH\t\SYN\n\
    \\r\n\
    \\ENQ\EOT\a\STX\SOH\ETX\DC2\EOT\134\SOH\EM\SUBb\ACKproto3"