{- This file was auto-generated from cardano/rpc/node.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Cardano.Rpc.Node (
        Node(..), CurrentEra(), Era(..), Era(), Era'UnrecognizedValue,
        ProtocolParamsJson()
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
import qualified Proto.Google.Protobuf.Empty
{- | Fields :
     
         * 'Proto.Cardano.Rpc.Node_Fields.era' @:: Lens' CurrentEra Era@ -}
data CurrentEra
  = CurrentEra'_constructor {_CurrentEra'era :: !Era,
                             _CurrentEra'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show CurrentEra where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField CurrentEra "era" Era where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _CurrentEra'era (\ x__ y__ -> x__ {_CurrentEra'era = y__}))
        Prelude.id
instance Data.ProtoLens.Message CurrentEra where
  messageName _ = Data.Text.pack "cardano.rpc.CurrentEra"
  packedMessageDescriptor _
    = "\n\
      \\n\
      \CurrentEra\DC2\"\n\
      \\ETXera\CAN\SOH \SOH(\SO2\DLE.cardano.rpc.EraR\ETXera"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        era__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "era"
              (Data.ProtoLens.ScalarField Data.ProtoLens.EnumField ::
                 Data.ProtoLens.FieldTypeDescriptor Era)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"era")) ::
              Data.ProtoLens.FieldDescriptor CurrentEra
      in
        Data.Map.fromList [(Data.ProtoLens.Tag 1, era__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _CurrentEra'_unknownFields
        (\ x__ y__ -> x__ {_CurrentEra'_unknownFields = y__})
  defMessage
    = CurrentEra'_constructor
        {_CurrentEra'era = Data.ProtoLens.fieldDefault,
         _CurrentEra'_unknownFields = []}
  parseMessage
    = let
        loop ::
          CurrentEra -> Data.ProtoLens.Encoding.Bytes.Parser CurrentEra
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
                                          Prelude.toEnum
                                          (Prelude.fmap
                                             Prelude.fromIntegral
                                             Data.ProtoLens.Encoding.Bytes.getVarInt))
                                       "era"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"era") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "CurrentEra"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"era") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                      ((Prelude..)
                         ((Prelude..)
                            Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral)
                         Prelude.fromEnum _v))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData CurrentEra where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_CurrentEra'_unknownFields x__)
             (Control.DeepSeq.deepseq (_CurrentEra'era x__) ())
newtype Era'UnrecognizedValue
  = Era'UnrecognizedValue Data.Int.Int32
  deriving stock (Prelude.Eq, Prelude.Ord, Prelude.Show)
data Era
  = Byron |
    Shelley |
    Allegra |
    Mary |
    Alonzo |
    Babbage |
    Conway |
    Era'Unrecognized !Era'UnrecognizedValue
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.MessageEnum Era where
  maybeToEnum 0 = Prelude.Just Byron
  maybeToEnum 1 = Prelude.Just Shelley
  maybeToEnum 2 = Prelude.Just Allegra
  maybeToEnum 3 = Prelude.Just Mary
  maybeToEnum 4 = Prelude.Just Alonzo
  maybeToEnum 5 = Prelude.Just Babbage
  maybeToEnum 6 = Prelude.Just Conway
  maybeToEnum k
    = Prelude.Just
        (Era'Unrecognized (Era'UnrecognizedValue (Prelude.fromIntegral k)))
  showEnum Byron = "byron"
  showEnum Shelley = "shelley"
  showEnum Allegra = "allegra"
  showEnum Mary = "mary"
  showEnum Alonzo = "alonzo"
  showEnum Babbage = "babbage"
  showEnum Conway = "conway"
  showEnum (Era'Unrecognized (Era'UnrecognizedValue k))
    = Prelude.show k
  readEnum k
    | (Prelude.==) k "byron" = Prelude.Just Byron
    | (Prelude.==) k "shelley" = Prelude.Just Shelley
    | (Prelude.==) k "allegra" = Prelude.Just Allegra
    | (Prelude.==) k "mary" = Prelude.Just Mary
    | (Prelude.==) k "alonzo" = Prelude.Just Alonzo
    | (Prelude.==) k "babbage" = Prelude.Just Babbage
    | (Prelude.==) k "conway" = Prelude.Just Conway
    | Prelude.otherwise
    = (Prelude.>>=) (Text.Read.readMaybe k) Data.ProtoLens.maybeToEnum
instance Prelude.Bounded Era where
  minBound = Byron
  maxBound = Conway
instance Prelude.Enum Era where
  toEnum k__
    = Prelude.maybe
        (Prelude.error
           ((Prelude.++)
              "toEnum: unknown value for enum Era: " (Prelude.show k__)))
        Prelude.id (Data.ProtoLens.maybeToEnum k__)
  fromEnum Byron = 0
  fromEnum Shelley = 1
  fromEnum Allegra = 2
  fromEnum Mary = 3
  fromEnum Alonzo = 4
  fromEnum Babbage = 5
  fromEnum Conway = 6
  fromEnum (Era'Unrecognized (Era'UnrecognizedValue k))
    = Prelude.fromIntegral k
  succ Conway
    = Prelude.error
        "Era.succ: bad argument Conway. This value would be out of bounds."
  succ Byron = Shelley
  succ Shelley = Allegra
  succ Allegra = Mary
  succ Mary = Alonzo
  succ Alonzo = Babbage
  succ Babbage = Conway
  succ (Era'Unrecognized _)
    = Prelude.error "Era.succ: bad argument: unrecognized value"
  pred Byron
    = Prelude.error
        "Era.pred: bad argument Byron. This value would be out of bounds."
  pred Shelley = Byron
  pred Allegra = Shelley
  pred Mary = Allegra
  pred Alonzo = Mary
  pred Babbage = Alonzo
  pred Conway = Babbage
  pred (Era'Unrecognized _)
    = Prelude.error "Era.pred: bad argument: unrecognized value"
  enumFrom = Data.ProtoLens.Message.Enum.messageEnumFrom
  enumFromTo = Data.ProtoLens.Message.Enum.messageEnumFromTo
  enumFromThen = Data.ProtoLens.Message.Enum.messageEnumFromThen
  enumFromThenTo = Data.ProtoLens.Message.Enum.messageEnumFromThenTo
instance Data.ProtoLens.FieldDefault Era where
  fieldDefault = Byron
instance Control.DeepSeq.NFData Era where
  rnf x__ = Prelude.seq x__ ()
{- | Fields :
     
         * 'Proto.Cardano.Rpc.Node_Fields.json' @:: Lens' ProtocolParamsJson Data.ByteString.ByteString@ -}
data ProtocolParamsJson
  = ProtocolParamsJson'_constructor {_ProtocolParamsJson'json :: !Data.ByteString.ByteString,
                                     _ProtocolParamsJson'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ProtocolParamsJson where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ProtocolParamsJson "json" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ProtocolParamsJson'json
           (\ x__ y__ -> x__ {_ProtocolParamsJson'json = y__}))
        Prelude.id
instance Data.ProtoLens.Message ProtocolParamsJson where
  messageName _ = Data.Text.pack "cardano.rpc.ProtocolParamsJson"
  packedMessageDescriptor _
    = "\n\
      \\DC2ProtocolParamsJson\DC2\DC2\n\
      \\EOTjson\CAN\SOH \SOH(\fR\EOTjson"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        json__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "json"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"json")) ::
              Data.ProtoLens.FieldDescriptor ProtocolParamsJson
      in
        Data.Map.fromList [(Data.ProtoLens.Tag 1, json__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ProtocolParamsJson'_unknownFields
        (\ x__ y__ -> x__ {_ProtocolParamsJson'_unknownFields = y__})
  defMessage
    = ProtocolParamsJson'_constructor
        {_ProtocolParamsJson'json = Data.ProtoLens.fieldDefault,
         _ProtocolParamsJson'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ProtocolParamsJson
          -> Data.ProtoLens.Encoding.Bytes.Parser ProtocolParamsJson
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
                                           Data.ProtoLens.Encoding.Bytes.getBytes
                                             (Prelude.fromIntegral len))
                                       "json"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"json") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "ProtocolParamsJson"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"json") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((\ bs
                          -> (Data.Monoid.<>)
                               (Data.ProtoLens.Encoding.Bytes.putVarInt
                                  (Prelude.fromIntegral (Data.ByteString.length bs)))
                               (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         _v))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData ProtocolParamsJson where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ProtocolParamsJson'_unknownFields x__)
             (Control.DeepSeq.deepseq (_ProtocolParamsJson'json x__) ())
data Node = Node {}
instance Data.ProtoLens.Service.Types.Service Node where
  type ServiceName Node = "Node"
  type ServicePackage Node = "cardano.rpc"
  type ServiceMethods Node = '["getEra", "getProtocolParamsJson"]
  packedServiceDescriptor _
    = "\n\
      \\EOTNode\DC2;\n\
      \\ACKGetEra\DC2\SYN.google.protobuf.Empty\SUB\ETB.cardano.rpc.CurrentEra\"\NUL\DC2R\n\
      \\NAKGetProtocolParamsJson\DC2\SYN.google.protobuf.Empty\SUB\US.cardano.rpc.ProtocolParamsJson\"\NUL"
instance Data.ProtoLens.Service.Types.HasMethodImpl Node "getEra" where
  type MethodName Node "getEra" = "GetEra"
  type MethodInput Node "getEra" = Proto.Google.Protobuf.Empty.Empty
  type MethodOutput Node "getEra" = CurrentEra
  type MethodStreamingType Node "getEra" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl Node "getProtocolParamsJson" where
  type MethodName Node "getProtocolParamsJson" = "GetProtocolParamsJson"
  type MethodInput Node "getProtocolParamsJson" = Proto.Google.Protobuf.Empty.Empty
  type MethodOutput Node "getProtocolParamsJson" = ProtocolParamsJson
  type MethodStreamingType Node "getProtocolParamsJson" = 'Data.ProtoLens.Service.Types.NonStreaming
packedFileDescriptor :: Data.ByteString.ByteString
packedFileDescriptor
  = "\n\
    \\SYNcardano/rpc/node.proto\DC2\vcardano.rpc\SUB\ESCgoogle/protobuf/empty.proto\"0\n\
    \\n\
    \CurrentEra\DC2\"\n\
    \\ETXera\CAN\SOH \SOH(\SO2\DLE.cardano.rpc.EraR\ETXera\"(\n\
    \\DC2ProtocolParamsJson\DC2\DC2\n\
    \\EOTjson\CAN\SOH \SOH(\fR\EOTjson*Y\n\
    \\ETXEra\DC2\t\n\
    \\ENQbyron\DLE\NUL\DC2\v\n\
    \\ashelley\DLE\SOH\DC2\v\n\
    \\aallegra\DLE\STX\DC2\b\n\
    \\EOTmary\DLE\ETX\DC2\n\
    \\n\
    \\ACKalonzo\DLE\EOT\DC2\v\n\
    \\ababbage\DLE\ENQ\DC2\n\
    \\n\
    \\ACKconway\DLE\ACK2\151\SOH\n\
    \\EOTNode\DC2;\n\
    \\ACKGetEra\DC2\SYN.google.protobuf.Empty\SUB\ETB.cardano.rpc.CurrentEra\"\NUL\DC2R\n\
    \\NAKGetProtocolParamsJson\DC2\SYN.google.protobuf.Empty\SUB\US.cardano.rpc.ProtocolParamsJson\"\NULBi\n\
    \\SIcom.cardano.rpcB\tNodeProtoP\SOH\162\STX\ETXCRX\170\STX\vCardano.Rpc\202\STX\vCardano\\Rpc\226\STX\ETBCardano\\Rpc\\GPBMetadata\234\STX\fCardano::RpcJ\180\ENQ\n\
    \\ACK\DC2\EOT\NUL\NUL\GS\SOH\n\
    \\b\n\
    \\SOH\f\DC2\ETX\NUL\NUL\DC2\n\
    \\b\n\
    \\SOH\STX\DC2\ETX\STX\NUL\DC4\n\
    \\t\n\
    \\STX\ETX\NUL\DC2\ETX\EOT\NUL%\n\
    \\n\
    \\n\
    \\STX\ACK\NUL\DC2\EOT\ACK\NUL\n\
    \\SOH\n\
    \\n\
    \\n\
    \\ETX\ACK\NUL\SOH\DC2\ETX\ACK\b\f\n\
    \\v\n\
    \\EOT\ACK\NUL\STX\NUL\DC2\ETX\a\EOT=\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\SOH\DC2\ETX\a\b\SO\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\STX\DC2\ETX\a\SI$\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\ETX\DC2\ETX\a/9\n\
    \\v\n\
    \\EOT\ACK\NUL\STX\SOH\DC2\ETX\t\EOTT\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\SOH\SOH\DC2\ETX\t\b\GS\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\SOH\STX\DC2\ETX\t\RS3\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\SOH\ETX\DC2\ETX\t>P\n\
    \\n\
    \\n\
    \\STX\ENQ\NUL\DC2\EOT\f\NUL\DC4\SOH\n\
    \\n\
    \\n\
    \\ETX\ENQ\NUL\SOH\DC2\ETX\f\ENQ\b\n\
    \\v\n\
    \\EOT\ENQ\NUL\STX\NUL\DC2\ETX\r\EOT\SO\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\NUL\SOH\DC2\ETX\r\EOT\t\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\NUL\STX\DC2\ETX\r\f\r\n\
    \\v\n\
    \\EOT\ENQ\NUL\STX\SOH\DC2\ETX\SO\EOT\DLE\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\SOH\SOH\DC2\ETX\SO\EOT\v\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\SOH\STX\DC2\ETX\SO\SO\SI\n\
    \\v\n\
    \\EOT\ENQ\NUL\STX\STX\DC2\ETX\SI\EOT\DLE\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\STX\SOH\DC2\ETX\SI\EOT\v\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\STX\STX\DC2\ETX\SI\SO\SI\n\
    \\v\n\
    \\EOT\ENQ\NUL\STX\ETX\DC2\ETX\DLE\EOT\r\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\ETX\SOH\DC2\ETX\DLE\EOT\b\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\ETX\STX\DC2\ETX\DLE\v\f\n\
    \\v\n\
    \\EOT\ENQ\NUL\STX\EOT\DC2\ETX\DC1\EOT\SI\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\EOT\SOH\DC2\ETX\DC1\EOT\n\
    \\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\EOT\STX\DC2\ETX\DC1\r\SO\n\
    \\v\n\
    \\EOT\ENQ\NUL\STX\ENQ\DC2\ETX\DC2\EOT\DLE\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\ENQ\SOH\DC2\ETX\DC2\EOT\v\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\ENQ\STX\DC2\ETX\DC2\SO\SI\n\
    \\v\n\
    \\EOT\ENQ\NUL\STX\ACK\DC2\ETX\DC3\EOT\SI\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\ACK\SOH\DC2\ETX\DC3\EOT\n\
    \\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\ACK\STX\DC2\ETX\DC3\r\SO\n\
    \\n\
    \\n\
    \\STX\EOT\NUL\DC2\EOT\ETB\NUL\EM\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\NUL\SOH\DC2\ETX\ETB\b\DC2\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\NUL\DC2\ETX\CAN\EOT\DLE\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ACK\DC2\ETX\CAN\EOT\a\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\SOH\DC2\ETX\CAN\b\v\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ETX\DC2\ETX\CAN\SO\SI\n\
    \\n\
    \\n\
    \\STX\EOT\SOH\DC2\EOT\ESC\NUL\GS\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\SOH\SOH\DC2\ETX\ESC\b\SUB\n\
    \=\n\
    \\EOT\EOT\SOH\STX\NUL\DC2\ETX\FS\EOT\DC3\"0 JSON representation of the protocol parameters\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ENQ\DC2\ETX\FS\EOT\t\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\SOH\DC2\ETX\FS\n\
    \\SO\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ETX\DC2\ETX\FS\DC1\DC2b\ACKproto3"