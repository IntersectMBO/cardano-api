{- This file was auto-generated from utxorpc/v1beta/sync/sync.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Utxorpc.V1beta.Sync.Sync (
        SyncService(..), AnyChainBlock(), AnyChainBlock'Chain(..),
        _AnyChainBlock'Cardano, BlockRef(), DumpHistoryRequest(),
        DumpHistoryResponse(), FetchBlockRequest(), FetchBlockResponse(),
        FollowTipRequest(), FollowTipResponse(),
        FollowTipResponse'Action(..), _FollowTipResponse'Apply,
        _FollowTipResponse'Undo, _FollowTipResponse'Reset,
        ReadTipRequest(), ReadTipResponse()
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
import qualified Proto.Google.Protobuf.FieldMask
import qualified Proto.Utxorpc.V1beta.Cardano.Cardano
{- | Fields :
     
         * 'Proto.Utxorpc.V1beta.Sync.Sync_Fields.nativeBytes' @:: Lens' AnyChainBlock Data.ByteString.ByteString@
         * 'Proto.Utxorpc.V1beta.Sync.Sync_Fields.maybe'chain' @:: Lens' AnyChainBlock (Prelude.Maybe AnyChainBlock'Chain)@
         * 'Proto.Utxorpc.V1beta.Sync.Sync_Fields.maybe'cardano' @:: Lens' AnyChainBlock (Prelude.Maybe Proto.Utxorpc.V1beta.Cardano.Cardano.Block)@
         * 'Proto.Utxorpc.V1beta.Sync.Sync_Fields.cardano' @:: Lens' AnyChainBlock Proto.Utxorpc.V1beta.Cardano.Cardano.Block@ -}
data AnyChainBlock
  = AnyChainBlock'_constructor {_AnyChainBlock'nativeBytes :: !Data.ByteString.ByteString,
                                _AnyChainBlock'chain :: !(Prelude.Maybe AnyChainBlock'Chain),
                                _AnyChainBlock'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show AnyChainBlock where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
data AnyChainBlock'Chain
  = AnyChainBlock'Cardano !Proto.Utxorpc.V1beta.Cardano.Cardano.Block
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.Field.HasField AnyChainBlock "nativeBytes" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AnyChainBlock'nativeBytes
           (\ x__ y__ -> x__ {_AnyChainBlock'nativeBytes = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField AnyChainBlock "maybe'chain" (Prelude.Maybe AnyChainBlock'Chain) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AnyChainBlock'chain
           (\ x__ y__ -> x__ {_AnyChainBlock'chain = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField AnyChainBlock "maybe'cardano" (Prelude.Maybe Proto.Utxorpc.V1beta.Cardano.Cardano.Block) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AnyChainBlock'chain
           (\ x__ y__ -> x__ {_AnyChainBlock'chain = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (AnyChainBlock'Cardano x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap AnyChainBlock'Cardano y__))
instance Data.ProtoLens.Field.HasField AnyChainBlock "cardano" Proto.Utxorpc.V1beta.Cardano.Cardano.Block where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AnyChainBlock'chain
           (\ x__ y__ -> x__ {_AnyChainBlock'chain = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (AnyChainBlock'Cardano x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap AnyChainBlock'Cardano y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Message AnyChainBlock where
  messageName _ = Data.Text.pack "utxorpc.v1beta.sync.AnyChainBlock"
  packedMessageDescriptor _
    = "\n\
      \\rAnyChainBlock\DC2!\n\
      \\fnative_bytes\CAN\SOH \SOH(\fR\vnativeBytes\DC29\n\
      \\acardano\CAN\STX \SOH(\v2\GS.utxorpc.v1beta.cardano.BlockH\NULR\acardanoB\a\n\
      \\ENQchain"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        nativeBytes__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "native_bytes"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"nativeBytes")) ::
              Data.ProtoLens.FieldDescriptor AnyChainBlock
        cardano__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "cardano"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Utxorpc.V1beta.Cardano.Cardano.Block)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'cardano")) ::
              Data.ProtoLens.FieldDescriptor AnyChainBlock
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, nativeBytes__field_descriptor),
           (Data.ProtoLens.Tag 2, cardano__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _AnyChainBlock'_unknownFields
        (\ x__ y__ -> x__ {_AnyChainBlock'_unknownFields = y__})
  defMessage
    = AnyChainBlock'_constructor
        {_AnyChainBlock'nativeBytes = Data.ProtoLens.fieldDefault,
         _AnyChainBlock'chain = Prelude.Nothing,
         _AnyChainBlock'_unknownFields = []}
  parseMessage
    = let
        loop ::
          AnyChainBlock -> Data.ProtoLens.Encoding.Bytes.Parser AnyChainBlock
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
                                       "native_bytes"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"nativeBytes") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "cardano"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"cardano") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "AnyChainBlock"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v
                  = Lens.Family2.view (Data.ProtoLens.Field.field @"nativeBytes") _x
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
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'chain") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just (AnyChainBlock'Cardano v))
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData AnyChainBlock where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_AnyChainBlock'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_AnyChainBlock'nativeBytes x__)
                (Control.DeepSeq.deepseq (_AnyChainBlock'chain x__) ()))
instance Control.DeepSeq.NFData AnyChainBlock'Chain where
  rnf (AnyChainBlock'Cardano x__) = Control.DeepSeq.rnf x__
_AnyChainBlock'Cardano ::
  Data.ProtoLens.Prism.Prism' AnyChainBlock'Chain Proto.Utxorpc.V1beta.Cardano.Cardano.Block
_AnyChainBlock'Cardano
  = Data.ProtoLens.Prism.prism'
      AnyChainBlock'Cardano
      (\ p__
         -> case p__ of
              (AnyChainBlock'Cardano p__val) -> Prelude.Just p__val)
{- | Fields :
     
         * 'Proto.Utxorpc.V1beta.Sync.Sync_Fields.slot' @:: Lens' BlockRef Data.Word.Word64@
         * 'Proto.Utxorpc.V1beta.Sync.Sync_Fields.hash' @:: Lens' BlockRef Data.ByteString.ByteString@
         * 'Proto.Utxorpc.V1beta.Sync.Sync_Fields.height' @:: Lens' BlockRef Data.Word.Word64@
         * 'Proto.Utxorpc.V1beta.Sync.Sync_Fields.timestamp' @:: Lens' BlockRef Data.Word.Word64@ -}
data BlockRef
  = BlockRef'_constructor {_BlockRef'slot :: !Data.Word.Word64,
                           _BlockRef'hash :: !Data.ByteString.ByteString,
                           _BlockRef'height :: !Data.Word.Word64,
                           _BlockRef'timestamp :: !Data.Word.Word64,
                           _BlockRef'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show BlockRef where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField BlockRef "slot" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _BlockRef'slot (\ x__ y__ -> x__ {_BlockRef'slot = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField BlockRef "hash" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _BlockRef'hash (\ x__ y__ -> x__ {_BlockRef'hash = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField BlockRef "height" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _BlockRef'height (\ x__ y__ -> x__ {_BlockRef'height = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField BlockRef "timestamp" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _BlockRef'timestamp (\ x__ y__ -> x__ {_BlockRef'timestamp = y__}))
        Prelude.id
instance Data.ProtoLens.Message BlockRef where
  messageName _ = Data.Text.pack "utxorpc.v1beta.sync.BlockRef"
  packedMessageDescriptor _
    = "\n\
      \\bBlockRef\DC2\DC2\n\
      \\EOTslot\CAN\SOH \SOH(\EOTR\EOTslot\DC2\DC2\n\
      \\EOThash\CAN\STX \SOH(\fR\EOThash\DC2\SYN\n\
      \\ACKheight\CAN\ETX \SOH(\EOTR\ACKheight\DC2\FS\n\
      \\ttimestamp\CAN\EOT \SOH(\EOTR\ttimestamp"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        slot__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "slot"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"slot")) ::
              Data.ProtoLens.FieldDescriptor BlockRef
        hash__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "hash"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"hash")) ::
              Data.ProtoLens.FieldDescriptor BlockRef
        height__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "height"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"height")) ::
              Data.ProtoLens.FieldDescriptor BlockRef
        timestamp__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "timestamp"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"timestamp")) ::
              Data.ProtoLens.FieldDescriptor BlockRef
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, slot__field_descriptor),
           (Data.ProtoLens.Tag 2, hash__field_descriptor),
           (Data.ProtoLens.Tag 3, height__field_descriptor),
           (Data.ProtoLens.Tag 4, timestamp__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _BlockRef'_unknownFields
        (\ x__ y__ -> x__ {_BlockRef'_unknownFields = y__})
  defMessage
    = BlockRef'_constructor
        {_BlockRef'slot = Data.ProtoLens.fieldDefault,
         _BlockRef'hash = Data.ProtoLens.fieldDefault,
         _BlockRef'height = Data.ProtoLens.fieldDefault,
         _BlockRef'timestamp = Data.ProtoLens.fieldDefault,
         _BlockRef'_unknownFields = []}
  parseMessage
    = let
        loop :: BlockRef -> Data.ProtoLens.Encoding.Bytes.Parser BlockRef
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
                                       Data.ProtoLens.Encoding.Bytes.getVarInt "slot"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"slot") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getBytes
                                             (Prelude.fromIntegral len))
                                       "hash"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"hash") y x)
                        24
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getVarInt "height"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"height") y x)
                        32
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getVarInt "timestamp"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"timestamp") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "BlockRef"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"slot") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt _v))
             ((Data.Monoid.<>)
                (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"hash") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                         ((\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                            _v))
                ((Data.Monoid.<>)
                   (let
                      _v = Lens.Family2.view (Data.ProtoLens.Field.field @"height") _x
                    in
                      if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty
                      else
                          (Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 24)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt _v))
                   ((Data.Monoid.<>)
                      (let
                         _v = Lens.Family2.view (Data.ProtoLens.Field.field @"timestamp") _x
                       in
                         if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                             Data.Monoid.mempty
                         else
                             (Data.Monoid.<>)
                               (Data.ProtoLens.Encoding.Bytes.putVarInt 32)
                               (Data.ProtoLens.Encoding.Bytes.putVarInt _v))
                      (Data.ProtoLens.Encoding.Wire.buildFieldSet
                         (Lens.Family2.view Data.ProtoLens.unknownFields _x)))))
instance Control.DeepSeq.NFData BlockRef where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_BlockRef'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_BlockRef'slot x__)
                (Control.DeepSeq.deepseq
                   (_BlockRef'hash x__)
                   (Control.DeepSeq.deepseq
                      (_BlockRef'height x__)
                      (Control.DeepSeq.deepseq (_BlockRef'timestamp x__) ()))))
{- | Fields :
     
         * 'Proto.Utxorpc.V1beta.Sync.Sync_Fields.startToken' @:: Lens' DumpHistoryRequest BlockRef@
         * 'Proto.Utxorpc.V1beta.Sync.Sync_Fields.maybe'startToken' @:: Lens' DumpHistoryRequest (Prelude.Maybe BlockRef)@
         * 'Proto.Utxorpc.V1beta.Sync.Sync_Fields.maxItems' @:: Lens' DumpHistoryRequest Data.Word.Word32@
         * 'Proto.Utxorpc.V1beta.Sync.Sync_Fields.fieldMask' @:: Lens' DumpHistoryRequest Proto.Google.Protobuf.FieldMask.FieldMask@
         * 'Proto.Utxorpc.V1beta.Sync.Sync_Fields.maybe'fieldMask' @:: Lens' DumpHistoryRequest (Prelude.Maybe Proto.Google.Protobuf.FieldMask.FieldMask)@ -}
data DumpHistoryRequest
  = DumpHistoryRequest'_constructor {_DumpHistoryRequest'startToken :: !(Prelude.Maybe BlockRef),
                                     _DumpHistoryRequest'maxItems :: !Data.Word.Word32,
                                     _DumpHistoryRequest'fieldMask :: !(Prelude.Maybe Proto.Google.Protobuf.FieldMask.FieldMask),
                                     _DumpHistoryRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show DumpHistoryRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField DumpHistoryRequest "startToken" BlockRef where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DumpHistoryRequest'startToken
           (\ x__ y__ -> x__ {_DumpHistoryRequest'startToken = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField DumpHistoryRequest "maybe'startToken" (Prelude.Maybe BlockRef) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DumpHistoryRequest'startToken
           (\ x__ y__ -> x__ {_DumpHistoryRequest'startToken = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField DumpHistoryRequest "maxItems" Data.Word.Word32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DumpHistoryRequest'maxItems
           (\ x__ y__ -> x__ {_DumpHistoryRequest'maxItems = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField DumpHistoryRequest "fieldMask" Proto.Google.Protobuf.FieldMask.FieldMask where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DumpHistoryRequest'fieldMask
           (\ x__ y__ -> x__ {_DumpHistoryRequest'fieldMask = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField DumpHistoryRequest "maybe'fieldMask" (Prelude.Maybe Proto.Google.Protobuf.FieldMask.FieldMask) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DumpHistoryRequest'fieldMask
           (\ x__ y__ -> x__ {_DumpHistoryRequest'fieldMask = y__}))
        Prelude.id
instance Data.ProtoLens.Message DumpHistoryRequest where
  messageName _
    = Data.Text.pack "utxorpc.v1beta.sync.DumpHistoryRequest"
  packedMessageDescriptor _
    = "\n\
      \\DC2DumpHistoryRequest\DC2>\n\
      \\vstart_token\CAN\STX \SOH(\v2\GS.utxorpc.v1beta.sync.BlockRefR\n\
      \startToken\DC2\ESC\n\
      \\tmax_items\CAN\ETX \SOH(\rR\bmaxItems\DC29\n\
      \\n\
      \field_mask\CAN\EOT \SOH(\v2\SUB.google.protobuf.FieldMaskR\tfieldMask"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        startToken__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "start_token"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor BlockRef)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'startToken")) ::
              Data.ProtoLens.FieldDescriptor DumpHistoryRequest
        maxItems__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "max_items"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"maxItems")) ::
              Data.ProtoLens.FieldDescriptor DumpHistoryRequest
        fieldMask__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "field_mask"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Google.Protobuf.FieldMask.FieldMask)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'fieldMask")) ::
              Data.ProtoLens.FieldDescriptor DumpHistoryRequest
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 2, startToken__field_descriptor),
           (Data.ProtoLens.Tag 3, maxItems__field_descriptor),
           (Data.ProtoLens.Tag 4, fieldMask__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _DumpHistoryRequest'_unknownFields
        (\ x__ y__ -> x__ {_DumpHistoryRequest'_unknownFields = y__})
  defMessage
    = DumpHistoryRequest'_constructor
        {_DumpHistoryRequest'startToken = Prelude.Nothing,
         _DumpHistoryRequest'maxItems = Data.ProtoLens.fieldDefault,
         _DumpHistoryRequest'fieldMask = Prelude.Nothing,
         _DumpHistoryRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          DumpHistoryRequest
          -> Data.ProtoLens.Encoding.Bytes.Parser DumpHistoryRequest
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
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "start_token"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"startToken") y x)
                        24
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "max_items"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"maxItems") y x)
                        34
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "field_mask"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"fieldMask") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "DumpHistoryRequest"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view
                    (Data.ProtoLens.Field.field @"maybe'startToken") _x
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
                (let
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"maxItems") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 24)
                         ((Prelude..)
                            Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
                ((Data.Monoid.<>)
                   (case
                        Lens.Family2.view
                          (Data.ProtoLens.Field.field @"maybe'fieldMask") _x
                    of
                      Prelude.Nothing -> Data.Monoid.mempty
                      (Prelude.Just _v)
                        -> (Data.Monoid.<>)
                             (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                             ((Prelude..)
                                (\ bs
                                   -> (Data.Monoid.<>)
                                        (Data.ProtoLens.Encoding.Bytes.putVarInt
                                           (Prelude.fromIntegral (Data.ByteString.length bs)))
                                        (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                Data.ProtoLens.encodeMessage _v))
                   (Data.ProtoLens.Encoding.Wire.buildFieldSet
                      (Lens.Family2.view Data.ProtoLens.unknownFields _x))))
instance Control.DeepSeq.NFData DumpHistoryRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_DumpHistoryRequest'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_DumpHistoryRequest'startToken x__)
                (Control.DeepSeq.deepseq
                   (_DumpHistoryRequest'maxItems x__)
                   (Control.DeepSeq.deepseq (_DumpHistoryRequest'fieldMask x__) ())))
{- | Fields :
     
         * 'Proto.Utxorpc.V1beta.Sync.Sync_Fields.block' @:: Lens' DumpHistoryResponse [AnyChainBlock]@
         * 'Proto.Utxorpc.V1beta.Sync.Sync_Fields.vec'block' @:: Lens' DumpHistoryResponse (Data.Vector.Vector AnyChainBlock)@
         * 'Proto.Utxorpc.V1beta.Sync.Sync_Fields.nextToken' @:: Lens' DumpHistoryResponse BlockRef@
         * 'Proto.Utxorpc.V1beta.Sync.Sync_Fields.maybe'nextToken' @:: Lens' DumpHistoryResponse (Prelude.Maybe BlockRef)@ -}
data DumpHistoryResponse
  = DumpHistoryResponse'_constructor {_DumpHistoryResponse'block :: !(Data.Vector.Vector AnyChainBlock),
                                      _DumpHistoryResponse'nextToken :: !(Prelude.Maybe BlockRef),
                                      _DumpHistoryResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show DumpHistoryResponse where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField DumpHistoryResponse "block" [AnyChainBlock] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DumpHistoryResponse'block
           (\ x__ y__ -> x__ {_DumpHistoryResponse'block = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField DumpHistoryResponse "vec'block" (Data.Vector.Vector AnyChainBlock) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DumpHistoryResponse'block
           (\ x__ y__ -> x__ {_DumpHistoryResponse'block = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField DumpHistoryResponse "nextToken" BlockRef where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DumpHistoryResponse'nextToken
           (\ x__ y__ -> x__ {_DumpHistoryResponse'nextToken = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField DumpHistoryResponse "maybe'nextToken" (Prelude.Maybe BlockRef) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DumpHistoryResponse'nextToken
           (\ x__ y__ -> x__ {_DumpHistoryResponse'nextToken = y__}))
        Prelude.id
instance Data.ProtoLens.Message DumpHistoryResponse where
  messageName _
    = Data.Text.pack "utxorpc.v1beta.sync.DumpHistoryResponse"
  packedMessageDescriptor _
    = "\n\
      \\DC3DumpHistoryResponse\DC28\n\
      \\ENQblock\CAN\SOH \ETX(\v2\".utxorpc.v1beta.sync.AnyChainBlockR\ENQblock\DC2<\n\
      \\n\
      \next_token\CAN\STX \SOH(\v2\GS.utxorpc.v1beta.sync.BlockRefR\tnextToken"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        block__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "block"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor AnyChainBlock)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"block")) ::
              Data.ProtoLens.FieldDescriptor DumpHistoryResponse
        nextToken__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "next_token"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor BlockRef)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'nextToken")) ::
              Data.ProtoLens.FieldDescriptor DumpHistoryResponse
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, block__field_descriptor),
           (Data.ProtoLens.Tag 2, nextToken__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _DumpHistoryResponse'_unknownFields
        (\ x__ y__ -> x__ {_DumpHistoryResponse'_unknownFields = y__})
  defMessage
    = DumpHistoryResponse'_constructor
        {_DumpHistoryResponse'block = Data.Vector.Generic.empty,
         _DumpHistoryResponse'nextToken = Prelude.Nothing,
         _DumpHistoryResponse'_unknownFields = []}
  parseMessage
    = let
        loop ::
          DumpHistoryResponse
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld AnyChainBlock
             -> Data.ProtoLens.Encoding.Bytes.Parser DumpHistoryResponse
        loop x mutable'block
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'block <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                        (Data.ProtoLens.Encoding.Growing.unsafeFreeze mutable'block)
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
                              (Data.ProtoLens.Field.field @"vec'block") frozen'block x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "block"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'block y)
                                loop x v
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "next_token"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"nextToken") y x)
                                  mutable'block
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'block
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'block <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                 Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'block)
          "DumpHistoryResponse"
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
                (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'block") _x))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view
                       (Data.ProtoLens.Field.field @"maybe'nextToken") _x
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
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData DumpHistoryResponse where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_DumpHistoryResponse'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_DumpHistoryResponse'block x__)
                (Control.DeepSeq.deepseq (_DumpHistoryResponse'nextToken x__) ()))
{- | Fields :
     
         * 'Proto.Utxorpc.V1beta.Sync.Sync_Fields.ref' @:: Lens' FetchBlockRequest BlockRef@
         * 'Proto.Utxorpc.V1beta.Sync.Sync_Fields.maybe'ref' @:: Lens' FetchBlockRequest (Prelude.Maybe BlockRef)@
         * 'Proto.Utxorpc.V1beta.Sync.Sync_Fields.fieldMask' @:: Lens' FetchBlockRequest Proto.Google.Protobuf.FieldMask.FieldMask@
         * 'Proto.Utxorpc.V1beta.Sync.Sync_Fields.maybe'fieldMask' @:: Lens' FetchBlockRequest (Prelude.Maybe Proto.Google.Protobuf.FieldMask.FieldMask)@ -}
data FetchBlockRequest
  = FetchBlockRequest'_constructor {_FetchBlockRequest'ref :: !(Prelude.Maybe BlockRef),
                                    _FetchBlockRequest'fieldMask :: !(Prelude.Maybe Proto.Google.Protobuf.FieldMask.FieldMask),
                                    _FetchBlockRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show FetchBlockRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField FetchBlockRequest "ref" BlockRef where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _FetchBlockRequest'ref
           (\ x__ y__ -> x__ {_FetchBlockRequest'ref = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField FetchBlockRequest "maybe'ref" (Prelude.Maybe BlockRef) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _FetchBlockRequest'ref
           (\ x__ y__ -> x__ {_FetchBlockRequest'ref = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField FetchBlockRequest "fieldMask" Proto.Google.Protobuf.FieldMask.FieldMask where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _FetchBlockRequest'fieldMask
           (\ x__ y__ -> x__ {_FetchBlockRequest'fieldMask = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField FetchBlockRequest "maybe'fieldMask" (Prelude.Maybe Proto.Google.Protobuf.FieldMask.FieldMask) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _FetchBlockRequest'fieldMask
           (\ x__ y__ -> x__ {_FetchBlockRequest'fieldMask = y__}))
        Prelude.id
instance Data.ProtoLens.Message FetchBlockRequest where
  messageName _
    = Data.Text.pack "utxorpc.v1beta.sync.FetchBlockRequest"
  packedMessageDescriptor _
    = "\n\
      \\DC1FetchBlockRequest\DC2/\n\
      \\ETXref\CAN\SOH \SOH(\v2\GS.utxorpc.v1beta.sync.BlockRefR\ETXref\DC29\n\
      \\n\
      \field_mask\CAN\STX \SOH(\v2\SUB.google.protobuf.FieldMaskR\tfieldMask"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        ref__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "ref"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor BlockRef)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'ref")) ::
              Data.ProtoLens.FieldDescriptor FetchBlockRequest
        fieldMask__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "field_mask"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Google.Protobuf.FieldMask.FieldMask)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'fieldMask")) ::
              Data.ProtoLens.FieldDescriptor FetchBlockRequest
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, ref__field_descriptor),
           (Data.ProtoLens.Tag 2, fieldMask__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _FetchBlockRequest'_unknownFields
        (\ x__ y__ -> x__ {_FetchBlockRequest'_unknownFields = y__})
  defMessage
    = FetchBlockRequest'_constructor
        {_FetchBlockRequest'ref = Prelude.Nothing,
         _FetchBlockRequest'fieldMask = Prelude.Nothing,
         _FetchBlockRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          FetchBlockRequest
          -> Data.ProtoLens.Encoding.Bytes.Parser FetchBlockRequest
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
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "ref"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"ref") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "field_mask"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"fieldMask") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "FetchBlockRequest"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'ref") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just _v)
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
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
                       (Data.ProtoLens.Field.field @"maybe'fieldMask") _x
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
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData FetchBlockRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_FetchBlockRequest'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_FetchBlockRequest'ref x__)
                (Control.DeepSeq.deepseq (_FetchBlockRequest'fieldMask x__) ()))
{- | Fields :
     
         * 'Proto.Utxorpc.V1beta.Sync.Sync_Fields.block' @:: Lens' FetchBlockResponse AnyChainBlock@
         * 'Proto.Utxorpc.V1beta.Sync.Sync_Fields.maybe'block' @:: Lens' FetchBlockResponse (Prelude.Maybe AnyChainBlock)@ -}
data FetchBlockResponse
  = FetchBlockResponse'_constructor {_FetchBlockResponse'block :: !(Prelude.Maybe AnyChainBlock),
                                     _FetchBlockResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show FetchBlockResponse where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField FetchBlockResponse "block" AnyChainBlock where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _FetchBlockResponse'block
           (\ x__ y__ -> x__ {_FetchBlockResponse'block = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField FetchBlockResponse "maybe'block" (Prelude.Maybe AnyChainBlock) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _FetchBlockResponse'block
           (\ x__ y__ -> x__ {_FetchBlockResponse'block = y__}))
        Prelude.id
instance Data.ProtoLens.Message FetchBlockResponse where
  messageName _
    = Data.Text.pack "utxorpc.v1beta.sync.FetchBlockResponse"
  packedMessageDescriptor _
    = "\n\
      \\DC2FetchBlockResponse\DC28\n\
      \\ENQblock\CAN\SOH \SOH(\v2\".utxorpc.v1beta.sync.AnyChainBlockR\ENQblock"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        block__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "block"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor AnyChainBlock)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'block")) ::
              Data.ProtoLens.FieldDescriptor FetchBlockResponse
      in
        Data.Map.fromList [(Data.ProtoLens.Tag 1, block__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _FetchBlockResponse'_unknownFields
        (\ x__ y__ -> x__ {_FetchBlockResponse'_unknownFields = y__})
  defMessage
    = FetchBlockResponse'_constructor
        {_FetchBlockResponse'block = Prelude.Nothing,
         _FetchBlockResponse'_unknownFields = []}
  parseMessage
    = let
        loop ::
          FetchBlockResponse
          -> Data.ProtoLens.Encoding.Bytes.Parser FetchBlockResponse
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
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "block"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"block") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "FetchBlockResponse"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'block") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just _v)
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage _v))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData FetchBlockResponse where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_FetchBlockResponse'_unknownFields x__)
             (Control.DeepSeq.deepseq (_FetchBlockResponse'block x__) ())
{- | Fields :
     
         * 'Proto.Utxorpc.V1beta.Sync.Sync_Fields.intersect' @:: Lens' FollowTipRequest [BlockRef]@
         * 'Proto.Utxorpc.V1beta.Sync.Sync_Fields.vec'intersect' @:: Lens' FollowTipRequest (Data.Vector.Vector BlockRef)@
         * 'Proto.Utxorpc.V1beta.Sync.Sync_Fields.fieldMask' @:: Lens' FollowTipRequest Proto.Google.Protobuf.FieldMask.FieldMask@
         * 'Proto.Utxorpc.V1beta.Sync.Sync_Fields.maybe'fieldMask' @:: Lens' FollowTipRequest (Prelude.Maybe Proto.Google.Protobuf.FieldMask.FieldMask)@ -}
data FollowTipRequest
  = FollowTipRequest'_constructor {_FollowTipRequest'intersect :: !(Data.Vector.Vector BlockRef),
                                   _FollowTipRequest'fieldMask :: !(Prelude.Maybe Proto.Google.Protobuf.FieldMask.FieldMask),
                                   _FollowTipRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show FollowTipRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField FollowTipRequest "intersect" [BlockRef] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _FollowTipRequest'intersect
           (\ x__ y__ -> x__ {_FollowTipRequest'intersect = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField FollowTipRequest "vec'intersect" (Data.Vector.Vector BlockRef) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _FollowTipRequest'intersect
           (\ x__ y__ -> x__ {_FollowTipRequest'intersect = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField FollowTipRequest "fieldMask" Proto.Google.Protobuf.FieldMask.FieldMask where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _FollowTipRequest'fieldMask
           (\ x__ y__ -> x__ {_FollowTipRequest'fieldMask = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField FollowTipRequest "maybe'fieldMask" (Prelude.Maybe Proto.Google.Protobuf.FieldMask.FieldMask) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _FollowTipRequest'fieldMask
           (\ x__ y__ -> x__ {_FollowTipRequest'fieldMask = y__}))
        Prelude.id
instance Data.ProtoLens.Message FollowTipRequest where
  messageName _
    = Data.Text.pack "utxorpc.v1beta.sync.FollowTipRequest"
  packedMessageDescriptor _
    = "\n\
      \\DLEFollowTipRequest\DC2;\n\
      \\tintersect\CAN\SOH \ETX(\v2\GS.utxorpc.v1beta.sync.BlockRefR\tintersect\DC29\n\
      \\n\
      \field_mask\CAN\STX \SOH(\v2\SUB.google.protobuf.FieldMaskR\tfieldMask"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        intersect__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "intersect"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor BlockRef)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"intersect")) ::
              Data.ProtoLens.FieldDescriptor FollowTipRequest
        fieldMask__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "field_mask"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Google.Protobuf.FieldMask.FieldMask)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'fieldMask")) ::
              Data.ProtoLens.FieldDescriptor FollowTipRequest
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, intersect__field_descriptor),
           (Data.ProtoLens.Tag 2, fieldMask__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _FollowTipRequest'_unknownFields
        (\ x__ y__ -> x__ {_FollowTipRequest'_unknownFields = y__})
  defMessage
    = FollowTipRequest'_constructor
        {_FollowTipRequest'intersect = Data.Vector.Generic.empty,
         _FollowTipRequest'fieldMask = Prelude.Nothing,
         _FollowTipRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          FollowTipRequest
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld BlockRef
             -> Data.ProtoLens.Encoding.Bytes.Parser FollowTipRequest
        loop x mutable'intersect
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'intersect <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                            (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                               mutable'intersect)
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
                              (Data.ProtoLens.Field.field @"vec'intersect") frozen'intersect x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "intersect"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'intersect y)
                                loop x v
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "field_mask"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"fieldMask") y x)
                                  mutable'intersect
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'intersect
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'intersect <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                     Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'intersect)
          "FollowTipRequest"
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
                (Lens.Family2.view
                   (Data.ProtoLens.Field.field @"vec'intersect") _x))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view
                       (Data.ProtoLens.Field.field @"maybe'fieldMask") _x
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
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData FollowTipRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_FollowTipRequest'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_FollowTipRequest'intersect x__)
                (Control.DeepSeq.deepseq (_FollowTipRequest'fieldMask x__) ()))
{- | Fields :
     
         * 'Proto.Utxorpc.V1beta.Sync.Sync_Fields.tip' @:: Lens' FollowTipResponse BlockRef@
         * 'Proto.Utxorpc.V1beta.Sync.Sync_Fields.maybe'tip' @:: Lens' FollowTipResponse (Prelude.Maybe BlockRef)@
         * 'Proto.Utxorpc.V1beta.Sync.Sync_Fields.maybe'action' @:: Lens' FollowTipResponse (Prelude.Maybe FollowTipResponse'Action)@
         * 'Proto.Utxorpc.V1beta.Sync.Sync_Fields.maybe'apply' @:: Lens' FollowTipResponse (Prelude.Maybe AnyChainBlock)@
         * 'Proto.Utxorpc.V1beta.Sync.Sync_Fields.apply' @:: Lens' FollowTipResponse AnyChainBlock@
         * 'Proto.Utxorpc.V1beta.Sync.Sync_Fields.maybe'undo' @:: Lens' FollowTipResponse (Prelude.Maybe AnyChainBlock)@
         * 'Proto.Utxorpc.V1beta.Sync.Sync_Fields.undo' @:: Lens' FollowTipResponse AnyChainBlock@
         * 'Proto.Utxorpc.V1beta.Sync.Sync_Fields.maybe'reset' @:: Lens' FollowTipResponse (Prelude.Maybe BlockRef)@
         * 'Proto.Utxorpc.V1beta.Sync.Sync_Fields.reset' @:: Lens' FollowTipResponse BlockRef@ -}
data FollowTipResponse
  = FollowTipResponse'_constructor {_FollowTipResponse'tip :: !(Prelude.Maybe BlockRef),
                                    _FollowTipResponse'action :: !(Prelude.Maybe FollowTipResponse'Action),
                                    _FollowTipResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show FollowTipResponse where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
data FollowTipResponse'Action
  = FollowTipResponse'Apply !AnyChainBlock |
    FollowTipResponse'Undo !AnyChainBlock |
    FollowTipResponse'Reset !BlockRef
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.Field.HasField FollowTipResponse "tip" BlockRef where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _FollowTipResponse'tip
           (\ x__ y__ -> x__ {_FollowTipResponse'tip = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField FollowTipResponse "maybe'tip" (Prelude.Maybe BlockRef) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _FollowTipResponse'tip
           (\ x__ y__ -> x__ {_FollowTipResponse'tip = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField FollowTipResponse "maybe'action" (Prelude.Maybe FollowTipResponse'Action) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _FollowTipResponse'action
           (\ x__ y__ -> x__ {_FollowTipResponse'action = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField FollowTipResponse "maybe'apply" (Prelude.Maybe AnyChainBlock) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _FollowTipResponse'action
           (\ x__ y__ -> x__ {_FollowTipResponse'action = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (FollowTipResponse'Apply x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap FollowTipResponse'Apply y__))
instance Data.ProtoLens.Field.HasField FollowTipResponse "apply" AnyChainBlock where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _FollowTipResponse'action
           (\ x__ y__ -> x__ {_FollowTipResponse'action = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (FollowTipResponse'Apply x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap FollowTipResponse'Apply y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Field.HasField FollowTipResponse "maybe'undo" (Prelude.Maybe AnyChainBlock) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _FollowTipResponse'action
           (\ x__ y__ -> x__ {_FollowTipResponse'action = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (FollowTipResponse'Undo x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap FollowTipResponse'Undo y__))
instance Data.ProtoLens.Field.HasField FollowTipResponse "undo" AnyChainBlock where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _FollowTipResponse'action
           (\ x__ y__ -> x__ {_FollowTipResponse'action = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (FollowTipResponse'Undo x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap FollowTipResponse'Undo y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Field.HasField FollowTipResponse "maybe'reset" (Prelude.Maybe BlockRef) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _FollowTipResponse'action
           (\ x__ y__ -> x__ {_FollowTipResponse'action = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (FollowTipResponse'Reset x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap FollowTipResponse'Reset y__))
instance Data.ProtoLens.Field.HasField FollowTipResponse "reset" BlockRef where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _FollowTipResponse'action
           (\ x__ y__ -> x__ {_FollowTipResponse'action = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (FollowTipResponse'Reset x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap FollowTipResponse'Reset y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Message FollowTipResponse where
  messageName _
    = Data.Text.pack "utxorpc.v1beta.sync.FollowTipResponse"
  packedMessageDescriptor _
    = "\n\
      \\DC1FollowTipResponse\DC2:\n\
      \\ENQapply\CAN\SOH \SOH(\v2\".utxorpc.v1beta.sync.AnyChainBlockH\NULR\ENQapply\DC28\n\
      \\EOTundo\CAN\STX \SOH(\v2\".utxorpc.v1beta.sync.AnyChainBlockH\NULR\EOTundo\DC25\n\
      \\ENQreset\CAN\ETX \SOH(\v2\GS.utxorpc.v1beta.sync.BlockRefH\NULR\ENQreset\DC2/\n\
      \\ETXtip\CAN\EOT \SOH(\v2\GS.utxorpc.v1beta.sync.BlockRefR\ETXtipB\b\n\
      \\ACKaction"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        tip__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "tip"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor BlockRef)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'tip")) ::
              Data.ProtoLens.FieldDescriptor FollowTipResponse
        apply__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "apply"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor AnyChainBlock)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'apply")) ::
              Data.ProtoLens.FieldDescriptor FollowTipResponse
        undo__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "undo"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor AnyChainBlock)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'undo")) ::
              Data.ProtoLens.FieldDescriptor FollowTipResponse
        reset__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "reset"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor BlockRef)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'reset")) ::
              Data.ProtoLens.FieldDescriptor FollowTipResponse
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 4, tip__field_descriptor),
           (Data.ProtoLens.Tag 1, apply__field_descriptor),
           (Data.ProtoLens.Tag 2, undo__field_descriptor),
           (Data.ProtoLens.Tag 3, reset__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _FollowTipResponse'_unknownFields
        (\ x__ y__ -> x__ {_FollowTipResponse'_unknownFields = y__})
  defMessage
    = FollowTipResponse'_constructor
        {_FollowTipResponse'tip = Prelude.Nothing,
         _FollowTipResponse'action = Prelude.Nothing,
         _FollowTipResponse'_unknownFields = []}
  parseMessage
    = let
        loop ::
          FollowTipResponse
          -> Data.ProtoLens.Encoding.Bytes.Parser FollowTipResponse
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
                        34
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "tip"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"tip") y x)
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "apply"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"apply") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "undo"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"undo") y x)
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "reset"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"reset") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "FollowTipResponse"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'tip") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just _v)
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'action") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just (FollowTipResponse'Apply v))
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage v)
                   (Prelude.Just (FollowTipResponse'Undo v))
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage v)
                   (Prelude.Just (FollowTipResponse'Reset v))
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData FollowTipResponse where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_FollowTipResponse'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_FollowTipResponse'tip x__)
                (Control.DeepSeq.deepseq (_FollowTipResponse'action x__) ()))
instance Control.DeepSeq.NFData FollowTipResponse'Action where
  rnf (FollowTipResponse'Apply x__) = Control.DeepSeq.rnf x__
  rnf (FollowTipResponse'Undo x__) = Control.DeepSeq.rnf x__
  rnf (FollowTipResponse'Reset x__) = Control.DeepSeq.rnf x__
_FollowTipResponse'Apply ::
  Data.ProtoLens.Prism.Prism' FollowTipResponse'Action AnyChainBlock
_FollowTipResponse'Apply
  = Data.ProtoLens.Prism.prism'
      FollowTipResponse'Apply
      (\ p__
         -> case p__ of
              (FollowTipResponse'Apply p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_FollowTipResponse'Undo ::
  Data.ProtoLens.Prism.Prism' FollowTipResponse'Action AnyChainBlock
_FollowTipResponse'Undo
  = Data.ProtoLens.Prism.prism'
      FollowTipResponse'Undo
      (\ p__
         -> case p__ of
              (FollowTipResponse'Undo p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_FollowTipResponse'Reset ::
  Data.ProtoLens.Prism.Prism' FollowTipResponse'Action BlockRef
_FollowTipResponse'Reset
  = Data.ProtoLens.Prism.prism'
      FollowTipResponse'Reset
      (\ p__
         -> case p__ of
              (FollowTipResponse'Reset p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
{- | Fields :
      -}
data ReadTipRequest
  = ReadTipRequest'_constructor {_ReadTipRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ReadTipRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Message ReadTipRequest where
  messageName _ = Data.Text.pack "utxorpc.v1beta.sync.ReadTipRequest"
  packedMessageDescriptor _
    = "\n\
      \\SOReadTipRequest"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag = let in Data.Map.fromList []
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ReadTipRequest'_unknownFields
        (\ x__ y__ -> x__ {_ReadTipRequest'_unknownFields = y__})
  defMessage
    = ReadTipRequest'_constructor {_ReadTipRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ReadTipRequest
          -> Data.ProtoLens.Encoding.Bytes.Parser ReadTipRequest
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
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "ReadTipRequest"
  buildMessage
    = \ _x
        -> Data.ProtoLens.Encoding.Wire.buildFieldSet
             (Lens.Family2.view Data.ProtoLens.unknownFields _x)
instance Control.DeepSeq.NFData ReadTipRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq (_ReadTipRequest'_unknownFields x__) ()
{- | Fields :
     
         * 'Proto.Utxorpc.V1beta.Sync.Sync_Fields.tip' @:: Lens' ReadTipResponse BlockRef@
         * 'Proto.Utxorpc.V1beta.Sync.Sync_Fields.maybe'tip' @:: Lens' ReadTipResponse (Prelude.Maybe BlockRef)@ -}
data ReadTipResponse
  = ReadTipResponse'_constructor {_ReadTipResponse'tip :: !(Prelude.Maybe BlockRef),
                                  _ReadTipResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ReadTipResponse where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ReadTipResponse "tip" BlockRef where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReadTipResponse'tip
           (\ x__ y__ -> x__ {_ReadTipResponse'tip = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ReadTipResponse "maybe'tip" (Prelude.Maybe BlockRef) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReadTipResponse'tip
           (\ x__ y__ -> x__ {_ReadTipResponse'tip = y__}))
        Prelude.id
instance Data.ProtoLens.Message ReadTipResponse where
  messageName _
    = Data.Text.pack "utxorpc.v1beta.sync.ReadTipResponse"
  packedMessageDescriptor _
    = "\n\
      \\SIReadTipResponse\DC2/\n\
      \\ETXtip\CAN\SOH \SOH(\v2\GS.utxorpc.v1beta.sync.BlockRefR\ETXtip"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        tip__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "tip"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor BlockRef)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'tip")) ::
              Data.ProtoLens.FieldDescriptor ReadTipResponse
      in
        Data.Map.fromList [(Data.ProtoLens.Tag 1, tip__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ReadTipResponse'_unknownFields
        (\ x__ y__ -> x__ {_ReadTipResponse'_unknownFields = y__})
  defMessage
    = ReadTipResponse'_constructor
        {_ReadTipResponse'tip = Prelude.Nothing,
         _ReadTipResponse'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ReadTipResponse
          -> Data.ProtoLens.Encoding.Bytes.Parser ReadTipResponse
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
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "tip"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"tip") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "ReadTipResponse"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'tip") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just _v)
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage _v))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData ReadTipResponse where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ReadTipResponse'_unknownFields x__)
             (Control.DeepSeq.deepseq (_ReadTipResponse'tip x__) ())
data SyncService = SyncService {}
instance Data.ProtoLens.Service.Types.Service SyncService where
  type ServiceName SyncService = "SyncService"
  type ServicePackage SyncService = "utxorpc.v1beta.sync"
  type ServiceMethods SyncService = '["dumpHistory",
                                      "fetchBlock",
                                      "followTip",
                                      "readTip"]
  packedServiceDescriptor _
    = "\n\
      \\vSyncService\DC2]\n\
      \\n\
      \FetchBlock\DC2&.utxorpc.v1beta.sync.FetchBlockRequest\SUB'.utxorpc.v1beta.sync.FetchBlockResponse\DC2`\n\
      \\vDumpHistory\DC2'.utxorpc.v1beta.sync.DumpHistoryRequest\SUB(.utxorpc.v1beta.sync.DumpHistoryResponse\DC2\\\n\
      \\tFollowTip\DC2%.utxorpc.v1beta.sync.FollowTipRequest\SUB&.utxorpc.v1beta.sync.FollowTipResponse0\SOH\DC2T\n\
      \\aReadTip\DC2#.utxorpc.v1beta.sync.ReadTipRequest\SUB$.utxorpc.v1beta.sync.ReadTipResponse"
instance Data.ProtoLens.Service.Types.HasMethodImpl SyncService "fetchBlock" where
  type MethodName SyncService "fetchBlock" = "FetchBlock"
  type MethodInput SyncService "fetchBlock" = FetchBlockRequest
  type MethodOutput SyncService "fetchBlock" = FetchBlockResponse
  type MethodStreamingType SyncService "fetchBlock" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl SyncService "dumpHistory" where
  type MethodName SyncService "dumpHistory" = "DumpHistory"
  type MethodInput SyncService "dumpHistory" = DumpHistoryRequest
  type MethodOutput SyncService "dumpHistory" = DumpHistoryResponse
  type MethodStreamingType SyncService "dumpHistory" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl SyncService "followTip" where
  type MethodName SyncService "followTip" = "FollowTip"
  type MethodInput SyncService "followTip" = FollowTipRequest
  type MethodOutput SyncService "followTip" = FollowTipResponse
  type MethodStreamingType SyncService "followTip" = 'Data.ProtoLens.Service.Types.ServerStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl SyncService "readTip" where
  type MethodName SyncService "readTip" = "ReadTip"
  type MethodInput SyncService "readTip" = ReadTipRequest
  type MethodOutput SyncService "readTip" = ReadTipResponse
  type MethodStreamingType SyncService "readTip" = 'Data.ProtoLens.Service.Types.NonStreaming
packedFileDescriptor :: Data.ByteString.ByteString
packedFileDescriptor
  = "\n\
    \\RSutxorpc/v1beta/sync/sync.proto\DC2\DC3utxorpc.v1beta.sync\SUB google/protobuf/field_mask.proto\SUB$utxorpc/v1beta/cardano/cardano.proto\"h\n\
    \\bBlockRef\DC2\DC2\n\
    \\EOTslot\CAN\SOH \SOH(\EOTR\EOTslot\DC2\DC2\n\
    \\EOThash\CAN\STX \SOH(\fR\EOThash\DC2\SYN\n\
    \\ACKheight\CAN\ETX \SOH(\EOTR\ACKheight\DC2\FS\n\
    \\ttimestamp\CAN\EOT \SOH(\EOTR\ttimestamp\"v\n\
    \\rAnyChainBlock\DC2!\n\
    \\fnative_bytes\CAN\SOH \SOH(\fR\vnativeBytes\DC29\n\
    \\acardano\CAN\STX \SOH(\v2\GS.utxorpc.v1beta.cardano.BlockH\NULR\acardanoB\a\n\
    \\ENQchain\"\DEL\n\
    \\DC1FetchBlockRequest\DC2/\n\
    \\ETXref\CAN\SOH \SOH(\v2\GS.utxorpc.v1beta.sync.BlockRefR\ETXref\DC29\n\
    \\n\
    \field_mask\CAN\STX \SOH(\v2\SUB.google.protobuf.FieldMaskR\tfieldMask\"N\n\
    \\DC2FetchBlockResponse\DC28\n\
    \\ENQblock\CAN\SOH \SOH(\v2\".utxorpc.v1beta.sync.AnyChainBlockR\ENQblock\"\172\SOH\n\
    \\DC2DumpHistoryRequest\DC2>\n\
    \\vstart_token\CAN\STX \SOH(\v2\GS.utxorpc.v1beta.sync.BlockRefR\n\
    \startToken\DC2\ESC\n\
    \\tmax_items\CAN\ETX \SOH(\rR\bmaxItems\DC29\n\
    \\n\
    \field_mask\CAN\EOT \SOH(\v2\SUB.google.protobuf.FieldMaskR\tfieldMask\"\141\SOH\n\
    \\DC3DumpHistoryResponse\DC28\n\
    \\ENQblock\CAN\SOH \ETX(\v2\".utxorpc.v1beta.sync.AnyChainBlockR\ENQblock\DC2<\n\
    \\n\
    \next_token\CAN\STX \SOH(\v2\GS.utxorpc.v1beta.sync.BlockRefR\tnextToken\"\138\SOH\n\
    \\DLEFollowTipRequest\DC2;\n\
    \\tintersect\CAN\SOH \ETX(\v2\GS.utxorpc.v1beta.sync.BlockRefR\tintersect\DC29\n\
    \\n\
    \field_mask\CAN\STX \SOH(\v2\SUB.google.protobuf.FieldMaskR\tfieldMask\"\251\SOH\n\
    \\DC1FollowTipResponse\DC2:\n\
    \\ENQapply\CAN\SOH \SOH(\v2\".utxorpc.v1beta.sync.AnyChainBlockH\NULR\ENQapply\DC28\n\
    \\EOTundo\CAN\STX \SOH(\v2\".utxorpc.v1beta.sync.AnyChainBlockH\NULR\EOTundo\DC25\n\
    \\ENQreset\CAN\ETX \SOH(\v2\GS.utxorpc.v1beta.sync.BlockRefH\NULR\ENQreset\DC2/\n\
    \\ETXtip\CAN\EOT \SOH(\v2\GS.utxorpc.v1beta.sync.BlockRefR\ETXtipB\b\n\
    \\ACKaction\"\DLE\n\
    \\SOReadTipRequest\"B\n\
    \\SIReadTipResponse\DC2/\n\
    \\ETXtip\CAN\SOH \SOH(\v2\GS.utxorpc.v1beta.sync.BlockRefR\ETXtip2\130\ETX\n\
    \\vSyncService\DC2]\n\
    \\n\
    \FetchBlock\DC2&.utxorpc.v1beta.sync.FetchBlockRequest\SUB'.utxorpc.v1beta.sync.FetchBlockResponse\DC2`\n\
    \\vDumpHistory\DC2'.utxorpc.v1beta.sync.DumpHistoryRequest\SUB(.utxorpc.v1beta.sync.DumpHistoryResponse\DC2\\\n\
    \\tFollowTip\DC2%.utxorpc.v1beta.sync.FollowTipRequest\SUB&.utxorpc.v1beta.sync.FollowTipResponse0\SOH\DC2T\n\
    \\aReadTip\DC2#.utxorpc.v1beta.sync.ReadTipRequest\SUB$.utxorpc.v1beta.sync.ReadTipResponseB\146\SOH\n\
    \\ETBcom.utxorpc.v1beta.syncB\tSyncProtoP\SOH\162\STX\ETXUVS\170\STX\DC3Utxorpc.V1beta.Sync\202\STX\DC3Utxorpc\\V1beta\\Sync\226\STX\USUtxorpc\\V1beta\\Sync\\GPBMetadata\234\STX\NAKUtxorpc::V1beta::SyncJ\139\EM\n\
    \\ACK\DC2\EOT\NUL\NULL\SOH\n\
    \\b\n\
    \\SOH\f\DC2\ETX\NUL\NUL\DC2\n\
    \\b\n\
    \\SOH\STX\DC2\ETX\STX\NUL\FS\n\
    \\t\n\
    \\STX\ETX\NUL\DC2\ETX\EOT\NUL*\n\
    \\t\n\
    \\STX\ETX\SOH\DC2\ETX\ENQ\NUL.\n\
    \Z\n\
    \\STX\EOT\NUL\DC2\EOT\b\NUL\r\SOH\SUBN Represents a reference to a specific block by a chosen combination of fields\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\NUL\SOH\DC2\ETX\b\b\DLE\n\
    \B\n\
    \\EOT\EOT\NUL\STX\NUL\DC2\ETX\t\STX\DC2\"5 Height or slot number (depending on the blockchain)\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ENQ\DC2\ETX\t\STX\b\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\SOH\DC2\ETX\t\t\r\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ETX\DC2\ETX\t\DLE\DC1\n\
    \/\n\
    \\EOT\EOT\NUL\STX\SOH\DC2\ETX\n\
    \\STX\DC1\"\" Hash of the content of the block\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\ENQ\DC2\ETX\n\
    \\STX\a\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\SOH\DC2\ETX\n\
    \\b\f\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\ETX\DC2\ETX\n\
    \\SI\DLE\n\
    \\ESC\n\
    \\EOT\EOT\NUL\STX\STX\DC2\ETX\v\STX\DC4\"\SO Block height\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\STX\ENQ\DC2\ETX\v\STX\b\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\STX\SOH\DC2\ETX\v\t\SI\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\STX\ETX\DC2\ETX\v\DC2\DC3\n\
    \!\n\
    \\EOT\EOT\NUL\STX\ETX\DC2\ETX\f\STX\ETB\"\DC4 Block ms timestamp\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ETX\ENQ\DC2\ETX\f\STX\b\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ETX\SOH\DC2\ETX\f\t\DC2\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ETX\ETX\DC2\ETX\f\NAK\SYN\n\
    \\n\
    \\n\
    \\STX\EOT\SOH\DC2\EOT\SI\NUL\DC4\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\SOH\SOH\DC2\ETX\SI\b\NAK\n\
    \5\n\
    \\EOT\EOT\SOH\STX\NUL\DC2\ETX\DLE\STX\EM\"( Original bytes as defined by the chain\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ENQ\DC2\ETX\DLE\STX\a\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\SOH\DC2\ETX\DLE\b\DC4\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ETX\DC2\ETX\DLE\ETB\CAN\n\
    \\f\n\
    \\EOT\EOT\SOH\b\NUL\DC2\EOT\DC1\STX\DC3\ETX\n\
    \\f\n\
    \\ENQ\EOT\SOH\b\NUL\SOH\DC2\ETX\DC1\b\r\n\
    \&\n\
    \\EOT\EOT\SOH\STX\SOH\DC2\ETX\DC2\EOT-\"\EM A parsed Cardano block.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\ACK\DC2\ETX\DC2\EOT \n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\SOH\DC2\ETX\DC2!(\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\ETX\DC2\ETX\DC2+,\n\
    \8\n\
    \\STX\EOT\STX\DC2\EOT\ETB\NUL\SUB\SOH\SUB, Request to fetch a block by its reference.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\STX\SOH\DC2\ETX\ETB\b\EM\n\
    \(\n\
    \\EOT\EOT\STX\STX\NUL\DC2\ETX\CAN\STX\DC3\"\ESC Block reference to fetch.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ACK\DC2\ETX\CAN\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\SOH\DC2\ETX\CAN\v\SO\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ETX\DC2\ETX\CAN\DC1\DC2\n\
    \7\n\
    \\EOT\EOT\STX\STX\SOH\DC2\ETX\EM\STX+\"* Field mask to selectively return fields.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\ACK\DC2\ETX\EM\STX\ESC\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\SOH\DC2\ETX\EM\FS&\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\ETX\DC2\ETX\EM)*\n\
    \4\n\
    \\STX\EOT\ETX\DC2\EOT\GS\NUL\US\SOH\SUB( Response containing the fetched block.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\ETX\SOH\DC2\ETX\GS\b\SUB\n\
    \!\n\
    \\EOT\EOT\ETX\STX\NUL\DC2\ETX\RS\STX\SUB\"\DC4 The fetched block.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\ACK\DC2\ETX\RS\STX\SI\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\SOH\DC2\ETX\RS\DLE\NAK\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\ETX\DC2\ETX\RS\CAN\EM\n\
    \0\n\
    \\STX\EOT\EOT\DC2\EOT\"\NUL&\SOH\SUB$ Request to dump the block history.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\EOT\SOH\DC2\ETX\"\b\SUB\n\
    \9\n\
    \\EOT\EOT\EOT\STX\NUL\DC2\ETX#\STX\ESC\", Starting point for the block history dump.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\ACK\DC2\ETX#\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\SOH\DC2\ETX#\v\SYN\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\ETX\DC2\ETX#\EM\SUB\n\
    \1\n\
    \\EOT\EOT\EOT\STX\SOH\DC2\ETX$\STX\ETB\"$ Maximum number of items to return.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\SOH\ENQ\DC2\ETX$\STX\b\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\SOH\SOH\DC2\ETX$\t\DC2\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\SOH\ETX\DC2\ETX$\NAK\SYN\n\
    \7\n\
    \\EOT\EOT\EOT\STX\STX\DC2\ETX%\STX+\"* Field mask to selectively return fields.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\STX\ACK\DC2\ETX%\STX\ESC\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\STX\SOH\DC2\ETX%\FS&\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\STX\ETX\DC2\ETX%)*\n\
    \;\n\
    \\STX\EOT\ENQ\DC2\EOT)\NUL,\SOH\SUB/ Response containing the dumped block history.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\ENQ\SOH\DC2\ETX)\b\ESC\n\
    \-\n\
    \\EOT\EOT\ENQ\STX\NUL\DC2\ETX*\STX#\"  List of blocks in the history.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\NUL\EOT\DC2\ETX*\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\NUL\ACK\DC2\ETX*\v\CAN\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\NUL\SOH\DC2\ETX*\EM\RS\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\NUL\ETX\DC2\ETX*!\"\n\
    \)\n\
    \\EOT\EOT\ENQ\STX\SOH\DC2\ETX+\STX\SUB\"\FS Next token for pagination.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\SOH\ACK\DC2\ETX+\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\SOH\SOH\DC2\ETX+\v\NAK\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\SOH\ETX\DC2\ETX+\CAN\EM\n\
    \:\n\
    \\STX\EOT\ACK\DC2\EOT/\NUL2\SOH\SUB. Request to follow the tip of the blockchain.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\ACK\SOH\DC2\ETX/\b\CAN\n\
    \A\n\
    \\EOT\EOT\ACK\STX\NUL\DC2\ETX0\STX\"\"4 List of block references to find the intersection.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\NUL\EOT\DC2\ETX0\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\NUL\ACK\DC2\ETX0\v\DC3\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\NUL\SOH\DC2\ETX0\DC4\GS\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\NUL\ETX\DC2\ETX0 !\n\
    \7\n\
    \\EOT\EOT\ACK\STX\SOH\DC2\ETX1\STX+\"* Field mask to selectively return fields.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\SOH\ACK\DC2\ETX1\STX\ESC\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\SOH\SOH\DC2\ETX1\FS&\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\SOH\ETX\DC2\ETX1)*\n\
    \P\n\
    \\STX\EOT\a\DC2\EOT5\NUL<\SOH\SUBD Response containing the action to perform while following the tip.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\a\SOH\DC2\ETX5\b\EM\n\
    \\f\n\
    \\EOT\EOT\a\b\NUL\DC2\EOT6\STX:\ETX\n\
    \\f\n\
    \\ENQ\EOT\a\b\NUL\SOH\DC2\ETX6\b\SO\n\
    \ \n\
    \\EOT\EOT\a\STX\NUL\DC2\ETX7\EOT\FS\"\DC3 Apply this block.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\a\STX\NUL\ACK\DC2\ETX7\EOT\DC1\n\
    \\f\n\
    \\ENQ\EOT\a\STX\NUL\SOH\DC2\ETX7\DC2\ETB\n\
    \\f\n\
    \\ENQ\EOT\a\STX\NUL\ETX\DC2\ETX7\SUB\ESC\n\
    \\US\n\
    \\EOT\EOT\a\STX\SOH\DC2\ETX8\EOT\ESC\"\DC2 Undo this block.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\a\STX\SOH\ACK\DC2\ETX8\EOT\DC1\n\
    \\f\n\
    \\ENQ\EOT\a\STX\SOH\SOH\DC2\ETX8\DC2\SYN\n\
    \\f\n\
    \\ENQ\EOT\a\STX\SOH\ETX\DC2\ETX8\EM\SUB\n\
    \-\n\
    \\EOT\EOT\a\STX\STX\DC2\ETX9\EOT\ETB\"  Reset to this block reference.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\a\STX\STX\ACK\DC2\ETX9\EOT\f\n\
    \\f\n\
    \\ENQ\EOT\a\STX\STX\SOH\DC2\ETX9\r\DC2\n\
    \\f\n\
    \\ENQ\EOT\a\STX\STX\ETX\DC2\ETX9\NAK\SYN\n\
    \C\n\
    \\EOT\EOT\a\STX\ETX\DC2\ETX;\STX\DC3\"6 The current tip of the blockchain after this action.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\a\STX\ETX\ACK\DC2\ETX;\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\a\STX\ETX\SOH\DC2\ETX;\v\SO\n\
    \\f\n\
    \\ENQ\EOT\a\STX\ETX\ETX\DC2\ETX;\DC1\DC2\n\
    \?\n\
    \\STX\EOT\b\DC2\ETX?\NUL\EM\SUB4 Request to read the current tip of the blockchain.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\b\SOH\DC2\ETX?\b\SYN\n\
    \D\n\
    \\STX\EOT\t\DC2\EOTB\NULD\SOH\SUB8 Response containing the current tip of the blockchain.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\t\SOH\DC2\ETXB\b\ETB\n\
    \1\n\
    \\EOT\EOT\t\STX\NUL\DC2\ETXC\STX\DC3\"$ The current tip of the blockchain.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\t\STX\NUL\ACK\DC2\ETXC\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\t\STX\NUL\SOH\DC2\ETXC\v\SO\n\
    \\f\n\
    \\ENQ\EOT\t\STX\NUL\ETX\DC2\ETXC\DC1\DC2\n\
    \8\n\
    \\STX\ACK\NUL\DC2\EOTG\NULL\SOH\SUB, Service definition for syncing chain data.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\ACK\NUL\SOH\DC2\ETXG\b\DC3\n\
    \.\n\
    \\EOT\ACK\NUL\STX\NUL\DC2\ETXH\STXA\"! Fetch a block by its reference.\n\
    \\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\SOH\DC2\ETXH\ACK\DLE\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\STX\DC2\ETXH\DC1\"\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\ETX\DC2\ETXH-?\n\
    \&\n\
    \\EOT\ACK\NUL\STX\SOH\DC2\ETXI\STXD\"\EM Dump the block history.\n\
    \\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\SOH\SOH\DC2\ETXI\ACK\DC1\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\SOH\STX\DC2\ETXI\DC2$\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\SOH\ETX\DC2\ETXI/B\n\
    \0\n\
    \\EOT\ACK\NUL\STX\STX\DC2\ETXJ\STXE\"# Follow the tip of the blockchain.\n\
    \\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\STX\SOH\DC2\ETXJ\ACK\SI\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\STX\STX\DC2\ETXJ\DLE \n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\STX\ACK\DC2\ETXJ+1\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\STX\ETX\DC2\ETXJ2C\n\
    \6\n\
    \\EOT\ACK\NUL\STX\ETX\DC2\ETXK\STX8\") Read the current tip of the blockchain.\n\
    \\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\ETX\SOH\DC2\ETXK\ACK\r\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\ETX\STX\DC2\ETXK\SO\FS\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\ETX\ETX\DC2\ETXK'6b\ACKproto3"