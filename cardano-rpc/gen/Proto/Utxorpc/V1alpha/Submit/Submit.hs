{- This file was auto-generated from utxorpc/v1alpha/submit/submit.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Utxorpc.V1alpha.Submit.Submit (
        SubmitService(..), AnyChainTx(), AnyChainTx'Type(..),
        _AnyChainTx'Raw, SubmitTxRequest(), SubmitTxResponse(),
        TxSubmitResult(), TxSubmitResult'Result(..), _TxSubmitResult'Ref,
        _TxSubmitResult'ErrorMessage
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
     
         * 'Proto.Utxorpc.V1alpha.Submit.Submit_Fields.maybe'type'' @:: Lens' AnyChainTx (Prelude.Maybe AnyChainTx'Type)@
         * 'Proto.Utxorpc.V1alpha.Submit.Submit_Fields.maybe'raw' @:: Lens' AnyChainTx (Prelude.Maybe Data.ByteString.ByteString)@
         * 'Proto.Utxorpc.V1alpha.Submit.Submit_Fields.raw' @:: Lens' AnyChainTx Data.ByteString.ByteString@ -}
data AnyChainTx
  = AnyChainTx'_constructor {_AnyChainTx'type' :: !(Prelude.Maybe AnyChainTx'Type),
                             _AnyChainTx'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show AnyChainTx where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
data AnyChainTx'Type
  = AnyChainTx'Raw !Data.ByteString.ByteString
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.Field.HasField AnyChainTx "maybe'type'" (Prelude.Maybe AnyChainTx'Type) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AnyChainTx'type' (\ x__ y__ -> x__ {_AnyChainTx'type' = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField AnyChainTx "maybe'raw" (Prelude.Maybe Data.ByteString.ByteString) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AnyChainTx'type' (\ x__ y__ -> x__ {_AnyChainTx'type' = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (AnyChainTx'Raw x__val)) -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap AnyChainTx'Raw y__))
instance Data.ProtoLens.Field.HasField AnyChainTx "raw" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AnyChainTx'type' (\ x__ y__ -> x__ {_AnyChainTx'type' = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (AnyChainTx'Raw x__val)) -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap AnyChainTx'Raw y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault))
instance Data.ProtoLens.Message AnyChainTx where
  messageName _ = Data.Text.pack "utxorpc.v1alpha.submit.AnyChainTx"
  packedMessageDescriptor _
    = "\n\
      \\n\
      \AnyChainTx\DC2\DC2\n\
      \\ETXraw\CAN\SOH \SOH(\fH\NULR\ETXrawB\ACK\n\
      \\EOTtype"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        raw__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "raw"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'raw")) ::
              Data.ProtoLens.FieldDescriptor AnyChainTx
      in
        Data.Map.fromList [(Data.ProtoLens.Tag 1, raw__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _AnyChainTx'_unknownFields
        (\ x__ y__ -> x__ {_AnyChainTx'_unknownFields = y__})
  defMessage
    = AnyChainTx'_constructor
        {_AnyChainTx'type' = Prelude.Nothing,
         _AnyChainTx'_unknownFields = []}
  parseMessage
    = let
        loop ::
          AnyChainTx -> Data.ProtoLens.Encoding.Bytes.Parser AnyChainTx
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
                                       "raw"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"raw") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "AnyChainTx"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'type'") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just (AnyChainTx'Raw v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                       ((\ bs
                           -> (Data.Monoid.<>)
                                (Data.ProtoLens.Encoding.Bytes.putVarInt
                                   (Prelude.fromIntegral (Data.ByteString.length bs)))
                                (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          v))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData AnyChainTx where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_AnyChainTx'_unknownFields x__)
             (Control.DeepSeq.deepseq (_AnyChainTx'type' x__) ())
instance Control.DeepSeq.NFData AnyChainTx'Type where
  rnf (AnyChainTx'Raw x__) = Control.DeepSeq.rnf x__
_AnyChainTx'Raw ::
  Data.ProtoLens.Prism.Prism' AnyChainTx'Type Data.ByteString.ByteString
_AnyChainTx'Raw
  = Data.ProtoLens.Prism.prism'
      AnyChainTx'Raw
      (\ p__
         -> case p__ of (AnyChainTx'Raw p__val) -> Prelude.Just p__val)
{- | Fields :
     
         * 'Proto.Utxorpc.V1alpha.Submit.Submit_Fields.tx' @:: Lens' SubmitTxRequest [AnyChainTx]@
         * 'Proto.Utxorpc.V1alpha.Submit.Submit_Fields.vec'tx' @:: Lens' SubmitTxRequest (Data.Vector.Vector AnyChainTx)@ -}
data SubmitTxRequest
  = SubmitTxRequest'_constructor {_SubmitTxRequest'tx :: !(Data.Vector.Vector AnyChainTx),
                                  _SubmitTxRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show SubmitTxRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField SubmitTxRequest "tx" [AnyChainTx] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SubmitTxRequest'tx (\ x__ y__ -> x__ {_SubmitTxRequest'tx = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField SubmitTxRequest "vec'tx" (Data.Vector.Vector AnyChainTx) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SubmitTxRequest'tx (\ x__ y__ -> x__ {_SubmitTxRequest'tx = y__}))
        Prelude.id
instance Data.ProtoLens.Message SubmitTxRequest where
  messageName _
    = Data.Text.pack "utxorpc.v1alpha.submit.SubmitTxRequest"
  packedMessageDescriptor _
    = "\n\
      \\SISubmitTxRequest\DC22\n\
      \\STXtx\CAN\SOH \ETX(\v2\".utxorpc.v1alpha.submit.AnyChainTxR\STXtx"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        tx__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "tx"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor AnyChainTx)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"tx")) ::
              Data.ProtoLens.FieldDescriptor SubmitTxRequest
      in Data.Map.fromList [(Data.ProtoLens.Tag 1, tx__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _SubmitTxRequest'_unknownFields
        (\ x__ y__ -> x__ {_SubmitTxRequest'_unknownFields = y__})
  defMessage
    = SubmitTxRequest'_constructor
        {_SubmitTxRequest'tx = Data.Vector.Generic.empty,
         _SubmitTxRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          SubmitTxRequest
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld AnyChainTx
             -> Data.ProtoLens.Encoding.Bytes.Parser SubmitTxRequest
        loop x mutable'tx
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'tx <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                     (Data.ProtoLens.Encoding.Growing.unsafeFreeze mutable'tx)
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
                              (Data.ProtoLens.Field.field @"vec'tx") frozen'tx x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "tx"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'tx y)
                                loop x v
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'tx
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'tx <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                              Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'tx)
          "SubmitTxRequest"
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
                (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'tx") _x))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData SubmitTxRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_SubmitTxRequest'_unknownFields x__)
             (Control.DeepSeq.deepseq (_SubmitTxRequest'tx x__) ())
{- | Fields :
     
         * 'Proto.Utxorpc.V1alpha.Submit.Submit_Fields.results' @:: Lens' SubmitTxResponse [TxSubmitResult]@
         * 'Proto.Utxorpc.V1alpha.Submit.Submit_Fields.vec'results' @:: Lens' SubmitTxResponse (Data.Vector.Vector TxSubmitResult)@ -}
data SubmitTxResponse
  = SubmitTxResponse'_constructor {_SubmitTxResponse'results :: !(Data.Vector.Vector TxSubmitResult),
                                   _SubmitTxResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show SubmitTxResponse where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField SubmitTxResponse "results" [TxSubmitResult] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SubmitTxResponse'results
           (\ x__ y__ -> x__ {_SubmitTxResponse'results = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField SubmitTxResponse "vec'results" (Data.Vector.Vector TxSubmitResult) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SubmitTxResponse'results
           (\ x__ y__ -> x__ {_SubmitTxResponse'results = y__}))
        Prelude.id
instance Data.ProtoLens.Message SubmitTxResponse where
  messageName _
    = Data.Text.pack "utxorpc.v1alpha.submit.SubmitTxResponse"
  packedMessageDescriptor _
    = "\n\
      \\DLESubmitTxResponse\DC2@\n\
      \\aresults\CAN\SOH \ETX(\v2&.utxorpc.v1alpha.submit.TxSubmitResultR\aresults"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        results__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "results"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor TxSubmitResult)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"results")) ::
              Data.ProtoLens.FieldDescriptor SubmitTxResponse
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, results__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _SubmitTxResponse'_unknownFields
        (\ x__ y__ -> x__ {_SubmitTxResponse'_unknownFields = y__})
  defMessage
    = SubmitTxResponse'_constructor
        {_SubmitTxResponse'results = Data.Vector.Generic.empty,
         _SubmitTxResponse'_unknownFields = []}
  parseMessage
    = let
        loop ::
          SubmitTxResponse
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld TxSubmitResult
             -> Data.ProtoLens.Encoding.Bytes.Parser SubmitTxResponse
        loop x mutable'results
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'results <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                          (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                             mutable'results)
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
                              (Data.ProtoLens.Field.field @"vec'results") frozen'results x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "results"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'results y)
                                loop x v
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'results
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'results <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                   Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'results)
          "SubmitTxResponse"
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
                (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'results") _x))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData SubmitTxResponse where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_SubmitTxResponse'_unknownFields x__)
             (Control.DeepSeq.deepseq (_SubmitTxResponse'results x__) ())
{- | Fields :
     
         * 'Proto.Utxorpc.V1alpha.Submit.Submit_Fields.maybe'result' @:: Lens' TxSubmitResult (Prelude.Maybe TxSubmitResult'Result)@
         * 'Proto.Utxorpc.V1alpha.Submit.Submit_Fields.maybe'ref' @:: Lens' TxSubmitResult (Prelude.Maybe Data.ByteString.ByteString)@
         * 'Proto.Utxorpc.V1alpha.Submit.Submit_Fields.ref' @:: Lens' TxSubmitResult Data.ByteString.ByteString@
         * 'Proto.Utxorpc.V1alpha.Submit.Submit_Fields.maybe'errorMessage' @:: Lens' TxSubmitResult (Prelude.Maybe Data.Text.Text)@
         * 'Proto.Utxorpc.V1alpha.Submit.Submit_Fields.errorMessage' @:: Lens' TxSubmitResult Data.Text.Text@ -}
data TxSubmitResult
  = TxSubmitResult'_constructor {_TxSubmitResult'result :: !(Prelude.Maybe TxSubmitResult'Result),
                                 _TxSubmitResult'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show TxSubmitResult where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
data TxSubmitResult'Result
  = TxSubmitResult'Ref !Data.ByteString.ByteString |
    TxSubmitResult'ErrorMessage !Data.Text.Text
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.Field.HasField TxSubmitResult "maybe'result" (Prelude.Maybe TxSubmitResult'Result) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TxSubmitResult'result
           (\ x__ y__ -> x__ {_TxSubmitResult'result = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField TxSubmitResult "maybe'ref" (Prelude.Maybe Data.ByteString.ByteString) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TxSubmitResult'result
           (\ x__ y__ -> x__ {_TxSubmitResult'result = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (TxSubmitResult'Ref x__val)) -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap TxSubmitResult'Ref y__))
instance Data.ProtoLens.Field.HasField TxSubmitResult "ref" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TxSubmitResult'result
           (\ x__ y__ -> x__ {_TxSubmitResult'result = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (TxSubmitResult'Ref x__val)) -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap TxSubmitResult'Ref y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault))
instance Data.ProtoLens.Field.HasField TxSubmitResult "maybe'errorMessage" (Prelude.Maybe Data.Text.Text) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TxSubmitResult'result
           (\ x__ y__ -> x__ {_TxSubmitResult'result = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (TxSubmitResult'ErrorMessage x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap TxSubmitResult'ErrorMessage y__))
instance Data.ProtoLens.Field.HasField TxSubmitResult "errorMessage" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TxSubmitResult'result
           (\ x__ y__ -> x__ {_TxSubmitResult'result = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (TxSubmitResult'ErrorMessage x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap TxSubmitResult'ErrorMessage y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault))
instance Data.ProtoLens.Message TxSubmitResult where
  messageName _
    = Data.Text.pack "utxorpc.v1alpha.submit.TxSubmitResult"
  packedMessageDescriptor _
    = "\n\
      \\SOTxSubmitResult\DC2\DC2\n\
      \\ETXref\CAN\SOH \SOH(\fH\NULR\ETXref\DC2%\n\
      \\rerror_message\CAN\STX \SOH(\tH\NULR\ferrorMessageB\b\n\
      \\ACKresult"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        ref__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "ref"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'ref")) ::
              Data.ProtoLens.FieldDescriptor TxSubmitResult
        errorMessage__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "error_message"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'errorMessage")) ::
              Data.ProtoLens.FieldDescriptor TxSubmitResult
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, ref__field_descriptor),
           (Data.ProtoLens.Tag 2, errorMessage__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _TxSubmitResult'_unknownFields
        (\ x__ y__ -> x__ {_TxSubmitResult'_unknownFields = y__})
  defMessage
    = TxSubmitResult'_constructor
        {_TxSubmitResult'result = Prelude.Nothing,
         _TxSubmitResult'_unknownFields = []}
  parseMessage
    = let
        loop ::
          TxSubmitResult
          -> Data.ProtoLens.Encoding.Bytes.Parser TxSubmitResult
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
                                       "ref"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"ref") y x)
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
          (do loop Data.ProtoLens.defMessage) "TxSubmitResult"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'result") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just (TxSubmitResult'Ref v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                       ((\ bs
                           -> (Data.Monoid.<>)
                                (Data.ProtoLens.Encoding.Bytes.putVarInt
                                   (Prelude.fromIntegral (Data.ByteString.length bs)))
                                (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          v)
                (Prelude.Just (TxSubmitResult'ErrorMessage v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.Text.Encoding.encodeUtf8 v))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData TxSubmitResult where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_TxSubmitResult'_unknownFields x__)
             (Control.DeepSeq.deepseq (_TxSubmitResult'result x__) ())
instance Control.DeepSeq.NFData TxSubmitResult'Result where
  rnf (TxSubmitResult'Ref x__) = Control.DeepSeq.rnf x__
  rnf (TxSubmitResult'ErrorMessage x__) = Control.DeepSeq.rnf x__
_TxSubmitResult'Ref ::
  Data.ProtoLens.Prism.Prism' TxSubmitResult'Result Data.ByteString.ByteString
_TxSubmitResult'Ref
  = Data.ProtoLens.Prism.prism'
      TxSubmitResult'Ref
      (\ p__
         -> case p__ of
              (TxSubmitResult'Ref p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_TxSubmitResult'ErrorMessage ::
  Data.ProtoLens.Prism.Prism' TxSubmitResult'Result Data.Text.Text
_TxSubmitResult'ErrorMessage
  = Data.ProtoLens.Prism.prism'
      TxSubmitResult'ErrorMessage
      (\ p__
         -> case p__ of
              (TxSubmitResult'ErrorMessage p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
data SubmitService = SubmitService {}
instance Data.ProtoLens.Service.Types.Service SubmitService where
  type ServiceName SubmitService = "SubmitService"
  type ServicePackage SubmitService = "utxorpc.v1alpha.submit"
  type ServiceMethods SubmitService = '["submitTx"]
  packedServiceDescriptor _
    = "\n\
      \\rSubmitService\DC2]\n\
      \\bSubmitTx\DC2'.utxorpc.v1alpha.submit.SubmitTxRequest\SUB(.utxorpc.v1alpha.submit.SubmitTxResponse"
instance Data.ProtoLens.Service.Types.HasMethodImpl SubmitService "submitTx" where
  type MethodName SubmitService "submitTx" = "SubmitTx"
  type MethodInput SubmitService "submitTx" = SubmitTxRequest
  type MethodOutput SubmitService "submitTx" = SubmitTxResponse
  type MethodStreamingType SubmitService "submitTx" = 'Data.ProtoLens.Service.Types.NonStreaming
packedFileDescriptor :: Data.ByteString.ByteString
packedFileDescriptor
  = "\n\
    \#utxorpc/v1alpha/submit/submit.proto\DC2\SYNutxorpc.v1alpha.submit\"(\n\
    \\n\
    \AnyChainTx\DC2\DC2\n\
    \\ETXraw\CAN\SOH \SOH(\fH\NULR\ETXrawB\ACK\n\
    \\EOTtype\"E\n\
    \\SISubmitTxRequest\DC22\n\
    \\STXtx\CAN\SOH \ETX(\v2\".utxorpc.v1alpha.submit.AnyChainTxR\STXtx\"U\n\
    \\SOTxSubmitResult\DC2\DC2\n\
    \\ETXref\CAN\SOH \SOH(\fH\NULR\ETXref\DC2%\n\
    \\rerror_message\CAN\STX \SOH(\tH\NULR\ferrorMessageB\b\n\
    \\ACKresult\"T\n\
    \\DLESubmitTxResponse\DC2@\n\
    \\aresults\CAN\SOH \ETX(\v2&.utxorpc.v1alpha.submit.TxSubmitResultR\aresults2n\n\
    \\rSubmitService\DC2]\n\
    \\bSubmitTx\DC2'.utxorpc.v1alpha.submit.SubmitTxRequest\SUB(.utxorpc.v1alpha.submit.SubmitTxResponseB\163\SOH\n\
    \\SUBcom.utxorpc.v1alpha.submitB\vSubmitProtoP\SOH\162\STX\ETXUVS\170\STX\SYNUtxorpc.V1alpha.Submit\202\STX\SYNUtxorpc\\V1alpha\\Submit\226\STX\"Utxorpc\\V1alpha\\Submit\\GPBMetadata\234\STX\CANUtxorpc::V1alpha::SubmitJ\136\t\n\
    \\ACK\DC2\EOT\NUL\NUL!\SOH\n\
    \\b\n\
    \\SOH\f\DC2\ETX\NUL\NUL\DC2\n\
    \\b\n\
    \\SOH\STX\DC2\ETX\STX\NUL\US\n\
    \E\n\
    \\STX\EOT\NUL\DC2\EOT\ENQ\NUL\t\SOH\SUB9 Represents a transaction from any supported blockchain.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\NUL\SOH\DC2\ETX\ENQ\b\DC2\n\
    \\f\n\
    \\EOT\EOT\NUL\b\NUL\DC2\EOT\ACK\STX\b\ETX\n\
    \\f\n\
    \\ENQ\EOT\NUL\b\NUL\SOH\DC2\ETX\ACK\b\f\n\
    \$\n\
    \\EOT\EOT\NUL\STX\NUL\DC2\ETX\a\EOT\DC2\"\ETB Raw transaction data.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ENQ\DC2\ETX\a\EOT\t\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\SOH\DC2\ETX\a\n\
    \\r\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ETX\DC2\ETX\a\DLE\DC1\n\
    \?\n\
    \\STX\EOT\SOH\DC2\EOT\f\NUL\SO\SOH\SUB3 Request to submit transactions to the blockchain.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\SOH\SOH\DC2\ETX\f\b\ETB\n\
    \.\n\
    \\EOT\EOT\SOH\STX\NUL\DC2\ETX\r\STX\GS\"! List of transactions to submit.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\EOT\DC2\ETX\r\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ACK\DC2\ETX\r\v\NAK\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\SOH\DC2\ETX\r\SYN\CAN\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ETX\DC2\ETX\r\ESC\FS\n\
    \K\n\
    \\STX\EOT\STX\DC2\EOT\DC1\NUL\SYN\SOH\SUB? TODO u5c: new type - https://github.com/utxorpc/spec/pull/163\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\STX\SOH\DC2\ETX\DC1\b\SYN\n\
    \\f\n\
    \\EOT\EOT\STX\b\NUL\DC2\EOT\DC2\STX\NAK\ETX\n\
    \\f\n\
    \\ENQ\EOT\STX\b\NUL\SOH\DC2\ETX\DC2\b\SO\n\
    \&\n\
    \\EOT\EOT\STX\STX\NUL\DC2\ETX\DC3\EOT\DC2\"\EM Transaction references.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ENQ\DC2\ETX\DC3\EOT\t\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\SOH\DC2\ETX\DC3\n\
    \\r\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ETX\DC2\ETX\DC3\DLE\DC1\n\
    \ \n\
    \\EOT\EOT\STX\STX\SOH\DC2\ETX\DC4\EOT\GS\"\DC3 The error message\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\ENQ\DC2\ETX\DC4\EOT\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\SOH\DC2\ETX\DC4\v\CAN\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\ETX\DC2\ETX\DC4\ESC\FS\n\
    \\143\SOH\n\
    \\STX\EOT\ETX\DC2\EOT\SUB\NUL\FS\SOH\SUB\130\SOH Response containing references to the submitted transactions.\n\
    \ TODO u5c: changed type - https://github.com/utxorpc/spec/pull/163\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\ETX\SOH\DC2\ETX\SUB\b\CAN\n\
    \G\n\
    \\EOT\EOT\ETX\STX\NUL\DC2\ETX\ESC\STX&\": List of either transaction references or error messages.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\EOT\DC2\ETX\ESC\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\ACK\DC2\ETX\ESC\v\EM\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\SOH\DC2\ETX\ESC\SUB!\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\ETX\DC2\ETX\ESC$%\n\
    \W\n\
    \\STX\ACK\NUL\DC2\EOT\US\NUL!\SOH\SUBK Service definition for submitting transactions and checking their status.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\ACK\NUL\SOH\DC2\ETX\US\b\NAK\n\
    \5\n\
    \\EOT\ACK\NUL\STX\NUL\DC2\ETX \STX;\"( Submit transactions to the blockchain.\n\
    \\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\SOH\DC2\ETX \ACK\SO\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\STX\DC2\ETX \SI\RS\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\ETX\DC2\ETX )9b\ACKproto3"