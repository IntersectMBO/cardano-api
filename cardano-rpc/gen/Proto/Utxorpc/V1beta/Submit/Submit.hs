{- This file was auto-generated from utxorpc/v1beta/submit/submit.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Utxorpc.V1beta.Submit.Submit (
        SubmitService(..), AnyChainEval(), AnyChainEval'Chain(..),
        _AnyChainEval'Cardano, AnyChainTx(), AnyChainTx'Type(..),
        _AnyChainTx'Raw, AnyChainTxPattern(), AnyChainTxPattern'Chain(..),
        _AnyChainTxPattern'Cardano, EvalTxRequest(), EvalTxResponse(),
        ReadMempoolRequest(), ReadMempoolResponse(), Stage(..), Stage(),
        Stage'UnrecognizedValue, SubmitTxRequest(), SubmitTxResponse(),
        TxInMempool(), TxInMempool'ParsedState(..), _TxInMempool'Cardano,
        TxPredicate(), WaitForTxRequest(), WaitForTxResponse(),
        WatchMempoolRequest(), WatchMempoolResponse()
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
     
         * 'Proto.Utxorpc.V1beta.Submit.Submit_Fields.maybe'chain' @:: Lens' AnyChainEval (Prelude.Maybe AnyChainEval'Chain)@
         * 'Proto.Utxorpc.V1beta.Submit.Submit_Fields.maybe'cardano' @:: Lens' AnyChainEval (Prelude.Maybe Proto.Utxorpc.V1beta.Cardano.Cardano.TxEval)@
         * 'Proto.Utxorpc.V1beta.Submit.Submit_Fields.cardano' @:: Lens' AnyChainEval Proto.Utxorpc.V1beta.Cardano.Cardano.TxEval@ -}
data AnyChainEval
  = AnyChainEval'_constructor {_AnyChainEval'chain :: !(Prelude.Maybe AnyChainEval'Chain),
                               _AnyChainEval'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show AnyChainEval where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
data AnyChainEval'Chain
  = AnyChainEval'Cardano !Proto.Utxorpc.V1beta.Cardano.Cardano.TxEval
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.Field.HasField AnyChainEval "maybe'chain" (Prelude.Maybe AnyChainEval'Chain) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AnyChainEval'chain (\ x__ y__ -> x__ {_AnyChainEval'chain = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField AnyChainEval "maybe'cardano" (Prelude.Maybe Proto.Utxorpc.V1beta.Cardano.Cardano.TxEval) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AnyChainEval'chain (\ x__ y__ -> x__ {_AnyChainEval'chain = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (AnyChainEval'Cardano x__val)) -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap AnyChainEval'Cardano y__))
instance Data.ProtoLens.Field.HasField AnyChainEval "cardano" Proto.Utxorpc.V1beta.Cardano.Cardano.TxEval where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AnyChainEval'chain (\ x__ y__ -> x__ {_AnyChainEval'chain = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (AnyChainEval'Cardano x__val)) -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap AnyChainEval'Cardano y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Message AnyChainEval where
  messageName _ = Data.Text.pack "utxorpc.v1beta.submit.AnyChainEval"
  packedMessageDescriptor _
    = "\n\
      \\fAnyChainEval\DC2:\n\
      \\acardano\CAN\SOH \SOH(\v2\RS.utxorpc.v1beta.cardano.TxEvalH\NULR\acardanoB\a\n\
      \\ENQchain"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        cardano__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "cardano"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Utxorpc.V1beta.Cardano.Cardano.TxEval)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'cardano")) ::
              Data.ProtoLens.FieldDescriptor AnyChainEval
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, cardano__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _AnyChainEval'_unknownFields
        (\ x__ y__ -> x__ {_AnyChainEval'_unknownFields = y__})
  defMessage
    = AnyChainEval'_constructor
        {_AnyChainEval'chain = Prelude.Nothing,
         _AnyChainEval'_unknownFields = []}
  parseMessage
    = let
        loop ::
          AnyChainEval -> Data.ProtoLens.Encoding.Bytes.Parser AnyChainEval
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
          (do loop Data.ProtoLens.defMessage) "AnyChainEval"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'chain") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just (AnyChainEval'Cardano v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage v))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData AnyChainEval where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_AnyChainEval'_unknownFields x__)
             (Control.DeepSeq.deepseq (_AnyChainEval'chain x__) ())
instance Control.DeepSeq.NFData AnyChainEval'Chain where
  rnf (AnyChainEval'Cardano x__) = Control.DeepSeq.rnf x__
_AnyChainEval'Cardano ::
  Data.ProtoLens.Prism.Prism' AnyChainEval'Chain Proto.Utxorpc.V1beta.Cardano.Cardano.TxEval
_AnyChainEval'Cardano
  = Data.ProtoLens.Prism.prism'
      AnyChainEval'Cardano
      (\ p__
         -> case p__ of
              (AnyChainEval'Cardano p__val) -> Prelude.Just p__val)
{- | Fields :
     
         * 'Proto.Utxorpc.V1beta.Submit.Submit_Fields.maybe'type'' @:: Lens' AnyChainTx (Prelude.Maybe AnyChainTx'Type)@
         * 'Proto.Utxorpc.V1beta.Submit.Submit_Fields.maybe'raw' @:: Lens' AnyChainTx (Prelude.Maybe Data.ByteString.ByteString)@
         * 'Proto.Utxorpc.V1beta.Submit.Submit_Fields.raw' @:: Lens' AnyChainTx Data.ByteString.ByteString@ -}
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
  messageName _ = Data.Text.pack "utxorpc.v1beta.submit.AnyChainTx"
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
     
         * 'Proto.Utxorpc.V1beta.Submit.Submit_Fields.maybe'chain' @:: Lens' AnyChainTxPattern (Prelude.Maybe AnyChainTxPattern'Chain)@
         * 'Proto.Utxorpc.V1beta.Submit.Submit_Fields.maybe'cardano' @:: Lens' AnyChainTxPattern (Prelude.Maybe Proto.Utxorpc.V1beta.Cardano.Cardano.TxPattern)@
         * 'Proto.Utxorpc.V1beta.Submit.Submit_Fields.cardano' @:: Lens' AnyChainTxPattern Proto.Utxorpc.V1beta.Cardano.Cardano.TxPattern@ -}
data AnyChainTxPattern
  = AnyChainTxPattern'_constructor {_AnyChainTxPattern'chain :: !(Prelude.Maybe AnyChainTxPattern'Chain),
                                    _AnyChainTxPattern'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show AnyChainTxPattern where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
data AnyChainTxPattern'Chain
  = AnyChainTxPattern'Cardano !Proto.Utxorpc.V1beta.Cardano.Cardano.TxPattern
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.Field.HasField AnyChainTxPattern "maybe'chain" (Prelude.Maybe AnyChainTxPattern'Chain) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AnyChainTxPattern'chain
           (\ x__ y__ -> x__ {_AnyChainTxPattern'chain = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField AnyChainTxPattern "maybe'cardano" (Prelude.Maybe Proto.Utxorpc.V1beta.Cardano.Cardano.TxPattern) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AnyChainTxPattern'chain
           (\ x__ y__ -> x__ {_AnyChainTxPattern'chain = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (AnyChainTxPattern'Cardano x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap AnyChainTxPattern'Cardano y__))
instance Data.ProtoLens.Field.HasField AnyChainTxPattern "cardano" Proto.Utxorpc.V1beta.Cardano.Cardano.TxPattern where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AnyChainTxPattern'chain
           (\ x__ y__ -> x__ {_AnyChainTxPattern'chain = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (AnyChainTxPattern'Cardano x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap AnyChainTxPattern'Cardano y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Message AnyChainTxPattern where
  messageName _
    = Data.Text.pack "utxorpc.v1beta.submit.AnyChainTxPattern"
  packedMessageDescriptor _
    = "\n\
      \\DC1AnyChainTxPattern\DC2=\n\
      \\acardano\CAN\SOH \SOH(\v2!.utxorpc.v1beta.cardano.TxPatternH\NULR\acardanoB\a\n\
      \\ENQchain"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        cardano__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "cardano"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Utxorpc.V1beta.Cardano.Cardano.TxPattern)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'cardano")) ::
              Data.ProtoLens.FieldDescriptor AnyChainTxPattern
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, cardano__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _AnyChainTxPattern'_unknownFields
        (\ x__ y__ -> x__ {_AnyChainTxPattern'_unknownFields = y__})
  defMessage
    = AnyChainTxPattern'_constructor
        {_AnyChainTxPattern'chain = Prelude.Nothing,
         _AnyChainTxPattern'_unknownFields = []}
  parseMessage
    = let
        loop ::
          AnyChainTxPattern
          -> Data.ProtoLens.Encoding.Bytes.Parser AnyChainTxPattern
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
          (do loop Data.ProtoLens.defMessage) "AnyChainTxPattern"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'chain") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just (AnyChainTxPattern'Cardano v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage v))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData AnyChainTxPattern where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_AnyChainTxPattern'_unknownFields x__)
             (Control.DeepSeq.deepseq (_AnyChainTxPattern'chain x__) ())
instance Control.DeepSeq.NFData AnyChainTxPattern'Chain where
  rnf (AnyChainTxPattern'Cardano x__) = Control.DeepSeq.rnf x__
_AnyChainTxPattern'Cardano ::
  Data.ProtoLens.Prism.Prism' AnyChainTxPattern'Chain Proto.Utxorpc.V1beta.Cardano.Cardano.TxPattern
_AnyChainTxPattern'Cardano
  = Data.ProtoLens.Prism.prism'
      AnyChainTxPattern'Cardano
      (\ p__
         -> case p__ of
              (AnyChainTxPattern'Cardano p__val) -> Prelude.Just p__val)
{- | Fields :
     
         * 'Proto.Utxorpc.V1beta.Submit.Submit_Fields.tx' @:: Lens' EvalTxRequest AnyChainTx@
         * 'Proto.Utxorpc.V1beta.Submit.Submit_Fields.maybe'tx' @:: Lens' EvalTxRequest (Prelude.Maybe AnyChainTx)@ -}
data EvalTxRequest
  = EvalTxRequest'_constructor {_EvalTxRequest'tx :: !(Prelude.Maybe AnyChainTx),
                                _EvalTxRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show EvalTxRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField EvalTxRequest "tx" AnyChainTx where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _EvalTxRequest'tx (\ x__ y__ -> x__ {_EvalTxRequest'tx = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField EvalTxRequest "maybe'tx" (Prelude.Maybe AnyChainTx) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _EvalTxRequest'tx (\ x__ y__ -> x__ {_EvalTxRequest'tx = y__}))
        Prelude.id
instance Data.ProtoLens.Message EvalTxRequest where
  messageName _
    = Data.Text.pack "utxorpc.v1beta.submit.EvalTxRequest"
  packedMessageDescriptor _
    = "\n\
      \\rEvalTxRequest\DC21\n\
      \\STXtx\CAN\SOH \SOH(\v2!.utxorpc.v1beta.submit.AnyChainTxR\STXtx"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        tx__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "tx"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor AnyChainTx)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'tx")) ::
              Data.ProtoLens.FieldDescriptor EvalTxRequest
      in Data.Map.fromList [(Data.ProtoLens.Tag 1, tx__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _EvalTxRequest'_unknownFields
        (\ x__ y__ -> x__ {_EvalTxRequest'_unknownFields = y__})
  defMessage
    = EvalTxRequest'_constructor
        {_EvalTxRequest'tx = Prelude.Nothing,
         _EvalTxRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          EvalTxRequest -> Data.ProtoLens.Encoding.Bytes.Parser EvalTxRequest
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
                                       "tx"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"tx") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "EvalTxRequest"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'tx") _x
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
instance Control.DeepSeq.NFData EvalTxRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_EvalTxRequest'_unknownFields x__)
             (Control.DeepSeq.deepseq (_EvalTxRequest'tx x__) ())
{- | Fields :
     
         * 'Proto.Utxorpc.V1beta.Submit.Submit_Fields.report' @:: Lens' EvalTxResponse AnyChainEval@
         * 'Proto.Utxorpc.V1beta.Submit.Submit_Fields.maybe'report' @:: Lens' EvalTxResponse (Prelude.Maybe AnyChainEval)@ -}
data EvalTxResponse
  = EvalTxResponse'_constructor {_EvalTxResponse'report :: !(Prelude.Maybe AnyChainEval),
                                 _EvalTxResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show EvalTxResponse where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField EvalTxResponse "report" AnyChainEval where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _EvalTxResponse'report
           (\ x__ y__ -> x__ {_EvalTxResponse'report = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField EvalTxResponse "maybe'report" (Prelude.Maybe AnyChainEval) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _EvalTxResponse'report
           (\ x__ y__ -> x__ {_EvalTxResponse'report = y__}))
        Prelude.id
instance Data.ProtoLens.Message EvalTxResponse where
  messageName _
    = Data.Text.pack "utxorpc.v1beta.submit.EvalTxResponse"
  packedMessageDescriptor _
    = "\n\
      \\SOEvalTxResponse\DC2;\n\
      \\ACKreport\CAN\SOH \SOH(\v2#.utxorpc.v1beta.submit.AnyChainEvalR\ACKreport"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        report__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "report"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor AnyChainEval)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'report")) ::
              Data.ProtoLens.FieldDescriptor EvalTxResponse
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, report__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _EvalTxResponse'_unknownFields
        (\ x__ y__ -> x__ {_EvalTxResponse'_unknownFields = y__})
  defMessage
    = EvalTxResponse'_constructor
        {_EvalTxResponse'report = Prelude.Nothing,
         _EvalTxResponse'_unknownFields = []}
  parseMessage
    = let
        loop ::
          EvalTxResponse
          -> Data.ProtoLens.Encoding.Bytes.Parser EvalTxResponse
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
                                       "report"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"report") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "EvalTxResponse"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'report") _x
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
instance Control.DeepSeq.NFData EvalTxResponse where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_EvalTxResponse'_unknownFields x__)
             (Control.DeepSeq.deepseq (_EvalTxResponse'report x__) ())
{- | Fields :
      -}
data ReadMempoolRequest
  = ReadMempoolRequest'_constructor {_ReadMempoolRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ReadMempoolRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Message ReadMempoolRequest where
  messageName _
    = Data.Text.pack "utxorpc.v1beta.submit.ReadMempoolRequest"
  packedMessageDescriptor _
    = "\n\
      \\DC2ReadMempoolRequest"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag = let in Data.Map.fromList []
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ReadMempoolRequest'_unknownFields
        (\ x__ y__ -> x__ {_ReadMempoolRequest'_unknownFields = y__})
  defMessage
    = ReadMempoolRequest'_constructor
        {_ReadMempoolRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ReadMempoolRequest
          -> Data.ProtoLens.Encoding.Bytes.Parser ReadMempoolRequest
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
          (do loop Data.ProtoLens.defMessage) "ReadMempoolRequest"
  buildMessage
    = \ _x
        -> Data.ProtoLens.Encoding.Wire.buildFieldSet
             (Lens.Family2.view Data.ProtoLens.unknownFields _x)
instance Control.DeepSeq.NFData ReadMempoolRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ReadMempoolRequest'_unknownFields x__) ()
{- | Fields :
     
         * 'Proto.Utxorpc.V1beta.Submit.Submit_Fields.items' @:: Lens' ReadMempoolResponse [TxInMempool]@
         * 'Proto.Utxorpc.V1beta.Submit.Submit_Fields.vec'items' @:: Lens' ReadMempoolResponse (Data.Vector.Vector TxInMempool)@ -}
data ReadMempoolResponse
  = ReadMempoolResponse'_constructor {_ReadMempoolResponse'items :: !(Data.Vector.Vector TxInMempool),
                                      _ReadMempoolResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ReadMempoolResponse where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ReadMempoolResponse "items" [TxInMempool] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReadMempoolResponse'items
           (\ x__ y__ -> x__ {_ReadMempoolResponse'items = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ReadMempoolResponse "vec'items" (Data.Vector.Vector TxInMempool) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReadMempoolResponse'items
           (\ x__ y__ -> x__ {_ReadMempoolResponse'items = y__}))
        Prelude.id
instance Data.ProtoLens.Message ReadMempoolResponse where
  messageName _
    = Data.Text.pack "utxorpc.v1beta.submit.ReadMempoolResponse"
  packedMessageDescriptor _
    = "\n\
      \\DC3ReadMempoolResponse\DC28\n\
      \\ENQitems\CAN\SOH \ETX(\v2\".utxorpc.v1beta.submit.TxInMempoolR\ENQitems"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        items__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "items"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor TxInMempool)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"items")) ::
              Data.ProtoLens.FieldDescriptor ReadMempoolResponse
      in
        Data.Map.fromList [(Data.ProtoLens.Tag 1, items__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ReadMempoolResponse'_unknownFields
        (\ x__ y__ -> x__ {_ReadMempoolResponse'_unknownFields = y__})
  defMessage
    = ReadMempoolResponse'_constructor
        {_ReadMempoolResponse'items = Data.Vector.Generic.empty,
         _ReadMempoolResponse'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ReadMempoolResponse
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld TxInMempool
             -> Data.ProtoLens.Encoding.Bytes.Parser ReadMempoolResponse
        loop x mutable'items
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'items <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                        (Data.ProtoLens.Encoding.Growing.unsafeFreeze mutable'items)
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
                              (Data.ProtoLens.Field.field @"vec'items") frozen'items x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "items"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'items y)
                                loop x v
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'items
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'items <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                 Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'items)
          "ReadMempoolResponse"
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
                (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'items") _x))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData ReadMempoolResponse where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ReadMempoolResponse'_unknownFields x__)
             (Control.DeepSeq.deepseq (_ReadMempoolResponse'items x__) ())
newtype Stage'UnrecognizedValue
  = Stage'UnrecognizedValue Data.Int.Int32
  deriving stock (Prelude.Eq, Prelude.Ord, Prelude.Show)
data Stage
  = STAGE_UNSPECIFIED |
    STAGE_ACKNOWLEDGED |
    STAGE_MEMPOOL |
    STAGE_NETWORK |
    STAGE_CONFIRMED |
    Stage'Unrecognized !Stage'UnrecognizedValue
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.MessageEnum Stage where
  maybeToEnum 0 = Prelude.Just STAGE_UNSPECIFIED
  maybeToEnum 1 = Prelude.Just STAGE_ACKNOWLEDGED
  maybeToEnum 2 = Prelude.Just STAGE_MEMPOOL
  maybeToEnum 3 = Prelude.Just STAGE_NETWORK
  maybeToEnum 4 = Prelude.Just STAGE_CONFIRMED
  maybeToEnum k
    = Prelude.Just
        (Stage'Unrecognized
           (Stage'UnrecognizedValue (Prelude.fromIntegral k)))
  showEnum STAGE_UNSPECIFIED = "STAGE_UNSPECIFIED"
  showEnum STAGE_ACKNOWLEDGED = "STAGE_ACKNOWLEDGED"
  showEnum STAGE_MEMPOOL = "STAGE_MEMPOOL"
  showEnum STAGE_NETWORK = "STAGE_NETWORK"
  showEnum STAGE_CONFIRMED = "STAGE_CONFIRMED"
  showEnum (Stage'Unrecognized (Stage'UnrecognizedValue k))
    = Prelude.show k
  readEnum k
    | (Prelude.==) k "STAGE_UNSPECIFIED"
    = Prelude.Just STAGE_UNSPECIFIED
    | (Prelude.==) k "STAGE_ACKNOWLEDGED"
    = Prelude.Just STAGE_ACKNOWLEDGED
    | (Prelude.==) k "STAGE_MEMPOOL" = Prelude.Just STAGE_MEMPOOL
    | (Prelude.==) k "STAGE_NETWORK" = Prelude.Just STAGE_NETWORK
    | (Prelude.==) k "STAGE_CONFIRMED" = Prelude.Just STAGE_CONFIRMED
    | Prelude.otherwise
    = (Prelude.>>=) (Text.Read.readMaybe k) Data.ProtoLens.maybeToEnum
instance Prelude.Bounded Stage where
  minBound = STAGE_UNSPECIFIED
  maxBound = STAGE_CONFIRMED
instance Prelude.Enum Stage where
  toEnum k__
    = Prelude.maybe
        (Prelude.error
           ((Prelude.++)
              "toEnum: unknown value for enum Stage: " (Prelude.show k__)))
        Prelude.id (Data.ProtoLens.maybeToEnum k__)
  fromEnum STAGE_UNSPECIFIED = 0
  fromEnum STAGE_ACKNOWLEDGED = 1
  fromEnum STAGE_MEMPOOL = 2
  fromEnum STAGE_NETWORK = 3
  fromEnum STAGE_CONFIRMED = 4
  fromEnum (Stage'Unrecognized (Stage'UnrecognizedValue k))
    = Prelude.fromIntegral k
  succ STAGE_CONFIRMED
    = Prelude.error
        "Stage.succ: bad argument STAGE_CONFIRMED. This value would be out of bounds."
  succ STAGE_UNSPECIFIED = STAGE_ACKNOWLEDGED
  succ STAGE_ACKNOWLEDGED = STAGE_MEMPOOL
  succ STAGE_MEMPOOL = STAGE_NETWORK
  succ STAGE_NETWORK = STAGE_CONFIRMED
  succ (Stage'Unrecognized _)
    = Prelude.error "Stage.succ: bad argument: unrecognized value"
  pred STAGE_UNSPECIFIED
    = Prelude.error
        "Stage.pred: bad argument STAGE_UNSPECIFIED. This value would be out of bounds."
  pred STAGE_ACKNOWLEDGED = STAGE_UNSPECIFIED
  pred STAGE_MEMPOOL = STAGE_ACKNOWLEDGED
  pred STAGE_NETWORK = STAGE_MEMPOOL
  pred STAGE_CONFIRMED = STAGE_NETWORK
  pred (Stage'Unrecognized _)
    = Prelude.error "Stage.pred: bad argument: unrecognized value"
  enumFrom = Data.ProtoLens.Message.Enum.messageEnumFrom
  enumFromTo = Data.ProtoLens.Message.Enum.messageEnumFromTo
  enumFromThen = Data.ProtoLens.Message.Enum.messageEnumFromThen
  enumFromThenTo = Data.ProtoLens.Message.Enum.messageEnumFromThenTo
instance Data.ProtoLens.FieldDefault Stage where
  fieldDefault = STAGE_UNSPECIFIED
instance Control.DeepSeq.NFData Stage where
  rnf x__ = Prelude.seq x__ ()
{- | Fields :
     
         * 'Proto.Utxorpc.V1beta.Submit.Submit_Fields.tx' @:: Lens' SubmitTxRequest AnyChainTx@
         * 'Proto.Utxorpc.V1beta.Submit.Submit_Fields.maybe'tx' @:: Lens' SubmitTxRequest (Prelude.Maybe AnyChainTx)@ -}
data SubmitTxRequest
  = SubmitTxRequest'_constructor {_SubmitTxRequest'tx :: !(Prelude.Maybe AnyChainTx),
                                  _SubmitTxRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show SubmitTxRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField SubmitTxRequest "tx" AnyChainTx where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SubmitTxRequest'tx (\ x__ y__ -> x__ {_SubmitTxRequest'tx = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField SubmitTxRequest "maybe'tx" (Prelude.Maybe AnyChainTx) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SubmitTxRequest'tx (\ x__ y__ -> x__ {_SubmitTxRequest'tx = y__}))
        Prelude.id
instance Data.ProtoLens.Message SubmitTxRequest where
  messageName _
    = Data.Text.pack "utxorpc.v1beta.submit.SubmitTxRequest"
  packedMessageDescriptor _
    = "\n\
      \\SISubmitTxRequest\DC21\n\
      \\STXtx\CAN\SOH \SOH(\v2!.utxorpc.v1beta.submit.AnyChainTxR\STXtx"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        tx__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "tx"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor AnyChainTx)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'tx")) ::
              Data.ProtoLens.FieldDescriptor SubmitTxRequest
      in Data.Map.fromList [(Data.ProtoLens.Tag 1, tx__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _SubmitTxRequest'_unknownFields
        (\ x__ y__ -> x__ {_SubmitTxRequest'_unknownFields = y__})
  defMessage
    = SubmitTxRequest'_constructor
        {_SubmitTxRequest'tx = Prelude.Nothing,
         _SubmitTxRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          SubmitTxRequest
          -> Data.ProtoLens.Encoding.Bytes.Parser SubmitTxRequest
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
                                       "tx"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"tx") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "SubmitTxRequest"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'tx") _x
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
instance Control.DeepSeq.NFData SubmitTxRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_SubmitTxRequest'_unknownFields x__)
             (Control.DeepSeq.deepseq (_SubmitTxRequest'tx x__) ())
{- | Fields :
     
         * 'Proto.Utxorpc.V1beta.Submit.Submit_Fields.ref' @:: Lens' SubmitTxResponse Data.ByteString.ByteString@ -}
data SubmitTxResponse
  = SubmitTxResponse'_constructor {_SubmitTxResponse'ref :: !Data.ByteString.ByteString,
                                   _SubmitTxResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show SubmitTxResponse where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField SubmitTxResponse "ref" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SubmitTxResponse'ref
           (\ x__ y__ -> x__ {_SubmitTxResponse'ref = y__}))
        Prelude.id
instance Data.ProtoLens.Message SubmitTxResponse where
  messageName _
    = Data.Text.pack "utxorpc.v1beta.submit.SubmitTxResponse"
  packedMessageDescriptor _
    = "\n\
      \\DLESubmitTxResponse\DC2\DLE\n\
      \\ETXref\CAN\SOH \SOH(\fR\ETXref"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        ref__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "ref"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"ref")) ::
              Data.ProtoLens.FieldDescriptor SubmitTxResponse
      in
        Data.Map.fromList [(Data.ProtoLens.Tag 1, ref__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _SubmitTxResponse'_unknownFields
        (\ x__ y__ -> x__ {_SubmitTxResponse'_unknownFields = y__})
  defMessage
    = SubmitTxResponse'_constructor
        {_SubmitTxResponse'ref = Data.ProtoLens.fieldDefault,
         _SubmitTxResponse'_unknownFields = []}
  parseMessage
    = let
        loop ::
          SubmitTxResponse
          -> Data.ProtoLens.Encoding.Bytes.Parser SubmitTxResponse
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
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "SubmitTxResponse"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"ref") _x
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
instance Control.DeepSeq.NFData SubmitTxResponse where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_SubmitTxResponse'_unknownFields x__)
             (Control.DeepSeq.deepseq (_SubmitTxResponse'ref x__) ())
{- | Fields :
     
         * 'Proto.Utxorpc.V1beta.Submit.Submit_Fields.ref' @:: Lens' TxInMempool Data.ByteString.ByteString@
         * 'Proto.Utxorpc.V1beta.Submit.Submit_Fields.nativeBytes' @:: Lens' TxInMempool Data.ByteString.ByteString@
         * 'Proto.Utxorpc.V1beta.Submit.Submit_Fields.stage' @:: Lens' TxInMempool Stage@
         * 'Proto.Utxorpc.V1beta.Submit.Submit_Fields.maybe'parsedState' @:: Lens' TxInMempool (Prelude.Maybe TxInMempool'ParsedState)@
         * 'Proto.Utxorpc.V1beta.Submit.Submit_Fields.maybe'cardano' @:: Lens' TxInMempool (Prelude.Maybe Proto.Utxorpc.V1beta.Cardano.Cardano.Tx)@
         * 'Proto.Utxorpc.V1beta.Submit.Submit_Fields.cardano' @:: Lens' TxInMempool Proto.Utxorpc.V1beta.Cardano.Cardano.Tx@ -}
data TxInMempool
  = TxInMempool'_constructor {_TxInMempool'ref :: !Data.ByteString.ByteString,
                              _TxInMempool'nativeBytes :: !Data.ByteString.ByteString,
                              _TxInMempool'stage :: !Stage,
                              _TxInMempool'parsedState :: !(Prelude.Maybe TxInMempool'ParsedState),
                              _TxInMempool'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show TxInMempool where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
data TxInMempool'ParsedState
  = TxInMempool'Cardano !Proto.Utxorpc.V1beta.Cardano.Cardano.Tx
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.Field.HasField TxInMempool "ref" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TxInMempool'ref (\ x__ y__ -> x__ {_TxInMempool'ref = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField TxInMempool "nativeBytes" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TxInMempool'nativeBytes
           (\ x__ y__ -> x__ {_TxInMempool'nativeBytes = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField TxInMempool "stage" Stage where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TxInMempool'stage (\ x__ y__ -> x__ {_TxInMempool'stage = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField TxInMempool "maybe'parsedState" (Prelude.Maybe TxInMempool'ParsedState) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TxInMempool'parsedState
           (\ x__ y__ -> x__ {_TxInMempool'parsedState = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField TxInMempool "maybe'cardano" (Prelude.Maybe Proto.Utxorpc.V1beta.Cardano.Cardano.Tx) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TxInMempool'parsedState
           (\ x__ y__ -> x__ {_TxInMempool'parsedState = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (TxInMempool'Cardano x__val)) -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap TxInMempool'Cardano y__))
instance Data.ProtoLens.Field.HasField TxInMempool "cardano" Proto.Utxorpc.V1beta.Cardano.Cardano.Tx where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TxInMempool'parsedState
           (\ x__ y__ -> x__ {_TxInMempool'parsedState = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (TxInMempool'Cardano x__val)) -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap TxInMempool'Cardano y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Message TxInMempool where
  messageName _ = Data.Text.pack "utxorpc.v1beta.submit.TxInMempool"
  packedMessageDescriptor _
    = "\n\
      \\vTxInMempool\DC2\DLE\n\
      \\ETXref\CAN\SOH \SOH(\fR\ETXref\DC2!\n\
      \\fnative_bytes\CAN\STX \SOH(\fR\vnativeBytes\DC22\n\
      \\ENQstage\CAN\ETX \SOH(\SO2\FS.utxorpc.v1beta.submit.StageR\ENQstage\DC26\n\
      \\acardano\CAN\EOT \SOH(\v2\SUB.utxorpc.v1beta.cardano.TxH\NULR\acardanoB\SO\n\
      \\fparsed_state"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        ref__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "ref"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"ref")) ::
              Data.ProtoLens.FieldDescriptor TxInMempool
        nativeBytes__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "native_bytes"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"nativeBytes")) ::
              Data.ProtoLens.FieldDescriptor TxInMempool
        stage__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "stage"
              (Data.ProtoLens.ScalarField Data.ProtoLens.EnumField ::
                 Data.ProtoLens.FieldTypeDescriptor Stage)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"stage")) ::
              Data.ProtoLens.FieldDescriptor TxInMempool
        cardano__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "cardano"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Utxorpc.V1beta.Cardano.Cardano.Tx)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'cardano")) ::
              Data.ProtoLens.FieldDescriptor TxInMempool
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, ref__field_descriptor),
           (Data.ProtoLens.Tag 2, nativeBytes__field_descriptor),
           (Data.ProtoLens.Tag 3, stage__field_descriptor),
           (Data.ProtoLens.Tag 4, cardano__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _TxInMempool'_unknownFields
        (\ x__ y__ -> x__ {_TxInMempool'_unknownFields = y__})
  defMessage
    = TxInMempool'_constructor
        {_TxInMempool'ref = Data.ProtoLens.fieldDefault,
         _TxInMempool'nativeBytes = Data.ProtoLens.fieldDefault,
         _TxInMempool'stage = Data.ProtoLens.fieldDefault,
         _TxInMempool'parsedState = Prelude.Nothing,
         _TxInMempool'_unknownFields = []}
  parseMessage
    = let
        loop ::
          TxInMempool -> Data.ProtoLens.Encoding.Bytes.Parser TxInMempool
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
                                           Data.ProtoLens.Encoding.Bytes.getBytes
                                             (Prelude.fromIntegral len))
                                       "native_bytes"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"nativeBytes") y x)
                        24
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.toEnum
                                          (Prelude.fmap
                                             Prelude.fromIntegral
                                             Data.ProtoLens.Encoding.Bytes.getVarInt))
                                       "stage"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"stage") y x)
                        34
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
          (do loop Data.ProtoLens.defMessage) "TxInMempool"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"ref") _x
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
                (let
                   _v
                     = Lens.Family2.view (Data.ProtoLens.Field.field @"nativeBytes") _x
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
                      _v = Lens.Family2.view (Data.ProtoLens.Field.field @"stage") _x
                    in
                      if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty
                      else
                          (Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 24)
                            ((Prelude..)
                               ((Prelude..)
                                  Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral)
                               Prelude.fromEnum _v))
                   ((Data.Monoid.<>)
                      (case
                           Lens.Family2.view
                             (Data.ProtoLens.Field.field @"maybe'parsedState") _x
                       of
                         Prelude.Nothing -> Data.Monoid.mempty
                         (Prelude.Just (TxInMempool'Cardano v))
                           -> (Data.Monoid.<>)
                                (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                                ((Prelude..)
                                   (\ bs
                                      -> (Data.Monoid.<>)
                                           (Data.ProtoLens.Encoding.Bytes.putVarInt
                                              (Prelude.fromIntegral (Data.ByteString.length bs)))
                                           (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                   Data.ProtoLens.encodeMessage v))
                      (Data.ProtoLens.Encoding.Wire.buildFieldSet
                         (Lens.Family2.view Data.ProtoLens.unknownFields _x)))))
instance Control.DeepSeq.NFData TxInMempool where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_TxInMempool'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_TxInMempool'ref x__)
                (Control.DeepSeq.deepseq
                   (_TxInMempool'nativeBytes x__)
                   (Control.DeepSeq.deepseq
                      (_TxInMempool'stage x__)
                      (Control.DeepSeq.deepseq (_TxInMempool'parsedState x__) ()))))
instance Control.DeepSeq.NFData TxInMempool'ParsedState where
  rnf (TxInMempool'Cardano x__) = Control.DeepSeq.rnf x__
_TxInMempool'Cardano ::
  Data.ProtoLens.Prism.Prism' TxInMempool'ParsedState Proto.Utxorpc.V1beta.Cardano.Cardano.Tx
_TxInMempool'Cardano
  = Data.ProtoLens.Prism.prism'
      TxInMempool'Cardano
      (\ p__
         -> case p__ of (TxInMempool'Cardano p__val) -> Prelude.Just p__val)
{- | Fields :
     
         * 'Proto.Utxorpc.V1beta.Submit.Submit_Fields.match' @:: Lens' TxPredicate AnyChainTxPattern@
         * 'Proto.Utxorpc.V1beta.Submit.Submit_Fields.maybe'match' @:: Lens' TxPredicate (Prelude.Maybe AnyChainTxPattern)@
         * 'Proto.Utxorpc.V1beta.Submit.Submit_Fields.not' @:: Lens' TxPredicate [TxPredicate]@
         * 'Proto.Utxorpc.V1beta.Submit.Submit_Fields.vec'not' @:: Lens' TxPredicate (Data.Vector.Vector TxPredicate)@
         * 'Proto.Utxorpc.V1beta.Submit.Submit_Fields.allOf' @:: Lens' TxPredicate [TxPredicate]@
         * 'Proto.Utxorpc.V1beta.Submit.Submit_Fields.vec'allOf' @:: Lens' TxPredicate (Data.Vector.Vector TxPredicate)@
         * 'Proto.Utxorpc.V1beta.Submit.Submit_Fields.anyOf' @:: Lens' TxPredicate [TxPredicate]@
         * 'Proto.Utxorpc.V1beta.Submit.Submit_Fields.vec'anyOf' @:: Lens' TxPredicate (Data.Vector.Vector TxPredicate)@ -}
data TxPredicate
  = TxPredicate'_constructor {_TxPredicate'match :: !(Prelude.Maybe AnyChainTxPattern),
                              _TxPredicate'not :: !(Data.Vector.Vector TxPredicate),
                              _TxPredicate'allOf :: !(Data.Vector.Vector TxPredicate),
                              _TxPredicate'anyOf :: !(Data.Vector.Vector TxPredicate),
                              _TxPredicate'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show TxPredicate where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField TxPredicate "match" AnyChainTxPattern where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TxPredicate'match (\ x__ y__ -> x__ {_TxPredicate'match = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField TxPredicate "maybe'match" (Prelude.Maybe AnyChainTxPattern) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TxPredicate'match (\ x__ y__ -> x__ {_TxPredicate'match = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField TxPredicate "not" [TxPredicate] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TxPredicate'not (\ x__ y__ -> x__ {_TxPredicate'not = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField TxPredicate "vec'not" (Data.Vector.Vector TxPredicate) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TxPredicate'not (\ x__ y__ -> x__ {_TxPredicate'not = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField TxPredicate "allOf" [TxPredicate] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TxPredicate'allOf (\ x__ y__ -> x__ {_TxPredicate'allOf = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField TxPredicate "vec'allOf" (Data.Vector.Vector TxPredicate) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TxPredicate'allOf (\ x__ y__ -> x__ {_TxPredicate'allOf = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField TxPredicate "anyOf" [TxPredicate] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TxPredicate'anyOf (\ x__ y__ -> x__ {_TxPredicate'anyOf = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField TxPredicate "vec'anyOf" (Data.Vector.Vector TxPredicate) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TxPredicate'anyOf (\ x__ y__ -> x__ {_TxPredicate'anyOf = y__}))
        Prelude.id
instance Data.ProtoLens.Message TxPredicate where
  messageName _ = Data.Text.pack "utxorpc.v1beta.submit.TxPredicate"
  packedMessageDescriptor _
    = "\n\
      \\vTxPredicate\DC2>\n\
      \\ENQmatch\CAN\SOH \SOH(\v2(.utxorpc.v1beta.submit.AnyChainTxPatternR\ENQmatch\DC24\n\
      \\ETXnot\CAN\STX \ETX(\v2\".utxorpc.v1beta.submit.TxPredicateR\ETXnot\DC29\n\
      \\ACKall_of\CAN\ETX \ETX(\v2\".utxorpc.v1beta.submit.TxPredicateR\ENQallOf\DC29\n\
      \\ACKany_of\CAN\EOT \ETX(\v2\".utxorpc.v1beta.submit.TxPredicateR\ENQanyOf"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        match__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "match"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor AnyChainTxPattern)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'match")) ::
              Data.ProtoLens.FieldDescriptor TxPredicate
        not__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "not"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor TxPredicate)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"not")) ::
              Data.ProtoLens.FieldDescriptor TxPredicate
        allOf__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "all_of"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor TxPredicate)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"allOf")) ::
              Data.ProtoLens.FieldDescriptor TxPredicate
        anyOf__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "any_of"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor TxPredicate)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"anyOf")) ::
              Data.ProtoLens.FieldDescriptor TxPredicate
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, match__field_descriptor),
           (Data.ProtoLens.Tag 2, not__field_descriptor),
           (Data.ProtoLens.Tag 3, allOf__field_descriptor),
           (Data.ProtoLens.Tag 4, anyOf__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _TxPredicate'_unknownFields
        (\ x__ y__ -> x__ {_TxPredicate'_unknownFields = y__})
  defMessage
    = TxPredicate'_constructor
        {_TxPredicate'match = Prelude.Nothing,
         _TxPredicate'not = Data.Vector.Generic.empty,
         _TxPredicate'allOf = Data.Vector.Generic.empty,
         _TxPredicate'anyOf = Data.Vector.Generic.empty,
         _TxPredicate'_unknownFields = []}
  parseMessage
    = let
        loop ::
          TxPredicate
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld TxPredicate
             -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld TxPredicate
                -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld TxPredicate
                   -> Data.ProtoLens.Encoding.Bytes.Parser TxPredicate
        loop x mutable'allOf mutable'anyOf mutable'not
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'allOf <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                        (Data.ProtoLens.Encoding.Growing.unsafeFreeze mutable'allOf)
                      frozen'anyOf <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                        (Data.ProtoLens.Encoding.Growing.unsafeFreeze mutable'anyOf)
                      frozen'not <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                      (Data.ProtoLens.Encoding.Growing.unsafeFreeze mutable'not)
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
                              (Data.ProtoLens.Field.field @"vec'allOf") frozen'allOf
                              (Lens.Family2.set
                                 (Data.ProtoLens.Field.field @"vec'anyOf") frozen'anyOf
                                 (Lens.Family2.set
                                    (Data.ProtoLens.Field.field @"vec'not") frozen'not x))))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "match"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"match") y x)
                                  mutable'allOf mutable'anyOf mutable'not
                        18
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "not"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'not y)
                                loop x mutable'allOf mutable'anyOf v
                        26
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "all_of"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'allOf y)
                                loop x v mutable'anyOf mutable'not
                        34
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "any_of"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'anyOf y)
                                loop x mutable'allOf v mutable'not
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'allOf mutable'anyOf mutable'not
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'allOf <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                 Data.ProtoLens.Encoding.Growing.new
              mutable'anyOf <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                 Data.ProtoLens.Encoding.Growing.new
              mutable'not <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                               Data.ProtoLens.Encoding.Growing.new
              loop
                Data.ProtoLens.defMessage mutable'allOf mutable'anyOf mutable'not)
          "TxPredicate"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'match") _x
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
                (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                   (\ _v
                      -> (Data.Monoid.<>)
                           (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                           ((Prelude..)
                              (\ bs
                                 -> (Data.Monoid.<>)
                                      (Data.ProtoLens.Encoding.Bytes.putVarInt
                                         (Prelude.fromIntegral (Data.ByteString.length bs)))
                                      (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                              Data.ProtoLens.encodeMessage _v))
                   (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'not") _x))
                ((Data.Monoid.<>)
                   (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                      (\ _v
                         -> (Data.Monoid.<>)
                              (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                              ((Prelude..)
                                 (\ bs
                                    -> (Data.Monoid.<>)
                                         (Data.ProtoLens.Encoding.Bytes.putVarInt
                                            (Prelude.fromIntegral (Data.ByteString.length bs)))
                                         (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                 Data.ProtoLens.encodeMessage _v))
                      (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'allOf") _x))
                   ((Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                         (\ _v
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                                 ((Prelude..)
                                    (\ bs
                                       -> (Data.Monoid.<>)
                                            (Data.ProtoLens.Encoding.Bytes.putVarInt
                                               (Prelude.fromIntegral (Data.ByteString.length bs)))
                                            (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                    Data.ProtoLens.encodeMessage _v))
                         (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'anyOf") _x))
                      (Data.ProtoLens.Encoding.Wire.buildFieldSet
                         (Lens.Family2.view Data.ProtoLens.unknownFields _x)))))
instance Control.DeepSeq.NFData TxPredicate where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_TxPredicate'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_TxPredicate'match x__)
                (Control.DeepSeq.deepseq
                   (_TxPredicate'not x__)
                   (Control.DeepSeq.deepseq
                      (_TxPredicate'allOf x__)
                      (Control.DeepSeq.deepseq (_TxPredicate'anyOf x__) ()))))
{- | Fields :
     
         * 'Proto.Utxorpc.V1beta.Submit.Submit_Fields.ref' @:: Lens' WaitForTxRequest [Data.ByteString.ByteString]@
         * 'Proto.Utxorpc.V1beta.Submit.Submit_Fields.vec'ref' @:: Lens' WaitForTxRequest (Data.Vector.Vector Data.ByteString.ByteString)@ -}
data WaitForTxRequest
  = WaitForTxRequest'_constructor {_WaitForTxRequest'ref :: !(Data.Vector.Vector Data.ByteString.ByteString),
                                   _WaitForTxRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show WaitForTxRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField WaitForTxRequest "ref" [Data.ByteString.ByteString] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _WaitForTxRequest'ref
           (\ x__ y__ -> x__ {_WaitForTxRequest'ref = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField WaitForTxRequest "vec'ref" (Data.Vector.Vector Data.ByteString.ByteString) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _WaitForTxRequest'ref
           (\ x__ y__ -> x__ {_WaitForTxRequest'ref = y__}))
        Prelude.id
instance Data.ProtoLens.Message WaitForTxRequest where
  messageName _
    = Data.Text.pack "utxorpc.v1beta.submit.WaitForTxRequest"
  packedMessageDescriptor _
    = "\n\
      \\DLEWaitForTxRequest\DC2\DLE\n\
      \\ETXref\CAN\SOH \ETX(\fR\ETXref"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        ref__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "ref"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"ref")) ::
              Data.ProtoLens.FieldDescriptor WaitForTxRequest
      in
        Data.Map.fromList [(Data.ProtoLens.Tag 1, ref__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _WaitForTxRequest'_unknownFields
        (\ x__ y__ -> x__ {_WaitForTxRequest'_unknownFields = y__})
  defMessage
    = WaitForTxRequest'_constructor
        {_WaitForTxRequest'ref = Data.Vector.Generic.empty,
         _WaitForTxRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          WaitForTxRequest
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Data.ByteString.ByteString
             -> Data.ProtoLens.Encoding.Bytes.Parser WaitForTxRequest
        loop x mutable'ref
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'ref <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                      (Data.ProtoLens.Encoding.Growing.unsafeFreeze mutable'ref)
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
                              (Data.ProtoLens.Field.field @"vec'ref") frozen'ref x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.getBytes
                                              (Prelude.fromIntegral len))
                                        "ref"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'ref y)
                                loop x v
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'ref
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'ref <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                               Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'ref)
          "WaitForTxRequest"
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
                (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'ref") _x))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData WaitForTxRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_WaitForTxRequest'_unknownFields x__)
             (Control.DeepSeq.deepseq (_WaitForTxRequest'ref x__) ())
{- | Fields :
     
         * 'Proto.Utxorpc.V1beta.Submit.Submit_Fields.ref' @:: Lens' WaitForTxResponse Data.ByteString.ByteString@
         * 'Proto.Utxorpc.V1beta.Submit.Submit_Fields.stage' @:: Lens' WaitForTxResponse Stage@ -}
data WaitForTxResponse
  = WaitForTxResponse'_constructor {_WaitForTxResponse'ref :: !Data.ByteString.ByteString,
                                    _WaitForTxResponse'stage :: !Stage,
                                    _WaitForTxResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show WaitForTxResponse where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField WaitForTxResponse "ref" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _WaitForTxResponse'ref
           (\ x__ y__ -> x__ {_WaitForTxResponse'ref = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField WaitForTxResponse "stage" Stage where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _WaitForTxResponse'stage
           (\ x__ y__ -> x__ {_WaitForTxResponse'stage = y__}))
        Prelude.id
instance Data.ProtoLens.Message WaitForTxResponse where
  messageName _
    = Data.Text.pack "utxorpc.v1beta.submit.WaitForTxResponse"
  packedMessageDescriptor _
    = "\n\
      \\DC1WaitForTxResponse\DC2\DLE\n\
      \\ETXref\CAN\SOH \SOH(\fR\ETXref\DC22\n\
      \\ENQstage\CAN\STX \SOH(\SO2\FS.utxorpc.v1beta.submit.StageR\ENQstage"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        ref__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "ref"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"ref")) ::
              Data.ProtoLens.FieldDescriptor WaitForTxResponse
        stage__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "stage"
              (Data.ProtoLens.ScalarField Data.ProtoLens.EnumField ::
                 Data.ProtoLens.FieldTypeDescriptor Stage)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"stage")) ::
              Data.ProtoLens.FieldDescriptor WaitForTxResponse
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, ref__field_descriptor),
           (Data.ProtoLens.Tag 2, stage__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _WaitForTxResponse'_unknownFields
        (\ x__ y__ -> x__ {_WaitForTxResponse'_unknownFields = y__})
  defMessage
    = WaitForTxResponse'_constructor
        {_WaitForTxResponse'ref = Data.ProtoLens.fieldDefault,
         _WaitForTxResponse'stage = Data.ProtoLens.fieldDefault,
         _WaitForTxResponse'_unknownFields = []}
  parseMessage
    = let
        loop ::
          WaitForTxResponse
          -> Data.ProtoLens.Encoding.Bytes.Parser WaitForTxResponse
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
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.toEnum
                                          (Prelude.fmap
                                             Prelude.fromIntegral
                                             Data.ProtoLens.Encoding.Bytes.getVarInt))
                                       "stage"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"stage") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "WaitForTxResponse"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"ref") _x
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
                (let
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"stage") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 16)
                         ((Prelude..)
                            ((Prelude..)
                               Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral)
                            Prelude.fromEnum _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData WaitForTxResponse where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_WaitForTxResponse'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_WaitForTxResponse'ref x__)
                (Control.DeepSeq.deepseq (_WaitForTxResponse'stage x__) ()))
{- | Fields :
     
         * 'Proto.Utxorpc.V1beta.Submit.Submit_Fields.predicate' @:: Lens' WatchMempoolRequest TxPredicate@
         * 'Proto.Utxorpc.V1beta.Submit.Submit_Fields.maybe'predicate' @:: Lens' WatchMempoolRequest (Prelude.Maybe TxPredicate)@
         * 'Proto.Utxorpc.V1beta.Submit.Submit_Fields.fieldMask' @:: Lens' WatchMempoolRequest Proto.Google.Protobuf.FieldMask.FieldMask@
         * 'Proto.Utxorpc.V1beta.Submit.Submit_Fields.maybe'fieldMask' @:: Lens' WatchMempoolRequest (Prelude.Maybe Proto.Google.Protobuf.FieldMask.FieldMask)@ -}
data WatchMempoolRequest
  = WatchMempoolRequest'_constructor {_WatchMempoolRequest'predicate :: !(Prelude.Maybe TxPredicate),
                                      _WatchMempoolRequest'fieldMask :: !(Prelude.Maybe Proto.Google.Protobuf.FieldMask.FieldMask),
                                      _WatchMempoolRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show WatchMempoolRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField WatchMempoolRequest "predicate" TxPredicate where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _WatchMempoolRequest'predicate
           (\ x__ y__ -> x__ {_WatchMempoolRequest'predicate = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField WatchMempoolRequest "maybe'predicate" (Prelude.Maybe TxPredicate) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _WatchMempoolRequest'predicate
           (\ x__ y__ -> x__ {_WatchMempoolRequest'predicate = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField WatchMempoolRequest "fieldMask" Proto.Google.Protobuf.FieldMask.FieldMask where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _WatchMempoolRequest'fieldMask
           (\ x__ y__ -> x__ {_WatchMempoolRequest'fieldMask = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField WatchMempoolRequest "maybe'fieldMask" (Prelude.Maybe Proto.Google.Protobuf.FieldMask.FieldMask) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _WatchMempoolRequest'fieldMask
           (\ x__ y__ -> x__ {_WatchMempoolRequest'fieldMask = y__}))
        Prelude.id
instance Data.ProtoLens.Message WatchMempoolRequest where
  messageName _
    = Data.Text.pack "utxorpc.v1beta.submit.WatchMempoolRequest"
  packedMessageDescriptor _
    = "\n\
      \\DC3WatchMempoolRequest\DC2@\n\
      \\tpredicate\CAN\SOH \SOH(\v2\".utxorpc.v1beta.submit.TxPredicateR\tpredicate\DC29\n\
      \\n\
      \field_mask\CAN\STX \SOH(\v2\SUB.google.protobuf.FieldMaskR\tfieldMask"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        predicate__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "predicate"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor TxPredicate)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'predicate")) ::
              Data.ProtoLens.FieldDescriptor WatchMempoolRequest
        fieldMask__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "field_mask"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Google.Protobuf.FieldMask.FieldMask)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'fieldMask")) ::
              Data.ProtoLens.FieldDescriptor WatchMempoolRequest
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, predicate__field_descriptor),
           (Data.ProtoLens.Tag 2, fieldMask__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _WatchMempoolRequest'_unknownFields
        (\ x__ y__ -> x__ {_WatchMempoolRequest'_unknownFields = y__})
  defMessage
    = WatchMempoolRequest'_constructor
        {_WatchMempoolRequest'predicate = Prelude.Nothing,
         _WatchMempoolRequest'fieldMask = Prelude.Nothing,
         _WatchMempoolRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          WatchMempoolRequest
          -> Data.ProtoLens.Encoding.Bytes.Parser WatchMempoolRequest
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
                                       "predicate"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"predicate") y x)
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
          (do loop Data.ProtoLens.defMessage) "WatchMempoolRequest"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view
                    (Data.ProtoLens.Field.field @"maybe'predicate") _x
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
instance Control.DeepSeq.NFData WatchMempoolRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_WatchMempoolRequest'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_WatchMempoolRequest'predicate x__)
                (Control.DeepSeq.deepseq (_WatchMempoolRequest'fieldMask x__) ()))
{- | Fields :
     
         * 'Proto.Utxorpc.V1beta.Submit.Submit_Fields.tx' @:: Lens' WatchMempoolResponse TxInMempool@
         * 'Proto.Utxorpc.V1beta.Submit.Submit_Fields.maybe'tx' @:: Lens' WatchMempoolResponse (Prelude.Maybe TxInMempool)@ -}
data WatchMempoolResponse
  = WatchMempoolResponse'_constructor {_WatchMempoolResponse'tx :: !(Prelude.Maybe TxInMempool),
                                       _WatchMempoolResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show WatchMempoolResponse where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField WatchMempoolResponse "tx" TxInMempool where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _WatchMempoolResponse'tx
           (\ x__ y__ -> x__ {_WatchMempoolResponse'tx = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField WatchMempoolResponse "maybe'tx" (Prelude.Maybe TxInMempool) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _WatchMempoolResponse'tx
           (\ x__ y__ -> x__ {_WatchMempoolResponse'tx = y__}))
        Prelude.id
instance Data.ProtoLens.Message WatchMempoolResponse where
  messageName _
    = Data.Text.pack "utxorpc.v1beta.submit.WatchMempoolResponse"
  packedMessageDescriptor _
    = "\n\
      \\DC4WatchMempoolResponse\DC22\n\
      \\STXtx\CAN\SOH \SOH(\v2\".utxorpc.v1beta.submit.TxInMempoolR\STXtx"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        tx__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "tx"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor TxInMempool)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'tx")) ::
              Data.ProtoLens.FieldDescriptor WatchMempoolResponse
      in Data.Map.fromList [(Data.ProtoLens.Tag 1, tx__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _WatchMempoolResponse'_unknownFields
        (\ x__ y__ -> x__ {_WatchMempoolResponse'_unknownFields = y__})
  defMessage
    = WatchMempoolResponse'_constructor
        {_WatchMempoolResponse'tx = Prelude.Nothing,
         _WatchMempoolResponse'_unknownFields = []}
  parseMessage
    = let
        loop ::
          WatchMempoolResponse
          -> Data.ProtoLens.Encoding.Bytes.Parser WatchMempoolResponse
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
                                       "tx"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"tx") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "WatchMempoolResponse"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'tx") _x
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
instance Control.DeepSeq.NFData WatchMempoolResponse where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_WatchMempoolResponse'_unknownFields x__)
             (Control.DeepSeq.deepseq (_WatchMempoolResponse'tx x__) ())
data SubmitService = SubmitService {}
instance Data.ProtoLens.Service.Types.Service SubmitService where
  type ServiceName SubmitService = "SubmitService"
  type ServicePackage SubmitService = "utxorpc.v1beta.submit"
  type ServiceMethods SubmitService = '["submitTx"]
  packedServiceDescriptor _
    = "\n\
      \\rSubmitService\DC2[\n\
      \\bSubmitTx\DC2&.utxorpc.v1beta.submit.SubmitTxRequest\SUB'.utxorpc.v1beta.submit.SubmitTxResponse"
instance Data.ProtoLens.Service.Types.HasMethodImpl SubmitService "submitTx" where
  type MethodName SubmitService "submitTx" = "SubmitTx"
  type MethodInput SubmitService "submitTx" = SubmitTxRequest
  type MethodOutput SubmitService "submitTx" = SubmitTxResponse
  type MethodStreamingType SubmitService "submitTx" = 'Data.ProtoLens.Service.Types.NonStreaming
packedFileDescriptor :: Data.ByteString.ByteString
packedFileDescriptor
  = "\n\
    \\"utxorpc/v1beta/submit/submit.proto\DC2\NAKutxorpc.v1beta.submit\SUB google/protobuf/field_mask.proto\SUB$utxorpc/v1beta/cardano/cardano.proto\"(\n\
    \\n\
    \AnyChainTx\DC2\DC2\n\
    \\ETXraw\CAN\SOH \SOH(\fH\NULR\ETXrawB\ACK\n\
    \\EOTtype\"B\n\
    \\rEvalTxRequest\DC21\n\
    \\STXtx\CAN\SOH \SOH(\v2!.utxorpc.v1beta.submit.AnyChainTxR\STXtx\"S\n\
    \\fAnyChainEval\DC2:\n\
    \\acardano\CAN\SOH \SOH(\v2\RS.utxorpc.v1beta.cardano.TxEvalH\NULR\acardanoB\a\n\
    \\ENQchain\"M\n\
    \\SOEvalTxResponse\DC2;\n\
    \\ACKreport\CAN\SOH \SOH(\v2#.utxorpc.v1beta.submit.AnyChainEvalR\ACKreport\"D\n\
    \\SISubmitTxRequest\DC21\n\
    \\STXtx\CAN\SOH \SOH(\v2!.utxorpc.v1beta.submit.AnyChainTxR\STXtx\"$\n\
    \\DLESubmitTxResponse\DC2\DLE\n\
    \\ETXref\CAN\SOH \SOH(\fR\ETXref\"\190\SOH\n\
    \\vTxInMempool\DC2\DLE\n\
    \\ETXref\CAN\SOH \SOH(\fR\ETXref\DC2!\n\
    \\fnative_bytes\CAN\STX \SOH(\fR\vnativeBytes\DC22\n\
    \\ENQstage\CAN\ETX \SOH(\SO2\FS.utxorpc.v1beta.submit.StageR\ENQstage\DC26\n\
    \\acardano\CAN\EOT \SOH(\v2\SUB.utxorpc.v1beta.cardano.TxH\NULR\acardanoB\SO\n\
    \\fparsed_state\"\DC4\n\
    \\DC2ReadMempoolRequest\"O\n\
    \\DC3ReadMempoolResponse\DC28\n\
    \\ENQitems\CAN\SOH \ETX(\v2\".utxorpc.v1beta.submit.TxInMempoolR\ENQitems\"$\n\
    \\DLEWaitForTxRequest\DC2\DLE\n\
    \\ETXref\CAN\SOH \ETX(\fR\ETXref\"Y\n\
    \\DC1WaitForTxResponse\DC2\DLE\n\
    \\ETXref\CAN\SOH \SOH(\fR\ETXref\DC22\n\
    \\ENQstage\CAN\STX \SOH(\SO2\FS.utxorpc.v1beta.submit.StageR\ENQstage\"[\n\
    \\DC1AnyChainTxPattern\DC2=\n\
    \\acardano\CAN\SOH \SOH(\v2!.utxorpc.v1beta.cardano.TxPatternH\NULR\acardanoB\a\n\
    \\ENQchain\"\249\SOH\n\
    \\vTxPredicate\DC2>\n\
    \\ENQmatch\CAN\SOH \SOH(\v2(.utxorpc.v1beta.submit.AnyChainTxPatternR\ENQmatch\DC24\n\
    \\ETXnot\CAN\STX \ETX(\v2\".utxorpc.v1beta.submit.TxPredicateR\ETXnot\DC29\n\
    \\ACKall_of\CAN\ETX \ETX(\v2\".utxorpc.v1beta.submit.TxPredicateR\ENQallOf\DC29\n\
    \\ACKany_of\CAN\EOT \ETX(\v2\".utxorpc.v1beta.submit.TxPredicateR\ENQanyOf\"\146\SOH\n\
    \\DC3WatchMempoolRequest\DC2@\n\
    \\tpredicate\CAN\SOH \SOH(\v2\".utxorpc.v1beta.submit.TxPredicateR\tpredicate\DC29\n\
    \\n\
    \field_mask\CAN\STX \SOH(\v2\SUB.google.protobuf.FieldMaskR\tfieldMask\"J\n\
    \\DC4WatchMempoolResponse\DC22\n\
    \\STXtx\CAN\SOH \SOH(\v2\".utxorpc.v1beta.submit.TxInMempoolR\STXtx*q\n\
    \\ENQStage\DC2\NAK\n\
    \\DC1STAGE_UNSPECIFIED\DLE\NUL\DC2\SYN\n\
    \\DC2STAGE_ACKNOWLEDGED\DLE\SOH\DC2\DC1\n\
    \\rSTAGE_MEMPOOL\DLE\STX\DC2\DC1\n\
    \\rSTAGE_NETWORK\DLE\ETX\DC2\DC3\n\
    \\SISTAGE_CONFIRMED\DLE\EOT2l\n\
    \\rSubmitService\DC2[\n\
    \\bSubmitTx\DC2&.utxorpc.v1beta.submit.SubmitTxRequest\SUB'.utxorpc.v1beta.submit.SubmitTxResponseB\158\SOH\n\
    \\EMcom.utxorpc.v1beta.submitB\vSubmitProtoP\SOH\162\STX\ETXUVS\170\STX\NAKUtxorpc.V1beta.Submit\202\STX\NAKUtxorpc\\V1beta\\Submit\226\STX!Utxorpc\\V1beta\\Submit\\GPBMetadata\234\STX\ETBUtxorpc::V1beta::SubmitJ\148!\n\
    \\ACK\DC2\EOT\NUL\NULk\SOH\n\
    \\b\n\
    \\SOH\f\DC2\ETX\NUL\NUL\DC2\n\
    \\b\n\
    \\SOH\STX\DC2\ETX\STX\NUL\RS\n\
    \\t\n\
    \\STX\ETX\NUL\DC2\ETX\EOT\NUL*\n\
    \\t\n\
    \\STX\ETX\SOH\DC2\ETX\ENQ\NUL.\n\
    \E\n\
    \\STX\EOT\NUL\DC2\EOT\b\NUL\f\SOH\SUB9 Represents a transaction from any supported blockchain.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\NUL\SOH\DC2\ETX\b\b\DC2\n\
    \\f\n\
    \\EOT\EOT\NUL\b\NUL\DC2\EOT\t\STX\v\ETX\n\
    \\f\n\
    \\ENQ\EOT\NUL\b\NUL\SOH\DC2\ETX\t\b\f\n\
    \$\n\
    \\EOT\EOT\NUL\STX\NUL\DC2\ETX\n\
    \\EOT\DC2\"\ETB Raw transaction data.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ENQ\DC2\ETX\n\
    \\EOT\t\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\SOH\DC2\ETX\n\
    \\n\
    \\r\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ETX\DC2\ETX\n\
    \\DLE\DC1\n\
    \A\n\
    \\STX\EOT\SOH\DC2\EOT\SI\NUL\DC1\SOH\SUB5 Request to evaluate transaction without submitting.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\SOH\SOH\DC2\ETX\SI\b\NAK\n\
    \)\n\
    \\EOT\EOT\SOH\STX\NUL\DC2\ETX\DLE\STX\DC4\"\FS A transaction to evaluate.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ACK\DC2\ETX\DLE\STX\f\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\SOH\DC2\ETX\DLE\r\SI\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ETX\DC2\ETX\DLE\DC2\DC3\n\
    \Q\n\
    \\STX\EOT\STX\DC2\EOT\DC4\NUL\CAN\SOH\SUBE Report containing the result of evaluating a particular transaction\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\STX\SOH\DC2\ETX\DC4\b\DC4\n\
    \\f\n\
    \\EOT\EOT\STX\b\NUL\DC2\EOT\NAK\STX\ETB\ETX\n\
    \\f\n\
    \\ENQ\EOT\STX\b\NUL\SOH\DC2\ETX\NAK\b\r\n\
    \.\n\
    \\EOT\EOT\STX\STX\NUL\DC2\ETX\SYN\EOT.\"! A Cardano tx evaluation report.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ACK\DC2\ETX\SYN\EOT!\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\SOH\DC2\ETX\SYN\")\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ETX\DC2\ETX\SYN,-\n\
    \N\n\
    \\STX\EOT\ETX\DC2\EOT\ESC\NUL\GS\SOH\SUBB Response containing the reports form the transaction evaluation.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\ETX\SOH\DC2\ETX\ESC\b\SYN\n\
    \\v\n\
    \\EOT\EOT\ETX\STX\NUL\DC2\ETX\FS\STX\SUB\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\ACK\DC2\ETX\FS\STX\SO\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\SOH\DC2\ETX\FS\SI\NAK\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\ETX\DC2\ETX\FS\CAN\EM\n\
    \@\n\
    \\STX\EOT\EOT\DC2\EOT \NUL\"\SOH\SUB4 Request to submit a transaction to the blockchain.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\EOT\SOH\DC2\ETX \b\ETB\n\
    \'\n\
    \\EOT\EOT\EOT\STX\NUL\DC2\ETX!\STX\DC4\"\SUB A transaction to submit.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\ACK\DC2\ETX!\STX\f\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\SOH\DC2\ETX!\r\SI\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\ETX\DC2\ETX!\DC2\DC3\n\
    \K\n\
    \\STX\EOT\ENQ\DC2\EOT%\NUL'\SOH\SUB? Response containing references to the submitted transactions.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\ENQ\SOH\DC2\ETX%\b\CAN\n\
    \K\n\
    \\EOT\EOT\ENQ\STX\NUL\DC2\ETX&\STX\DLE\"> A transaction reference returned upon successful submission.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\NUL\ENQ\DC2\ETX&\STX\a\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\NUL\SOH\DC2\ETX&\b\v\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\NUL\ETX\DC2\ETX&\SO\SI\n\
    \P\n\
    \\STX\ENQ\NUL\DC2\EOT*\NUL0\SOH\SUBD Enum representing the various stages of a transaction's lifecycle.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\ENQ\NUL\SOH\DC2\ETX*\ENQ\n\
    \\n\
    \!\n\
    \\EOT\ENQ\NUL\STX\NUL\DC2\ETX+\STX\CAN\"\DC4 Unspecified stage.\n\
    \\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\NUL\SOH\DC2\ETX+\STX\DC3\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\NUL\STX\DC2\ETX+\SYN\ETB\n\
    \=\n\
    \\EOT\ENQ\NUL\STX\SOH\DC2\ETX,\STX\EM\"0 Transaction has been acknowledged by the node.\n\
    \\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\SOH\SOH\DC2\ETX,\STX\DC4\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\SOH\STX\DC2\ETX,\ETB\CAN\n\
    \-\n\
    \\EOT\ENQ\NUL\STX\STX\DC2\ETX-\STX\DC4\"  Transaction is in the mempool.\n\
    \\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\STX\SOH\DC2\ETX-\STX\SI\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\STX\STX\DC2\ETX-\DC2\DC3\n\
    \B\n\
    \\EOT\ENQ\NUL\STX\ETX\DC2\ETX.\STX\DC4\"5 Transaction has been propagated across the network.\n\
    \\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\ETX\SOH\DC2\ETX.\STX\SI\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\ETX\STX\DC2\ETX.\DC2\DC3\n\
    \@\n\
    \\EOT\ENQ\NUL\STX\EOT\DC2\ETX/\STX\SYN\"3 Transaction has been confirmed on the blockchain.\n\
    \\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\EOT\SOH\DC2\ETX/\STX\DC1\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\EOT\STX\DC2\ETX/\DC4\NAK\n\
    \\n\
    \\n\
    \\STX\EOT\ACK\DC2\EOT2\NUL9\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\ACK\SOH\DC2\ETX2\b\DC3\n\
    \)\n\
    \\EOT\EOT\ACK\STX\NUL\DC2\ETX3\STX\DLE\"\FS The transaction reference.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\NUL\ENQ\DC2\ETX3\STX\a\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\NUL\SOH\DC2\ETX3\b\v\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\NUL\ETX\DC2\ETX3\SO\SI\n\
    \5\n\
    \\EOT\EOT\ACK\STX\SOH\DC2\ETX4\STX\EM\"( Original bytes as defined by the chain\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\SOH\ENQ\DC2\ETX4\STX\a\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\SOH\SOH\DC2\ETX4\b\DC4\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\SOH\ETX\DC2\ETX4\ETB\CAN\n\
    \*\n\
    \\EOT\EOT\ACK\STX\STX\DC2\ETX5\STX\DC2\"\GS The current stage of the tx\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\STX\ACK\DC2\ETX5\STX\a\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\STX\SOH\DC2\ETX5\b\r\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\STX\ETX\DC2\ETX5\DLE\DC1\n\
    \\f\n\
    \\EOT\EOT\ACK\b\NUL\DC2\EOT6\STX8\ETX\n\
    \\f\n\
    \\ENQ\EOT\ACK\b\NUL\SOH\DC2\ETX6\b\DC4\n\
    \%\n\
    \\EOT\EOT\ACK\STX\ETX\DC2\ETX7\EOT*\"\CAN A Cardano transaction.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\ETX\ACK\DC2\ETX7\EOT\GS\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\ETX\SOH\DC2\ETX7\RS%\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\ETX\ETX\DC2\ETX7()\n\
    \C\n\
    \\STX\EOT\a\DC2\ETX<\NUL\GS\SUB8 Request to check the status of submitted transactions.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\a\SOH\DC2\ETX<\b\SUB\n\
    \J\n\
    \\STX\EOT\b\DC2\EOT?\NULA\SOH\SUB> Response containing the stage of the submitted transactions.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\b\SOH\DC2\ETX?\b\ESC\n\
    \<\n\
    \\EOT\EOT\b\STX\NUL\DC2\ETX@\STX!\"/ List of transaction currently on the mempool.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\b\STX\NUL\EOT\DC2\ETX@\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\b\STX\NUL\ACK\DC2\ETX@\v\SYN\n\
    \\f\n\
    \\ENQ\EOT\b\STX\NUL\SOH\DC2\ETX@\ETB\FS\n\
    \\f\n\
    \\ENQ\EOT\b\STX\NUL\ETX\DC2\ETX@\US \n\
    \H\n\
    \\STX\EOT\t\DC2\EOTD\NULF\SOH\SUB< Request to wait for transactions to reach a certain stage.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\t\SOH\DC2\ETXD\b\CAN\n\
    \:\n\
    \\EOT\EOT\t\STX\NUL\DC2\ETXE\STX\EM\"- List of transaction references to wait for.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\t\STX\NUL\EOT\DC2\ETXE\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\t\STX\NUL\ENQ\DC2\ETXE\v\DLE\n\
    \\f\n\
    \\ENQ\EOT\t\STX\NUL\SOH\DC2\ETXE\DC1\DC4\n\
    \\f\n\
    \\ENQ\EOT\t\STX\NUL\ETX\DC2\ETXE\ETB\CAN\n\
    \_\n\
    \\STX\EOT\n\
    \\DC2\EOTI\NULL\SOH\SUBS Response containing the transaction reference and stage once it has been reached.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\n\
    \\SOH\DC2\ETXI\b\EM\n\
    \%\n\
    \\EOT\EOT\n\
    \\STX\NUL\DC2\ETXJ\STX\DLE\"\CAN Transaction reference.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\NUL\ENQ\DC2\ETXJ\STX\a\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\NUL\SOH\DC2\ETXJ\b\v\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\NUL\ETX\DC2\ETXJ\SO\SI\n\
    \0\n\
    \\EOT\EOT\n\
    \\STX\SOH\DC2\ETXK\STX\DC2\"# Stage reached by the transaction.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\SOH\ACK\DC2\ETXK\STX\a\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\SOH\SOH\DC2\ETXK\b\r\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\SOH\ETX\DC2\ETXK\DLE\DC1\n\
    \D\n\
    \\STX\EOT\v\DC2\EOTO\NULS\SOH\SUB8 Represents a tx pattern from any supported blockchain.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\v\SOH\DC2\ETXO\b\EM\n\
    \\f\n\
    \\EOT\EOT\v\b\NUL\DC2\EOTP\STXR\ETX\n\
    \\f\n\
    \\ENQ\EOT\v\b\NUL\SOH\DC2\ETXP\b\r\n\
    \$\n\
    \\EOT\EOT\v\STX\NUL\DC2\ETXQ\EOT1\"\ETB A Cardano tx pattern.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\v\STX\NUL\ACK\DC2\ETXQ\EOT$\n\
    \\f\n\
    \\ENQ\EOT\v\STX\NUL\SOH\DC2\ETXQ%,\n\
    \\f\n\
    \\ENQ\EOT\v\STX\NUL\ETX\DC2\ETXQ/0\n\
    \\\\n\
    \\STX\EOT\f\DC2\EOTV\NUL[\SOH\SUBP Represents a simple tx predicate that can composed to create more complex ones\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\f\SOH\DC2\ETXV\b\DC3\n\
    \8\n\
    \\EOT\EOT\f\STX\NUL\DC2\ETXW\STX\RS\"+ Predicate is true if tx exhibits pattern.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\f\STX\NUL\ACK\DC2\ETXW\STX\DC3\n\
    \\f\n\
    \\ENQ\EOT\f\STX\NUL\SOH\DC2\ETXW\DC4\EM\n\
    \\f\n\
    \\ENQ\EOT\f\STX\NUL\ETX\DC2\ETXW\FS\GS\n\
    \?\n\
    \\EOT\EOT\f\STX\SOH\DC2\ETXX\STX\US\"2 Predicate is true if tx doesn't exhibit pattern.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\f\STX\SOH\EOT\DC2\ETXX\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\f\STX\SOH\ACK\DC2\ETXX\v\SYN\n\
    \\f\n\
    \\ENQ\EOT\f\STX\SOH\SOH\DC2\ETXX\ETB\SUB\n\
    \\f\n\
    \\ENQ\EOT\f\STX\SOH\ETX\DC2\ETXX\GS\RS\n\
    \D\n\
    \\EOT\EOT\f\STX\STX\DC2\ETXY\STX\"\"7 Predicate is true if tx exhibits all of the patterns.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\f\STX\STX\EOT\DC2\ETXY\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\f\STX\STX\ACK\DC2\ETXY\v\SYN\n\
    \\f\n\
    \\ENQ\EOT\f\STX\STX\SOH\DC2\ETXY\ETB\GS\n\
    \\f\n\
    \\ENQ\EOT\f\STX\STX\ETX\DC2\ETXY !\n\
    \D\n\
    \\EOT\EOT\f\STX\ETX\DC2\ETXZ\STX\"\"7 Predicate is true if tx exhibits any of the patterns.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\f\STX\ETX\EOT\DC2\ETXZ\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\f\STX\ETX\ACK\DC2\ETXZ\v\SYN\n\
    \\f\n\
    \\ENQ\EOT\f\STX\ETX\SOH\DC2\ETXZ\ETB\GS\n\
    \\f\n\
    \\ENQ\EOT\f\STX\ETX\ETX\DC2\ETXZ !\n\
    \?\n\
    \\STX\EOT\r\DC2\EOT^\NULa\SOH\SUB3 Request to watch changes of specific mempool txs.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\r\SOH\DC2\ETX^\b\ESC\n\
    \5\n\
    \\EOT\EOT\r\STX\NUL\DC2\ETX_\STX\FS\"( A predicate to filter transactions by.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\r\STX\NUL\ACK\DC2\ETX_\STX\r\n\
    \\f\n\
    \\ENQ\EOT\r\STX\NUL\SOH\DC2\ETX_\SO\ETB\n\
    \\f\n\
    \\ENQ\EOT\r\STX\NUL\ETX\DC2\ETX_\SUB\ESC\n\
    \7\n\
    \\EOT\EOT\r\STX\SOH\DC2\ETX`\STX+\"* Field mask to selectively return fields.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\r\STX\SOH\ACK\DC2\ETX`\STX\ESC\n\
    \\f\n\
    \\ENQ\EOT\r\STX\SOH\SOH\DC2\ETX`\FS&\n\
    \\f\n\
    \\ENQ\EOT\r\STX\SOH\ETX\DC2\ETX`)*\n\
    \@\n\
    \\STX\EOT\SO\DC2\EOTd\NULf\SOH\SUB4 Response that represents a change in a mempool tx.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\SO\SOH\DC2\ETXd\b\FS\n\
    \?\n\
    \\EOT\EOT\SO\STX\NUL\DC2\ETXe\STX\NAK\"2 The content and stage of the tx that has changed\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\NUL\ACK\DC2\ETXe\STX\r\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\NUL\SOH\DC2\ETXe\SO\DLE\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\NUL\ETX\DC2\ETXe\DC3\DC4\n\
    \W\n\
    \\STX\ACK\NUL\DC2\EOTi\NULk\SOH\SUBK Service definition for submitting transactions and checking their status.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\ACK\NUL\SOH\DC2\ETXi\b\NAK\n\
    \5\n\
    \\EOT\ACK\NUL\STX\NUL\DC2\ETXj\STX;\"( Submit transactions to the blockchain.\n\
    \\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\SOH\DC2\ETXj\ACK\SO\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\STX\DC2\ETXj\SI\RS\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\ETX\DC2\ETXj)9b\ACKproto3"