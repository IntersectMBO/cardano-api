{- This file was auto-generated from utxorpc/v1alpha/query/query.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Utxorpc.V1alpha.Query.Query (
        QueryService(..), AnyChainParams(), AnyChainParams'Params(..),
        _AnyChainParams'Cardano, AnyUtxoData(),
        AnyUtxoData'ParsedState(..), _AnyUtxoData'Cardano, ChainPoint(),
        ReadParamsRequest(), ReadParamsResponse(), ReadUtxosRequest(),
        ReadUtxosRequest'QueryArgs(..), _ReadUtxosRequest'TxoRefs,
        _ReadUtxosRequest'CardanoAddresses, ReadUtxosResponse(), TxoRef(),
        TxoRefArray()
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
import qualified Proto.Utxorpc.V1alpha.Cardano.Cardano
{- | Fields :
     
         * 'Proto.Utxorpc.V1alpha.Query.Query_Fields.maybe'params' @:: Lens' AnyChainParams (Prelude.Maybe AnyChainParams'Params)@
         * 'Proto.Utxorpc.V1alpha.Query.Query_Fields.maybe'cardano' @:: Lens' AnyChainParams (Prelude.Maybe Proto.Utxorpc.V1alpha.Cardano.Cardano.PParams)@
         * 'Proto.Utxorpc.V1alpha.Query.Query_Fields.cardano' @:: Lens' AnyChainParams Proto.Utxorpc.V1alpha.Cardano.Cardano.PParams@ -}
data AnyChainParams
  = AnyChainParams'_constructor {_AnyChainParams'params :: !(Prelude.Maybe AnyChainParams'Params),
                                 _AnyChainParams'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show AnyChainParams where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
data AnyChainParams'Params
  = AnyChainParams'Cardano !Proto.Utxorpc.V1alpha.Cardano.Cardano.PParams
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.Field.HasField AnyChainParams "maybe'params" (Prelude.Maybe AnyChainParams'Params) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AnyChainParams'params
           (\ x__ y__ -> x__ {_AnyChainParams'params = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField AnyChainParams "maybe'cardano" (Prelude.Maybe Proto.Utxorpc.V1alpha.Cardano.Cardano.PParams) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AnyChainParams'params
           (\ x__ y__ -> x__ {_AnyChainParams'params = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (AnyChainParams'Cardano x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap AnyChainParams'Cardano y__))
instance Data.ProtoLens.Field.HasField AnyChainParams "cardano" Proto.Utxorpc.V1alpha.Cardano.Cardano.PParams where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AnyChainParams'params
           (\ x__ y__ -> x__ {_AnyChainParams'params = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (AnyChainParams'Cardano x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap AnyChainParams'Cardano y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Message AnyChainParams where
  messageName _
    = Data.Text.pack "utxorpc.v1alpha.query.AnyChainParams"
  packedMessageDescriptor _
    = "\n\
      \\SOAnyChainParams\DC2<\n\
      \\acardano\CAN\SOH \SOH(\v2 .utxorpc.v1alpha.cardano.PParamsH\NULR\acardanoB\b\n\
      \\ACKparams"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        cardano__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "cardano"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Utxorpc.V1alpha.Cardano.Cardano.PParams)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'cardano")) ::
              Data.ProtoLens.FieldDescriptor AnyChainParams
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, cardano__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _AnyChainParams'_unknownFields
        (\ x__ y__ -> x__ {_AnyChainParams'_unknownFields = y__})
  defMessage
    = AnyChainParams'_constructor
        {_AnyChainParams'params = Prelude.Nothing,
         _AnyChainParams'_unknownFields = []}
  parseMessage
    = let
        loop ::
          AnyChainParams
          -> Data.ProtoLens.Encoding.Bytes.Parser AnyChainParams
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
          (do loop Data.ProtoLens.defMessage) "AnyChainParams"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'params") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just (AnyChainParams'Cardano v))
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
instance Control.DeepSeq.NFData AnyChainParams where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_AnyChainParams'_unknownFields x__)
             (Control.DeepSeq.deepseq (_AnyChainParams'params x__) ())
instance Control.DeepSeq.NFData AnyChainParams'Params where
  rnf (AnyChainParams'Cardano x__) = Control.DeepSeq.rnf x__
_AnyChainParams'Cardano ::
  Data.ProtoLens.Prism.Prism' AnyChainParams'Params Proto.Utxorpc.V1alpha.Cardano.Cardano.PParams
_AnyChainParams'Cardano
  = Data.ProtoLens.Prism.prism'
      AnyChainParams'Cardano
      (\ p__
         -> case p__ of
              (AnyChainParams'Cardano p__val) -> Prelude.Just p__val)
{- | Fields :
     
         * 'Proto.Utxorpc.V1alpha.Query.Query_Fields.nativeBytes' @:: Lens' AnyUtxoData Data.ByteString.ByteString@
         * 'Proto.Utxorpc.V1alpha.Query.Query_Fields.txoRef' @:: Lens' AnyUtxoData TxoRef@
         * 'Proto.Utxorpc.V1alpha.Query.Query_Fields.maybe'txoRef' @:: Lens' AnyUtxoData (Prelude.Maybe TxoRef)@
         * 'Proto.Utxorpc.V1alpha.Query.Query_Fields.maybe'parsedState' @:: Lens' AnyUtxoData (Prelude.Maybe AnyUtxoData'ParsedState)@
         * 'Proto.Utxorpc.V1alpha.Query.Query_Fields.maybe'cardano' @:: Lens' AnyUtxoData (Prelude.Maybe Proto.Utxorpc.V1alpha.Cardano.Cardano.TxOutput)@
         * 'Proto.Utxorpc.V1alpha.Query.Query_Fields.cardano' @:: Lens' AnyUtxoData Proto.Utxorpc.V1alpha.Cardano.Cardano.TxOutput@ -}
data AnyUtxoData
  = AnyUtxoData'_constructor {_AnyUtxoData'nativeBytes :: !Data.ByteString.ByteString,
                              _AnyUtxoData'txoRef :: !(Prelude.Maybe TxoRef),
                              _AnyUtxoData'parsedState :: !(Prelude.Maybe AnyUtxoData'ParsedState),
                              _AnyUtxoData'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show AnyUtxoData where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
data AnyUtxoData'ParsedState
  = AnyUtxoData'Cardano !Proto.Utxorpc.V1alpha.Cardano.Cardano.TxOutput
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.Field.HasField AnyUtxoData "nativeBytes" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AnyUtxoData'nativeBytes
           (\ x__ y__ -> x__ {_AnyUtxoData'nativeBytes = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField AnyUtxoData "txoRef" TxoRef where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AnyUtxoData'txoRef (\ x__ y__ -> x__ {_AnyUtxoData'txoRef = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField AnyUtxoData "maybe'txoRef" (Prelude.Maybe TxoRef) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AnyUtxoData'txoRef (\ x__ y__ -> x__ {_AnyUtxoData'txoRef = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField AnyUtxoData "maybe'parsedState" (Prelude.Maybe AnyUtxoData'ParsedState) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AnyUtxoData'parsedState
           (\ x__ y__ -> x__ {_AnyUtxoData'parsedState = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField AnyUtxoData "maybe'cardano" (Prelude.Maybe Proto.Utxorpc.V1alpha.Cardano.Cardano.TxOutput) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AnyUtxoData'parsedState
           (\ x__ y__ -> x__ {_AnyUtxoData'parsedState = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (AnyUtxoData'Cardano x__val)) -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap AnyUtxoData'Cardano y__))
instance Data.ProtoLens.Field.HasField AnyUtxoData "cardano" Proto.Utxorpc.V1alpha.Cardano.Cardano.TxOutput where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AnyUtxoData'parsedState
           (\ x__ y__ -> x__ {_AnyUtxoData'parsedState = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (AnyUtxoData'Cardano x__val)) -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap AnyUtxoData'Cardano y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Message AnyUtxoData where
  messageName _ = Data.Text.pack "utxorpc.v1alpha.query.AnyUtxoData"
  packedMessageDescriptor _
    = "\n\
      \\vAnyUtxoData\DC2!\n\
      \\fnative_bytes\CAN\SOH \SOH(\fR\vnativeBytes\DC26\n\
      \\atxo_ref\CAN\STX \SOH(\v2\GS.utxorpc.v1alpha.query.TxoRefR\ACKtxoRef\DC2=\n\
      \\acardano\CAN\ETX \SOH(\v2!.utxorpc.v1alpha.cardano.TxOutputH\NULR\acardanoB\SO\n\
      \\fparsed_state"
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
              Data.ProtoLens.FieldDescriptor AnyUtxoData
        txoRef__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "txo_ref"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor TxoRef)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'txoRef")) ::
              Data.ProtoLens.FieldDescriptor AnyUtxoData
        cardano__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "cardano"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Utxorpc.V1alpha.Cardano.Cardano.TxOutput)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'cardano")) ::
              Data.ProtoLens.FieldDescriptor AnyUtxoData
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, nativeBytes__field_descriptor),
           (Data.ProtoLens.Tag 2, txoRef__field_descriptor),
           (Data.ProtoLens.Tag 3, cardano__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _AnyUtxoData'_unknownFields
        (\ x__ y__ -> x__ {_AnyUtxoData'_unknownFields = y__})
  defMessage
    = AnyUtxoData'_constructor
        {_AnyUtxoData'nativeBytes = Data.ProtoLens.fieldDefault,
         _AnyUtxoData'txoRef = Prelude.Nothing,
         _AnyUtxoData'parsedState = Prelude.Nothing,
         _AnyUtxoData'_unknownFields = []}
  parseMessage
    = let
        loop ::
          AnyUtxoData -> Data.ProtoLens.Encoding.Bytes.Parser AnyUtxoData
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
                                       "txo_ref"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"txoRef") y x)
                        26
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
          (do loop Data.ProtoLens.defMessage) "AnyUtxoData"
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
                     Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'txoRef") _x
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
                          (Data.ProtoLens.Field.field @"maybe'parsedState") _x
                    of
                      Prelude.Nothing -> Data.Monoid.mempty
                      (Prelude.Just (AnyUtxoData'Cardano v))
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
                      (Lens.Family2.view Data.ProtoLens.unknownFields _x))))
instance Control.DeepSeq.NFData AnyUtxoData where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_AnyUtxoData'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_AnyUtxoData'nativeBytes x__)
                (Control.DeepSeq.deepseq
                   (_AnyUtxoData'txoRef x__)
                   (Control.DeepSeq.deepseq (_AnyUtxoData'parsedState x__) ())))
instance Control.DeepSeq.NFData AnyUtxoData'ParsedState where
  rnf (AnyUtxoData'Cardano x__) = Control.DeepSeq.rnf x__
_AnyUtxoData'Cardano ::
  Data.ProtoLens.Prism.Prism' AnyUtxoData'ParsedState Proto.Utxorpc.V1alpha.Cardano.Cardano.TxOutput
_AnyUtxoData'Cardano
  = Data.ProtoLens.Prism.prism'
      AnyUtxoData'Cardano
      (\ p__
         -> case p__ of (AnyUtxoData'Cardano p__val) -> Prelude.Just p__val)
{- | Fields :
     
         * 'Proto.Utxorpc.V1alpha.Query.Query_Fields.slot' @:: Lens' ChainPoint Data.Word.Word64@
         * 'Proto.Utxorpc.V1alpha.Query.Query_Fields.hash' @:: Lens' ChainPoint Data.ByteString.ByteString@
         * 'Proto.Utxorpc.V1alpha.Query.Query_Fields.height' @:: Lens' ChainPoint Data.Word.Word64@
         * 'Proto.Utxorpc.V1alpha.Query.Query_Fields.timestamp' @:: Lens' ChainPoint Data.Word.Word64@ -}
data ChainPoint
  = ChainPoint'_constructor {_ChainPoint'slot :: !Data.Word.Word64,
                             _ChainPoint'hash :: !Data.ByteString.ByteString,
                             _ChainPoint'height :: !Data.Word.Word64,
                             _ChainPoint'timestamp :: !Data.Word.Word64,
                             _ChainPoint'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ChainPoint where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ChainPoint "slot" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ChainPoint'slot (\ x__ y__ -> x__ {_ChainPoint'slot = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ChainPoint "hash" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ChainPoint'hash (\ x__ y__ -> x__ {_ChainPoint'hash = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ChainPoint "height" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ChainPoint'height (\ x__ y__ -> x__ {_ChainPoint'height = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ChainPoint "timestamp" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ChainPoint'timestamp
           (\ x__ y__ -> x__ {_ChainPoint'timestamp = y__}))
        Prelude.id
instance Data.ProtoLens.Message ChainPoint where
  messageName _ = Data.Text.pack "utxorpc.v1alpha.query.ChainPoint"
  packedMessageDescriptor _
    = "\n\
      \\n\
      \ChainPoint\DC2\DC2\n\
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
              Data.ProtoLens.FieldDescriptor ChainPoint
        hash__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "hash"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"hash")) ::
              Data.ProtoLens.FieldDescriptor ChainPoint
        height__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "height"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"height")) ::
              Data.ProtoLens.FieldDescriptor ChainPoint
        timestamp__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "timestamp"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"timestamp")) ::
              Data.ProtoLens.FieldDescriptor ChainPoint
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, slot__field_descriptor),
           (Data.ProtoLens.Tag 2, hash__field_descriptor),
           (Data.ProtoLens.Tag 3, height__field_descriptor),
           (Data.ProtoLens.Tag 4, timestamp__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ChainPoint'_unknownFields
        (\ x__ y__ -> x__ {_ChainPoint'_unknownFields = y__})
  defMessage
    = ChainPoint'_constructor
        {_ChainPoint'slot = Data.ProtoLens.fieldDefault,
         _ChainPoint'hash = Data.ProtoLens.fieldDefault,
         _ChainPoint'height = Data.ProtoLens.fieldDefault,
         _ChainPoint'timestamp = Data.ProtoLens.fieldDefault,
         _ChainPoint'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ChainPoint -> Data.ProtoLens.Encoding.Bytes.Parser ChainPoint
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
          (do loop Data.ProtoLens.defMessage) "ChainPoint"
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
instance Control.DeepSeq.NFData ChainPoint where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ChainPoint'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ChainPoint'slot x__)
                (Control.DeepSeq.deepseq
                   (_ChainPoint'hash x__)
                   (Control.DeepSeq.deepseq
                      (_ChainPoint'height x__)
                      (Control.DeepSeq.deepseq (_ChainPoint'timestamp x__) ()))))
{- | Fields :
     
         * 'Proto.Utxorpc.V1alpha.Query.Query_Fields.fieldMask' @:: Lens' ReadParamsRequest Proto.Google.Protobuf.FieldMask.FieldMask@
         * 'Proto.Utxorpc.V1alpha.Query.Query_Fields.maybe'fieldMask' @:: Lens' ReadParamsRequest (Prelude.Maybe Proto.Google.Protobuf.FieldMask.FieldMask)@ -}
data ReadParamsRequest
  = ReadParamsRequest'_constructor {_ReadParamsRequest'fieldMask :: !(Prelude.Maybe Proto.Google.Protobuf.FieldMask.FieldMask),
                                    _ReadParamsRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ReadParamsRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ReadParamsRequest "fieldMask" Proto.Google.Protobuf.FieldMask.FieldMask where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReadParamsRequest'fieldMask
           (\ x__ y__ -> x__ {_ReadParamsRequest'fieldMask = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ReadParamsRequest "maybe'fieldMask" (Prelude.Maybe Proto.Google.Protobuf.FieldMask.FieldMask) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReadParamsRequest'fieldMask
           (\ x__ y__ -> x__ {_ReadParamsRequest'fieldMask = y__}))
        Prelude.id
instance Data.ProtoLens.Message ReadParamsRequest where
  messageName _
    = Data.Text.pack "utxorpc.v1alpha.query.ReadParamsRequest"
  packedMessageDescriptor _
    = "\n\
      \\DC1ReadParamsRequest\DC29\n\
      \\n\
      \field_mask\CAN\SOH \SOH(\v2\SUB.google.protobuf.FieldMaskR\tfieldMask"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        fieldMask__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "field_mask"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Google.Protobuf.FieldMask.FieldMask)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'fieldMask")) ::
              Data.ProtoLens.FieldDescriptor ReadParamsRequest
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, fieldMask__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ReadParamsRequest'_unknownFields
        (\ x__ y__ -> x__ {_ReadParamsRequest'_unknownFields = y__})
  defMessage
    = ReadParamsRequest'_constructor
        {_ReadParamsRequest'fieldMask = Prelude.Nothing,
         _ReadParamsRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ReadParamsRequest
          -> Data.ProtoLens.Encoding.Bytes.Parser ReadParamsRequest
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
          (do loop Data.ProtoLens.defMessage) "ReadParamsRequest"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view
                    (Data.ProtoLens.Field.field @"maybe'fieldMask") _x
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
instance Control.DeepSeq.NFData ReadParamsRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ReadParamsRequest'_unknownFields x__)
             (Control.DeepSeq.deepseq (_ReadParamsRequest'fieldMask x__) ())
{- | Fields :
     
         * 'Proto.Utxorpc.V1alpha.Query.Query_Fields.values' @:: Lens' ReadParamsResponse AnyChainParams@
         * 'Proto.Utxorpc.V1alpha.Query.Query_Fields.maybe'values' @:: Lens' ReadParamsResponse (Prelude.Maybe AnyChainParams)@
         * 'Proto.Utxorpc.V1alpha.Query.Query_Fields.ledgerTip' @:: Lens' ReadParamsResponse ChainPoint@
         * 'Proto.Utxorpc.V1alpha.Query.Query_Fields.maybe'ledgerTip' @:: Lens' ReadParamsResponse (Prelude.Maybe ChainPoint)@ -}
data ReadParamsResponse
  = ReadParamsResponse'_constructor {_ReadParamsResponse'values :: !(Prelude.Maybe AnyChainParams),
                                     _ReadParamsResponse'ledgerTip :: !(Prelude.Maybe ChainPoint),
                                     _ReadParamsResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ReadParamsResponse where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ReadParamsResponse "values" AnyChainParams where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReadParamsResponse'values
           (\ x__ y__ -> x__ {_ReadParamsResponse'values = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ReadParamsResponse "maybe'values" (Prelude.Maybe AnyChainParams) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReadParamsResponse'values
           (\ x__ y__ -> x__ {_ReadParamsResponse'values = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ReadParamsResponse "ledgerTip" ChainPoint where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReadParamsResponse'ledgerTip
           (\ x__ y__ -> x__ {_ReadParamsResponse'ledgerTip = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ReadParamsResponse "maybe'ledgerTip" (Prelude.Maybe ChainPoint) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReadParamsResponse'ledgerTip
           (\ x__ y__ -> x__ {_ReadParamsResponse'ledgerTip = y__}))
        Prelude.id
instance Data.ProtoLens.Message ReadParamsResponse where
  messageName _
    = Data.Text.pack "utxorpc.v1alpha.query.ReadParamsResponse"
  packedMessageDescriptor _
    = "\n\
      \\DC2ReadParamsResponse\DC2=\n\
      \\ACKvalues\CAN\SOH \SOH(\v2%.utxorpc.v1alpha.query.AnyChainParamsR\ACKvalues\DC2@\n\
      \\n\
      \ledger_tip\CAN\STX \SOH(\v2!.utxorpc.v1alpha.query.ChainPointR\tledgerTip"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        values__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "values"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor AnyChainParams)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'values")) ::
              Data.ProtoLens.FieldDescriptor ReadParamsResponse
        ledgerTip__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "ledger_tip"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ChainPoint)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'ledgerTip")) ::
              Data.ProtoLens.FieldDescriptor ReadParamsResponse
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, values__field_descriptor),
           (Data.ProtoLens.Tag 2, ledgerTip__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ReadParamsResponse'_unknownFields
        (\ x__ y__ -> x__ {_ReadParamsResponse'_unknownFields = y__})
  defMessage
    = ReadParamsResponse'_constructor
        {_ReadParamsResponse'values = Prelude.Nothing,
         _ReadParamsResponse'ledgerTip = Prelude.Nothing,
         _ReadParamsResponse'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ReadParamsResponse
          -> Data.ProtoLens.Encoding.Bytes.Parser ReadParamsResponse
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
                                       "values"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"values") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "ledger_tip"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"ledgerTip") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "ReadParamsResponse"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'values") _x
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
                       (Data.ProtoLens.Field.field @"maybe'ledgerTip") _x
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
instance Control.DeepSeq.NFData ReadParamsResponse where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ReadParamsResponse'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ReadParamsResponse'values x__)
                (Control.DeepSeq.deepseq (_ReadParamsResponse'ledgerTip x__) ()))
{- | Fields :
     
         * 'Proto.Utxorpc.V1alpha.Query.Query_Fields.fieldMask' @:: Lens' ReadUtxosRequest Proto.Google.Protobuf.FieldMask.FieldMask@
         * 'Proto.Utxorpc.V1alpha.Query.Query_Fields.maybe'fieldMask' @:: Lens' ReadUtxosRequest (Prelude.Maybe Proto.Google.Protobuf.FieldMask.FieldMask)@
         * 'Proto.Utxorpc.V1alpha.Query.Query_Fields.maybe'queryArgs' @:: Lens' ReadUtxosRequest (Prelude.Maybe ReadUtxosRequest'QueryArgs)@
         * 'Proto.Utxorpc.V1alpha.Query.Query_Fields.maybe'txoRefs' @:: Lens' ReadUtxosRequest (Prelude.Maybe TxoRefArray)@
         * 'Proto.Utxorpc.V1alpha.Query.Query_Fields.txoRefs' @:: Lens' ReadUtxosRequest TxoRefArray@
         * 'Proto.Utxorpc.V1alpha.Query.Query_Fields.maybe'cardanoAddresses' @:: Lens' ReadUtxosRequest (Prelude.Maybe Proto.Utxorpc.V1alpha.Cardano.Cardano.AddressArray)@
         * 'Proto.Utxorpc.V1alpha.Query.Query_Fields.cardanoAddresses' @:: Lens' ReadUtxosRequest Proto.Utxorpc.V1alpha.Cardano.Cardano.AddressArray@ -}
data ReadUtxosRequest
  = ReadUtxosRequest'_constructor {_ReadUtxosRequest'fieldMask :: !(Prelude.Maybe Proto.Google.Protobuf.FieldMask.FieldMask),
                                   _ReadUtxosRequest'queryArgs :: !(Prelude.Maybe ReadUtxosRequest'QueryArgs),
                                   _ReadUtxosRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ReadUtxosRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
data ReadUtxosRequest'QueryArgs
  = ReadUtxosRequest'TxoRefs !TxoRefArray |
    ReadUtxosRequest'CardanoAddresses !Proto.Utxorpc.V1alpha.Cardano.Cardano.AddressArray
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.Field.HasField ReadUtxosRequest "fieldMask" Proto.Google.Protobuf.FieldMask.FieldMask where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReadUtxosRequest'fieldMask
           (\ x__ y__ -> x__ {_ReadUtxosRequest'fieldMask = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ReadUtxosRequest "maybe'fieldMask" (Prelude.Maybe Proto.Google.Protobuf.FieldMask.FieldMask) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReadUtxosRequest'fieldMask
           (\ x__ y__ -> x__ {_ReadUtxosRequest'fieldMask = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ReadUtxosRequest "maybe'queryArgs" (Prelude.Maybe ReadUtxosRequest'QueryArgs) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReadUtxosRequest'queryArgs
           (\ x__ y__ -> x__ {_ReadUtxosRequest'queryArgs = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ReadUtxosRequest "maybe'txoRefs" (Prelude.Maybe TxoRefArray) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReadUtxosRequest'queryArgs
           (\ x__ y__ -> x__ {_ReadUtxosRequest'queryArgs = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (ReadUtxosRequest'TxoRefs x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap ReadUtxosRequest'TxoRefs y__))
instance Data.ProtoLens.Field.HasField ReadUtxosRequest "txoRefs" TxoRefArray where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReadUtxosRequest'queryArgs
           (\ x__ y__ -> x__ {_ReadUtxosRequest'queryArgs = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (ReadUtxosRequest'TxoRefs x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap ReadUtxosRequest'TxoRefs y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Field.HasField ReadUtxosRequest "maybe'cardanoAddresses" (Prelude.Maybe Proto.Utxorpc.V1alpha.Cardano.Cardano.AddressArray) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReadUtxosRequest'queryArgs
           (\ x__ y__ -> x__ {_ReadUtxosRequest'queryArgs = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (ReadUtxosRequest'CardanoAddresses x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap ReadUtxosRequest'CardanoAddresses y__))
instance Data.ProtoLens.Field.HasField ReadUtxosRequest "cardanoAddresses" Proto.Utxorpc.V1alpha.Cardano.Cardano.AddressArray where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReadUtxosRequest'queryArgs
           (\ x__ y__ -> x__ {_ReadUtxosRequest'queryArgs = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (ReadUtxosRequest'CardanoAddresses x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap ReadUtxosRequest'CardanoAddresses y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Message ReadUtxosRequest where
  messageName _
    = Data.Text.pack "utxorpc.v1alpha.query.ReadUtxosRequest"
  packedMessageDescriptor _
    = "\n\
      \\DLEReadUtxosRequest\DC2>\n\
      \\atxoRefs\CAN\SOH \SOH(\v2\".utxorpc.v1alpha.query.TxoRefArrayH\NULR\atxoRefs\DC2T\n\
      \\DC1cardano_addresses\CAN\STX \SOH(\v2%.utxorpc.v1alpha.cardano.AddressArrayH\NULR\DLEcardanoAddresses\DC29\n\
      \\n\
      \field_mask\CAN\ETX \SOH(\v2\SUB.google.protobuf.FieldMaskR\tfieldMaskB\f\n\
      \\n\
      \query_args"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        fieldMask__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "field_mask"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Google.Protobuf.FieldMask.FieldMask)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'fieldMask")) ::
              Data.ProtoLens.FieldDescriptor ReadUtxosRequest
        txoRefs__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "txoRefs"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor TxoRefArray)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'txoRefs")) ::
              Data.ProtoLens.FieldDescriptor ReadUtxosRequest
        cardanoAddresses__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "cardano_addresses"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Utxorpc.V1alpha.Cardano.Cardano.AddressArray)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'cardanoAddresses")) ::
              Data.ProtoLens.FieldDescriptor ReadUtxosRequest
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 3, fieldMask__field_descriptor),
           (Data.ProtoLens.Tag 1, txoRefs__field_descriptor),
           (Data.ProtoLens.Tag 2, cardanoAddresses__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ReadUtxosRequest'_unknownFields
        (\ x__ y__ -> x__ {_ReadUtxosRequest'_unknownFields = y__})
  defMessage
    = ReadUtxosRequest'_constructor
        {_ReadUtxosRequest'fieldMask = Prelude.Nothing,
         _ReadUtxosRequest'queryArgs = Prelude.Nothing,
         _ReadUtxosRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ReadUtxosRequest
          -> Data.ProtoLens.Encoding.Bytes.Parser ReadUtxosRequest
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
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "field_mask"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"fieldMask") y x)
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "txoRefs"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"txoRefs") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "cardano_addresses"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"cardanoAddresses") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "ReadUtxosRequest"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view
                    (Data.ProtoLens.Field.field @"maybe'fieldMask") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just _v)
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
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
                       (Data.ProtoLens.Field.field @"maybe'queryArgs") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just (ReadUtxosRequest'TxoRefs v))
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage v)
                   (Prelude.Just (ReadUtxosRequest'CardanoAddresses v))
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
instance Control.DeepSeq.NFData ReadUtxosRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ReadUtxosRequest'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ReadUtxosRequest'fieldMask x__)
                (Control.DeepSeq.deepseq (_ReadUtxosRequest'queryArgs x__) ()))
instance Control.DeepSeq.NFData ReadUtxosRequest'QueryArgs where
  rnf (ReadUtxosRequest'TxoRefs x__) = Control.DeepSeq.rnf x__
  rnf (ReadUtxosRequest'CardanoAddresses x__)
    = Control.DeepSeq.rnf x__
_ReadUtxosRequest'TxoRefs ::
  Data.ProtoLens.Prism.Prism' ReadUtxosRequest'QueryArgs TxoRefArray
_ReadUtxosRequest'TxoRefs
  = Data.ProtoLens.Prism.prism'
      ReadUtxosRequest'TxoRefs
      (\ p__
         -> case p__ of
              (ReadUtxosRequest'TxoRefs p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_ReadUtxosRequest'CardanoAddresses ::
  Data.ProtoLens.Prism.Prism' ReadUtxosRequest'QueryArgs Proto.Utxorpc.V1alpha.Cardano.Cardano.AddressArray
_ReadUtxosRequest'CardanoAddresses
  = Data.ProtoLens.Prism.prism'
      ReadUtxosRequest'CardanoAddresses
      (\ p__
         -> case p__ of
              (ReadUtxosRequest'CardanoAddresses p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
{- | Fields :
     
         * 'Proto.Utxorpc.V1alpha.Query.Query_Fields.items' @:: Lens' ReadUtxosResponse [AnyUtxoData]@
         * 'Proto.Utxorpc.V1alpha.Query.Query_Fields.vec'items' @:: Lens' ReadUtxosResponse (Data.Vector.Vector AnyUtxoData)@
         * 'Proto.Utxorpc.V1alpha.Query.Query_Fields.ledgerTip' @:: Lens' ReadUtxosResponse ChainPoint@
         * 'Proto.Utxorpc.V1alpha.Query.Query_Fields.maybe'ledgerTip' @:: Lens' ReadUtxosResponse (Prelude.Maybe ChainPoint)@ -}
data ReadUtxosResponse
  = ReadUtxosResponse'_constructor {_ReadUtxosResponse'items :: !(Data.Vector.Vector AnyUtxoData),
                                    _ReadUtxosResponse'ledgerTip :: !(Prelude.Maybe ChainPoint),
                                    _ReadUtxosResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ReadUtxosResponse where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ReadUtxosResponse "items" [AnyUtxoData] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReadUtxosResponse'items
           (\ x__ y__ -> x__ {_ReadUtxosResponse'items = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ReadUtxosResponse "vec'items" (Data.Vector.Vector AnyUtxoData) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReadUtxosResponse'items
           (\ x__ y__ -> x__ {_ReadUtxosResponse'items = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ReadUtxosResponse "ledgerTip" ChainPoint where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReadUtxosResponse'ledgerTip
           (\ x__ y__ -> x__ {_ReadUtxosResponse'ledgerTip = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ReadUtxosResponse "maybe'ledgerTip" (Prelude.Maybe ChainPoint) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReadUtxosResponse'ledgerTip
           (\ x__ y__ -> x__ {_ReadUtxosResponse'ledgerTip = y__}))
        Prelude.id
instance Data.ProtoLens.Message ReadUtxosResponse where
  messageName _
    = Data.Text.pack "utxorpc.v1alpha.query.ReadUtxosResponse"
  packedMessageDescriptor _
    = "\n\
      \\DC1ReadUtxosResponse\DC28\n\
      \\ENQitems\CAN\SOH \ETX(\v2\".utxorpc.v1alpha.query.AnyUtxoDataR\ENQitems\DC2@\n\
      \\n\
      \ledger_tip\CAN\STX \SOH(\v2!.utxorpc.v1alpha.query.ChainPointR\tledgerTip"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        items__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "items"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor AnyUtxoData)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"items")) ::
              Data.ProtoLens.FieldDescriptor ReadUtxosResponse
        ledgerTip__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "ledger_tip"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ChainPoint)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'ledgerTip")) ::
              Data.ProtoLens.FieldDescriptor ReadUtxosResponse
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, items__field_descriptor),
           (Data.ProtoLens.Tag 2, ledgerTip__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ReadUtxosResponse'_unknownFields
        (\ x__ y__ -> x__ {_ReadUtxosResponse'_unknownFields = y__})
  defMessage
    = ReadUtxosResponse'_constructor
        {_ReadUtxosResponse'items = Data.Vector.Generic.empty,
         _ReadUtxosResponse'ledgerTip = Prelude.Nothing,
         _ReadUtxosResponse'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ReadUtxosResponse
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld AnyUtxoData
             -> Data.ProtoLens.Encoding.Bytes.Parser ReadUtxosResponse
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
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "ledger_tip"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"ledgerTip") y x)
                                  mutable'items
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
          "ReadUtxosResponse"
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
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view
                       (Data.ProtoLens.Field.field @"maybe'ledgerTip") _x
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
instance Control.DeepSeq.NFData ReadUtxosResponse where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ReadUtxosResponse'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ReadUtxosResponse'items x__)
                (Control.DeepSeq.deepseq (_ReadUtxosResponse'ledgerTip x__) ()))
{- | Fields :
     
         * 'Proto.Utxorpc.V1alpha.Query.Query_Fields.hash' @:: Lens' TxoRef Data.ByteString.ByteString@
         * 'Proto.Utxorpc.V1alpha.Query.Query_Fields.index' @:: Lens' TxoRef Data.Word.Word32@ -}
data TxoRef
  = TxoRef'_constructor {_TxoRef'hash :: !Data.ByteString.ByteString,
                         _TxoRef'index :: !Data.Word.Word32,
                         _TxoRef'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show TxoRef where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField TxoRef "hash" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TxoRef'hash (\ x__ y__ -> x__ {_TxoRef'hash = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField TxoRef "index" Data.Word.Word32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TxoRef'index (\ x__ y__ -> x__ {_TxoRef'index = y__}))
        Prelude.id
instance Data.ProtoLens.Message TxoRef where
  messageName _ = Data.Text.pack "utxorpc.v1alpha.query.TxoRef"
  packedMessageDescriptor _
    = "\n\
      \\ACKTxoRef\DC2\DC2\n\
      \\EOThash\CAN\SOH \SOH(\fR\EOThash\DC2\DC4\n\
      \\ENQindex\CAN\STX \SOH(\rR\ENQindex"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        hash__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "hash"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"hash")) ::
              Data.ProtoLens.FieldDescriptor TxoRef
        index__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "index"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"index")) ::
              Data.ProtoLens.FieldDescriptor TxoRef
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, hash__field_descriptor),
           (Data.ProtoLens.Tag 2, index__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _TxoRef'_unknownFields
        (\ x__ y__ -> x__ {_TxoRef'_unknownFields = y__})
  defMessage
    = TxoRef'_constructor
        {_TxoRef'hash = Data.ProtoLens.fieldDefault,
         _TxoRef'index = Data.ProtoLens.fieldDefault,
         _TxoRef'_unknownFields = []}
  parseMessage
    = let
        loop :: TxoRef -> Data.ProtoLens.Encoding.Bytes.Parser TxoRef
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
                                       "hash"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"hash") y x)
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "index"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"index") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "TxoRef"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"hash") _x
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
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"index") _x
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
instance Control.DeepSeq.NFData TxoRef where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_TxoRef'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_TxoRef'hash x__)
                (Control.DeepSeq.deepseq (_TxoRef'index x__) ()))
{- | Fields :
     
         * 'Proto.Utxorpc.V1alpha.Query.Query_Fields.items' @:: Lens' TxoRefArray [TxoRef]@
         * 'Proto.Utxorpc.V1alpha.Query.Query_Fields.vec'items' @:: Lens' TxoRefArray (Data.Vector.Vector TxoRef)@ -}
data TxoRefArray
  = TxoRefArray'_constructor {_TxoRefArray'items :: !(Data.Vector.Vector TxoRef),
                              _TxoRefArray'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show TxoRefArray where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField TxoRefArray "items" [TxoRef] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TxoRefArray'items (\ x__ y__ -> x__ {_TxoRefArray'items = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField TxoRefArray "vec'items" (Data.Vector.Vector TxoRef) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TxoRefArray'items (\ x__ y__ -> x__ {_TxoRefArray'items = y__}))
        Prelude.id
instance Data.ProtoLens.Message TxoRefArray where
  messageName _ = Data.Text.pack "utxorpc.v1alpha.query.TxoRefArray"
  packedMessageDescriptor _
    = "\n\
      \\vTxoRefArray\DC23\n\
      \\ENQitems\CAN\SOH \ETX(\v2\GS.utxorpc.v1alpha.query.TxoRefR\ENQitems"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        items__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "items"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor TxoRef)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"items")) ::
              Data.ProtoLens.FieldDescriptor TxoRefArray
      in
        Data.Map.fromList [(Data.ProtoLens.Tag 1, items__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _TxoRefArray'_unknownFields
        (\ x__ y__ -> x__ {_TxoRefArray'_unknownFields = y__})
  defMessage
    = TxoRefArray'_constructor
        {_TxoRefArray'items = Data.Vector.Generic.empty,
         _TxoRefArray'_unknownFields = []}
  parseMessage
    = let
        loop ::
          TxoRefArray
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld TxoRef
             -> Data.ProtoLens.Encoding.Bytes.Parser TxoRefArray
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
          "TxoRefArray"
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
instance Control.DeepSeq.NFData TxoRefArray where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_TxoRefArray'_unknownFields x__)
             (Control.DeepSeq.deepseq (_TxoRefArray'items x__) ())
data QueryService = QueryService {}
instance Data.ProtoLens.Service.Types.Service QueryService where
  type ServiceName QueryService = "QueryService"
  type ServicePackage QueryService = "utxorpc.v1alpha.query"
  type ServiceMethods QueryService = '["readParams", "readUtxos"]
  packedServiceDescriptor _
    = "\n\
      \\fQueryService\DC2a\n\
      \\n\
      \ReadParams\DC2(.utxorpc.v1alpha.query.ReadParamsRequest\SUB).utxorpc.v1alpha.query.ReadParamsResponse\DC2^\n\
      \\tReadUtxos\DC2'.utxorpc.v1alpha.query.ReadUtxosRequest\SUB(.utxorpc.v1alpha.query.ReadUtxosResponse"
instance Data.ProtoLens.Service.Types.HasMethodImpl QueryService "readParams" where
  type MethodName QueryService "readParams" = "ReadParams"
  type MethodInput QueryService "readParams" = ReadParamsRequest
  type MethodOutput QueryService "readParams" = ReadParamsResponse
  type MethodStreamingType QueryService "readParams" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl QueryService "readUtxos" where
  type MethodName QueryService "readUtxos" = "ReadUtxos"
  type MethodInput QueryService "readUtxos" = ReadUtxosRequest
  type MethodOutput QueryService "readUtxos" = ReadUtxosResponse
  type MethodStreamingType QueryService "readUtxos" = 'Data.ProtoLens.Service.Types.NonStreaming
packedFileDescriptor :: Data.ByteString.ByteString
packedFileDescriptor
  = "\n\
    \!utxorpc/v1alpha/query/query.proto\DC2\NAKutxorpc.v1alpha.query\SUB google/protobuf/field_mask.proto\SUB%utxorpc/v1alpha/cardano/cardano.proto\"j\n\
    \\n\
    \ChainPoint\DC2\DC2\n\
    \\EOTslot\CAN\SOH \SOH(\EOTR\EOTslot\DC2\DC2\n\
    \\EOThash\CAN\STX \SOH(\fR\EOThash\DC2\SYN\n\
    \\ACKheight\CAN\ETX \SOH(\EOTR\ACKheight\DC2\FS\n\
    \\ttimestamp\CAN\EOT \SOH(\EOTR\ttimestamp\"2\n\
    \\ACKTxoRef\DC2\DC2\n\
    \\EOThash\CAN\SOH \SOH(\fR\EOThash\DC2\DC4\n\
    \\ENQindex\CAN\STX \SOH(\rR\ENQindex\"B\n\
    \\vTxoRefArray\DC23\n\
    \\ENQitems\CAN\SOH \ETX(\v2\GS.utxorpc.v1alpha.query.TxoRefR\ENQitems\"N\n\
    \\DC1ReadParamsRequest\DC29\n\
    \\n\
    \field_mask\CAN\SOH \SOH(\v2\SUB.google.protobuf.FieldMaskR\tfieldMask\"X\n\
    \\SOAnyChainParams\DC2<\n\
    \\acardano\CAN\SOH \SOH(\v2 .utxorpc.v1alpha.cardano.PParamsH\NULR\acardanoB\b\n\
    \\ACKparams\"\149\SOH\n\
    \\DC2ReadParamsResponse\DC2=\n\
    \\ACKvalues\CAN\SOH \SOH(\v2%.utxorpc.v1alpha.query.AnyChainParamsR\ACKvalues\DC2@\n\
    \\n\
    \ledger_tip\CAN\STX \SOH(\v2!.utxorpc.v1alpha.query.ChainPointR\tledgerTip\"\183\SOH\n\
    \\vAnyUtxoData\DC2!\n\
    \\fnative_bytes\CAN\SOH \SOH(\fR\vnativeBytes\DC26\n\
    \\atxo_ref\CAN\STX \SOH(\v2\GS.utxorpc.v1alpha.query.TxoRefR\ACKtxoRef\DC2=\n\
    \\acardano\CAN\ETX \SOH(\v2!.utxorpc.v1alpha.cardano.TxOutputH\NULR\acardanoB\SO\n\
    \\fparsed_state\"\241\SOH\n\
    \\DLEReadUtxosRequest\DC2>\n\
    \\atxoRefs\CAN\SOH \SOH(\v2\".utxorpc.v1alpha.query.TxoRefArrayH\NULR\atxoRefs\DC2T\n\
    \\DC1cardano_addresses\CAN\STX \SOH(\v2%.utxorpc.v1alpha.cardano.AddressArrayH\NULR\DLEcardanoAddresses\DC29\n\
    \\n\
    \field_mask\CAN\ETX \SOH(\v2\SUB.google.protobuf.FieldMaskR\tfieldMaskB\f\n\
    \\n\
    \query_args\"\143\SOH\n\
    \\DC1ReadUtxosResponse\DC28\n\
    \\ENQitems\CAN\SOH \ETX(\v2\".utxorpc.v1alpha.query.AnyUtxoDataR\ENQitems\DC2@\n\
    \\n\
    \ledger_tip\CAN\STX \SOH(\v2!.utxorpc.v1alpha.query.ChainPointR\tledgerTip2\209\SOH\n\
    \\fQueryService\DC2a\n\
    \\n\
    \ReadParams\DC2(.utxorpc.v1alpha.query.ReadParamsRequest\SUB).utxorpc.v1alpha.query.ReadParamsResponse\DC2^\n\
    \\tReadUtxos\DC2'.utxorpc.v1alpha.query.ReadUtxosRequest\SUB(.utxorpc.v1alpha.query.ReadUtxosResponseB\157\SOH\n\
    \\EMcom.utxorpc.v1alpha.queryB\n\
    \QueryProtoP\SOH\162\STX\ETXUVQ\170\STX\NAKUtxorpc.V1alpha.Query\202\STX\NAKUtxorpc\\V1alpha\\Query\226\STX!Utxorpc\\V1alpha\\Query\\GPBMetadata\234\STX\ETBUtxorpc::V1alpha::QueryJ\173\SYN\n\
    \\ACK\DC2\EOT\STX\NULK\SOH\n\
    \9\n\
    \\SOH\f\DC2\ETX\STX\NUL\DC22// A consistent view of the state of the ledger\n\
    \\n\
    \\b\n\
    \\SOH\STX\DC2\ETX\EOT\NUL\RS\n\
    \\t\n\
    \\STX\ETX\NUL\DC2\ETX\ACK\NUL*\n\
    \\t\n\
    \\STX\ETX\SOH\DC2\ETX\a\NUL/\n\
    \<\n\
    \\STX\EOT\NUL\DC2\EOT\n\
    \\NUL\SI\SOH\SUB0 Represents a specific point in the blockchain.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\NUL\SOH\DC2\ETX\n\
    \\b\DC2\n\
    \\ESC\n\
    \\EOT\EOT\NUL\STX\NUL\DC2\ETX\v\STX\DC2\"\SO Slot number.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ENQ\DC2\ETX\v\STX\b\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\SOH\DC2\ETX\v\t\r\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ETX\DC2\ETX\v\DLE\DC1\n\
    \\SUB\n\
    \\EOT\EOT\NUL\STX\SOH\DC2\ETX\f\STX\DC1\"\r Block hash.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\ENQ\DC2\ETX\f\STX\a\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\SOH\DC2\ETX\f\b\f\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\ETX\DC2\ETX\f\SI\DLE\n\
    \\FS\n\
    \\EOT\EOT\NUL\STX\STX\DC2\ETX\r\STX\DC4\"\SI Block height.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\STX\ENQ\DC2\ETX\r\STX\b\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\STX\SOH\DC2\ETX\r\t\SI\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\STX\ETX\DC2\ETX\r\DC2\DC3\n\
    \!\n\
    \\EOT\EOT\NUL\STX\ETX\DC2\ETX\SO\STX\ETB\"\DC4 Block ms timestamp\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ETX\ENQ\DC2\ETX\SO\STX\b\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ETX\SOH\DC2\ETX\SO\t\DC2\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ETX\ETX\DC2\ETX\SO\NAK\SYN\n\
    \<\n\
    \\STX\EOT\SOH\DC2\EOT\DC2\NUL\NAK\SOH\SUB0 Represents a reference to a transaction output\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\SOH\SOH\DC2\ETX\DC2\b\SO\n\
    \\ETB\n\
    \\EOT\EOT\SOH\STX\NUL\DC2\ETX\DC3\STX\DC1\"\n\
    \ Tx hash.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ENQ\DC2\ETX\DC3\STX\a\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\SOH\DC2\ETX\DC3\b\f\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ETX\DC2\ETX\DC3\SI\DLE\n\
    \\FS\n\
    \\EOT\EOT\SOH\STX\SOH\DC2\ETX\DC4\STX\DC3\"\SI Output index.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\ENQ\DC2\ETX\DC4\STX\b\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\SOH\DC2\ETX\DC4\t\SO\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\ETX\DC2\ETX\DC4\DC1\DC2\n\
    \\n\
    \\n\
    \\STX\EOT\STX\DC2\EOT\ETB\NUL\SUB\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\STX\SOH\DC2\ETX\ETB\b\DC3\n\
    \W\n\
    \\EOT\EOT\STX\STX\NUL\DC2\ETX\EM\STX\FS\SUBJ TODO u5c: changed to repeated - https://github.com/utxorpc/spec/pull/167\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\EOT\DC2\ETX\EM\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ACK\DC2\ETX\EM\v\DC1\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\SOH\DC2\ETX\EM\DC2\ETB\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ETX\DC2\ETX\EM\SUB\ESC\n\
    \1\n\
    \\STX\EOT\ETX\DC2\EOT\GS\NUL\US\SOH\SUB% Request to get the chain parameters\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\ETX\SOH\DC2\ETX\GS\b\EM\n\
    \N\n\
    \\EOT\EOT\ETX\STX\NUL\DC2\ETX\RS\STX+\"A Field mask to selectively return fields in the parsed response.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\ACK\DC2\ETX\RS\STX\ESC\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\SOH\DC2\ETX\RS\FS&\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\ETX\DC2\ETX\RS)*\n\
    \U\n\
    \\STX\EOT\EOT\DC2\EOT\"\NUL&\SOH\SUBI An evenlope that holds parameter data from any of the compatible chains\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\EOT\SOH\DC2\ETX\"\b\SYN\n\
    \\f\n\
    \\EOT\EOT\EOT\b\NUL\DC2\EOT#\STX%\ETX\n\
    \\f\n\
    \\ENQ\EOT\EOT\b\NUL\SOH\DC2\ETX#\b\SO\n\
    \!\n\
    \\EOT\EOT\EOT\STX\NUL\DC2\ETX$\EOT0\"\DC4 Cardano parameters\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\ACK\DC2\ETX$\EOT#\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\SOH\DC2\ETX$$+\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\ETX\DC2\ETX$./\n\
    \6\n\
    \\STX\EOT\ENQ\DC2\EOT)\NUL,\SOH\SUB* Response containing the chain parameters\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\ENQ\SOH\DC2\ETX)\b\SUB\n\
    \+\n\
    \\EOT\EOT\ENQ\STX\NUL\DC2\ETX*\STX\FS\"\RS The value of the parameters.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\NUL\ACK\DC2\ETX*\STX\DLE\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\NUL\SOH\DC2\ETX*\DC1\ETB\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\NUL\ETX\DC2\ETX*\SUB\ESC\n\
    \J\n\
    \\EOT\EOT\ENQ\STX\SOH\DC2\ETX+\STX\FS\"= The chain point that represent the ledger current position.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\SOH\ACK\DC2\ETX+\STX\f\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\SOH\SOH\DC2\ETX+\r\ETB\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\SOH\ETX\DC2\ETX+\SUB\ESC\n\
    \J\n\
    \\STX\EOT\ACK\DC2\EOT/\NUL5\SOH\SUB> An evenlope that holds an UTxO from any of compatible chains\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\ACK\SOH\DC2\ETX/\b\DC3\n\
    \5\n\
    \\EOT\EOT\ACK\STX\NUL\DC2\ETX0\STX\EM\"( Original bytes as defined by the chain\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\NUL\ENQ\DC2\ETX0\STX\a\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\NUL\SOH\DC2\ETX0\b\DC4\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\NUL\ETX\DC2\ETX0\ETB\CAN\n\
    \0\n\
    \\EOT\EOT\ACK\STX\SOH\DC2\ETX1\STX\NAK\"# Hash of the previous transaction.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\SOH\ACK\DC2\ETX1\STX\b\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\SOH\SOH\DC2\ETX1\t\DLE\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\SOH\ETX\DC2\ETX1\DC3\DC4\n\
    \\f\n\
    \\EOT\EOT\ACK\b\NUL\DC2\EOT2\STX4\ETX\n\
    \\f\n\
    \\ENQ\EOT\ACK\b\NUL\SOH\DC2\ETX2\b\DC4\n\
    \\GS\n\
    \\EOT\EOT\ACK\STX\STX\DC2\ETX3\EOT1\"\DLE A cardano UTxO\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\STX\ACK\DC2\ETX3\EOT$\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\STX\SOH\DC2\ETX3%,\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\STX\ETX\DC2\ETX3/0\n\
    \+\n\
    \\STX\EOT\a\DC2\EOT8\NUL?\SOH\SUB\US Request to get specific UTxOs\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\a\SOH\DC2\ETX8\b\CAN\n\
    \N\n\
    \\EOT\EOT\a\b\NUL\DC2\EOT:\STX=\ETX\SUB@ TODO u5c: new oneof - https://github.com/utxorpc/spec/pull/167\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\a\b\NUL\SOH\DC2\ETX:\b\DC2\n\
    \,\n\
    \\EOT\EOT\a\STX\NUL\DC2\ETX;\EOT\FS\"\US Array of Tx Output references\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\a\STX\NUL\ACK\DC2\ETX;\EOT\SI\n\
    \\f\n\
    \\ENQ\EOT\a\STX\NUL\SOH\DC2\ETX;\DLE\ETB\n\
    \\f\n\
    \\ENQ\EOT\a\STX\NUL\ETX\DC2\ETX;\SUB\ESC\n\
    \!\n\
    \\EOT\EOT\a\STX\SOH\DC2\ETX<\EOT?\"\DC4 Array of addresses\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\a\STX\SOH\ACK\DC2\ETX<\EOT(\n\
    \\f\n\
    \\ENQ\EOT\a\STX\SOH\SOH\DC2\ETX<):\n\
    \\f\n\
    \\ENQ\EOT\a\STX\SOH\ETX\DC2\ETX<=>\n\
    \7\n\
    \\EOT\EOT\a\STX\STX\DC2\ETX>\STX+\"* Field mask to selectively return fields.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\a\STX\STX\ACK\DC2\ETX>\STX\ESC\n\
    \\f\n\
    \\ENQ\EOT\a\STX\STX\SOH\DC2\ETX>\FS&\n\
    \\f\n\
    \\ENQ\EOT\a\STX\STX\ETX\DC2\ETX>)*\n\
    \T\n\
    \\STX\EOT\b\DC2\EOTB\NULE\SOH\SUBH Response containing the UTxOs associated with the requested addresses.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\b\SOH\DC2\ETXB\b\EM\n\
    \\GS\n\
    \\EOT\EOT\b\STX\NUL\DC2\ETXC\STX!\"\DLE List of UTxOs.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\b\STX\NUL\EOT\DC2\ETXC\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\b\STX\NUL\ACK\DC2\ETXC\v\SYN\n\
    \\f\n\
    \\ENQ\EOT\b\STX\NUL\SOH\DC2\ETXC\ETB\FS\n\
    \\f\n\
    \\ENQ\EOT\b\STX\NUL\ETX\DC2\ETXC\US \n\
    \J\n\
    \\EOT\EOT\b\STX\SOH\DC2\ETXD\STX\FS\"= The chain point that represent the ledger current position.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\b\STX\SOH\ACK\DC2\ETXD\STX\f\n\
    \\f\n\
    \\ENQ\EOT\b\STX\SOH\SOH\DC2\ETXD\r\ETB\n\
    \\f\n\
    \\ENQ\EOT\b\STX\SOH\ETX\DC2\ETXD\SUB\ESC\n\
    \E\n\
    \\STX\ACK\NUL\DC2\EOTH\NULK\SOH\SUB9 Service definition for querying the state of the chain.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\ACK\NUL\SOH\DC2\ETXH\b\DC4\n\
    \'\n\
    \\EOT\ACK\NUL\STX\NUL\DC2\ETXI\STXA\"\SUB Get overall chain state.\n\
    \\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\SOH\DC2\ETXI\ACK\DLE\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\STX\DC2\ETXI\DC1\"\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\ETX\DC2\ETXI-?\n\
    \0\n\
    \\EOT\ACK\NUL\STX\SOH\DC2\ETXJ\STX>\"# Read specific UTxOs by reference.\n\
    \\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\SOH\SOH\DC2\ETXJ\ACK\SI\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\SOH\STX\DC2\ETXJ\DLE \n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\SOH\ETX\DC2\ETXJ+<b\ACKproto3"