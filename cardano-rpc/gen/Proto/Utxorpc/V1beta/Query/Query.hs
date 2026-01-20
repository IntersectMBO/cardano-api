{- This file was auto-generated from utxorpc/v1beta/query/query.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Utxorpc.V1beta.Query.Query (
        QueryService(..), AnyChainBlock(), AnyChainBlock'Chain(..),
        _AnyChainBlock'Cardano, AnyChainDatum(),
        AnyChainDatum'ParsedState(..), _AnyChainDatum'Cardano,
        AnyChainParams(), AnyChainParams'Params(..),
        _AnyChainParams'Cardano, AnyChainTx(), AnyChainTx'Chain(..),
        _AnyChainTx'Cardano, AnyUtxoData(), AnyUtxoData'ParsedState(..),
        _AnyUtxoData'Cardano, AnyUtxoPattern(),
        AnyUtxoPattern'UtxoPattern(..), _AnyUtxoPattern'Cardano,
        ChainPoint(), ReadDataRequest(), ReadDataResponse(),
        ReadEraSummaryRequest(), ReadEraSummaryResponse(),
        ReadEraSummaryResponse'Summary(..),
        _ReadEraSummaryResponse'Cardano, ReadGenesisRequest(),
        ReadGenesisResponse(), ReadGenesisResponse'Config(..),
        _ReadGenesisResponse'Cardano, ReadParamsRequest(),
        ReadParamsResponse(), ReadTxRequest(), ReadTxResponse(),
        ReadUtxosRequest(), ReadUtxosResponse(), SearchUtxosRequest(),
        SearchUtxosResponse(), TxoRef(), UtxoPredicate()
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
     
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.nativeBytes' @:: Lens' AnyChainBlock Data.ByteString.ByteString@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.maybe'chain' @:: Lens' AnyChainBlock (Prelude.Maybe AnyChainBlock'Chain)@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.maybe'cardano' @:: Lens' AnyChainBlock (Prelude.Maybe Proto.Utxorpc.V1beta.Cardano.Cardano.Block)@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.cardano' @:: Lens' AnyChainBlock Proto.Utxorpc.V1beta.Cardano.Cardano.Block@ -}
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
  messageName _ = Data.Text.pack "utxorpc.v1beta.query.AnyChainBlock"
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
     
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.nativeBytes' @:: Lens' AnyChainDatum Data.ByteString.ByteString@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.key' @:: Lens' AnyChainDatum Data.ByteString.ByteString@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.maybe'parsedState' @:: Lens' AnyChainDatum (Prelude.Maybe AnyChainDatum'ParsedState)@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.maybe'cardano' @:: Lens' AnyChainDatum (Prelude.Maybe Proto.Utxorpc.V1beta.Cardano.Cardano.PlutusData)@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.cardano' @:: Lens' AnyChainDatum Proto.Utxorpc.V1beta.Cardano.Cardano.PlutusData@ -}
data AnyChainDatum
  = AnyChainDatum'_constructor {_AnyChainDatum'nativeBytes :: !Data.ByteString.ByteString,
                                _AnyChainDatum'key :: !Data.ByteString.ByteString,
                                _AnyChainDatum'parsedState :: !(Prelude.Maybe AnyChainDatum'ParsedState),
                                _AnyChainDatum'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show AnyChainDatum where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
data AnyChainDatum'ParsedState
  = AnyChainDatum'Cardano !Proto.Utxorpc.V1beta.Cardano.Cardano.PlutusData
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.Field.HasField AnyChainDatum "nativeBytes" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AnyChainDatum'nativeBytes
           (\ x__ y__ -> x__ {_AnyChainDatum'nativeBytes = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField AnyChainDatum "key" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AnyChainDatum'key (\ x__ y__ -> x__ {_AnyChainDatum'key = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField AnyChainDatum "maybe'parsedState" (Prelude.Maybe AnyChainDatum'ParsedState) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AnyChainDatum'parsedState
           (\ x__ y__ -> x__ {_AnyChainDatum'parsedState = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField AnyChainDatum "maybe'cardano" (Prelude.Maybe Proto.Utxorpc.V1beta.Cardano.Cardano.PlutusData) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AnyChainDatum'parsedState
           (\ x__ y__ -> x__ {_AnyChainDatum'parsedState = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (AnyChainDatum'Cardano x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap AnyChainDatum'Cardano y__))
instance Data.ProtoLens.Field.HasField AnyChainDatum "cardano" Proto.Utxorpc.V1beta.Cardano.Cardano.PlutusData where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AnyChainDatum'parsedState
           (\ x__ y__ -> x__ {_AnyChainDatum'parsedState = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (AnyChainDatum'Cardano x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap AnyChainDatum'Cardano y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Message AnyChainDatum where
  messageName _ = Data.Text.pack "utxorpc.v1beta.query.AnyChainDatum"
  packedMessageDescriptor _
    = "\n\
      \\rAnyChainDatum\DC2!\n\
      \\fnative_bytes\CAN\SOH \SOH(\fR\vnativeBytes\DC2\DLE\n\
      \\ETXkey\CAN\STX \SOH(\fR\ETXkey\DC2>\n\
      \\acardano\CAN\ETX \SOH(\v2\".utxorpc.v1beta.cardano.PlutusDataH\NULR\acardanoB\SO\n\
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
              Data.ProtoLens.FieldDescriptor AnyChainDatum
        key__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "key"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"key")) ::
              Data.ProtoLens.FieldDescriptor AnyChainDatum
        cardano__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "cardano"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Utxorpc.V1beta.Cardano.Cardano.PlutusData)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'cardano")) ::
              Data.ProtoLens.FieldDescriptor AnyChainDatum
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, nativeBytes__field_descriptor),
           (Data.ProtoLens.Tag 2, key__field_descriptor),
           (Data.ProtoLens.Tag 3, cardano__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _AnyChainDatum'_unknownFields
        (\ x__ y__ -> x__ {_AnyChainDatum'_unknownFields = y__})
  defMessage
    = AnyChainDatum'_constructor
        {_AnyChainDatum'nativeBytes = Data.ProtoLens.fieldDefault,
         _AnyChainDatum'key = Data.ProtoLens.fieldDefault,
         _AnyChainDatum'parsedState = Prelude.Nothing,
         _AnyChainDatum'_unknownFields = []}
  parseMessage
    = let
        loop ::
          AnyChainDatum -> Data.ProtoLens.Encoding.Bytes.Parser AnyChainDatum
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
                                           Data.ProtoLens.Encoding.Bytes.getBytes
                                             (Prelude.fromIntegral len))
                                       "key"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"key") y x)
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
          (do loop Data.ProtoLens.defMessage) "AnyChainDatum"
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
                (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"key") _x
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
                   (case
                        Lens.Family2.view
                          (Data.ProtoLens.Field.field @"maybe'parsedState") _x
                    of
                      Prelude.Nothing -> Data.Monoid.mempty
                      (Prelude.Just (AnyChainDatum'Cardano v))
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
instance Control.DeepSeq.NFData AnyChainDatum where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_AnyChainDatum'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_AnyChainDatum'nativeBytes x__)
                (Control.DeepSeq.deepseq
                   (_AnyChainDatum'key x__)
                   (Control.DeepSeq.deepseq (_AnyChainDatum'parsedState x__) ())))
instance Control.DeepSeq.NFData AnyChainDatum'ParsedState where
  rnf (AnyChainDatum'Cardano x__) = Control.DeepSeq.rnf x__
_AnyChainDatum'Cardano ::
  Data.ProtoLens.Prism.Prism' AnyChainDatum'ParsedState Proto.Utxorpc.V1beta.Cardano.Cardano.PlutusData
_AnyChainDatum'Cardano
  = Data.ProtoLens.Prism.prism'
      AnyChainDatum'Cardano
      (\ p__
         -> case p__ of
              (AnyChainDatum'Cardano p__val) -> Prelude.Just p__val)
{- | Fields :
     
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.maybe'params' @:: Lens' AnyChainParams (Prelude.Maybe AnyChainParams'Params)@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.maybe'cardano' @:: Lens' AnyChainParams (Prelude.Maybe Proto.Utxorpc.V1beta.Cardano.Cardano.PParams)@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.cardano' @:: Lens' AnyChainParams Proto.Utxorpc.V1beta.Cardano.Cardano.PParams@ -}
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
  = AnyChainParams'Cardano !Proto.Utxorpc.V1beta.Cardano.Cardano.PParams
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.Field.HasField AnyChainParams "maybe'params" (Prelude.Maybe AnyChainParams'Params) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AnyChainParams'params
           (\ x__ y__ -> x__ {_AnyChainParams'params = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField AnyChainParams "maybe'cardano" (Prelude.Maybe Proto.Utxorpc.V1beta.Cardano.Cardano.PParams) where
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
instance Data.ProtoLens.Field.HasField AnyChainParams "cardano" Proto.Utxorpc.V1beta.Cardano.Cardano.PParams where
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
    = Data.Text.pack "utxorpc.v1beta.query.AnyChainParams"
  packedMessageDescriptor _
    = "\n\
      \\SOAnyChainParams\DC2;\n\
      \\acardano\CAN\SOH \SOH(\v2\US.utxorpc.v1beta.cardano.PParamsH\NULR\acardanoB\b\n\
      \\ACKparams"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        cardano__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "cardano"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Utxorpc.V1beta.Cardano.Cardano.PParams)
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
  Data.ProtoLens.Prism.Prism' AnyChainParams'Params Proto.Utxorpc.V1beta.Cardano.Cardano.PParams
_AnyChainParams'Cardano
  = Data.ProtoLens.Prism.prism'
      AnyChainParams'Cardano
      (\ p__
         -> case p__ of
              (AnyChainParams'Cardano p__val) -> Prelude.Just p__val)
{- | Fields :
     
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.nativeBytes' @:: Lens' AnyChainTx Data.ByteString.ByteString@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.blockRef' @:: Lens' AnyChainTx ChainPoint@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.maybe'blockRef' @:: Lens' AnyChainTx (Prelude.Maybe ChainPoint)@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.maybe'chain' @:: Lens' AnyChainTx (Prelude.Maybe AnyChainTx'Chain)@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.maybe'cardano' @:: Lens' AnyChainTx (Prelude.Maybe Proto.Utxorpc.V1beta.Cardano.Cardano.Tx)@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.cardano' @:: Lens' AnyChainTx Proto.Utxorpc.V1beta.Cardano.Cardano.Tx@ -}
data AnyChainTx
  = AnyChainTx'_constructor {_AnyChainTx'nativeBytes :: !Data.ByteString.ByteString,
                             _AnyChainTx'blockRef :: !(Prelude.Maybe ChainPoint),
                             _AnyChainTx'chain :: !(Prelude.Maybe AnyChainTx'Chain),
                             _AnyChainTx'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show AnyChainTx where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
data AnyChainTx'Chain
  = AnyChainTx'Cardano !Proto.Utxorpc.V1beta.Cardano.Cardano.Tx
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.Field.HasField AnyChainTx "nativeBytes" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AnyChainTx'nativeBytes
           (\ x__ y__ -> x__ {_AnyChainTx'nativeBytes = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField AnyChainTx "blockRef" ChainPoint where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AnyChainTx'blockRef
           (\ x__ y__ -> x__ {_AnyChainTx'blockRef = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField AnyChainTx "maybe'blockRef" (Prelude.Maybe ChainPoint) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AnyChainTx'blockRef
           (\ x__ y__ -> x__ {_AnyChainTx'blockRef = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField AnyChainTx "maybe'chain" (Prelude.Maybe AnyChainTx'Chain) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AnyChainTx'chain (\ x__ y__ -> x__ {_AnyChainTx'chain = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField AnyChainTx "maybe'cardano" (Prelude.Maybe Proto.Utxorpc.V1beta.Cardano.Cardano.Tx) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AnyChainTx'chain (\ x__ y__ -> x__ {_AnyChainTx'chain = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (AnyChainTx'Cardano x__val)) -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap AnyChainTx'Cardano y__))
instance Data.ProtoLens.Field.HasField AnyChainTx "cardano" Proto.Utxorpc.V1beta.Cardano.Cardano.Tx where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AnyChainTx'chain (\ x__ y__ -> x__ {_AnyChainTx'chain = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (AnyChainTx'Cardano x__val)) -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap AnyChainTx'Cardano y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Message AnyChainTx where
  messageName _ = Data.Text.pack "utxorpc.v1beta.query.AnyChainTx"
  packedMessageDescriptor _
    = "\n\
      \\n\
      \AnyChainTx\DC2!\n\
      \\fnative_bytes\CAN\SOH \SOH(\fR\vnativeBytes\DC26\n\
      \\acardano\CAN\STX \SOH(\v2\SUB.utxorpc.v1beta.cardano.TxH\NULR\acardano\DC2=\n\
      \\tblock_ref\CAN\ETX \SOH(\v2 .utxorpc.v1beta.query.ChainPointR\bblockRefB\a\n\
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
              Data.ProtoLens.FieldDescriptor AnyChainTx
        blockRef__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "block_ref"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ChainPoint)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'blockRef")) ::
              Data.ProtoLens.FieldDescriptor AnyChainTx
        cardano__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "cardano"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Utxorpc.V1beta.Cardano.Cardano.Tx)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'cardano")) ::
              Data.ProtoLens.FieldDescriptor AnyChainTx
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, nativeBytes__field_descriptor),
           (Data.ProtoLens.Tag 3, blockRef__field_descriptor),
           (Data.ProtoLens.Tag 2, cardano__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _AnyChainTx'_unknownFields
        (\ x__ y__ -> x__ {_AnyChainTx'_unknownFields = y__})
  defMessage
    = AnyChainTx'_constructor
        {_AnyChainTx'nativeBytes = Data.ProtoLens.fieldDefault,
         _AnyChainTx'blockRef = Prelude.Nothing,
         _AnyChainTx'chain = Prelude.Nothing,
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
                                       "native_bytes"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"nativeBytes") y x)
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "block_ref"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"blockRef") y x)
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
          (do loop Data.ProtoLens.defMessage) "AnyChainTx"
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
                     Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'blockRef") _x
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
                        Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'chain") _x
                    of
                      Prelude.Nothing -> Data.Monoid.mempty
                      (Prelude.Just (AnyChainTx'Cardano v))
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
                      (Lens.Family2.view Data.ProtoLens.unknownFields _x))))
instance Control.DeepSeq.NFData AnyChainTx where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_AnyChainTx'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_AnyChainTx'nativeBytes x__)
                (Control.DeepSeq.deepseq
                   (_AnyChainTx'blockRef x__)
                   (Control.DeepSeq.deepseq (_AnyChainTx'chain x__) ())))
instance Control.DeepSeq.NFData AnyChainTx'Chain where
  rnf (AnyChainTx'Cardano x__) = Control.DeepSeq.rnf x__
_AnyChainTx'Cardano ::
  Data.ProtoLens.Prism.Prism' AnyChainTx'Chain Proto.Utxorpc.V1beta.Cardano.Cardano.Tx
_AnyChainTx'Cardano
  = Data.ProtoLens.Prism.prism'
      AnyChainTx'Cardano
      (\ p__
         -> case p__ of (AnyChainTx'Cardano p__val) -> Prelude.Just p__val)
{- | Fields :
     
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.nativeBytes' @:: Lens' AnyUtxoData Data.ByteString.ByteString@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.txoRef' @:: Lens' AnyUtxoData TxoRef@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.maybe'txoRef' @:: Lens' AnyUtxoData (Prelude.Maybe TxoRef)@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.blockRef' @:: Lens' AnyUtxoData ChainPoint@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.maybe'blockRef' @:: Lens' AnyUtxoData (Prelude.Maybe ChainPoint)@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.maybe'parsedState' @:: Lens' AnyUtxoData (Prelude.Maybe AnyUtxoData'ParsedState)@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.maybe'cardano' @:: Lens' AnyUtxoData (Prelude.Maybe Proto.Utxorpc.V1beta.Cardano.Cardano.TxOutput)@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.cardano' @:: Lens' AnyUtxoData Proto.Utxorpc.V1beta.Cardano.Cardano.TxOutput@ -}
data AnyUtxoData
  = AnyUtxoData'_constructor {_AnyUtxoData'nativeBytes :: !Data.ByteString.ByteString,
                              _AnyUtxoData'txoRef :: !(Prelude.Maybe TxoRef),
                              _AnyUtxoData'blockRef :: !(Prelude.Maybe ChainPoint),
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
  = AnyUtxoData'Cardano !Proto.Utxorpc.V1beta.Cardano.Cardano.TxOutput
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
instance Data.ProtoLens.Field.HasField AnyUtxoData "blockRef" ChainPoint where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AnyUtxoData'blockRef
           (\ x__ y__ -> x__ {_AnyUtxoData'blockRef = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField AnyUtxoData "maybe'blockRef" (Prelude.Maybe ChainPoint) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AnyUtxoData'blockRef
           (\ x__ y__ -> x__ {_AnyUtxoData'blockRef = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField AnyUtxoData "maybe'parsedState" (Prelude.Maybe AnyUtxoData'ParsedState) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AnyUtxoData'parsedState
           (\ x__ y__ -> x__ {_AnyUtxoData'parsedState = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField AnyUtxoData "maybe'cardano" (Prelude.Maybe Proto.Utxorpc.V1beta.Cardano.Cardano.TxOutput) where
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
instance Data.ProtoLens.Field.HasField AnyUtxoData "cardano" Proto.Utxorpc.V1beta.Cardano.Cardano.TxOutput where
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
  messageName _ = Data.Text.pack "utxorpc.v1beta.query.AnyUtxoData"
  packedMessageDescriptor _
    = "\n\
      \\vAnyUtxoData\DC2!\n\
      \\fnative_bytes\CAN\SOH \SOH(\fR\vnativeBytes\DC25\n\
      \\atxo_ref\CAN\STX \SOH(\v2\FS.utxorpc.v1beta.query.TxoRefR\ACKtxoRef\DC2<\n\
      \\acardano\CAN\ETX \SOH(\v2 .utxorpc.v1beta.cardano.TxOutputH\NULR\acardano\DC2=\n\
      \\tblock_ref\CAN\EOT \SOH(\v2 .utxorpc.v1beta.query.ChainPointR\bblockRefB\SO\n\
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
        blockRef__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "block_ref"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ChainPoint)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'blockRef")) ::
              Data.ProtoLens.FieldDescriptor AnyUtxoData
        cardano__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "cardano"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Utxorpc.V1beta.Cardano.Cardano.TxOutput)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'cardano")) ::
              Data.ProtoLens.FieldDescriptor AnyUtxoData
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, nativeBytes__field_descriptor),
           (Data.ProtoLens.Tag 2, txoRef__field_descriptor),
           (Data.ProtoLens.Tag 4, blockRef__field_descriptor),
           (Data.ProtoLens.Tag 3, cardano__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _AnyUtxoData'_unknownFields
        (\ x__ y__ -> x__ {_AnyUtxoData'_unknownFields = y__})
  defMessage
    = AnyUtxoData'_constructor
        {_AnyUtxoData'nativeBytes = Data.ProtoLens.fieldDefault,
         _AnyUtxoData'txoRef = Prelude.Nothing,
         _AnyUtxoData'blockRef = Prelude.Nothing,
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
                        34
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "block_ref"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"blockRef") y x)
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
                        Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'blockRef") _x
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
                         (Lens.Family2.view Data.ProtoLens.unknownFields _x)))))
instance Control.DeepSeq.NFData AnyUtxoData where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_AnyUtxoData'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_AnyUtxoData'nativeBytes x__)
                (Control.DeepSeq.deepseq
                   (_AnyUtxoData'txoRef x__)
                   (Control.DeepSeq.deepseq
                      (_AnyUtxoData'blockRef x__)
                      (Control.DeepSeq.deepseq (_AnyUtxoData'parsedState x__) ()))))
instance Control.DeepSeq.NFData AnyUtxoData'ParsedState where
  rnf (AnyUtxoData'Cardano x__) = Control.DeepSeq.rnf x__
_AnyUtxoData'Cardano ::
  Data.ProtoLens.Prism.Prism' AnyUtxoData'ParsedState Proto.Utxorpc.V1beta.Cardano.Cardano.TxOutput
_AnyUtxoData'Cardano
  = Data.ProtoLens.Prism.prism'
      AnyUtxoData'Cardano
      (\ p__
         -> case p__ of (AnyUtxoData'Cardano p__val) -> Prelude.Just p__val)
{- | Fields :
     
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.maybe'utxoPattern' @:: Lens' AnyUtxoPattern (Prelude.Maybe AnyUtxoPattern'UtxoPattern)@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.maybe'cardano' @:: Lens' AnyUtxoPattern (Prelude.Maybe Proto.Utxorpc.V1beta.Cardano.Cardano.TxOutputPattern)@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.cardano' @:: Lens' AnyUtxoPattern Proto.Utxorpc.V1beta.Cardano.Cardano.TxOutputPattern@ -}
data AnyUtxoPattern
  = AnyUtxoPattern'_constructor {_AnyUtxoPattern'utxoPattern :: !(Prelude.Maybe AnyUtxoPattern'UtxoPattern),
                                 _AnyUtxoPattern'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show AnyUtxoPattern where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
data AnyUtxoPattern'UtxoPattern
  = AnyUtxoPattern'Cardano !Proto.Utxorpc.V1beta.Cardano.Cardano.TxOutputPattern
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.Field.HasField AnyUtxoPattern "maybe'utxoPattern" (Prelude.Maybe AnyUtxoPattern'UtxoPattern) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AnyUtxoPattern'utxoPattern
           (\ x__ y__ -> x__ {_AnyUtxoPattern'utxoPattern = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField AnyUtxoPattern "maybe'cardano" (Prelude.Maybe Proto.Utxorpc.V1beta.Cardano.Cardano.TxOutputPattern) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AnyUtxoPattern'utxoPattern
           (\ x__ y__ -> x__ {_AnyUtxoPattern'utxoPattern = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (AnyUtxoPattern'Cardano x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap AnyUtxoPattern'Cardano y__))
instance Data.ProtoLens.Field.HasField AnyUtxoPattern "cardano" Proto.Utxorpc.V1beta.Cardano.Cardano.TxOutputPattern where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AnyUtxoPattern'utxoPattern
           (\ x__ y__ -> x__ {_AnyUtxoPattern'utxoPattern = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (AnyUtxoPattern'Cardano x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap AnyUtxoPattern'Cardano y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Message AnyUtxoPattern where
  messageName _
    = Data.Text.pack "utxorpc.v1beta.query.AnyUtxoPattern"
  packedMessageDescriptor _
    = "\n\
      \\SOAnyUtxoPattern\DC2C\n\
      \\acardano\CAN\SOH \SOH(\v2'.utxorpc.v1beta.cardano.TxOutputPatternH\NULR\acardanoB\SO\n\
      \\futxo_pattern"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        cardano__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "cardano"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Utxorpc.V1beta.Cardano.Cardano.TxOutputPattern)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'cardano")) ::
              Data.ProtoLens.FieldDescriptor AnyUtxoPattern
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, cardano__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _AnyUtxoPattern'_unknownFields
        (\ x__ y__ -> x__ {_AnyUtxoPattern'_unknownFields = y__})
  defMessage
    = AnyUtxoPattern'_constructor
        {_AnyUtxoPattern'utxoPattern = Prelude.Nothing,
         _AnyUtxoPattern'_unknownFields = []}
  parseMessage
    = let
        loop ::
          AnyUtxoPattern
          -> Data.ProtoLens.Encoding.Bytes.Parser AnyUtxoPattern
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
          (do loop Data.ProtoLens.defMessage) "AnyUtxoPattern"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view
                    (Data.ProtoLens.Field.field @"maybe'utxoPattern") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just (AnyUtxoPattern'Cardano v))
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
instance Control.DeepSeq.NFData AnyUtxoPattern where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_AnyUtxoPattern'_unknownFields x__)
             (Control.DeepSeq.deepseq (_AnyUtxoPattern'utxoPattern x__) ())
instance Control.DeepSeq.NFData AnyUtxoPattern'UtxoPattern where
  rnf (AnyUtxoPattern'Cardano x__) = Control.DeepSeq.rnf x__
_AnyUtxoPattern'Cardano ::
  Data.ProtoLens.Prism.Prism' AnyUtxoPattern'UtxoPattern Proto.Utxorpc.V1beta.Cardano.Cardano.TxOutputPattern
_AnyUtxoPattern'Cardano
  = Data.ProtoLens.Prism.prism'
      AnyUtxoPattern'Cardano
      (\ p__
         -> case p__ of
              (AnyUtxoPattern'Cardano p__val) -> Prelude.Just p__val)
{- | Fields :
     
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.slot' @:: Lens' ChainPoint Data.Word.Word64@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.hash' @:: Lens' ChainPoint Data.ByteString.ByteString@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.height' @:: Lens' ChainPoint Data.Word.Word64@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.timestamp' @:: Lens' ChainPoint Data.Word.Word64@ -}
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
  messageName _ = Data.Text.pack "utxorpc.v1beta.query.ChainPoint"
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
     
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.keys' @:: Lens' ReadDataRequest [Data.ByteString.ByteString]@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.vec'keys' @:: Lens' ReadDataRequest (Data.Vector.Vector Data.ByteString.ByteString)@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.fieldMask' @:: Lens' ReadDataRequest Proto.Google.Protobuf.FieldMask.FieldMask@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.maybe'fieldMask' @:: Lens' ReadDataRequest (Prelude.Maybe Proto.Google.Protobuf.FieldMask.FieldMask)@ -}
data ReadDataRequest
  = ReadDataRequest'_constructor {_ReadDataRequest'keys :: !(Data.Vector.Vector Data.ByteString.ByteString),
                                  _ReadDataRequest'fieldMask :: !(Prelude.Maybe Proto.Google.Protobuf.FieldMask.FieldMask),
                                  _ReadDataRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ReadDataRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ReadDataRequest "keys" [Data.ByteString.ByteString] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReadDataRequest'keys
           (\ x__ y__ -> x__ {_ReadDataRequest'keys = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ReadDataRequest "vec'keys" (Data.Vector.Vector Data.ByteString.ByteString) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReadDataRequest'keys
           (\ x__ y__ -> x__ {_ReadDataRequest'keys = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ReadDataRequest "fieldMask" Proto.Google.Protobuf.FieldMask.FieldMask where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReadDataRequest'fieldMask
           (\ x__ y__ -> x__ {_ReadDataRequest'fieldMask = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ReadDataRequest "maybe'fieldMask" (Prelude.Maybe Proto.Google.Protobuf.FieldMask.FieldMask) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReadDataRequest'fieldMask
           (\ x__ y__ -> x__ {_ReadDataRequest'fieldMask = y__}))
        Prelude.id
instance Data.ProtoLens.Message ReadDataRequest where
  messageName _
    = Data.Text.pack "utxorpc.v1beta.query.ReadDataRequest"
  packedMessageDescriptor _
    = "\n\
      \\SIReadDataRequest\DC2\DC2\n\
      \\EOTkeys\CAN\SOH \ETX(\fR\EOTkeys\DC29\n\
      \\n\
      \field_mask\CAN\STX \SOH(\v2\SUB.google.protobuf.FieldMaskR\tfieldMask"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        keys__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "keys"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"keys")) ::
              Data.ProtoLens.FieldDescriptor ReadDataRequest
        fieldMask__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "field_mask"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Google.Protobuf.FieldMask.FieldMask)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'fieldMask")) ::
              Data.ProtoLens.FieldDescriptor ReadDataRequest
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, keys__field_descriptor),
           (Data.ProtoLens.Tag 2, fieldMask__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ReadDataRequest'_unknownFields
        (\ x__ y__ -> x__ {_ReadDataRequest'_unknownFields = y__})
  defMessage
    = ReadDataRequest'_constructor
        {_ReadDataRequest'keys = Data.Vector.Generic.empty,
         _ReadDataRequest'fieldMask = Prelude.Nothing,
         _ReadDataRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ReadDataRequest
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Data.ByteString.ByteString
             -> Data.ProtoLens.Encoding.Bytes.Parser ReadDataRequest
        loop x mutable'keys
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'keys <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.unsafeFreeze mutable'keys)
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
                              (Data.ProtoLens.Field.field @"vec'keys") frozen'keys x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.getBytes
                                              (Prelude.fromIntegral len))
                                        "keys"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'keys y)
                                loop x v
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "field_mask"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"fieldMask") y x)
                                  mutable'keys
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'keys
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'keys <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'keys)
          "ReadDataRequest"
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
                (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'keys") _x))
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
instance Control.DeepSeq.NFData ReadDataRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ReadDataRequest'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ReadDataRequest'keys x__)
                (Control.DeepSeq.deepseq (_ReadDataRequest'fieldMask x__) ()))
{- | Fields :
     
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.values' @:: Lens' ReadDataResponse [AnyChainDatum]@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.vec'values' @:: Lens' ReadDataResponse (Data.Vector.Vector AnyChainDatum)@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.ledgerTip' @:: Lens' ReadDataResponse ChainPoint@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.maybe'ledgerTip' @:: Lens' ReadDataResponse (Prelude.Maybe ChainPoint)@ -}
data ReadDataResponse
  = ReadDataResponse'_constructor {_ReadDataResponse'values :: !(Data.Vector.Vector AnyChainDatum),
                                   _ReadDataResponse'ledgerTip :: !(Prelude.Maybe ChainPoint),
                                   _ReadDataResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ReadDataResponse where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ReadDataResponse "values" [AnyChainDatum] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReadDataResponse'values
           (\ x__ y__ -> x__ {_ReadDataResponse'values = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ReadDataResponse "vec'values" (Data.Vector.Vector AnyChainDatum) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReadDataResponse'values
           (\ x__ y__ -> x__ {_ReadDataResponse'values = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ReadDataResponse "ledgerTip" ChainPoint where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReadDataResponse'ledgerTip
           (\ x__ y__ -> x__ {_ReadDataResponse'ledgerTip = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ReadDataResponse "maybe'ledgerTip" (Prelude.Maybe ChainPoint) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReadDataResponse'ledgerTip
           (\ x__ y__ -> x__ {_ReadDataResponse'ledgerTip = y__}))
        Prelude.id
instance Data.ProtoLens.Message ReadDataResponse where
  messageName _
    = Data.Text.pack "utxorpc.v1beta.query.ReadDataResponse"
  packedMessageDescriptor _
    = "\n\
      \\DLEReadDataResponse\DC2;\n\
      \\ACKvalues\CAN\SOH \ETX(\v2#.utxorpc.v1beta.query.AnyChainDatumR\ACKvalues\DC2?\n\
      \\n\
      \ledger_tip\CAN\STX \SOH(\v2 .utxorpc.v1beta.query.ChainPointR\tledgerTip"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        values__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "values"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor AnyChainDatum)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"values")) ::
              Data.ProtoLens.FieldDescriptor ReadDataResponse
        ledgerTip__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "ledger_tip"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ChainPoint)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'ledgerTip")) ::
              Data.ProtoLens.FieldDescriptor ReadDataResponse
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, values__field_descriptor),
           (Data.ProtoLens.Tag 2, ledgerTip__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ReadDataResponse'_unknownFields
        (\ x__ y__ -> x__ {_ReadDataResponse'_unknownFields = y__})
  defMessage
    = ReadDataResponse'_constructor
        {_ReadDataResponse'values = Data.Vector.Generic.empty,
         _ReadDataResponse'ledgerTip = Prelude.Nothing,
         _ReadDataResponse'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ReadDataResponse
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld AnyChainDatum
             -> Data.ProtoLens.Encoding.Bytes.Parser ReadDataResponse
        loop x mutable'values
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'values <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                         (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                            mutable'values)
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
                              (Data.ProtoLens.Field.field @"vec'values") frozen'values x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "values"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'values y)
                                loop x v
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "ledger_tip"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"ledgerTip") y x)
                                  mutable'values
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'values
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'values <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                  Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'values)
          "ReadDataResponse"
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
                (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'values") _x))
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
instance Control.DeepSeq.NFData ReadDataResponse where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ReadDataResponse'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ReadDataResponse'values x__)
                (Control.DeepSeq.deepseq (_ReadDataResponse'ledgerTip x__) ()))
{- | Fields :
     
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.fieldMask' @:: Lens' ReadEraSummaryRequest Proto.Google.Protobuf.FieldMask.FieldMask@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.maybe'fieldMask' @:: Lens' ReadEraSummaryRequest (Prelude.Maybe Proto.Google.Protobuf.FieldMask.FieldMask)@ -}
data ReadEraSummaryRequest
  = ReadEraSummaryRequest'_constructor {_ReadEraSummaryRequest'fieldMask :: !(Prelude.Maybe Proto.Google.Protobuf.FieldMask.FieldMask),
                                        _ReadEraSummaryRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ReadEraSummaryRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ReadEraSummaryRequest "fieldMask" Proto.Google.Protobuf.FieldMask.FieldMask where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReadEraSummaryRequest'fieldMask
           (\ x__ y__ -> x__ {_ReadEraSummaryRequest'fieldMask = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ReadEraSummaryRequest "maybe'fieldMask" (Prelude.Maybe Proto.Google.Protobuf.FieldMask.FieldMask) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReadEraSummaryRequest'fieldMask
           (\ x__ y__ -> x__ {_ReadEraSummaryRequest'fieldMask = y__}))
        Prelude.id
instance Data.ProtoLens.Message ReadEraSummaryRequest where
  messageName _
    = Data.Text.pack "utxorpc.v1beta.query.ReadEraSummaryRequest"
  packedMessageDescriptor _
    = "\n\
      \\NAKReadEraSummaryRequest\DC29\n\
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
              Data.ProtoLens.FieldDescriptor ReadEraSummaryRequest
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, fieldMask__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ReadEraSummaryRequest'_unknownFields
        (\ x__ y__ -> x__ {_ReadEraSummaryRequest'_unknownFields = y__})
  defMessage
    = ReadEraSummaryRequest'_constructor
        {_ReadEraSummaryRequest'fieldMask = Prelude.Nothing,
         _ReadEraSummaryRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ReadEraSummaryRequest
          -> Data.ProtoLens.Encoding.Bytes.Parser ReadEraSummaryRequest
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
          (do loop Data.ProtoLens.defMessage) "ReadEraSummaryRequest"
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
instance Control.DeepSeq.NFData ReadEraSummaryRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ReadEraSummaryRequest'_unknownFields x__)
             (Control.DeepSeq.deepseq (_ReadEraSummaryRequest'fieldMask x__) ())
{- | Fields :
     
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.maybe'summary' @:: Lens' ReadEraSummaryResponse (Prelude.Maybe ReadEraSummaryResponse'Summary)@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.maybe'cardano' @:: Lens' ReadEraSummaryResponse (Prelude.Maybe Proto.Utxorpc.V1beta.Cardano.Cardano.EraSummaries)@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.cardano' @:: Lens' ReadEraSummaryResponse Proto.Utxorpc.V1beta.Cardano.Cardano.EraSummaries@ -}
data ReadEraSummaryResponse
  = ReadEraSummaryResponse'_constructor {_ReadEraSummaryResponse'summary :: !(Prelude.Maybe ReadEraSummaryResponse'Summary),
                                         _ReadEraSummaryResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ReadEraSummaryResponse where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
data ReadEraSummaryResponse'Summary
  = ReadEraSummaryResponse'Cardano !Proto.Utxorpc.V1beta.Cardano.Cardano.EraSummaries
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.Field.HasField ReadEraSummaryResponse "maybe'summary" (Prelude.Maybe ReadEraSummaryResponse'Summary) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReadEraSummaryResponse'summary
           (\ x__ y__ -> x__ {_ReadEraSummaryResponse'summary = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ReadEraSummaryResponse "maybe'cardano" (Prelude.Maybe Proto.Utxorpc.V1beta.Cardano.Cardano.EraSummaries) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReadEraSummaryResponse'summary
           (\ x__ y__ -> x__ {_ReadEraSummaryResponse'summary = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (ReadEraSummaryResponse'Cardano x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap ReadEraSummaryResponse'Cardano y__))
instance Data.ProtoLens.Field.HasField ReadEraSummaryResponse "cardano" Proto.Utxorpc.V1beta.Cardano.Cardano.EraSummaries where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReadEraSummaryResponse'summary
           (\ x__ y__ -> x__ {_ReadEraSummaryResponse'summary = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (ReadEraSummaryResponse'Cardano x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap ReadEraSummaryResponse'Cardano y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Message ReadEraSummaryResponse where
  messageName _
    = Data.Text.pack "utxorpc.v1beta.query.ReadEraSummaryResponse"
  packedMessageDescriptor _
    = "\n\
      \\SYNReadEraSummaryResponse\DC2@\n\
      \\acardano\CAN\SOH \SOH(\v2$.utxorpc.v1beta.cardano.EraSummariesH\NULR\acardanoB\t\n\
      \\asummary"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        cardano__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "cardano"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Utxorpc.V1beta.Cardano.Cardano.EraSummaries)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'cardano")) ::
              Data.ProtoLens.FieldDescriptor ReadEraSummaryResponse
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, cardano__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ReadEraSummaryResponse'_unknownFields
        (\ x__ y__ -> x__ {_ReadEraSummaryResponse'_unknownFields = y__})
  defMessage
    = ReadEraSummaryResponse'_constructor
        {_ReadEraSummaryResponse'summary = Prelude.Nothing,
         _ReadEraSummaryResponse'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ReadEraSummaryResponse
          -> Data.ProtoLens.Encoding.Bytes.Parser ReadEraSummaryResponse
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
          (do loop Data.ProtoLens.defMessage) "ReadEraSummaryResponse"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'summary") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just (ReadEraSummaryResponse'Cardano v))
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
instance Control.DeepSeq.NFData ReadEraSummaryResponse where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ReadEraSummaryResponse'_unknownFields x__)
             (Control.DeepSeq.deepseq (_ReadEraSummaryResponse'summary x__) ())
instance Control.DeepSeq.NFData ReadEraSummaryResponse'Summary where
  rnf (ReadEraSummaryResponse'Cardano x__) = Control.DeepSeq.rnf x__
_ReadEraSummaryResponse'Cardano ::
  Data.ProtoLens.Prism.Prism' ReadEraSummaryResponse'Summary Proto.Utxorpc.V1beta.Cardano.Cardano.EraSummaries
_ReadEraSummaryResponse'Cardano
  = Data.ProtoLens.Prism.prism'
      ReadEraSummaryResponse'Cardano
      (\ p__
         -> case p__ of
              (ReadEraSummaryResponse'Cardano p__val) -> Prelude.Just p__val)
{- | Fields :
     
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.fieldMask' @:: Lens' ReadGenesisRequest Proto.Google.Protobuf.FieldMask.FieldMask@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.maybe'fieldMask' @:: Lens' ReadGenesisRequest (Prelude.Maybe Proto.Google.Protobuf.FieldMask.FieldMask)@ -}
data ReadGenesisRequest
  = ReadGenesisRequest'_constructor {_ReadGenesisRequest'fieldMask :: !(Prelude.Maybe Proto.Google.Protobuf.FieldMask.FieldMask),
                                     _ReadGenesisRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ReadGenesisRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ReadGenesisRequest "fieldMask" Proto.Google.Protobuf.FieldMask.FieldMask where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReadGenesisRequest'fieldMask
           (\ x__ y__ -> x__ {_ReadGenesisRequest'fieldMask = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ReadGenesisRequest "maybe'fieldMask" (Prelude.Maybe Proto.Google.Protobuf.FieldMask.FieldMask) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReadGenesisRequest'fieldMask
           (\ x__ y__ -> x__ {_ReadGenesisRequest'fieldMask = y__}))
        Prelude.id
instance Data.ProtoLens.Message ReadGenesisRequest where
  messageName _
    = Data.Text.pack "utxorpc.v1beta.query.ReadGenesisRequest"
  packedMessageDescriptor _
    = "\n\
      \\DC2ReadGenesisRequest\DC29\n\
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
              Data.ProtoLens.FieldDescriptor ReadGenesisRequest
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, fieldMask__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ReadGenesisRequest'_unknownFields
        (\ x__ y__ -> x__ {_ReadGenesisRequest'_unknownFields = y__})
  defMessage
    = ReadGenesisRequest'_constructor
        {_ReadGenesisRequest'fieldMask = Prelude.Nothing,
         _ReadGenesisRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ReadGenesisRequest
          -> Data.ProtoLens.Encoding.Bytes.Parser ReadGenesisRequest
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
          (do loop Data.ProtoLens.defMessage) "ReadGenesisRequest"
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
instance Control.DeepSeq.NFData ReadGenesisRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ReadGenesisRequest'_unknownFields x__)
             (Control.DeepSeq.deepseq (_ReadGenesisRequest'fieldMask x__) ())
{- | Fields :
     
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.genesis' @:: Lens' ReadGenesisResponse Data.ByteString.ByteString@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.caip2' @:: Lens' ReadGenesisResponse Data.Text.Text@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.maybe'config' @:: Lens' ReadGenesisResponse (Prelude.Maybe ReadGenesisResponse'Config)@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.maybe'cardano' @:: Lens' ReadGenesisResponse (Prelude.Maybe Proto.Utxorpc.V1beta.Cardano.Cardano.Genesis)@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.cardano' @:: Lens' ReadGenesisResponse Proto.Utxorpc.V1beta.Cardano.Cardano.Genesis@ -}
data ReadGenesisResponse
  = ReadGenesisResponse'_constructor {_ReadGenesisResponse'genesis :: !Data.ByteString.ByteString,
                                      _ReadGenesisResponse'caip2 :: !Data.Text.Text,
                                      _ReadGenesisResponse'config :: !(Prelude.Maybe ReadGenesisResponse'Config),
                                      _ReadGenesisResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ReadGenesisResponse where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
data ReadGenesisResponse'Config
  = ReadGenesisResponse'Cardano !Proto.Utxorpc.V1beta.Cardano.Cardano.Genesis
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.Field.HasField ReadGenesisResponse "genesis" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReadGenesisResponse'genesis
           (\ x__ y__ -> x__ {_ReadGenesisResponse'genesis = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ReadGenesisResponse "caip2" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReadGenesisResponse'caip2
           (\ x__ y__ -> x__ {_ReadGenesisResponse'caip2 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ReadGenesisResponse "maybe'config" (Prelude.Maybe ReadGenesisResponse'Config) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReadGenesisResponse'config
           (\ x__ y__ -> x__ {_ReadGenesisResponse'config = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ReadGenesisResponse "maybe'cardano" (Prelude.Maybe Proto.Utxorpc.V1beta.Cardano.Cardano.Genesis) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReadGenesisResponse'config
           (\ x__ y__ -> x__ {_ReadGenesisResponse'config = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (ReadGenesisResponse'Cardano x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap ReadGenesisResponse'Cardano y__))
instance Data.ProtoLens.Field.HasField ReadGenesisResponse "cardano" Proto.Utxorpc.V1beta.Cardano.Cardano.Genesis where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReadGenesisResponse'config
           (\ x__ y__ -> x__ {_ReadGenesisResponse'config = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (ReadGenesisResponse'Cardano x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap ReadGenesisResponse'Cardano y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Message ReadGenesisResponse where
  messageName _
    = Data.Text.pack "utxorpc.v1beta.query.ReadGenesisResponse"
  packedMessageDescriptor _
    = "\n\
      \\DC3ReadGenesisResponse\DC2\CAN\n\
      \\agenesis\CAN\SOH \SOH(\fR\agenesis\DC2\DC4\n\
      \\ENQcaip2\CAN\STX \SOH(\tR\ENQcaip2\DC2;\n\
      \\acardano\CAN\ETX \SOH(\v2\US.utxorpc.v1beta.cardano.GenesisH\NULR\acardanoB\b\n\
      \\ACKconfig"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        genesis__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "genesis"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"genesis")) ::
              Data.ProtoLens.FieldDescriptor ReadGenesisResponse
        caip2__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "caip2"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"caip2")) ::
              Data.ProtoLens.FieldDescriptor ReadGenesisResponse
        cardano__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "cardano"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Utxorpc.V1beta.Cardano.Cardano.Genesis)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'cardano")) ::
              Data.ProtoLens.FieldDescriptor ReadGenesisResponse
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, genesis__field_descriptor),
           (Data.ProtoLens.Tag 2, caip2__field_descriptor),
           (Data.ProtoLens.Tag 3, cardano__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ReadGenesisResponse'_unknownFields
        (\ x__ y__ -> x__ {_ReadGenesisResponse'_unknownFields = y__})
  defMessage
    = ReadGenesisResponse'_constructor
        {_ReadGenesisResponse'genesis = Data.ProtoLens.fieldDefault,
         _ReadGenesisResponse'caip2 = Data.ProtoLens.fieldDefault,
         _ReadGenesisResponse'config = Prelude.Nothing,
         _ReadGenesisResponse'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ReadGenesisResponse
          -> Data.ProtoLens.Encoding.Bytes.Parser ReadGenesisResponse
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
                                       "genesis"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"genesis") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "caip2"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"caip2") y x)
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
          (do loop Data.ProtoLens.defMessage) "ReadGenesisResponse"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"genesis") _x
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
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"caip2") _x
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
                ((Data.Monoid.<>)
                   (case
                        Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'config") _x
                    of
                      Prelude.Nothing -> Data.Monoid.mempty
                      (Prelude.Just (ReadGenesisResponse'Cardano v))
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
instance Control.DeepSeq.NFData ReadGenesisResponse where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ReadGenesisResponse'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ReadGenesisResponse'genesis x__)
                (Control.DeepSeq.deepseq
                   (_ReadGenesisResponse'caip2 x__)
                   (Control.DeepSeq.deepseq (_ReadGenesisResponse'config x__) ())))
instance Control.DeepSeq.NFData ReadGenesisResponse'Config where
  rnf (ReadGenesisResponse'Cardano x__) = Control.DeepSeq.rnf x__
_ReadGenesisResponse'Cardano ::
  Data.ProtoLens.Prism.Prism' ReadGenesisResponse'Config Proto.Utxorpc.V1beta.Cardano.Cardano.Genesis
_ReadGenesisResponse'Cardano
  = Data.ProtoLens.Prism.prism'
      ReadGenesisResponse'Cardano
      (\ p__
         -> case p__ of
              (ReadGenesisResponse'Cardano p__val) -> Prelude.Just p__val)
{- | Fields :
     
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.fieldMask' @:: Lens' ReadParamsRequest Proto.Google.Protobuf.FieldMask.FieldMask@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.maybe'fieldMask' @:: Lens' ReadParamsRequest (Prelude.Maybe Proto.Google.Protobuf.FieldMask.FieldMask)@ -}
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
    = Data.Text.pack "utxorpc.v1beta.query.ReadParamsRequest"
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
     
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.values' @:: Lens' ReadParamsResponse AnyChainParams@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.maybe'values' @:: Lens' ReadParamsResponse (Prelude.Maybe AnyChainParams)@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.ledgerTip' @:: Lens' ReadParamsResponse ChainPoint@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.maybe'ledgerTip' @:: Lens' ReadParamsResponse (Prelude.Maybe ChainPoint)@ -}
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
    = Data.Text.pack "utxorpc.v1beta.query.ReadParamsResponse"
  packedMessageDescriptor _
    = "\n\
      \\DC2ReadParamsResponse\DC2<\n\
      \\ACKvalues\CAN\SOH \SOH(\v2$.utxorpc.v1beta.query.AnyChainParamsR\ACKvalues\DC2?\n\
      \\n\
      \ledger_tip\CAN\STX \SOH(\v2 .utxorpc.v1beta.query.ChainPointR\tledgerTip"
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
     
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.hash' @:: Lens' ReadTxRequest Data.ByteString.ByteString@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.fieldMask' @:: Lens' ReadTxRequest Proto.Google.Protobuf.FieldMask.FieldMask@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.maybe'fieldMask' @:: Lens' ReadTxRequest (Prelude.Maybe Proto.Google.Protobuf.FieldMask.FieldMask)@ -}
data ReadTxRequest
  = ReadTxRequest'_constructor {_ReadTxRequest'hash :: !Data.ByteString.ByteString,
                                _ReadTxRequest'fieldMask :: !(Prelude.Maybe Proto.Google.Protobuf.FieldMask.FieldMask),
                                _ReadTxRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ReadTxRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ReadTxRequest "hash" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReadTxRequest'hash (\ x__ y__ -> x__ {_ReadTxRequest'hash = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ReadTxRequest "fieldMask" Proto.Google.Protobuf.FieldMask.FieldMask where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReadTxRequest'fieldMask
           (\ x__ y__ -> x__ {_ReadTxRequest'fieldMask = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ReadTxRequest "maybe'fieldMask" (Prelude.Maybe Proto.Google.Protobuf.FieldMask.FieldMask) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReadTxRequest'fieldMask
           (\ x__ y__ -> x__ {_ReadTxRequest'fieldMask = y__}))
        Prelude.id
instance Data.ProtoLens.Message ReadTxRequest where
  messageName _ = Data.Text.pack "utxorpc.v1beta.query.ReadTxRequest"
  packedMessageDescriptor _
    = "\n\
      \\rReadTxRequest\DC2\DC2\n\
      \\EOThash\CAN\SOH \SOH(\fR\EOThash\DC29\n\
      \\n\
      \field_mask\CAN\STX \SOH(\v2\SUB.google.protobuf.FieldMaskR\tfieldMask"
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
              Data.ProtoLens.FieldDescriptor ReadTxRequest
        fieldMask__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "field_mask"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Google.Protobuf.FieldMask.FieldMask)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'fieldMask")) ::
              Data.ProtoLens.FieldDescriptor ReadTxRequest
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, hash__field_descriptor),
           (Data.ProtoLens.Tag 2, fieldMask__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ReadTxRequest'_unknownFields
        (\ x__ y__ -> x__ {_ReadTxRequest'_unknownFields = y__})
  defMessage
    = ReadTxRequest'_constructor
        {_ReadTxRequest'hash = Data.ProtoLens.fieldDefault,
         _ReadTxRequest'fieldMask = Prelude.Nothing,
         _ReadTxRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ReadTxRequest -> Data.ProtoLens.Encoding.Bytes.Parser ReadTxRequest
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
          (do loop Data.ProtoLens.defMessage) "ReadTxRequest"
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
instance Control.DeepSeq.NFData ReadTxRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ReadTxRequest'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ReadTxRequest'hash x__)
                (Control.DeepSeq.deepseq (_ReadTxRequest'fieldMask x__) ()))
{- | Fields :
     
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.tx' @:: Lens' ReadTxResponse AnyChainTx@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.maybe'tx' @:: Lens' ReadTxResponse (Prelude.Maybe AnyChainTx)@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.ledgerTip' @:: Lens' ReadTxResponse ChainPoint@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.maybe'ledgerTip' @:: Lens' ReadTxResponse (Prelude.Maybe ChainPoint)@ -}
data ReadTxResponse
  = ReadTxResponse'_constructor {_ReadTxResponse'tx :: !(Prelude.Maybe AnyChainTx),
                                 _ReadTxResponse'ledgerTip :: !(Prelude.Maybe ChainPoint),
                                 _ReadTxResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ReadTxResponse where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ReadTxResponse "tx" AnyChainTx where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReadTxResponse'tx (\ x__ y__ -> x__ {_ReadTxResponse'tx = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ReadTxResponse "maybe'tx" (Prelude.Maybe AnyChainTx) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReadTxResponse'tx (\ x__ y__ -> x__ {_ReadTxResponse'tx = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ReadTxResponse "ledgerTip" ChainPoint where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReadTxResponse'ledgerTip
           (\ x__ y__ -> x__ {_ReadTxResponse'ledgerTip = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ReadTxResponse "maybe'ledgerTip" (Prelude.Maybe ChainPoint) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReadTxResponse'ledgerTip
           (\ x__ y__ -> x__ {_ReadTxResponse'ledgerTip = y__}))
        Prelude.id
instance Data.ProtoLens.Message ReadTxResponse where
  messageName _
    = Data.Text.pack "utxorpc.v1beta.query.ReadTxResponse"
  packedMessageDescriptor _
    = "\n\
      \\SOReadTxResponse\DC20\n\
      \\STXtx\CAN\SOH \SOH(\v2 .utxorpc.v1beta.query.AnyChainTxR\STXtx\DC2?\n\
      \\n\
      \ledger_tip\CAN\STX \SOH(\v2 .utxorpc.v1beta.query.ChainPointR\tledgerTip"
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
              Data.ProtoLens.FieldDescriptor ReadTxResponse
        ledgerTip__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "ledger_tip"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ChainPoint)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'ledgerTip")) ::
              Data.ProtoLens.FieldDescriptor ReadTxResponse
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, tx__field_descriptor),
           (Data.ProtoLens.Tag 2, ledgerTip__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ReadTxResponse'_unknownFields
        (\ x__ y__ -> x__ {_ReadTxResponse'_unknownFields = y__})
  defMessage
    = ReadTxResponse'_constructor
        {_ReadTxResponse'tx = Prelude.Nothing,
         _ReadTxResponse'ledgerTip = Prelude.Nothing,
         _ReadTxResponse'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ReadTxResponse
          -> Data.ProtoLens.Encoding.Bytes.Parser ReadTxResponse
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
          (do loop Data.ProtoLens.defMessage) "ReadTxResponse"
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
instance Control.DeepSeq.NFData ReadTxResponse where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ReadTxResponse'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ReadTxResponse'tx x__)
                (Control.DeepSeq.deepseq (_ReadTxResponse'ledgerTip x__) ()))
{- | Fields :
     
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.keys' @:: Lens' ReadUtxosRequest [TxoRef]@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.vec'keys' @:: Lens' ReadUtxosRequest (Data.Vector.Vector TxoRef)@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.fieldMask' @:: Lens' ReadUtxosRequest Proto.Google.Protobuf.FieldMask.FieldMask@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.maybe'fieldMask' @:: Lens' ReadUtxosRequest (Prelude.Maybe Proto.Google.Protobuf.FieldMask.FieldMask)@ -}
data ReadUtxosRequest
  = ReadUtxosRequest'_constructor {_ReadUtxosRequest'keys :: !(Data.Vector.Vector TxoRef),
                                   _ReadUtxosRequest'fieldMask :: !(Prelude.Maybe Proto.Google.Protobuf.FieldMask.FieldMask),
                                   _ReadUtxosRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ReadUtxosRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ReadUtxosRequest "keys" [TxoRef] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReadUtxosRequest'keys
           (\ x__ y__ -> x__ {_ReadUtxosRequest'keys = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ReadUtxosRequest "vec'keys" (Data.Vector.Vector TxoRef) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReadUtxosRequest'keys
           (\ x__ y__ -> x__ {_ReadUtxosRequest'keys = y__}))
        Prelude.id
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
instance Data.ProtoLens.Message ReadUtxosRequest where
  messageName _
    = Data.Text.pack "utxorpc.v1beta.query.ReadUtxosRequest"
  packedMessageDescriptor _
    = "\n\
      \\DLEReadUtxosRequest\DC20\n\
      \\EOTkeys\CAN\SOH \ETX(\v2\FS.utxorpc.v1beta.query.TxoRefR\EOTkeys\DC29\n\
      \\n\
      \field_mask\CAN\STX \SOH(\v2\SUB.google.protobuf.FieldMaskR\tfieldMask"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        keys__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "keys"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor TxoRef)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"keys")) ::
              Data.ProtoLens.FieldDescriptor ReadUtxosRequest
        fieldMask__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "field_mask"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Google.Protobuf.FieldMask.FieldMask)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'fieldMask")) ::
              Data.ProtoLens.FieldDescriptor ReadUtxosRequest
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, keys__field_descriptor),
           (Data.ProtoLens.Tag 2, fieldMask__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ReadUtxosRequest'_unknownFields
        (\ x__ y__ -> x__ {_ReadUtxosRequest'_unknownFields = y__})
  defMessage
    = ReadUtxosRequest'_constructor
        {_ReadUtxosRequest'keys = Data.Vector.Generic.empty,
         _ReadUtxosRequest'fieldMask = Prelude.Nothing,
         _ReadUtxosRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ReadUtxosRequest
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld TxoRef
             -> Data.ProtoLens.Encoding.Bytes.Parser ReadUtxosRequest
        loop x mutable'keys
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'keys <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.unsafeFreeze mutable'keys)
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
                              (Data.ProtoLens.Field.field @"vec'keys") frozen'keys x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "keys"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'keys y)
                                loop x v
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "field_mask"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"fieldMask") y x)
                                  mutable'keys
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'keys
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'keys <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'keys)
          "ReadUtxosRequest"
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
                (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'keys") _x))
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
instance Control.DeepSeq.NFData ReadUtxosRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ReadUtxosRequest'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ReadUtxosRequest'keys x__)
                (Control.DeepSeq.deepseq (_ReadUtxosRequest'fieldMask x__) ()))
{- | Fields :
     
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.items' @:: Lens' ReadUtxosResponse [AnyUtxoData]@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.vec'items' @:: Lens' ReadUtxosResponse (Data.Vector.Vector AnyUtxoData)@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.ledgerTip' @:: Lens' ReadUtxosResponse ChainPoint@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.maybe'ledgerTip' @:: Lens' ReadUtxosResponse (Prelude.Maybe ChainPoint)@ -}
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
    = Data.Text.pack "utxorpc.v1beta.query.ReadUtxosResponse"
  packedMessageDescriptor _
    = "\n\
      \\DC1ReadUtxosResponse\DC27\n\
      \\ENQitems\CAN\SOH \ETX(\v2!.utxorpc.v1beta.query.AnyUtxoDataR\ENQitems\DC2?\n\
      \\n\
      \ledger_tip\CAN\STX \SOH(\v2 .utxorpc.v1beta.query.ChainPointR\tledgerTip"
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
     
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.predicate' @:: Lens' SearchUtxosRequest UtxoPredicate@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.maybe'predicate' @:: Lens' SearchUtxosRequest (Prelude.Maybe UtxoPredicate)@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.fieldMask' @:: Lens' SearchUtxosRequest Proto.Google.Protobuf.FieldMask.FieldMask@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.maybe'fieldMask' @:: Lens' SearchUtxosRequest (Prelude.Maybe Proto.Google.Protobuf.FieldMask.FieldMask)@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.maxItems' @:: Lens' SearchUtxosRequest Data.Int.Int32@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.startToken' @:: Lens' SearchUtxosRequest Data.Text.Text@ -}
data SearchUtxosRequest
  = SearchUtxosRequest'_constructor {_SearchUtxosRequest'predicate :: !(Prelude.Maybe UtxoPredicate),
                                     _SearchUtxosRequest'fieldMask :: !(Prelude.Maybe Proto.Google.Protobuf.FieldMask.FieldMask),
                                     _SearchUtxosRequest'maxItems :: !Data.Int.Int32,
                                     _SearchUtxosRequest'startToken :: !Data.Text.Text,
                                     _SearchUtxosRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show SearchUtxosRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField SearchUtxosRequest "predicate" UtxoPredicate where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SearchUtxosRequest'predicate
           (\ x__ y__ -> x__ {_SearchUtxosRequest'predicate = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField SearchUtxosRequest "maybe'predicate" (Prelude.Maybe UtxoPredicate) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SearchUtxosRequest'predicate
           (\ x__ y__ -> x__ {_SearchUtxosRequest'predicate = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField SearchUtxosRequest "fieldMask" Proto.Google.Protobuf.FieldMask.FieldMask where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SearchUtxosRequest'fieldMask
           (\ x__ y__ -> x__ {_SearchUtxosRequest'fieldMask = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField SearchUtxosRequest "maybe'fieldMask" (Prelude.Maybe Proto.Google.Protobuf.FieldMask.FieldMask) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SearchUtxosRequest'fieldMask
           (\ x__ y__ -> x__ {_SearchUtxosRequest'fieldMask = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField SearchUtxosRequest "maxItems" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SearchUtxosRequest'maxItems
           (\ x__ y__ -> x__ {_SearchUtxosRequest'maxItems = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField SearchUtxosRequest "startToken" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SearchUtxosRequest'startToken
           (\ x__ y__ -> x__ {_SearchUtxosRequest'startToken = y__}))
        Prelude.id
instance Data.ProtoLens.Message SearchUtxosRequest where
  messageName _
    = Data.Text.pack "utxorpc.v1beta.query.SearchUtxosRequest"
  packedMessageDescriptor _
    = "\n\
      \\DC2SearchUtxosRequest\DC2A\n\
      \\tpredicate\CAN\SOH \SOH(\v2#.utxorpc.v1beta.query.UtxoPredicateR\tpredicate\DC29\n\
      \\n\
      \field_mask\CAN\STX \SOH(\v2\SUB.google.protobuf.FieldMaskR\tfieldMask\DC2\ESC\n\
      \\tmax_items\CAN\ETX \SOH(\ENQR\bmaxItems\DC2\US\n\
      \\vstart_token\CAN\EOT \SOH(\tR\n\
      \startToken"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        predicate__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "predicate"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor UtxoPredicate)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'predicate")) ::
              Data.ProtoLens.FieldDescriptor SearchUtxosRequest
        fieldMask__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "field_mask"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Google.Protobuf.FieldMask.FieldMask)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'fieldMask")) ::
              Data.ProtoLens.FieldDescriptor SearchUtxosRequest
        maxItems__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "max_items"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"maxItems")) ::
              Data.ProtoLens.FieldDescriptor SearchUtxosRequest
        startToken__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "start_token"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"startToken")) ::
              Data.ProtoLens.FieldDescriptor SearchUtxosRequest
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, predicate__field_descriptor),
           (Data.ProtoLens.Tag 2, fieldMask__field_descriptor),
           (Data.ProtoLens.Tag 3, maxItems__field_descriptor),
           (Data.ProtoLens.Tag 4, startToken__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _SearchUtxosRequest'_unknownFields
        (\ x__ y__ -> x__ {_SearchUtxosRequest'_unknownFields = y__})
  defMessage
    = SearchUtxosRequest'_constructor
        {_SearchUtxosRequest'predicate = Prelude.Nothing,
         _SearchUtxosRequest'fieldMask = Prelude.Nothing,
         _SearchUtxosRequest'maxItems = Data.ProtoLens.fieldDefault,
         _SearchUtxosRequest'startToken = Data.ProtoLens.fieldDefault,
         _SearchUtxosRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          SearchUtxosRequest
          -> Data.ProtoLens.Encoding.Bytes.Parser SearchUtxosRequest
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
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "start_token"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"startToken") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "SearchUtxosRequest"
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
                      (let
                         _v
                           = Lens.Family2.view (Data.ProtoLens.Field.field @"startToken") _x
                       in
                         if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                             Data.Monoid.mempty
                         else
                             (Data.Monoid.<>)
                               (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                               ((Prelude..)
                                  (\ bs
                                     -> (Data.Monoid.<>)
                                          (Data.ProtoLens.Encoding.Bytes.putVarInt
                                             (Prelude.fromIntegral (Data.ByteString.length bs)))
                                          (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                  Data.Text.Encoding.encodeUtf8 _v))
                      (Data.ProtoLens.Encoding.Wire.buildFieldSet
                         (Lens.Family2.view Data.ProtoLens.unknownFields _x)))))
instance Control.DeepSeq.NFData SearchUtxosRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_SearchUtxosRequest'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_SearchUtxosRequest'predicate x__)
                (Control.DeepSeq.deepseq
                   (_SearchUtxosRequest'fieldMask x__)
                   (Control.DeepSeq.deepseq
                      (_SearchUtxosRequest'maxItems x__)
                      (Control.DeepSeq.deepseq
                         (_SearchUtxosRequest'startToken x__) ()))))
{- | Fields :
     
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.items' @:: Lens' SearchUtxosResponse [AnyUtxoData]@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.vec'items' @:: Lens' SearchUtxosResponse (Data.Vector.Vector AnyUtxoData)@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.ledgerTip' @:: Lens' SearchUtxosResponse ChainPoint@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.maybe'ledgerTip' @:: Lens' SearchUtxosResponse (Prelude.Maybe ChainPoint)@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.nextToken' @:: Lens' SearchUtxosResponse Data.Text.Text@ -}
data SearchUtxosResponse
  = SearchUtxosResponse'_constructor {_SearchUtxosResponse'items :: !(Data.Vector.Vector AnyUtxoData),
                                      _SearchUtxosResponse'ledgerTip :: !(Prelude.Maybe ChainPoint),
                                      _SearchUtxosResponse'nextToken :: !Data.Text.Text,
                                      _SearchUtxosResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show SearchUtxosResponse where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField SearchUtxosResponse "items" [AnyUtxoData] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SearchUtxosResponse'items
           (\ x__ y__ -> x__ {_SearchUtxosResponse'items = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField SearchUtxosResponse "vec'items" (Data.Vector.Vector AnyUtxoData) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SearchUtxosResponse'items
           (\ x__ y__ -> x__ {_SearchUtxosResponse'items = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField SearchUtxosResponse "ledgerTip" ChainPoint where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SearchUtxosResponse'ledgerTip
           (\ x__ y__ -> x__ {_SearchUtxosResponse'ledgerTip = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField SearchUtxosResponse "maybe'ledgerTip" (Prelude.Maybe ChainPoint) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SearchUtxosResponse'ledgerTip
           (\ x__ y__ -> x__ {_SearchUtxosResponse'ledgerTip = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField SearchUtxosResponse "nextToken" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SearchUtxosResponse'nextToken
           (\ x__ y__ -> x__ {_SearchUtxosResponse'nextToken = y__}))
        Prelude.id
instance Data.ProtoLens.Message SearchUtxosResponse where
  messageName _
    = Data.Text.pack "utxorpc.v1beta.query.SearchUtxosResponse"
  packedMessageDescriptor _
    = "\n\
      \\DC3SearchUtxosResponse\DC27\n\
      \\ENQitems\CAN\SOH \ETX(\v2!.utxorpc.v1beta.query.AnyUtxoDataR\ENQitems\DC2?\n\
      \\n\
      \ledger_tip\CAN\STX \SOH(\v2 .utxorpc.v1beta.query.ChainPointR\tledgerTip\DC2\GS\n\
      \\n\
      \next_token\CAN\ETX \SOH(\tR\tnextToken"
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
              Data.ProtoLens.FieldDescriptor SearchUtxosResponse
        ledgerTip__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "ledger_tip"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ChainPoint)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'ledgerTip")) ::
              Data.ProtoLens.FieldDescriptor SearchUtxosResponse
        nextToken__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "next_token"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"nextToken")) ::
              Data.ProtoLens.FieldDescriptor SearchUtxosResponse
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, items__field_descriptor),
           (Data.ProtoLens.Tag 2, ledgerTip__field_descriptor),
           (Data.ProtoLens.Tag 3, nextToken__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _SearchUtxosResponse'_unknownFields
        (\ x__ y__ -> x__ {_SearchUtxosResponse'_unknownFields = y__})
  defMessage
    = SearchUtxosResponse'_constructor
        {_SearchUtxosResponse'items = Data.Vector.Generic.empty,
         _SearchUtxosResponse'ledgerTip = Prelude.Nothing,
         _SearchUtxosResponse'nextToken = Data.ProtoLens.fieldDefault,
         _SearchUtxosResponse'_unknownFields = []}
  parseMessage
    = let
        loop ::
          SearchUtxosResponse
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld AnyUtxoData
             -> Data.ProtoLens.Encoding.Bytes.Parser SearchUtxosResponse
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
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "next_token"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"nextToken") y x)
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
          "SearchUtxosResponse"
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
                ((Data.Monoid.<>)
                   (let
                      _v = Lens.Family2.view (Data.ProtoLens.Field.field @"nextToken") _x
                    in
                      if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty
                      else
                          (Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                            ((Prelude..)
                               (\ bs
                                  -> (Data.Monoid.<>)
                                       (Data.ProtoLens.Encoding.Bytes.putVarInt
                                          (Prelude.fromIntegral (Data.ByteString.length bs)))
                                       (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                               Data.Text.Encoding.encodeUtf8 _v))
                   (Data.ProtoLens.Encoding.Wire.buildFieldSet
                      (Lens.Family2.view Data.ProtoLens.unknownFields _x))))
instance Control.DeepSeq.NFData SearchUtxosResponse where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_SearchUtxosResponse'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_SearchUtxosResponse'items x__)
                (Control.DeepSeq.deepseq
                   (_SearchUtxosResponse'ledgerTip x__)
                   (Control.DeepSeq.deepseq (_SearchUtxosResponse'nextToken x__) ())))
{- | Fields :
     
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.hash' @:: Lens' TxoRef Data.ByteString.ByteString@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.index' @:: Lens' TxoRef Data.Word.Word32@ -}
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
  messageName _ = Data.Text.pack "utxorpc.v1beta.query.TxoRef"
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
     
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.match' @:: Lens' UtxoPredicate AnyUtxoPattern@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.maybe'match' @:: Lens' UtxoPredicate (Prelude.Maybe AnyUtxoPattern)@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.not' @:: Lens' UtxoPredicate [UtxoPredicate]@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.vec'not' @:: Lens' UtxoPredicate (Data.Vector.Vector UtxoPredicate)@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.allOf' @:: Lens' UtxoPredicate [UtxoPredicate]@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.vec'allOf' @:: Lens' UtxoPredicate (Data.Vector.Vector UtxoPredicate)@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.anyOf' @:: Lens' UtxoPredicate [UtxoPredicate]@
         * 'Proto.Utxorpc.V1beta.Query.Query_Fields.vec'anyOf' @:: Lens' UtxoPredicate (Data.Vector.Vector UtxoPredicate)@ -}
data UtxoPredicate
  = UtxoPredicate'_constructor {_UtxoPredicate'match :: !(Prelude.Maybe AnyUtxoPattern),
                                _UtxoPredicate'not :: !(Data.Vector.Vector UtxoPredicate),
                                _UtxoPredicate'allOf :: !(Data.Vector.Vector UtxoPredicate),
                                _UtxoPredicate'anyOf :: !(Data.Vector.Vector UtxoPredicate),
                                _UtxoPredicate'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show UtxoPredicate where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField UtxoPredicate "match" AnyUtxoPattern where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _UtxoPredicate'match
           (\ x__ y__ -> x__ {_UtxoPredicate'match = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField UtxoPredicate "maybe'match" (Prelude.Maybe AnyUtxoPattern) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _UtxoPredicate'match
           (\ x__ y__ -> x__ {_UtxoPredicate'match = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField UtxoPredicate "not" [UtxoPredicate] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _UtxoPredicate'not (\ x__ y__ -> x__ {_UtxoPredicate'not = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField UtxoPredicate "vec'not" (Data.Vector.Vector UtxoPredicate) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _UtxoPredicate'not (\ x__ y__ -> x__ {_UtxoPredicate'not = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField UtxoPredicate "allOf" [UtxoPredicate] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _UtxoPredicate'allOf
           (\ x__ y__ -> x__ {_UtxoPredicate'allOf = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField UtxoPredicate "vec'allOf" (Data.Vector.Vector UtxoPredicate) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _UtxoPredicate'allOf
           (\ x__ y__ -> x__ {_UtxoPredicate'allOf = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField UtxoPredicate "anyOf" [UtxoPredicate] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _UtxoPredicate'anyOf
           (\ x__ y__ -> x__ {_UtxoPredicate'anyOf = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField UtxoPredicate "vec'anyOf" (Data.Vector.Vector UtxoPredicate) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _UtxoPredicate'anyOf
           (\ x__ y__ -> x__ {_UtxoPredicate'anyOf = y__}))
        Prelude.id
instance Data.ProtoLens.Message UtxoPredicate where
  messageName _ = Data.Text.pack "utxorpc.v1beta.query.UtxoPredicate"
  packedMessageDescriptor _
    = "\n\
      \\rUtxoPredicate\DC2:\n\
      \\ENQmatch\CAN\SOH \SOH(\v2$.utxorpc.v1beta.query.AnyUtxoPatternR\ENQmatch\DC25\n\
      \\ETXnot\CAN\STX \ETX(\v2#.utxorpc.v1beta.query.UtxoPredicateR\ETXnot\DC2:\n\
      \\ACKall_of\CAN\ETX \ETX(\v2#.utxorpc.v1beta.query.UtxoPredicateR\ENQallOf\DC2:\n\
      \\ACKany_of\CAN\EOT \ETX(\v2#.utxorpc.v1beta.query.UtxoPredicateR\ENQanyOf"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        match__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "match"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor AnyUtxoPattern)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'match")) ::
              Data.ProtoLens.FieldDescriptor UtxoPredicate
        not__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "not"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor UtxoPredicate)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"not")) ::
              Data.ProtoLens.FieldDescriptor UtxoPredicate
        allOf__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "all_of"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor UtxoPredicate)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"allOf")) ::
              Data.ProtoLens.FieldDescriptor UtxoPredicate
        anyOf__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "any_of"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor UtxoPredicate)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"anyOf")) ::
              Data.ProtoLens.FieldDescriptor UtxoPredicate
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, match__field_descriptor),
           (Data.ProtoLens.Tag 2, not__field_descriptor),
           (Data.ProtoLens.Tag 3, allOf__field_descriptor),
           (Data.ProtoLens.Tag 4, anyOf__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _UtxoPredicate'_unknownFields
        (\ x__ y__ -> x__ {_UtxoPredicate'_unknownFields = y__})
  defMessage
    = UtxoPredicate'_constructor
        {_UtxoPredicate'match = Prelude.Nothing,
         _UtxoPredicate'not = Data.Vector.Generic.empty,
         _UtxoPredicate'allOf = Data.Vector.Generic.empty,
         _UtxoPredicate'anyOf = Data.Vector.Generic.empty,
         _UtxoPredicate'_unknownFields = []}
  parseMessage
    = let
        loop ::
          UtxoPredicate
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld UtxoPredicate
             -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld UtxoPredicate
                -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld UtxoPredicate
                   -> Data.ProtoLens.Encoding.Bytes.Parser UtxoPredicate
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
          "UtxoPredicate"
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
instance Control.DeepSeq.NFData UtxoPredicate where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_UtxoPredicate'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_UtxoPredicate'match x__)
                (Control.DeepSeq.deepseq
                   (_UtxoPredicate'not x__)
                   (Control.DeepSeq.deepseq
                      (_UtxoPredicate'allOf x__)
                      (Control.DeepSeq.deepseq (_UtxoPredicate'anyOf x__) ()))))
data QueryService = QueryService {}
instance Data.ProtoLens.Service.Types.Service QueryService where
  type ServiceName QueryService = "QueryService"
  type ServicePackage QueryService = "utxorpc.v1beta.query"
  type ServiceMethods QueryService = '["readData",
                                       "readEraSummary",
                                       "readGenesis",
                                       "readParams",
                                       "readTx",
                                       "readUtxos",
                                       "searchUtxos"]
  packedServiceDescriptor _
    = "\n\
      \\fQueryService\DC2_\n\
      \\n\
      \ReadParams\DC2'.utxorpc.v1beta.query.ReadParamsRequest\SUB(.utxorpc.v1beta.query.ReadParamsResponse\DC2\\\n\
      \\tReadUtxos\DC2&.utxorpc.v1beta.query.ReadUtxosRequest\SUB'.utxorpc.v1beta.query.ReadUtxosResponse\DC2b\n\
      \\vSearchUtxos\DC2(.utxorpc.v1beta.query.SearchUtxosRequest\SUB).utxorpc.v1beta.query.SearchUtxosResponse\DC2Y\n\
      \\bReadData\DC2%.utxorpc.v1beta.query.ReadDataRequest\SUB&.utxorpc.v1beta.query.ReadDataResponse\DC2S\n\
      \\ACKReadTx\DC2#.utxorpc.v1beta.query.ReadTxRequest\SUB$.utxorpc.v1beta.query.ReadTxResponse\DC2b\n\
      \\vReadGenesis\DC2(.utxorpc.v1beta.query.ReadGenesisRequest\SUB).utxorpc.v1beta.query.ReadGenesisResponse\DC2k\n\
      \\SOReadEraSummary\DC2+.utxorpc.v1beta.query.ReadEraSummaryRequest\SUB,.utxorpc.v1beta.query.ReadEraSummaryResponse"
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
instance Data.ProtoLens.Service.Types.HasMethodImpl QueryService "searchUtxos" where
  type MethodName QueryService "searchUtxos" = "SearchUtxos"
  type MethodInput QueryService "searchUtxos" = SearchUtxosRequest
  type MethodOutput QueryService "searchUtxos" = SearchUtxosResponse
  type MethodStreamingType QueryService "searchUtxos" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl QueryService "readData" where
  type MethodName QueryService "readData" = "ReadData"
  type MethodInput QueryService "readData" = ReadDataRequest
  type MethodOutput QueryService "readData" = ReadDataResponse
  type MethodStreamingType QueryService "readData" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl QueryService "readTx" where
  type MethodName QueryService "readTx" = "ReadTx"
  type MethodInput QueryService "readTx" = ReadTxRequest
  type MethodOutput QueryService "readTx" = ReadTxResponse
  type MethodStreamingType QueryService "readTx" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl QueryService "readGenesis" where
  type MethodName QueryService "readGenesis" = "ReadGenesis"
  type MethodInput QueryService "readGenesis" = ReadGenesisRequest
  type MethodOutput QueryService "readGenesis" = ReadGenesisResponse
  type MethodStreamingType QueryService "readGenesis" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl QueryService "readEraSummary" where
  type MethodName QueryService "readEraSummary" = "ReadEraSummary"
  type MethodInput QueryService "readEraSummary" = ReadEraSummaryRequest
  type MethodOutput QueryService "readEraSummary" = ReadEraSummaryResponse
  type MethodStreamingType QueryService "readEraSummary" = 'Data.ProtoLens.Service.Types.NonStreaming
packedFileDescriptor :: Data.ByteString.ByteString
packedFileDescriptor
  = "\n\
    \ utxorpc/v1beta/query/query.proto\DC2\DC4utxorpc.v1beta.query\SUB google/protobuf/field_mask.proto\SUB$utxorpc/v1beta/cardano/cardano.proto\"j\n\
    \\n\
    \ChainPoint\DC2\DC2\n\
    \\EOTslot\CAN\SOH \SOH(\EOTR\EOTslot\DC2\DC2\n\
    \\EOThash\CAN\STX \SOH(\fR\EOThash\DC2\SYN\n\
    \\ACKheight\CAN\ETX \SOH(\EOTR\ACKheight\DC2\FS\n\
    \\ttimestamp\CAN\EOT \SOH(\EOTR\ttimestamp\"v\n\
    \\rAnyChainBlock\DC2!\n\
    \\fnative_bytes\CAN\SOH \SOH(\fR\vnativeBytes\DC29\n\
    \\acardano\CAN\STX \SOH(\v2\GS.utxorpc.v1beta.cardano.BlockH\NULR\acardanoB\a\n\
    \\ENQchain\"2\n\
    \\ACKTxoRef\DC2\DC2\n\
    \\EOThash\CAN\SOH \SOH(\fR\EOThash\DC2\DC4\n\
    \\ENQindex\CAN\STX \SOH(\rR\ENQindex\"N\n\
    \\DC1ReadParamsRequest\DC29\n\
    \\n\
    \field_mask\CAN\SOH \SOH(\v2\SUB.google.protobuf.FieldMaskR\tfieldMask\"O\n\
    \\DC2ReadGenesisRequest\DC29\n\
    \\n\
    \field_mask\CAN\SOH \SOH(\v2\SUB.google.protobuf.FieldMaskR\tfieldMask\"R\n\
    \\NAKReadEraSummaryRequest\DC29\n\
    \\n\
    \field_mask\CAN\SOH \SOH(\v2\SUB.google.protobuf.FieldMaskR\tfieldMask\"\140\SOH\n\
    \\DC3ReadGenesisResponse\DC2\CAN\n\
    \\agenesis\CAN\SOH \SOH(\fR\agenesis\DC2\DC4\n\
    \\ENQcaip2\CAN\STX \SOH(\tR\ENQcaip2\DC2;\n\
    \\acardano\CAN\ETX \SOH(\v2\US.utxorpc.v1beta.cardano.GenesisH\NULR\acardanoB\b\n\
    \\ACKconfig\"e\n\
    \\SYNReadEraSummaryResponse\DC2@\n\
    \\acardano\CAN\SOH \SOH(\v2$.utxorpc.v1beta.cardano.EraSummariesH\NULR\acardanoB\t\n\
    \\asummary\"W\n\
    \\SOAnyChainParams\DC2;\n\
    \\acardano\CAN\SOH \SOH(\v2\US.utxorpc.v1beta.cardano.PParamsH\NULR\acardanoB\b\n\
    \\ACKparams\"\147\SOH\n\
    \\DC2ReadParamsResponse\DC2<\n\
    \\ACKvalues\CAN\SOH \SOH(\v2$.utxorpc.v1beta.query.AnyChainParamsR\ACKvalues\DC2?\n\
    \\n\
    \ledger_tip\CAN\STX \SOH(\v2 .utxorpc.v1beta.query.ChainPointR\tledgerTip\"e\n\
    \\SOAnyUtxoPattern\DC2C\n\
    \\acardano\CAN\SOH \SOH(\v2'.utxorpc.v1beta.cardano.TxOutputPatternH\NULR\acardanoB\SO\n\
    \\futxo_pattern\"\250\SOH\n\
    \\rUtxoPredicate\DC2:\n\
    \\ENQmatch\CAN\SOH \SOH(\v2$.utxorpc.v1beta.query.AnyUtxoPatternR\ENQmatch\DC25\n\
    \\ETXnot\CAN\STX \ETX(\v2#.utxorpc.v1beta.query.UtxoPredicateR\ETXnot\DC2:\n\
    \\ACKall_of\CAN\ETX \ETX(\v2#.utxorpc.v1beta.query.UtxoPredicateR\ENQallOf\DC2:\n\
    \\ACKany_of\CAN\EOT \ETX(\v2#.utxorpc.v1beta.query.UtxoPredicateR\ENQanyOf\"\244\SOH\n\
    \\vAnyUtxoData\DC2!\n\
    \\fnative_bytes\CAN\SOH \SOH(\fR\vnativeBytes\DC25\n\
    \\atxo_ref\CAN\STX \SOH(\v2\FS.utxorpc.v1beta.query.TxoRefR\ACKtxoRef\DC2<\n\
    \\acardano\CAN\ETX \SOH(\v2 .utxorpc.v1beta.cardano.TxOutputH\NULR\acardano\DC2=\n\
    \\tblock_ref\CAN\EOT \SOH(\v2 .utxorpc.v1beta.query.ChainPointR\bblockRefB\SO\n\
    \\fparsed_state\"\DEL\n\
    \\DLEReadUtxosRequest\DC20\n\
    \\EOTkeys\CAN\SOH \ETX(\v2\FS.utxorpc.v1beta.query.TxoRefR\EOTkeys\DC29\n\
    \\n\
    \field_mask\CAN\STX \SOH(\v2\SUB.google.protobuf.FieldMaskR\tfieldMask\"\141\SOH\n\
    \\DC1ReadUtxosResponse\DC27\n\
    \\ENQitems\CAN\SOH \ETX(\v2!.utxorpc.v1beta.query.AnyUtxoDataR\ENQitems\DC2?\n\
    \\n\
    \ledger_tip\CAN\STX \SOH(\v2 .utxorpc.v1beta.query.ChainPointR\tledgerTip\"\208\SOH\n\
    \\DC2SearchUtxosRequest\DC2A\n\
    \\tpredicate\CAN\SOH \SOH(\v2#.utxorpc.v1beta.query.UtxoPredicateR\tpredicate\DC29\n\
    \\n\
    \field_mask\CAN\STX \SOH(\v2\SUB.google.protobuf.FieldMaskR\tfieldMask\DC2\ESC\n\
    \\tmax_items\CAN\ETX \SOH(\ENQR\bmaxItems\DC2\US\n\
    \\vstart_token\CAN\EOT \SOH(\tR\n\
    \startToken\"\174\SOH\n\
    \\DC3SearchUtxosResponse\DC27\n\
    \\ENQitems\CAN\SOH \ETX(\v2!.utxorpc.v1beta.query.AnyUtxoDataR\ENQitems\DC2?\n\
    \\n\
    \ledger_tip\CAN\STX \SOH(\v2 .utxorpc.v1beta.query.ChainPointR\tledgerTip\DC2\GS\n\
    \\n\
    \next_token\CAN\ETX \SOH(\tR\tnextToken\"`\n\
    \\SIReadDataRequest\DC2\DC2\n\
    \\EOTkeys\CAN\SOH \ETX(\fR\EOTkeys\DC29\n\
    \\n\
    \field_mask\CAN\STX \SOH(\v2\SUB.google.protobuf.FieldMaskR\tfieldMask\"\148\SOH\n\
    \\rAnyChainDatum\DC2!\n\
    \\fnative_bytes\CAN\SOH \SOH(\fR\vnativeBytes\DC2\DLE\n\
    \\ETXkey\CAN\STX \SOH(\fR\ETXkey\DC2>\n\
    \\acardano\CAN\ETX \SOH(\v2\".utxorpc.v1beta.cardano.PlutusDataH\NULR\acardanoB\SO\n\
    \\fparsed_state\"\144\SOH\n\
    \\DLEReadDataResponse\DC2;\n\
    \\ACKvalues\CAN\SOH \ETX(\v2#.utxorpc.v1beta.query.AnyChainDatumR\ACKvalues\DC2?\n\
    \\n\
    \ledger_tip\CAN\STX \SOH(\v2 .utxorpc.v1beta.query.ChainPointR\tledgerTip\"^\n\
    \\rReadTxRequest\DC2\DC2\n\
    \\EOThash\CAN\SOH \SOH(\fR\EOThash\DC29\n\
    \\n\
    \field_mask\CAN\STX \SOH(\v2\SUB.google.protobuf.FieldMaskR\tfieldMask\"\175\SOH\n\
    \\n\
    \AnyChainTx\DC2!\n\
    \\fnative_bytes\CAN\SOH \SOH(\fR\vnativeBytes\DC26\n\
    \\acardano\CAN\STX \SOH(\v2\SUB.utxorpc.v1beta.cardano.TxH\NULR\acardano\DC2=\n\
    \\tblock_ref\CAN\ETX \SOH(\v2 .utxorpc.v1beta.query.ChainPointR\bblockRefB\a\n\
    \\ENQchain\"\131\SOH\n\
    \\SOReadTxResponse\DC20\n\
    \\STXtx\CAN\SOH \SOH(\v2 .utxorpc.v1beta.query.AnyChainTxR\STXtx\DC2?\n\
    \\n\
    \ledger_tip\CAN\STX \SOH(\v2 .utxorpc.v1beta.query.ChainPointR\tledgerTip2\178\ENQ\n\
    \\fQueryService\DC2_\n\
    \\n\
    \ReadParams\DC2'.utxorpc.v1beta.query.ReadParamsRequest\SUB(.utxorpc.v1beta.query.ReadParamsResponse\DC2\\\n\
    \\tReadUtxos\DC2&.utxorpc.v1beta.query.ReadUtxosRequest\SUB'.utxorpc.v1beta.query.ReadUtxosResponse\DC2b\n\
    \\vSearchUtxos\DC2(.utxorpc.v1beta.query.SearchUtxosRequest\SUB).utxorpc.v1beta.query.SearchUtxosResponse\DC2Y\n\
    \\bReadData\DC2%.utxorpc.v1beta.query.ReadDataRequest\SUB&.utxorpc.v1beta.query.ReadDataResponse\DC2S\n\
    \\ACKReadTx\DC2#.utxorpc.v1beta.query.ReadTxRequest\SUB$.utxorpc.v1beta.query.ReadTxResponse\DC2b\n\
    \\vReadGenesis\DC2(.utxorpc.v1beta.query.ReadGenesisRequest\SUB).utxorpc.v1beta.query.ReadGenesisResponse\DC2k\n\
    \\SOReadEraSummary\DC2+.utxorpc.v1beta.query.ReadEraSummaryRequest\SUB,.utxorpc.v1beta.query.ReadEraSummaryResponseB\152\SOH\n\
    \\CANcom.utxorpc.v1beta.queryB\n\
    \QueryProtoP\SOH\162\STX\ETXUVQ\170\STX\DC4Utxorpc.V1beta.Query\202\STX\DC4Utxorpc\\V1beta\\Query\226\STX Utxorpc\\V1beta\\Query\\GPBMetadata\234\STX\SYNUtxorpc::V1beta::QueryJ\188=\n\
    \\a\DC2\ENQ\STX\NUL\181\SOH\SOH\n\
    \9\n\
    \\SOH\f\DC2\ETX\STX\NUL\DC22// A consistent view of the state of the ledger\n\
    \\n\
    \\b\n\
    \\SOH\STX\DC2\ETX\EOT\NUL\GS\n\
    \\t\n\
    \\STX\ETX\NUL\DC2\ETX\ACK\NUL*\n\
    \\t\n\
    \\STX\ETX\SOH\DC2\ETX\a\NUL.\n\
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
    \\n\
    \\n\
    \\STX\EOT\SOH\DC2\EOT\DC1\NUL\SYN\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\SOH\SOH\DC2\ETX\DC1\b\NAK\n\
    \5\n\
    \\EOT\EOT\SOH\STX\NUL\DC2\ETX\DC2\STX\EM\"( Original bytes as defined by the chain\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ENQ\DC2\ETX\DC2\STX\a\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\SOH\DC2\ETX\DC2\b\DC4\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ETX\DC2\ETX\DC2\ETB\CAN\n\
    \\f\n\
    \\EOT\EOT\SOH\b\NUL\DC2\EOT\DC3\STX\NAK\ETX\n\
    \\f\n\
    \\ENQ\EOT\SOH\b\NUL\SOH\DC2\ETX\DC3\b\r\n\
    \&\n\
    \\EOT\EOT\SOH\STX\SOH\DC2\ETX\DC4\EOT-\"\EM A parsed Cardano block.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\ACK\DC2\ETX\DC4\EOT \n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\SOH\DC2\ETX\DC4!(\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\ETX\DC2\ETX\DC4+,\n\
    \<\n\
    \\STX\EOT\STX\DC2\EOT\EM\NUL\FS\SOH\SUB0 Represents a reference to a transaction output\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\STX\SOH\DC2\ETX\EM\b\SO\n\
    \\ETB\n\
    \\EOT\EOT\STX\STX\NUL\DC2\ETX\SUB\STX\DC1\"\n\
    \ Tx hash.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ENQ\DC2\ETX\SUB\STX\a\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\SOH\DC2\ETX\SUB\b\f\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ETX\DC2\ETX\SUB\SI\DLE\n\
    \\FS\n\
    \\EOT\EOT\STX\STX\SOH\DC2\ETX\ESC\STX\DC3\"\SI Output index.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\ENQ\DC2\ETX\ESC\STX\b\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\SOH\DC2\ETX\ESC\t\SO\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\ETX\DC2\ETX\ESC\DC1\DC2\n\
    \1\n\
    \\STX\EOT\ETX\DC2\EOT\US\NUL!\SOH\SUB% Request to get the chain parameters\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\ETX\SOH\DC2\ETX\US\b\EM\n\
    \N\n\
    \\EOT\EOT\ETX\STX\NUL\DC2\ETX \STX+\"A Field mask to selectively return fields in the parsed response.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\ACK\DC2\ETX \STX\ESC\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\SOH\DC2\ETX \FS&\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\ETX\DC2\ETX )*\n\
    \-\n\
    \\STX\EOT\EOT\DC2\EOT$\NUL&\SOH\SUB! Request to get the chain config\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\EOT\SOH\DC2\ETX$\b\SUB\n\
    \N\n\
    \\EOT\EOT\EOT\STX\NUL\DC2\ETX%\STX+\"A Field mask to selectively return fields in the parsed response.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\ACK\DC2\ETX%\STX\ESC\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\SOH\DC2\ETX%\FS&\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\ETX\DC2\ETX%)*\n\
    \,\n\
    \\STX\EOT\ENQ\DC2\EOT)\NUL+\SOH\SUB  Request to get the era summary\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\ENQ\SOH\DC2\ETX)\b\GS\n\
    \N\n\
    \\EOT\EOT\ENQ\STX\NUL\DC2\ETX*\STX+\"A Field mask to selectively return fields in the parsed response.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\NUL\ACK\DC2\ETX*\STX\ESC\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\NUL\SOH\DC2\ETX*\FS&\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\NUL\ETX\DC2\ETX*)*\n\
    \5\n\
    \\STX\EOT\ACK\DC2\EOT.\NUL4\SOH\SUB) Response containing the genesis configs\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\ACK\SOH\DC2\ETX.\b\ESC\n\
    \)\n\
    \\EOT\EOT\ACK\STX\NUL\DC2\ETX/\STX\DC4\"\FS genesis hash for the chain\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\NUL\ENQ\DC2\ETX/\STX\a\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\NUL\SOH\DC2\ETX/\b\SI\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\NUL\ETX\DC2\ETX/\DC2\DC3\n\
    \-\n\
    \\EOT\EOT\ACK\STX\SOH\DC2\ETX0\STX\DC3\"  the caip-2 ID for this network\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\SOH\ENQ\DC2\ETX0\STX\b\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\SOH\SOH\DC2\ETX0\t\SO\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\SOH\ETX\DC2\ETX0\DC1\DC2\n\
    \\f\n\
    \\EOT\EOT\ACK\b\NUL\DC2\EOT1\STX3\ETX\n\
    \\f\n\
    \\ENQ\EOT\ACK\b\NUL\SOH\DC2\ETX1\b\SO\n\
    \\RS\n\
    \\EOT\EOT\ACK\STX\STX\DC2\ETX2\EOT/\"\DC1 Cardano genesis\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\STX\ACK\DC2\ETX2\EOT\"\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\STX\SOH\DC2\ETX2#*\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\STX\ETX\DC2\ETX2-.\n\
    \3\n\
    \\STX\EOT\a\DC2\EOT7\NUL;\SOH\SUB' Response containing the era summaries\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\a\SOH\DC2\ETX7\b\RS\n\
    \\f\n\
    \\EOT\EOT\a\b\NUL\DC2\EOT8\STX:\ETX\n\
    \\f\n\
    \\ENQ\EOT\a\b\NUL\SOH\DC2\ETX8\b\SI\n\
    \$\n\
    \\EOT\EOT\a\STX\NUL\DC2\ETX9\EOT4\"\ETB Cardano era summaries\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\a\STX\NUL\ACK\DC2\ETX9\EOT'\n\
    \\f\n\
    \\ENQ\EOT\a\STX\NUL\SOH\DC2\ETX9(/\n\
    \\f\n\
    \\ENQ\EOT\a\STX\NUL\ETX\DC2\ETX923\n\
    \U\n\
    \\STX\EOT\b\DC2\EOT>\NULB\SOH\SUBI An evenlope that holds parameter data from any of the compatible chains\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\b\SOH\DC2\ETX>\b\SYN\n\
    \\f\n\
    \\EOT\EOT\b\b\NUL\DC2\EOT?\STXA\ETX\n\
    \\f\n\
    \\ENQ\EOT\b\b\NUL\SOH\DC2\ETX?\b\SO\n\
    \!\n\
    \\EOT\EOT\b\STX\NUL\DC2\ETX@\EOT/\"\DC4 Cardano parameters\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\b\STX\NUL\ACK\DC2\ETX@\EOT\"\n\
    \\f\n\
    \\ENQ\EOT\b\STX\NUL\SOH\DC2\ETX@#*\n\
    \\f\n\
    \\ENQ\EOT\b\STX\NUL\ETX\DC2\ETX@-.\n\
    \6\n\
    \\STX\EOT\t\DC2\EOTE\NULH\SOH\SUB* Response containing the chain parameters\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\t\SOH\DC2\ETXE\b\SUB\n\
    \+\n\
    \\EOT\EOT\t\STX\NUL\DC2\ETXF\STX\FS\"\RS The value of the parameters.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\t\STX\NUL\ACK\DC2\ETXF\STX\DLE\n\
    \\f\n\
    \\ENQ\EOT\t\STX\NUL\SOH\DC2\ETXF\DC1\ETB\n\
    \\f\n\
    \\ENQ\EOT\t\STX\NUL\ETX\DC2\ETXF\SUB\ESC\n\
    \J\n\
    \\EOT\EOT\t\STX\SOH\DC2\ETXG\STX\FS\"= The chain point that represent the ledger current position.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\t\STX\SOH\ACK\DC2\ETXG\STX\f\n\
    \\f\n\
    \\ENQ\EOT\t\STX\SOH\SOH\DC2\ETXG\r\ETB\n\
    \\f\n\
    \\ENQ\EOT\t\STX\SOH\ETX\DC2\ETXG\SUB\ESC\n\
    \S\n\
    \\STX\EOT\n\
    \\DC2\EOTK\NULO\SOH\SUBG An evenlope that holds an UTxO patterns from any of compatible chains\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\n\
    \\SOH\DC2\ETXK\b\SYN\n\
    \\f\n\
    \\EOT\EOT\n\
    \\b\NUL\DC2\EOTL\STXN\ETX\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\b\NUL\SOH\DC2\ETXL\b\DC4\n\
    \\v\n\
    \\EOT\EOT\n\
    \\STX\NUL\DC2\ETXM\EOT7\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\NUL\ACK\DC2\ETXM\EOT*\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\NUL\SOH\DC2\ETXM+2\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\NUL\ETX\DC2\ETXM56\n\
    \^\n\
    \\STX\EOT\v\DC2\EOTR\NULW\SOH\SUBR Represents a simple utxo predicate that can composed to create more complex ones\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\v\SOH\DC2\ETXR\b\NAK\n\
    \8\n\
    \\EOT\EOT\v\STX\NUL\DC2\ETXS\STX\ESC\"+ Predicate is true if tx exhibits pattern.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\v\STX\NUL\ACK\DC2\ETXS\STX\DLE\n\
    \\f\n\
    \\ENQ\EOT\v\STX\NUL\SOH\DC2\ETXS\DC1\SYN\n\
    \\f\n\
    \\ENQ\EOT\v\STX\NUL\ETX\DC2\ETXS\EM\SUB\n\
    \?\n\
    \\EOT\EOT\v\STX\SOH\DC2\ETXT\STX!\"2 Predicate is true if tx doesn't exhibit pattern.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\v\STX\SOH\EOT\DC2\ETXT\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\v\STX\SOH\ACK\DC2\ETXT\v\CAN\n\
    \\f\n\
    \\ENQ\EOT\v\STX\SOH\SOH\DC2\ETXT\EM\FS\n\
    \\f\n\
    \\ENQ\EOT\v\STX\SOH\ETX\DC2\ETXT\US \n\
    \F\n\
    \\EOT\EOT\v\STX\STX\DC2\ETXU\STX$\"9 Predicate is true if utxo exhibits all of the patterns.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\v\STX\STX\EOT\DC2\ETXU\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\v\STX\STX\ACK\DC2\ETXU\v\CAN\n\
    \\f\n\
    \\ENQ\EOT\v\STX\STX\SOH\DC2\ETXU\EM\US\n\
    \\f\n\
    \\ENQ\EOT\v\STX\STX\ETX\DC2\ETXU\"#\n\
    \F\n\
    \\EOT\EOT\v\STX\ETX\DC2\ETXV\STX$\"9 Predicate is true if utxo exhibits any of the patterns.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\v\STX\ETX\EOT\DC2\ETXV\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\v\STX\ETX\ACK\DC2\ETXV\v\CAN\n\
    \\f\n\
    \\ENQ\EOT\v\STX\ETX\SOH\DC2\ETXV\EM\US\n\
    \\f\n\
    \\ENQ\EOT\v\STX\ETX\ETX\DC2\ETXV\"#\n\
    \J\n\
    \\STX\EOT\f\DC2\EOTZ\NULa\SOH\SUB> An evenlope that holds an UTxO from any of compatible chains\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\f\SOH\DC2\ETXZ\b\DC3\n\
    \5\n\
    \\EOT\EOT\f\STX\NUL\DC2\ETX[\STX\EM\"( Original bytes as defined by the chain\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\f\STX\NUL\ENQ\DC2\ETX[\STX\a\n\
    \\f\n\
    \\ENQ\EOT\f\STX\NUL\SOH\DC2\ETX[\b\DC4\n\
    \\f\n\
    \\ENQ\EOT\f\STX\NUL\ETX\DC2\ETX[\ETB\CAN\n\
    \0\n\
    \\EOT\EOT\f\STX\SOH\DC2\ETX\\\STX\NAK\"# Hash of the previous transaction.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\f\STX\SOH\ACK\DC2\ETX\\\STX\b\n\
    \\f\n\
    \\ENQ\EOT\f\STX\SOH\SOH\DC2\ETX\\\t\DLE\n\
    \\f\n\
    \\ENQ\EOT\f\STX\SOH\ETX\DC2\ETX\\\DC3\DC4\n\
    \\f\n\
    \\EOT\EOT\f\b\NUL\DC2\EOT]\STX_\ETX\n\
    \\f\n\
    \\ENQ\EOT\f\b\NUL\SOH\DC2\ETX]\b\DC4\n\
    \\GS\n\
    \\EOT\EOT\f\STX\STX\DC2\ETX^\EOT0\"\DLE A cardano UTxO\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\f\STX\STX\ACK\DC2\ETX^\EOT#\n\
    \\f\n\
    \\ENQ\EOT\f\STX\STX\SOH\DC2\ETX^$+\n\
    \\f\n\
    \\ENQ\EOT\f\STX\STX\ETX\DC2\ETX^./\n\
    \R\n\
    \\EOT\EOT\f\STX\ETX\DC2\ETX`\STX\ESC\"E The chain point that represents the block this UTxO was created in.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\f\STX\ETX\ACK\DC2\ETX`\STX\f\n\
    \\f\n\
    \\ENQ\EOT\f\STX\ETX\SOH\DC2\ETX`\r\SYN\n\
    \\f\n\
    \\ENQ\EOT\f\STX\ETX\ETX\DC2\ETX`\EM\SUB\n\
    \+\n\
    \\STX\EOT\r\DC2\EOTd\NULg\SOH\SUB\US Request to get specific UTxOs\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\r\SOH\DC2\ETXd\b\CAN\n\
    \\"\n\
    \\EOT\EOT\r\STX\NUL\DC2\ETXe\STX\ESC\"\NAK List of keys UTxOs.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\r\STX\NUL\EOT\DC2\ETXe\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\r\STX\NUL\ACK\DC2\ETXe\v\DC1\n\
    \\f\n\
    \\ENQ\EOT\r\STX\NUL\SOH\DC2\ETXe\DC2\SYN\n\
    \\f\n\
    \\ENQ\EOT\r\STX\NUL\ETX\DC2\ETXe\EM\SUB\n\
    \7\n\
    \\EOT\EOT\r\STX\SOH\DC2\ETXf\STX+\"* Field mask to selectively return fields.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\r\STX\SOH\ACK\DC2\ETXf\STX\ESC\n\
    \\f\n\
    \\ENQ\EOT\r\STX\SOH\SOH\DC2\ETXf\FS&\n\
    \\f\n\
    \\ENQ\EOT\r\STX\SOH\ETX\DC2\ETXf)*\n\
    \T\n\
    \\STX\EOT\SO\DC2\EOTj\NULm\SOH\SUBH Response containing the UTxOs associated with the requested addresses.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\SO\SOH\DC2\ETXj\b\EM\n\
    \\GS\n\
    \\EOT\EOT\SO\STX\NUL\DC2\ETXk\STX!\"\DLE List of UTxOs.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\NUL\EOT\DC2\ETXk\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\NUL\ACK\DC2\ETXk\v\SYN\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\NUL\SOH\DC2\ETXk\ETB\FS\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\NUL\ETX\DC2\ETXk\US \n\
    \J\n\
    \\EOT\EOT\SO\STX\SOH\DC2\ETXl\STX\FS\"= The chain point that represent the ledger current position.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\SOH\ACK\DC2\ETXl\STX\f\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\SOH\SOH\DC2\ETXl\r\ETB\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\SOH\ETX\DC2\ETXl\SUB\ESC\n\
    \<\n\
    \\STX\EOT\SI\DC2\EOTp\NULu\SOH\SUB0 Request to search for UTxO based on a pattern.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\SI\SOH\DC2\ETXp\b\SUB\n\
    \)\n\
    \\EOT\EOT\SI\STX\NUL\DC2\ETXq\STX\RS\"\FS Pattern to match UTxOs by.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SI\STX\NUL\ACK\DC2\ETXq\STX\SI\n\
    \\f\n\
    \\ENQ\EOT\SI\STX\NUL\SOH\DC2\ETXq\DLE\EM\n\
    \\f\n\
    \\ENQ\EOT\SI\STX\NUL\ETX\DC2\ETXq\FS\GS\n\
    \7\n\
    \\EOT\EOT\SI\STX\SOH\DC2\ETXr\STX+\"* Field mask to selectively return fields.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SI\STX\SOH\ACK\DC2\ETXr\STX\ESC\n\
    \\f\n\
    \\ENQ\EOT\SI\STX\SOH\SOH\DC2\ETXr\FS&\n\
    \\f\n\
    \\ENQ\EOT\SI\STX\SOH\ETX\DC2\ETXr)*\n\
    \5\n\
    \\EOT\EOT\SI\STX\STX\DC2\ETXs\STX\SYN\"( The maximum number of items to return.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SI\STX\STX\ENQ\DC2\ETXs\STX\a\n\
    \\f\n\
    \\ENQ\EOT\SI\STX\STX\SOH\DC2\ETXs\b\DC1\n\
    \\f\n\
    \\ENQ\EOT\SI\STX\STX\ETX\DC2\ETXs\DC4\NAK\n\
    \R\n\
    \\EOT\EOT\SI\STX\ETX\DC2\ETXt\STX\EM\"E The next_page_token value returned from a previous request, if any.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SI\STX\ETX\ENQ\DC2\ETXt\STX\b\n\
    \\f\n\
    \\ENQ\EOT\SI\STX\ETX\SOH\DC2\ETXt\t\DC4\n\
    \\f\n\
    \\ENQ\EOT\SI\STX\ETX\ETX\DC2\ETXt\ETB\CAN\n\
    \O\n\
    \\STX\EOT\DLE\DC2\EOTx\NUL|\SOH\SUBC Response containing the UTxOs that match the requested addresses.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\DLE\SOH\DC2\ETXx\b\ESC\n\
    \\GS\n\
    \\EOT\EOT\DLE\STX\NUL\DC2\ETXy\STX!\"\DLE List of UTxOs.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\DLE\STX\NUL\EOT\DC2\ETXy\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\DLE\STX\NUL\ACK\DC2\ETXy\v\SYN\n\
    \\f\n\
    \\ENQ\EOT\DLE\STX\NUL\SOH\DC2\ETXy\ETB\FS\n\
    \\f\n\
    \\ENQ\EOT\DLE\STX\NUL\ETX\DC2\ETXy\US \n\
    \J\n\
    \\EOT\EOT\DLE\STX\SOH\DC2\ETXz\STX\FS\"= The chain point that represent the ledger current position.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\DLE\STX\SOH\ACK\DC2\ETXz\STX\f\n\
    \\f\n\
    \\ENQ\EOT\DLE\STX\SOH\SOH\DC2\ETXz\r\ETB\n\
    \\f\n\
    \\ENQ\EOT\DLE\STX\SOH\ETX\DC2\ETXz\SUB\ESC\n\
    \a\n\
    \\EOT\EOT\DLE\STX\STX\DC2\ETX{\STX\CAN\"T Token to retrieve the next page of results, or empty if there are no more results.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\DLE\STX\STX\ENQ\DC2\ETX{\STX\b\n\
    \\f\n\
    \\ENQ\EOT\DLE\STX\STX\SOH\DC2\ETX{\t\DC3\n\
    \\f\n\
    \\ENQ\EOT\DLE\STX\STX\ETX\DC2\ETX{\SYN\ETB\n\
    \:\n\
    \\STX\EOT\DC1\DC2\ENQ\DEL\NUL\130\SOH\SOH\SUB- Request to get data (as in plural of datum)\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\DC1\SOH\DC2\ETX\DEL\b\ETB\n\
    \\f\n\
    \\EOT\EOT\DC1\STX\NUL\DC2\EOT\128\SOH\STX\SUB\n\
    \\r\n\
    \\ENQ\EOT\DC1\STX\NUL\EOT\DC2\EOT\128\SOH\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\DC1\STX\NUL\ENQ\DC2\EOT\128\SOH\v\DLE\n\
    \\r\n\
    \\ENQ\EOT\DC1\STX\NUL\SOH\DC2\EOT\128\SOH\DC1\NAK\n\
    \\r\n\
    \\ENQ\EOT\DC1\STX\NUL\ETX\DC2\EOT\128\SOH\CAN\EM\n\
    \H\n\
    \\EOT\EOT\DC1\STX\SOH\DC2\EOT\129\SOH\STX+\": Field mask to selectively return fields in the response.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\DC1\STX\SOH\ACK\DC2\EOT\129\SOH\STX\ESC\n\
    \\r\n\
    \\ENQ\EOT\DC1\STX\SOH\SOH\DC2\EOT\129\SOH\FS&\n\
    \\r\n\
    \\ENQ\EOT\DC1\STX\SOH\ETX\DC2\EOT\129\SOH)*\n\
    \O\n\
    \\STX\EOT\DC2\DC2\ACK\133\SOH\NUL\139\SOH\SOH\SUBA An evenlope that holds a datum for any of the compatible chains\n\
    \\n\
    \\v\n\
    \\ETX\EOT\DC2\SOH\DC2\EOT\133\SOH\b\NAK\n\
    \6\n\
    \\EOT\EOT\DC2\STX\NUL\DC2\EOT\134\SOH\STX\EM\"( Original bytes as defined by the chain\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\DC2\STX\NUL\ENQ\DC2\EOT\134\SOH\STX\a\n\
    \\r\n\
    \\ENQ\EOT\DC2\STX\NUL\SOH\DC2\EOT\134\SOH\b\DC4\n\
    \\r\n\
    \\ENQ\EOT\DC2\STX\NUL\ETX\DC2\EOT\134\SOH\ETB\CAN\n\
    \\f\n\
    \\EOT\EOT\DC2\STX\SOH\DC2\EOT\135\SOH\STX\DLE\n\
    \\r\n\
    \\ENQ\EOT\DC2\STX\SOH\ENQ\DC2\EOT\135\SOH\STX\a\n\
    \\r\n\
    \\ENQ\EOT\DC2\STX\SOH\SOH\DC2\EOT\135\SOH\b\v\n\
    \\r\n\
    \\ENQ\EOT\DC2\STX\SOH\ETX\DC2\EOT\135\SOH\SO\SI\n\
    \\SO\n\
    \\EOT\EOT\DC2\b\NUL\DC2\ACK\136\SOH\STX\138\SOH\ETX\n\
    \\r\n\
    \\ENQ\EOT\DC2\b\NUL\SOH\DC2\EOT\136\SOH\b\DC4\n\
    \\RS\n\
    \\EOT\EOT\DC2\STX\STX\DC2\EOT\137\SOH\EOT2\"\DLE A cardano UTxO\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\DC2\STX\STX\ACK\DC2\EOT\137\SOH\EOT%\n\
    \\r\n\
    \\ENQ\EOT\DC2\STX\STX\SOH\DC2\EOT\137\SOH&-\n\
    \\r\n\
    \\ENQ\EOT\DC2\STX\STX\ETX\DC2\EOT\137\SOH01\n\
    \@\n\
    \\STX\EOT\DC3\DC2\ACK\142\SOH\NUL\145\SOH\SOH\SUB2 Response containing data (as in plural of datum)\n\
    \\n\
    \\v\n\
    \\ETX\EOT\DC3\SOH\DC2\EOT\142\SOH\b\CAN\n\
    \(\n\
    \\EOT\EOT\DC3\STX\NUL\DC2\EOT\143\SOH\STX$\"\SUB The value of each datum.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\DC3\STX\NUL\EOT\DC2\EOT\143\SOH\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\DC3\STX\NUL\ACK\DC2\EOT\143\SOH\v\CAN\n\
    \\r\n\
    \\ENQ\EOT\DC3\STX\NUL\SOH\DC2\EOT\143\SOH\EM\US\n\
    \\r\n\
    \\ENQ\EOT\DC3\STX\NUL\ETX\DC2\EOT\143\SOH\"#\n\
    \K\n\
    \\EOT\EOT\DC3\STX\SOH\DC2\EOT\144\SOH\STX\FS\"= The chain point that represent the ledger current position.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\DC3\STX\SOH\ACK\DC2\EOT\144\SOH\STX\f\n\
    \\r\n\
    \\ENQ\EOT\DC3\STX\SOH\SOH\DC2\EOT\144\SOH\r\ETB\n\
    \\r\n\
    \\ENQ\EOT\DC3\STX\SOH\ETX\DC2\EOT\144\SOH\SUB\ESC\n\
    \4\n\
    \\STX\EOT\DC4\DC2\ACK\148\SOH\NUL\151\SOH\SOH\SUB& Request to get a transaction by hash\n\
    \\n\
    \\v\n\
    \\ETX\EOT\DC4\SOH\DC2\EOT\148\SOH\b\NAK\n\
    \,\n\
    \\EOT\EOT\DC4\STX\NUL\DC2\EOT\149\SOH\STX\DC1\"\RS The hash of the transaction.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\DC4\STX\NUL\ENQ\DC2\EOT\149\SOH\STX\a\n\
    \\r\n\
    \\ENQ\EOT\DC4\STX\NUL\SOH\DC2\EOT\149\SOH\b\f\n\
    \\r\n\
    \\ENQ\EOT\DC4\STX\NUL\ETX\DC2\EOT\149\SOH\SI\DLE\n\
    \H\n\
    \\EOT\EOT\DC4\STX\SOH\DC2\EOT\150\SOH\STX+\": Field mask to selectively return fields in the response.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\DC4\STX\SOH\ACK\DC2\EOT\150\SOH\STX\ESC\n\
    \\r\n\
    \\ENQ\EOT\DC4\STX\SOH\SOH\DC2\EOT\150\SOH\FS&\n\
    \\r\n\
    \\ENQ\EOT\DC4\STX\SOH\ETX\DC2\EOT\150\SOH)*\n\
    \G\n\
    \\STX\EOT\NAK\DC2\ACK\154\SOH\NUL\160\SOH\SOH\SUB9 Represents a transaction from any supported blockchain.\n\
    \\n\
    \\v\n\
    \\ETX\EOT\NAK\SOH\DC2\EOT\154\SOH\b\DC2\n\
    \6\n\
    \\EOT\EOT\NAK\STX\NUL\DC2\EOT\155\SOH\STX\EM\"( Original bytes as defined by the chain\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\NAK\STX\NUL\ENQ\DC2\EOT\155\SOH\STX\a\n\
    \\r\n\
    \\ENQ\EOT\NAK\STX\NUL\SOH\DC2\EOT\155\SOH\b\DC4\n\
    \\r\n\
    \\ENQ\EOT\NAK\STX\NUL\ETX\DC2\EOT\155\SOH\ETB\CAN\n\
    \\SO\n\
    \\EOT\EOT\NAK\b\NUL\DC2\ACK\156\SOH\STX\158\SOH\ETX\n\
    \\r\n\
    \\ENQ\EOT\NAK\b\NUL\SOH\DC2\EOT\156\SOH\b\r\n\
    \&\n\
    \\EOT\EOT\NAK\STX\SOH\DC2\EOT\157\SOH\EOT*\"\CAN A Cardano transaction.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\NAK\STX\SOH\ACK\DC2\EOT\157\SOH\EOT\GS\n\
    \\r\n\
    \\ENQ\EOT\NAK\STX\SOH\SOH\DC2\EOT\157\SOH\RS%\n\
    \\r\n\
    \\ENQ\EOT\NAK\STX\SOH\ETX\DC2\EOT\157\SOH()\n\
    \V\n\
    \\EOT\EOT\NAK\STX\STX\DC2\EOT\159\SOH\STX\ESC\"H The chain point that represents the block this transaction belongs to.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\NAK\STX\STX\ACK\DC2\EOT\159\SOH\STX\f\n\
    \\r\n\
    \\ENQ\EOT\NAK\STX\STX\SOH\DC2\EOT\159\SOH\r\SYN\n\
    \\r\n\
    \\ENQ\EOT\NAK\STX\STX\ETX\DC2\EOT\159\SOH\EM\SUB\n\
    \W\n\
    \\STX\EOT\SYN\DC2\ACK\163\SOH\NUL\166\SOH\SOH\SUBI Response containing the transaction associated with the requested hash.\n\
    \\n\
    \\v\n\
    \\ETX\EOT\SYN\SOH\DC2\EOT\163\SOH\b\SYN\n\
    \ \n\
    \\EOT\EOT\SYN\STX\NUL\DC2\EOT\164\SOH\STX\DC4\"\DC2 The transaction.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\NUL\ACK\DC2\EOT\164\SOH\STX\f\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\NUL\SOH\DC2\EOT\164\SOH\r\SI\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\NUL\ETX\DC2\EOT\164\SOH\DC2\DC3\n\
    \K\n\
    \\EOT\EOT\SYN\STX\SOH\DC2\EOT\165\SOH\STX\FS\"= The chain point that represent the ledger current position.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\SOH\ACK\DC2\EOT\165\SOH\STX\f\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\SOH\SOH\DC2\EOT\165\SOH\r\ETB\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\SOH\ETX\DC2\EOT\165\SOH\SUB\ESC\n\
    \G\n\
    \\STX\ACK\NUL\DC2\ACK\169\SOH\NUL\181\SOH\SOH\SUB9 Service definition for querying the state of the chain.\n\
    \\n\
    \\v\n\
    \\ETX\ACK\NUL\SOH\DC2\EOT\169\SOH\b\DC4\n\
    \(\n\
    \\EOT\ACK\NUL\STX\NUL\DC2\EOT\170\SOH\STXA\"\SUB Get overall chain state.\n\
    \\n\
    \\r\n\
    \\ENQ\ACK\NUL\STX\NUL\SOH\DC2\EOT\170\SOH\ACK\DLE\n\
    \\r\n\
    \\ENQ\ACK\NUL\STX\NUL\STX\DC2\EOT\170\SOH\DC1\"\n\
    \\r\n\
    \\ENQ\ACK\NUL\STX\NUL\ETX\DC2\EOT\170\SOH-?\n\
    \1\n\
    \\EOT\ACK\NUL\STX\SOH\DC2\EOT\171\SOH\STX>\"# Read specific UTxOs by reference.\n\
    \\n\
    \\r\n\
    \\ENQ\ACK\NUL\STX\SOH\SOH\DC2\EOT\171\SOH\ACK\SI\n\
    \\r\n\
    \\ENQ\ACK\NUL\STX\SOH\STX\DC2\EOT\171\SOH\DLE \n\
    \\r\n\
    \\ENQ\ACK\NUL\STX\SOH\ETX\DC2\EOT\171\SOH+<\n\
    \3\n\
    \\EOT\ACK\NUL\STX\STX\DC2\EOT\172\SOH\STXD\"% Search for UTxO based on a pattern.\n\
    \\n\
    \\r\n\
    \\ENQ\ACK\NUL\STX\STX\SOH\DC2\EOT\172\SOH\ACK\DC1\n\
    \\r\n\
    \\ENQ\ACK\NUL\STX\STX\STX\DC2\EOT\172\SOH\DC2$\n\
    \\r\n\
    \\ENQ\ACK\NUL\STX\STX\ETX\DC2\EOT\172\SOH/B\n\
    \+\n\
    \\EOT\ACK\NUL\STX\ETX\DC2\EOT\173\SOH\STX;\"\GS Read specific datum by hash\n\
    \\n\
    \\r\n\
    \\ENQ\ACK\NUL\STX\ETX\SOH\DC2\EOT\173\SOH\ACK\SO\n\
    \\r\n\
    \\ENQ\ACK\NUL\STX\ETX\STX\DC2\EOT\173\SOH\SI\RS\n\
    \\r\n\
    \\ENQ\ACK\NUL\STX\ETX\ETX\DC2\EOT\173\SOH)9\n\
    \3\n\
    \\EOT\ACK\NUL\STX\EOT\DC2\EOT\174\SOH\STX5\"% Get Txs by chain-specific criteria.\n\
    \\n\
    \\r\n\
    \\ENQ\ACK\NUL\STX\EOT\SOH\DC2\EOT\174\SOH\ACK\f\n\
    \\r\n\
    \\ENQ\ACK\NUL\STX\EOT\STX\DC2\EOT\174\SOH\r\SUB\n\
    \\r\n\
    \\ENQ\ACK\NUL\STX\EOT\ETX\DC2\EOT\174\SOH%3\n\
    \-\n\
    \\EOT\ACK\NUL\STX\ENQ\DC2\EOT\175\SOH\STXD\"\US Get the genesis configuration\n\
    \\n\
    \\r\n\
    \\ENQ\ACK\NUL\STX\ENQ\SOH\DC2\EOT\175\SOH\ACK\DC1\n\
    \\r\n\
    \\ENQ\ACK\NUL\STX\ENQ\STX\DC2\EOT\175\SOH\DC2$\n\
    \\r\n\
    \\ENQ\ACK\NUL\STX\ENQ\ETX\DC2\EOT\175\SOH/B\n\
    \)\n\
    \\EOT\ACK\NUL\STX\ACK\DC2\EOT\176\SOH\STXM\"\ESC Get the chain era summary\n\
    \\n\
    \\r\n\
    \\ENQ\ACK\NUL\STX\ACK\SOH\DC2\EOT\176\SOH\ACK\DC4\n\
    \\r\n\
    \\ENQ\ACK\NUL\STX\ACK\STX\DC2\EOT\176\SOH\NAK*\n\
    \\r\n\
    \\ENQ\ACK\NUL\STX\ACK\ETX\DC2\EOT\176\SOH5Kb\ACKproto3"