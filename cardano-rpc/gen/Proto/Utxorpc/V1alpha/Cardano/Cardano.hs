{- This file was auto-generated from utxorpc/v1alpha/cardano/cardano.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Utxorpc.V1alpha.Cardano.Cardano (
        AddressArray(), Asset(), Asset'Quantity(..), _Asset'OutputCoin,
        _Asset'MintCoin, CostModel(), CostModels(), Datum(), ExPrices(),
        ExUnits(), MultiAsset(), PParams(), ProtocolVersion(),
        RationalNumber(), Script(), Script'Script(..), _Script'Native,
        _Script'PlutusV1, _Script'PlutusV2, _Script'PlutusV3,
        _Script'PlutusV4, TxOutput(), VotingThresholds()
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
     
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.items' @:: Lens' AddressArray [Data.ByteString.ByteString]@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.vec'items' @:: Lens' AddressArray (Data.Vector.Vector Data.ByteString.ByteString)@ -}
data AddressArray
  = AddressArray'_constructor {_AddressArray'items :: !(Data.Vector.Vector Data.ByteString.ByteString),
                               _AddressArray'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show AddressArray where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField AddressArray "items" [Data.ByteString.ByteString] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AddressArray'items (\ x__ y__ -> x__ {_AddressArray'items = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField AddressArray "vec'items" (Data.Vector.Vector Data.ByteString.ByteString) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AddressArray'items (\ x__ y__ -> x__ {_AddressArray'items = y__}))
        Prelude.id
instance Data.ProtoLens.Message AddressArray where
  messageName _
    = Data.Text.pack "utxorpc.v1alpha.cardano.AddressArray"
  packedMessageDescriptor _
    = "\n\
      \\fAddressArray\DC2\DC4\n\
      \\ENQitems\CAN\SOH \ETX(\fR\ENQitems"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        items__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "items"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"items")) ::
              Data.ProtoLens.FieldDescriptor AddressArray
      in
        Data.Map.fromList [(Data.ProtoLens.Tag 1, items__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _AddressArray'_unknownFields
        (\ x__ y__ -> x__ {_AddressArray'_unknownFields = y__})
  defMessage
    = AddressArray'_constructor
        {_AddressArray'items = Data.Vector.Generic.empty,
         _AddressArray'_unknownFields = []}
  parseMessage
    = let
        loop ::
          AddressArray
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Data.ByteString.ByteString
             -> Data.ProtoLens.Encoding.Bytes.Parser AddressArray
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
                                            Data.ProtoLens.Encoding.Bytes.getBytes
                                              (Prelude.fromIntegral len))
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
          "AddressArray"
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
                (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'items") _x))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData AddressArray where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_AddressArray'_unknownFields x__)
             (Control.DeepSeq.deepseq (_AddressArray'items x__) ())
{- | Fields :
     
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.name' @:: Lens' Asset Data.ByteString.ByteString@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maybe'quantity' @:: Lens' Asset (Prelude.Maybe Asset'Quantity)@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maybe'outputCoin' @:: Lens' Asset (Prelude.Maybe Data.Word.Word64)@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.outputCoin' @:: Lens' Asset Data.Word.Word64@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maybe'mintCoin' @:: Lens' Asset (Prelude.Maybe Data.Int.Int64)@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.mintCoin' @:: Lens' Asset Data.Int.Int64@ -}
data Asset
  = Asset'_constructor {_Asset'name :: !Data.ByteString.ByteString,
                        _Asset'quantity :: !(Prelude.Maybe Asset'Quantity),
                        _Asset'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Asset where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
data Asset'Quantity
  = Asset'OutputCoin !Data.Word.Word64 |
    Asset'MintCoin !Data.Int.Int64
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.Field.HasField Asset "name" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Asset'name (\ x__ y__ -> x__ {_Asset'name = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Asset "maybe'quantity" (Prelude.Maybe Asset'Quantity) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Asset'quantity (\ x__ y__ -> x__ {_Asset'quantity = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Asset "maybe'outputCoin" (Prelude.Maybe Data.Word.Word64) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Asset'quantity (\ x__ y__ -> x__ {_Asset'quantity = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (Asset'OutputCoin x__val)) -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap Asset'OutputCoin y__))
instance Data.ProtoLens.Field.HasField Asset "outputCoin" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Asset'quantity (\ x__ y__ -> x__ {_Asset'quantity = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (Asset'OutputCoin x__val)) -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap Asset'OutputCoin y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault))
instance Data.ProtoLens.Field.HasField Asset "maybe'mintCoin" (Prelude.Maybe Data.Int.Int64) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Asset'quantity (\ x__ y__ -> x__ {_Asset'quantity = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (Asset'MintCoin x__val)) -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap Asset'MintCoin y__))
instance Data.ProtoLens.Field.HasField Asset "mintCoin" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Asset'quantity (\ x__ y__ -> x__ {_Asset'quantity = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (Asset'MintCoin x__val)) -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap Asset'MintCoin y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault))
instance Data.ProtoLens.Message Asset where
  messageName _ = Data.Text.pack "utxorpc.v1alpha.cardano.Asset"
  packedMessageDescriptor _
    = "\n\
      \\ENQAsset\DC2\DC2\n\
      \\EOTname\CAN\SOH \SOH(\fR\EOTname\DC2%\n\
      \\voutput_coin\CAN\STX \SOH(\EOTH\NULR\n\
      \outputCoinB\STX0\SOH\DC2!\n\
      \\tmint_coin\CAN\ETX \SOH(\ETXH\NULR\bmintCoinB\STX0\SOHB\n\
      \\n\
      \\bquantity"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        name__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "name"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"name")) ::
              Data.ProtoLens.FieldDescriptor Asset
        outputCoin__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "output_coin"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'outputCoin")) ::
              Data.ProtoLens.FieldDescriptor Asset
        mintCoin__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "mint_coin"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'mintCoin")) ::
              Data.ProtoLens.FieldDescriptor Asset
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, name__field_descriptor),
           (Data.ProtoLens.Tag 2, outputCoin__field_descriptor),
           (Data.ProtoLens.Tag 3, mintCoin__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Asset'_unknownFields
        (\ x__ y__ -> x__ {_Asset'_unknownFields = y__})
  defMessage
    = Asset'_constructor
        {_Asset'name = Data.ProtoLens.fieldDefault,
         _Asset'quantity = Prelude.Nothing, _Asset'_unknownFields = []}
  parseMessage
    = let
        loop :: Asset -> Data.ProtoLens.Encoding.Bytes.Parser Asset
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
                                       "name"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"name") y x)
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getVarInt "output_coin"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"outputCoin") y x)
                        24
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "mint_coin"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"mintCoin") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "Asset"
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
                      ((\ bs
                          -> (Data.Monoid.<>)
                               (Data.ProtoLens.Encoding.Bytes.putVarInt
                                  (Prelude.fromIntegral (Data.ByteString.length bs)))
                               (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'quantity") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just (Asset'OutputCoin v))
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 16)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt v)
                   (Prelude.Just (Asset'MintCoin v))
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 24)
                          ((Prelude..)
                             Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData Asset where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Asset'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_Asset'name x__)
                (Control.DeepSeq.deepseq (_Asset'quantity x__) ()))
instance Control.DeepSeq.NFData Asset'Quantity where
  rnf (Asset'OutputCoin x__) = Control.DeepSeq.rnf x__
  rnf (Asset'MintCoin x__) = Control.DeepSeq.rnf x__
_Asset'OutputCoin ::
  Data.ProtoLens.Prism.Prism' Asset'Quantity Data.Word.Word64
_Asset'OutputCoin
  = Data.ProtoLens.Prism.prism'
      Asset'OutputCoin
      (\ p__
         -> case p__ of
              (Asset'OutputCoin p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_Asset'MintCoin ::
  Data.ProtoLens.Prism.Prism' Asset'Quantity Data.Int.Int64
_Asset'MintCoin
  = Data.ProtoLens.Prism.prism'
      Asset'MintCoin
      (\ p__
         -> case p__ of
              (Asset'MintCoin p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
{- | Fields :
     
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.values' @:: Lens' CostModel [Data.Int.Int64]@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.vec'values' @:: Lens' CostModel (Data.Vector.Unboxed.Vector Data.Int.Int64)@ -}
data CostModel
  = CostModel'_constructor {_CostModel'values :: !(Data.Vector.Unboxed.Vector Data.Int.Int64),
                            _CostModel'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show CostModel where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField CostModel "values" [Data.Int.Int64] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _CostModel'values (\ x__ y__ -> x__ {_CostModel'values = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField CostModel "vec'values" (Data.Vector.Unboxed.Vector Data.Int.Int64) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _CostModel'values (\ x__ y__ -> x__ {_CostModel'values = y__}))
        Prelude.id
instance Data.ProtoLens.Message CostModel where
  messageName _ = Data.Text.pack "utxorpc.v1alpha.cardano.CostModel"
  packedMessageDescriptor _
    = "\n\
      \\tCostModel\DC2\SYN\n\
      \\ACKvalues\CAN\SOH \ETX(\ETXR\ACKvalues"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        values__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "values"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Packed (Data.ProtoLens.Field.field @"values")) ::
              Data.ProtoLens.FieldDescriptor CostModel
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, values__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _CostModel'_unknownFields
        (\ x__ y__ -> x__ {_CostModel'_unknownFields = y__})
  defMessage
    = CostModel'_constructor
        {_CostModel'values = Data.Vector.Generic.empty,
         _CostModel'_unknownFields = []}
  parseMessage
    = let
        loop ::
          CostModel
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Unboxed.Vector Data.ProtoLens.Encoding.Growing.RealWorld Data.Int.Int64
             -> Data.ProtoLens.Encoding.Bytes.Parser CostModel
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
                        8 -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (Prelude.fmap
                                           Prelude.fromIntegral
                                           Data.ProtoLens.Encoding.Bytes.getVarInt)
                                        "values"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'values y)
                                loop x v
                        10
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
                                                                    "values"
                                                            qs' <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                                     (Data.ProtoLens.Encoding.Growing.append
                                                                        qs q)
                                                            ploop qs'
                                            in ploop)
                                             mutable'values)
                                loop x y
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
          "CostModel"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                p = Lens.Family2.view (Data.ProtoLens.Field.field @"vec'values") _x
              in
                if Data.Vector.Generic.null p then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
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
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData CostModel where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_CostModel'_unknownFields x__)
             (Control.DeepSeq.deepseq (_CostModel'values x__) ())
{- | Fields :
     
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.plutusV1' @:: Lens' CostModels CostModel@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maybe'plutusV1' @:: Lens' CostModels (Prelude.Maybe CostModel)@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.plutusV2' @:: Lens' CostModels CostModel@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maybe'plutusV2' @:: Lens' CostModels (Prelude.Maybe CostModel)@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.plutusV3' @:: Lens' CostModels CostModel@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maybe'plutusV3' @:: Lens' CostModels (Prelude.Maybe CostModel)@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.plutusV4' @:: Lens' CostModels CostModel@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maybe'plutusV4' @:: Lens' CostModels (Prelude.Maybe CostModel)@ -}
data CostModels
  = CostModels'_constructor {_CostModels'plutusV1 :: !(Prelude.Maybe CostModel),
                             _CostModels'plutusV2 :: !(Prelude.Maybe CostModel),
                             _CostModels'plutusV3 :: !(Prelude.Maybe CostModel),
                             _CostModels'plutusV4 :: !(Prelude.Maybe CostModel),
                             _CostModels'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show CostModels where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField CostModels "plutusV1" CostModel where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _CostModels'plutusV1
           (\ x__ y__ -> x__ {_CostModels'plutusV1 = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField CostModels "maybe'plutusV1" (Prelude.Maybe CostModel) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _CostModels'plutusV1
           (\ x__ y__ -> x__ {_CostModels'plutusV1 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField CostModels "plutusV2" CostModel where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _CostModels'plutusV2
           (\ x__ y__ -> x__ {_CostModels'plutusV2 = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField CostModels "maybe'plutusV2" (Prelude.Maybe CostModel) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _CostModels'plutusV2
           (\ x__ y__ -> x__ {_CostModels'plutusV2 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField CostModels "plutusV3" CostModel where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _CostModels'plutusV3
           (\ x__ y__ -> x__ {_CostModels'plutusV3 = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField CostModels "maybe'plutusV3" (Prelude.Maybe CostModel) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _CostModels'plutusV3
           (\ x__ y__ -> x__ {_CostModels'plutusV3 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField CostModels "plutusV4" CostModel where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _CostModels'plutusV4
           (\ x__ y__ -> x__ {_CostModels'plutusV4 = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField CostModels "maybe'plutusV4" (Prelude.Maybe CostModel) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _CostModels'plutusV4
           (\ x__ y__ -> x__ {_CostModels'plutusV4 = y__}))
        Prelude.id
instance Data.ProtoLens.Message CostModels where
  messageName _ = Data.Text.pack "utxorpc.v1alpha.cardano.CostModels"
  packedMessageDescriptor _
    = "\n\
      \\n\
      \CostModels\DC2?\n\
      \\tplutus_v1\CAN\SOH \SOH(\v2\".utxorpc.v1alpha.cardano.CostModelR\bplutusV1\DC2?\n\
      \\tplutus_v2\CAN\STX \SOH(\v2\".utxorpc.v1alpha.cardano.CostModelR\bplutusV2\DC2?\n\
      \\tplutus_v3\CAN\ETX \SOH(\v2\".utxorpc.v1alpha.cardano.CostModelR\bplutusV3\DC2?\n\
      \\tplutus_v4\CAN\EOT \SOH(\v2\".utxorpc.v1alpha.cardano.CostModelR\bplutusV4"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        plutusV1__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "plutus_v1"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor CostModel)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'plutusV1")) ::
              Data.ProtoLens.FieldDescriptor CostModels
        plutusV2__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "plutus_v2"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor CostModel)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'plutusV2")) ::
              Data.ProtoLens.FieldDescriptor CostModels
        plutusV3__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "plutus_v3"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor CostModel)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'plutusV3")) ::
              Data.ProtoLens.FieldDescriptor CostModels
        plutusV4__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "plutus_v4"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor CostModel)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'plutusV4")) ::
              Data.ProtoLens.FieldDescriptor CostModels
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, plutusV1__field_descriptor),
           (Data.ProtoLens.Tag 2, plutusV2__field_descriptor),
           (Data.ProtoLens.Tag 3, plutusV3__field_descriptor),
           (Data.ProtoLens.Tag 4, plutusV4__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _CostModels'_unknownFields
        (\ x__ y__ -> x__ {_CostModels'_unknownFields = y__})
  defMessage
    = CostModels'_constructor
        {_CostModels'plutusV1 = Prelude.Nothing,
         _CostModels'plutusV2 = Prelude.Nothing,
         _CostModels'plutusV3 = Prelude.Nothing,
         _CostModels'plutusV4 = Prelude.Nothing,
         _CostModels'_unknownFields = []}
  parseMessage
    = let
        loop ::
          CostModels -> Data.ProtoLens.Encoding.Bytes.Parser CostModels
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
                                       "plutus_v1"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"plutusV1") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "plutus_v2"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"plutusV2") y x)
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "plutus_v3"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"plutusV3") y x)
                        34
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "plutus_v4"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"plutusV4") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "CostModels"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'plutusV1") _x
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
                     Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'plutusV2") _x
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
                        Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'plutusV3") _x
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
                           Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'plutusV4") _x
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
                         (Lens.Family2.view Data.ProtoLens.unknownFields _x)))))
instance Control.DeepSeq.NFData CostModels where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_CostModels'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_CostModels'plutusV1 x__)
                (Control.DeepSeq.deepseq
                   (_CostModels'plutusV2 x__)
                   (Control.DeepSeq.deepseq
                      (_CostModels'plutusV3 x__)
                      (Control.DeepSeq.deepseq (_CostModels'plutusV4 x__) ()))))
{- | Fields :
     
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.hash' @:: Lens' Datum Data.ByteString.ByteString@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.originalCbor' @:: Lens' Datum Data.ByteString.ByteString@ -}
data Datum
  = Datum'_constructor {_Datum'hash :: !Data.ByteString.ByteString,
                        _Datum'originalCbor :: !Data.ByteString.ByteString,
                        _Datum'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Datum where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Datum "hash" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Datum'hash (\ x__ y__ -> x__ {_Datum'hash = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Datum "originalCbor" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Datum'originalCbor (\ x__ y__ -> x__ {_Datum'originalCbor = y__}))
        Prelude.id
instance Data.ProtoLens.Message Datum where
  messageName _ = Data.Text.pack "utxorpc.v1alpha.cardano.Datum"
  packedMessageDescriptor _
    = "\n\
      \\ENQDatum\DC2\DC2\n\
      \\EOThash\CAN\SOH \SOH(\fR\EOThash\DC2#\n\
      \\roriginal_cbor\CAN\ETX \SOH(\fR\foriginalCbor"
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
              Data.ProtoLens.FieldDescriptor Datum
        originalCbor__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "original_cbor"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"originalCbor")) ::
              Data.ProtoLens.FieldDescriptor Datum
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, hash__field_descriptor),
           (Data.ProtoLens.Tag 3, originalCbor__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Datum'_unknownFields
        (\ x__ y__ -> x__ {_Datum'_unknownFields = y__})
  defMessage
    = Datum'_constructor
        {_Datum'hash = Data.ProtoLens.fieldDefault,
         _Datum'originalCbor = Data.ProtoLens.fieldDefault,
         _Datum'_unknownFields = []}
  parseMessage
    = let
        loop :: Datum -> Data.ProtoLens.Encoding.Bytes.Parser Datum
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
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getBytes
                                             (Prelude.fromIntegral len))
                                       "original_cbor"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"originalCbor") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "Datum"
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
                   _v
                     = Lens.Family2.view (Data.ProtoLens.Field.field @"originalCbor") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                         ((\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                            _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData Datum where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Datum'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_Datum'hash x__)
                (Control.DeepSeq.deepseq (_Datum'originalCbor x__) ()))
{- | Fields :
     
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.steps' @:: Lens' ExPrices RationalNumber@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maybe'steps' @:: Lens' ExPrices (Prelude.Maybe RationalNumber)@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.memory' @:: Lens' ExPrices RationalNumber@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maybe'memory' @:: Lens' ExPrices (Prelude.Maybe RationalNumber)@ -}
data ExPrices
  = ExPrices'_constructor {_ExPrices'steps :: !(Prelude.Maybe RationalNumber),
                           _ExPrices'memory :: !(Prelude.Maybe RationalNumber),
                           _ExPrices'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ExPrices where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ExPrices "steps" RationalNumber where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExPrices'steps (\ x__ y__ -> x__ {_ExPrices'steps = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ExPrices "maybe'steps" (Prelude.Maybe RationalNumber) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExPrices'steps (\ x__ y__ -> x__ {_ExPrices'steps = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExPrices "memory" RationalNumber where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExPrices'memory (\ x__ y__ -> x__ {_ExPrices'memory = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ExPrices "maybe'memory" (Prelude.Maybe RationalNumber) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExPrices'memory (\ x__ y__ -> x__ {_ExPrices'memory = y__}))
        Prelude.id
instance Data.ProtoLens.Message ExPrices where
  messageName _ = Data.Text.pack "utxorpc.v1alpha.cardano.ExPrices"
  packedMessageDescriptor _
    = "\n\
      \\bExPrices\DC2=\n\
      \\ENQsteps\CAN\SOH \SOH(\v2'.utxorpc.v1alpha.cardano.RationalNumberR\ENQsteps\DC2?\n\
      \\ACKmemory\CAN\STX \SOH(\v2'.utxorpc.v1alpha.cardano.RationalNumberR\ACKmemory"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        steps__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "steps"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor RationalNumber)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'steps")) ::
              Data.ProtoLens.FieldDescriptor ExPrices
        memory__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "memory"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor RationalNumber)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'memory")) ::
              Data.ProtoLens.FieldDescriptor ExPrices
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, steps__field_descriptor),
           (Data.ProtoLens.Tag 2, memory__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ExPrices'_unknownFields
        (\ x__ y__ -> x__ {_ExPrices'_unknownFields = y__})
  defMessage
    = ExPrices'_constructor
        {_ExPrices'steps = Prelude.Nothing,
         _ExPrices'memory = Prelude.Nothing, _ExPrices'_unknownFields = []}
  parseMessage
    = let
        loop :: ExPrices -> Data.ProtoLens.Encoding.Bytes.Parser ExPrices
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
                                       "steps"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"steps") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "memory"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"memory") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "ExPrices"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'steps") _x
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
                     Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'memory") _x
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
instance Control.DeepSeq.NFData ExPrices where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ExPrices'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ExPrices'steps x__)
                (Control.DeepSeq.deepseq (_ExPrices'memory x__) ()))
{- | Fields :
     
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.steps' @:: Lens' ExUnits Data.Word.Word64@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.memory' @:: Lens' ExUnits Data.Word.Word64@ -}
data ExUnits
  = ExUnits'_constructor {_ExUnits'steps :: !Data.Word.Word64,
                          _ExUnits'memory :: !Data.Word.Word64,
                          _ExUnits'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ExUnits where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ExUnits "steps" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExUnits'steps (\ x__ y__ -> x__ {_ExUnits'steps = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExUnits "memory" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExUnits'memory (\ x__ y__ -> x__ {_ExUnits'memory = y__}))
        Prelude.id
instance Data.ProtoLens.Message ExUnits where
  messageName _ = Data.Text.pack "utxorpc.v1alpha.cardano.ExUnits"
  packedMessageDescriptor _
    = "\n\
      \\aExUnits\DC2\DC4\n\
      \\ENQsteps\CAN\SOH \SOH(\EOTR\ENQsteps\DC2\SYN\n\
      \\ACKmemory\CAN\STX \SOH(\EOTR\ACKmemory"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        steps__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "steps"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"steps")) ::
              Data.ProtoLens.FieldDescriptor ExUnits
        memory__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "memory"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"memory")) ::
              Data.ProtoLens.FieldDescriptor ExUnits
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, steps__field_descriptor),
           (Data.ProtoLens.Tag 2, memory__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ExUnits'_unknownFields
        (\ x__ y__ -> x__ {_ExUnits'_unknownFields = y__})
  defMessage
    = ExUnits'_constructor
        {_ExUnits'steps = Data.ProtoLens.fieldDefault,
         _ExUnits'memory = Data.ProtoLens.fieldDefault,
         _ExUnits'_unknownFields = []}
  parseMessage
    = let
        loop :: ExUnits -> Data.ProtoLens.Encoding.Bytes.Parser ExUnits
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
                                       Data.ProtoLens.Encoding.Bytes.getVarInt "steps"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"steps") y x)
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getVarInt "memory"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"memory") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "ExUnits"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"steps") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt _v))
             ((Data.Monoid.<>)
                (let
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"memory") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 16)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData ExUnits where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ExUnits'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ExUnits'steps x__)
                (Control.DeepSeq.deepseq (_ExUnits'memory x__) ()))
{- | Fields :
     
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.policyId' @:: Lens' MultiAsset Data.ByteString.ByteString@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.assets' @:: Lens' MultiAsset [Asset]@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.vec'assets' @:: Lens' MultiAsset (Data.Vector.Vector Asset)@ -}
data MultiAsset
  = MultiAsset'_constructor {_MultiAsset'policyId :: !Data.ByteString.ByteString,
                             _MultiAsset'assets :: !(Data.Vector.Vector Asset),
                             _MultiAsset'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show MultiAsset where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField MultiAsset "policyId" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _MultiAsset'policyId
           (\ x__ y__ -> x__ {_MultiAsset'policyId = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField MultiAsset "assets" [Asset] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _MultiAsset'assets (\ x__ y__ -> x__ {_MultiAsset'assets = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField MultiAsset "vec'assets" (Data.Vector.Vector Asset) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _MultiAsset'assets (\ x__ y__ -> x__ {_MultiAsset'assets = y__}))
        Prelude.id
instance Data.ProtoLens.Message MultiAsset where
  messageName _ = Data.Text.pack "utxorpc.v1alpha.cardano.MultiAsset"
  packedMessageDescriptor _
    = "\n\
      \\n\
      \MultiAsset\DC2\ESC\n\
      \\tpolicy_id\CAN\SOH \SOH(\fR\bpolicyId\DC26\n\
      \\ACKassets\CAN\STX \ETX(\v2\RS.utxorpc.v1alpha.cardano.AssetR\ACKassets"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        policyId__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "policy_id"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"policyId")) ::
              Data.ProtoLens.FieldDescriptor MultiAsset
        assets__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "assets"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Asset)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"assets")) ::
              Data.ProtoLens.FieldDescriptor MultiAsset
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, policyId__field_descriptor),
           (Data.ProtoLens.Tag 2, assets__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _MultiAsset'_unknownFields
        (\ x__ y__ -> x__ {_MultiAsset'_unknownFields = y__})
  defMessage
    = MultiAsset'_constructor
        {_MultiAsset'policyId = Data.ProtoLens.fieldDefault,
         _MultiAsset'assets = Data.Vector.Generic.empty,
         _MultiAsset'_unknownFields = []}
  parseMessage
    = let
        loop ::
          MultiAsset
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Asset
             -> Data.ProtoLens.Encoding.Bytes.Parser MultiAsset
        loop x mutable'assets
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'assets <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                         (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                            mutable'assets)
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
                              (Data.ProtoLens.Field.field @"vec'assets") frozen'assets x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getBytes
                                             (Prelude.fromIntegral len))
                                       "policy_id"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"policyId") y x)
                                  mutable'assets
                        18
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "assets"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'assets y)
                                loop x v
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'assets
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'assets <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                  Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'assets)
          "MultiAsset"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"policyId") _x
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
                   (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'assets") _x))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData MultiAsset where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_MultiAsset'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_MultiAsset'policyId x__)
                (Control.DeepSeq.deepseq (_MultiAsset'assets x__) ()))
{- | Fields :
     
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.coinsPerUtxoByte' @:: Lens' PParams Data.Word.Word64@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maxTxSize' @:: Lens' PParams Data.Word.Word64@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.minFeeCoefficient' @:: Lens' PParams Data.Word.Word64@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.minFeeConstant' @:: Lens' PParams Data.Word.Word64@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maxBlockBodySize' @:: Lens' PParams Data.Word.Word64@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maxBlockHeaderSize' @:: Lens' PParams Data.Word.Word64@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.stakeKeyDeposit' @:: Lens' PParams Data.Word.Word64@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.poolDeposit' @:: Lens' PParams Data.Word.Word64@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.poolRetirementEpochBound' @:: Lens' PParams Data.Word.Word64@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.desiredNumberOfPools' @:: Lens' PParams Data.Word.Word64@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.poolInfluence' @:: Lens' PParams RationalNumber@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maybe'poolInfluence' @:: Lens' PParams (Prelude.Maybe RationalNumber)@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.monetaryExpansion' @:: Lens' PParams RationalNumber@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maybe'monetaryExpansion' @:: Lens' PParams (Prelude.Maybe RationalNumber)@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.treasuryExpansion' @:: Lens' PParams RationalNumber@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maybe'treasuryExpansion' @:: Lens' PParams (Prelude.Maybe RationalNumber)@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.minPoolCost' @:: Lens' PParams Data.Word.Word64@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.protocolVersion' @:: Lens' PParams ProtocolVersion@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maybe'protocolVersion' @:: Lens' PParams (Prelude.Maybe ProtocolVersion)@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maxValueSize' @:: Lens' PParams Data.Word.Word64@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.collateralPercentage' @:: Lens' PParams Data.Word.Word64@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maxCollateralInputs' @:: Lens' PParams Data.Word.Word64@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.costModels' @:: Lens' PParams CostModels@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maybe'costModels' @:: Lens' PParams (Prelude.Maybe CostModels)@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.prices' @:: Lens' PParams ExPrices@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maybe'prices' @:: Lens' PParams (Prelude.Maybe ExPrices)@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maxExecutionUnitsPerTransaction' @:: Lens' PParams ExUnits@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maybe'maxExecutionUnitsPerTransaction' @:: Lens' PParams (Prelude.Maybe ExUnits)@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maxExecutionUnitsPerBlock' @:: Lens' PParams ExUnits@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maybe'maxExecutionUnitsPerBlock' @:: Lens' PParams (Prelude.Maybe ExUnits)@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.minFeeScriptRefCostPerByte' @:: Lens' PParams RationalNumber@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maybe'minFeeScriptRefCostPerByte' @:: Lens' PParams (Prelude.Maybe RationalNumber)@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.poolVotingThresholds' @:: Lens' PParams VotingThresholds@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maybe'poolVotingThresholds' @:: Lens' PParams (Prelude.Maybe VotingThresholds)@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.drepVotingThresholds' @:: Lens' PParams VotingThresholds@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maybe'drepVotingThresholds' @:: Lens' PParams (Prelude.Maybe VotingThresholds)@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.minCommitteeSize' @:: Lens' PParams Data.Word.Word32@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.committeeTermLimit' @:: Lens' PParams Data.Word.Word64@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.governanceActionValidityPeriod' @:: Lens' PParams Data.Word.Word64@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.governanceActionDeposit' @:: Lens' PParams Data.Word.Word64@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.drepDeposit' @:: Lens' PParams Data.Word.Word64@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.drepInactivityPeriod' @:: Lens' PParams Data.Word.Word64@ -}
data PParams
  = PParams'_constructor {_PParams'coinsPerUtxoByte :: !Data.Word.Word64,
                          _PParams'maxTxSize :: !Data.Word.Word64,
                          _PParams'minFeeCoefficient :: !Data.Word.Word64,
                          _PParams'minFeeConstant :: !Data.Word.Word64,
                          _PParams'maxBlockBodySize :: !Data.Word.Word64,
                          _PParams'maxBlockHeaderSize :: !Data.Word.Word64,
                          _PParams'stakeKeyDeposit :: !Data.Word.Word64,
                          _PParams'poolDeposit :: !Data.Word.Word64,
                          _PParams'poolRetirementEpochBound :: !Data.Word.Word64,
                          _PParams'desiredNumberOfPools :: !Data.Word.Word64,
                          _PParams'poolInfluence :: !(Prelude.Maybe RationalNumber),
                          _PParams'monetaryExpansion :: !(Prelude.Maybe RationalNumber),
                          _PParams'treasuryExpansion :: !(Prelude.Maybe RationalNumber),
                          _PParams'minPoolCost :: !Data.Word.Word64,
                          _PParams'protocolVersion :: !(Prelude.Maybe ProtocolVersion),
                          _PParams'maxValueSize :: !Data.Word.Word64,
                          _PParams'collateralPercentage :: !Data.Word.Word64,
                          _PParams'maxCollateralInputs :: !Data.Word.Word64,
                          _PParams'costModels :: !(Prelude.Maybe CostModels),
                          _PParams'prices :: !(Prelude.Maybe ExPrices),
                          _PParams'maxExecutionUnitsPerTransaction :: !(Prelude.Maybe ExUnits),
                          _PParams'maxExecutionUnitsPerBlock :: !(Prelude.Maybe ExUnits),
                          _PParams'minFeeScriptRefCostPerByte :: !(Prelude.Maybe RationalNumber),
                          _PParams'poolVotingThresholds :: !(Prelude.Maybe VotingThresholds),
                          _PParams'drepVotingThresholds :: !(Prelude.Maybe VotingThresholds),
                          _PParams'minCommitteeSize :: !Data.Word.Word32,
                          _PParams'committeeTermLimit :: !Data.Word.Word64,
                          _PParams'governanceActionValidityPeriod :: !Data.Word.Word64,
                          _PParams'governanceActionDeposit :: !Data.Word.Word64,
                          _PParams'drepDeposit :: !Data.Word.Word64,
                          _PParams'drepInactivityPeriod :: !Data.Word.Word64,
                          _PParams'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show PParams where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField PParams "coinsPerUtxoByte" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PParams'coinsPerUtxoByte
           (\ x__ y__ -> x__ {_PParams'coinsPerUtxoByte = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField PParams "maxTxSize" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PParams'maxTxSize (\ x__ y__ -> x__ {_PParams'maxTxSize = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField PParams "minFeeCoefficient" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PParams'minFeeCoefficient
           (\ x__ y__ -> x__ {_PParams'minFeeCoefficient = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField PParams "minFeeConstant" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PParams'minFeeConstant
           (\ x__ y__ -> x__ {_PParams'minFeeConstant = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField PParams "maxBlockBodySize" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PParams'maxBlockBodySize
           (\ x__ y__ -> x__ {_PParams'maxBlockBodySize = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField PParams "maxBlockHeaderSize" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PParams'maxBlockHeaderSize
           (\ x__ y__ -> x__ {_PParams'maxBlockHeaderSize = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField PParams "stakeKeyDeposit" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PParams'stakeKeyDeposit
           (\ x__ y__ -> x__ {_PParams'stakeKeyDeposit = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField PParams "poolDeposit" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PParams'poolDeposit
           (\ x__ y__ -> x__ {_PParams'poolDeposit = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField PParams "poolRetirementEpochBound" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PParams'poolRetirementEpochBound
           (\ x__ y__ -> x__ {_PParams'poolRetirementEpochBound = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField PParams "desiredNumberOfPools" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PParams'desiredNumberOfPools
           (\ x__ y__ -> x__ {_PParams'desiredNumberOfPools = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField PParams "poolInfluence" RationalNumber where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PParams'poolInfluence
           (\ x__ y__ -> x__ {_PParams'poolInfluence = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField PParams "maybe'poolInfluence" (Prelude.Maybe RationalNumber) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PParams'poolInfluence
           (\ x__ y__ -> x__ {_PParams'poolInfluence = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField PParams "monetaryExpansion" RationalNumber where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PParams'monetaryExpansion
           (\ x__ y__ -> x__ {_PParams'monetaryExpansion = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField PParams "maybe'monetaryExpansion" (Prelude.Maybe RationalNumber) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PParams'monetaryExpansion
           (\ x__ y__ -> x__ {_PParams'monetaryExpansion = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField PParams "treasuryExpansion" RationalNumber where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PParams'treasuryExpansion
           (\ x__ y__ -> x__ {_PParams'treasuryExpansion = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField PParams "maybe'treasuryExpansion" (Prelude.Maybe RationalNumber) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PParams'treasuryExpansion
           (\ x__ y__ -> x__ {_PParams'treasuryExpansion = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField PParams "minPoolCost" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PParams'minPoolCost
           (\ x__ y__ -> x__ {_PParams'minPoolCost = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField PParams "protocolVersion" ProtocolVersion where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PParams'protocolVersion
           (\ x__ y__ -> x__ {_PParams'protocolVersion = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField PParams "maybe'protocolVersion" (Prelude.Maybe ProtocolVersion) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PParams'protocolVersion
           (\ x__ y__ -> x__ {_PParams'protocolVersion = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField PParams "maxValueSize" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PParams'maxValueSize
           (\ x__ y__ -> x__ {_PParams'maxValueSize = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField PParams "collateralPercentage" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PParams'collateralPercentage
           (\ x__ y__ -> x__ {_PParams'collateralPercentage = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField PParams "maxCollateralInputs" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PParams'maxCollateralInputs
           (\ x__ y__ -> x__ {_PParams'maxCollateralInputs = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField PParams "costModels" CostModels where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PParams'costModels (\ x__ y__ -> x__ {_PParams'costModels = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField PParams "maybe'costModels" (Prelude.Maybe CostModels) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PParams'costModels (\ x__ y__ -> x__ {_PParams'costModels = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField PParams "prices" ExPrices where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PParams'prices (\ x__ y__ -> x__ {_PParams'prices = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField PParams "maybe'prices" (Prelude.Maybe ExPrices) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PParams'prices (\ x__ y__ -> x__ {_PParams'prices = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField PParams "maxExecutionUnitsPerTransaction" ExUnits where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PParams'maxExecutionUnitsPerTransaction
           (\ x__ y__
              -> x__ {_PParams'maxExecutionUnitsPerTransaction = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField PParams "maybe'maxExecutionUnitsPerTransaction" (Prelude.Maybe ExUnits) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PParams'maxExecutionUnitsPerTransaction
           (\ x__ y__
              -> x__ {_PParams'maxExecutionUnitsPerTransaction = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField PParams "maxExecutionUnitsPerBlock" ExUnits where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PParams'maxExecutionUnitsPerBlock
           (\ x__ y__ -> x__ {_PParams'maxExecutionUnitsPerBlock = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField PParams "maybe'maxExecutionUnitsPerBlock" (Prelude.Maybe ExUnits) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PParams'maxExecutionUnitsPerBlock
           (\ x__ y__ -> x__ {_PParams'maxExecutionUnitsPerBlock = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField PParams "minFeeScriptRefCostPerByte" RationalNumber where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PParams'minFeeScriptRefCostPerByte
           (\ x__ y__ -> x__ {_PParams'minFeeScriptRefCostPerByte = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField PParams "maybe'minFeeScriptRefCostPerByte" (Prelude.Maybe RationalNumber) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PParams'minFeeScriptRefCostPerByte
           (\ x__ y__ -> x__ {_PParams'minFeeScriptRefCostPerByte = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField PParams "poolVotingThresholds" VotingThresholds where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PParams'poolVotingThresholds
           (\ x__ y__ -> x__ {_PParams'poolVotingThresholds = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField PParams "maybe'poolVotingThresholds" (Prelude.Maybe VotingThresholds) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PParams'poolVotingThresholds
           (\ x__ y__ -> x__ {_PParams'poolVotingThresholds = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField PParams "drepVotingThresholds" VotingThresholds where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PParams'drepVotingThresholds
           (\ x__ y__ -> x__ {_PParams'drepVotingThresholds = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField PParams "maybe'drepVotingThresholds" (Prelude.Maybe VotingThresholds) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PParams'drepVotingThresholds
           (\ x__ y__ -> x__ {_PParams'drepVotingThresholds = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField PParams "minCommitteeSize" Data.Word.Word32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PParams'minCommitteeSize
           (\ x__ y__ -> x__ {_PParams'minCommitteeSize = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField PParams "committeeTermLimit" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PParams'committeeTermLimit
           (\ x__ y__ -> x__ {_PParams'committeeTermLimit = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField PParams "governanceActionValidityPeriod" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PParams'governanceActionValidityPeriod
           (\ x__ y__ -> x__ {_PParams'governanceActionValidityPeriod = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField PParams "governanceActionDeposit" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PParams'governanceActionDeposit
           (\ x__ y__ -> x__ {_PParams'governanceActionDeposit = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField PParams "drepDeposit" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PParams'drepDeposit
           (\ x__ y__ -> x__ {_PParams'drepDeposit = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField PParams "drepInactivityPeriod" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PParams'drepInactivityPeriod
           (\ x__ y__ -> x__ {_PParams'drepInactivityPeriod = y__}))
        Prelude.id
instance Data.ProtoLens.Message PParams where
  messageName _ = Data.Text.pack "utxorpc.v1alpha.cardano.PParams"
  packedMessageDescriptor _
    = "\n\
      \\aPParams\DC21\n\
      \\DC3coins_per_utxo_byte\CAN\SOH \SOH(\EOTR\DLEcoinsPerUtxoByteB\STX0\SOH\DC2\"\n\
      \\vmax_tx_size\CAN\STX \SOH(\EOTR\tmaxTxSizeB\STX0\SOH\DC22\n\
      \\DC3min_fee_coefficient\CAN\ETX \SOH(\EOTR\DC1minFeeCoefficientB\STX0\SOH\DC2,\n\
      \\DLEmin_fee_constant\CAN\EOT \SOH(\EOTR\SOminFeeConstantB\STX0\SOH\DC21\n\
      \\DC3max_block_body_size\CAN\ENQ \SOH(\EOTR\DLEmaxBlockBodySizeB\STX0\SOH\DC25\n\
      \\NAKmax_block_header_size\CAN\ACK \SOH(\EOTR\DC2maxBlockHeaderSizeB\STX0\SOH\DC2.\n\
      \\DC1stake_key_deposit\CAN\a \SOH(\EOTR\SIstakeKeyDepositB\STX0\SOH\DC2%\n\
      \\fpool_deposit\CAN\b \SOH(\EOTR\vpoolDepositB\STX0\SOH\DC2=\n\
      \\ESCpool_retirement_epoch_bound\CAN\t \SOH(\EOTR\CANpoolRetirementEpochBound\DC25\n\
      \\ETBdesired_number_of_pools\CAN\n\
      \ \SOH(\EOTR\DC4desiredNumberOfPools\DC2N\n\
      \\SOpool_influence\CAN\v \SOH(\v2'.utxorpc.v1alpha.cardano.RationalNumberR\rpoolInfluence\DC2V\n\
      \\DC2monetary_expansion\CAN\f \SOH(\v2'.utxorpc.v1alpha.cardano.RationalNumberR\DC1monetaryExpansion\DC2V\n\
      \\DC2treasury_expansion\CAN\r \SOH(\v2'.utxorpc.v1alpha.cardano.RationalNumberR\DC1treasuryExpansion\DC2&\n\
      \\rmin_pool_cost\CAN\SO \SOH(\EOTR\vminPoolCostB\STX0\SOH\DC2S\n\
      \\DLEprotocol_version\CAN\SI \SOH(\v2(.utxorpc.v1alpha.cardano.ProtocolVersionR\SIprotocolVersion\DC2(\n\
      \\SOmax_value_size\CAN\DLE \SOH(\EOTR\fmaxValueSizeB\STX0\SOH\DC27\n\
      \\NAKcollateral_percentage\CAN\DC1 \SOH(\EOTR\DC4collateralPercentageB\STX0\SOH\DC26\n\
      \\NAKmax_collateral_inputs\CAN\DC2 \SOH(\EOTR\DC3maxCollateralInputsB\STX0\SOH\DC2D\n\
      \\vcost_models\CAN\DC3 \SOH(\v2#.utxorpc.v1alpha.cardano.CostModelsR\n\
      \costModels\DC29\n\
      \\ACKprices\CAN\DC4 \SOH(\v2!.utxorpc.v1alpha.cardano.ExPricesR\ACKprices\DC2n\n\
      \#max_execution_units_per_transaction\CAN\NAK \SOH(\v2 .utxorpc.v1alpha.cardano.ExUnitsR\USmaxExecutionUnitsPerTransaction\DC2b\n\
      \\GSmax_execution_units_per_block\CAN\SYN \SOH(\v2 .utxorpc.v1alpha.cardano.ExUnitsR\EMmaxExecutionUnitsPerBlock\DC2m\n\
      \ min_fee_script_ref_cost_per_byte\CAN\ETB \SOH(\v2'.utxorpc.v1alpha.cardano.RationalNumberR\SUBminFeeScriptRefCostPerByte\DC2_\n\
      \\SYNpool_voting_thresholds\CAN\CAN \SOH(\v2).utxorpc.v1alpha.cardano.VotingThresholdsR\DC4poolVotingThresholds\DC2_\n\
      \\SYNdrep_voting_thresholds\CAN\EM \SOH(\v2).utxorpc.v1alpha.cardano.VotingThresholdsR\DC4drepVotingThresholds\DC2,\n\
      \\DC2min_committee_size\CAN\SUB \SOH(\rR\DLEminCommitteeSize\DC20\n\
      \\DC4committee_term_limit\CAN\ESC \SOH(\EOTR\DC2committeeTermLimit\DC2I\n\
      \!governance_action_validity_period\CAN\FS \SOH(\EOTR\RSgovernanceActionValidityPeriod\DC2>\n\
      \\EMgovernance_action_deposit\CAN\GS \SOH(\EOTR\ETBgovernanceActionDepositB\STX0\SOH\DC2%\n\
      \\fdrep_deposit\CAN\RS \SOH(\EOTR\vdrepDepositB\STX0\SOH\DC24\n\
      \\SYNdrep_inactivity_period\CAN\US \SOH(\EOTR\DC4drepInactivityPeriod"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        coinsPerUtxoByte__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "coins_per_utxo_byte"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"coinsPerUtxoByte")) ::
              Data.ProtoLens.FieldDescriptor PParams
        maxTxSize__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "max_tx_size"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"maxTxSize")) ::
              Data.ProtoLens.FieldDescriptor PParams
        minFeeCoefficient__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "min_fee_coefficient"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"minFeeCoefficient")) ::
              Data.ProtoLens.FieldDescriptor PParams
        minFeeConstant__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "min_fee_constant"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"minFeeConstant")) ::
              Data.ProtoLens.FieldDescriptor PParams
        maxBlockBodySize__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "max_block_body_size"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"maxBlockBodySize")) ::
              Data.ProtoLens.FieldDescriptor PParams
        maxBlockHeaderSize__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "max_block_header_size"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"maxBlockHeaderSize")) ::
              Data.ProtoLens.FieldDescriptor PParams
        stakeKeyDeposit__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "stake_key_deposit"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"stakeKeyDeposit")) ::
              Data.ProtoLens.FieldDescriptor PParams
        poolDeposit__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "pool_deposit"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"poolDeposit")) ::
              Data.ProtoLens.FieldDescriptor PParams
        poolRetirementEpochBound__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "pool_retirement_epoch_bound"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"poolRetirementEpochBound")) ::
              Data.ProtoLens.FieldDescriptor PParams
        desiredNumberOfPools__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "desired_number_of_pools"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"desiredNumberOfPools")) ::
              Data.ProtoLens.FieldDescriptor PParams
        poolInfluence__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "pool_influence"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor RationalNumber)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'poolInfluence")) ::
              Data.ProtoLens.FieldDescriptor PParams
        monetaryExpansion__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "monetary_expansion"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor RationalNumber)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'monetaryExpansion")) ::
              Data.ProtoLens.FieldDescriptor PParams
        treasuryExpansion__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "treasury_expansion"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor RationalNumber)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'treasuryExpansion")) ::
              Data.ProtoLens.FieldDescriptor PParams
        minPoolCost__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "min_pool_cost"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"minPoolCost")) ::
              Data.ProtoLens.FieldDescriptor PParams
        protocolVersion__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "protocol_version"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ProtocolVersion)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'protocolVersion")) ::
              Data.ProtoLens.FieldDescriptor PParams
        maxValueSize__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "max_value_size"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"maxValueSize")) ::
              Data.ProtoLens.FieldDescriptor PParams
        collateralPercentage__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "collateral_percentage"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"collateralPercentage")) ::
              Data.ProtoLens.FieldDescriptor PParams
        maxCollateralInputs__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "max_collateral_inputs"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"maxCollateralInputs")) ::
              Data.ProtoLens.FieldDescriptor PParams
        costModels__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "cost_models"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor CostModels)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'costModels")) ::
              Data.ProtoLens.FieldDescriptor PParams
        prices__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "prices"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ExPrices)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'prices")) ::
              Data.ProtoLens.FieldDescriptor PParams
        maxExecutionUnitsPerTransaction__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "max_execution_units_per_transaction"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ExUnits)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field
                    @"maybe'maxExecutionUnitsPerTransaction")) ::
              Data.ProtoLens.FieldDescriptor PParams
        maxExecutionUnitsPerBlock__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "max_execution_units_per_block"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ExUnits)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'maxExecutionUnitsPerBlock")) ::
              Data.ProtoLens.FieldDescriptor PParams
        minFeeScriptRefCostPerByte__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "min_fee_script_ref_cost_per_byte"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor RationalNumber)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field
                    @"maybe'minFeeScriptRefCostPerByte")) ::
              Data.ProtoLens.FieldDescriptor PParams
        poolVotingThresholds__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "pool_voting_thresholds"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor VotingThresholds)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'poolVotingThresholds")) ::
              Data.ProtoLens.FieldDescriptor PParams
        drepVotingThresholds__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "drep_voting_thresholds"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor VotingThresholds)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'drepVotingThresholds")) ::
              Data.ProtoLens.FieldDescriptor PParams
        minCommitteeSize__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "min_committee_size"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"minCommitteeSize")) ::
              Data.ProtoLens.FieldDescriptor PParams
        committeeTermLimit__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "committee_term_limit"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"committeeTermLimit")) ::
              Data.ProtoLens.FieldDescriptor PParams
        governanceActionValidityPeriod__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "governance_action_validity_period"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"governanceActionValidityPeriod")) ::
              Data.ProtoLens.FieldDescriptor PParams
        governanceActionDeposit__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "governance_action_deposit"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"governanceActionDeposit")) ::
              Data.ProtoLens.FieldDescriptor PParams
        drepDeposit__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "drep_deposit"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"drepDeposit")) ::
              Data.ProtoLens.FieldDescriptor PParams
        drepInactivityPeriod__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "drep_inactivity_period"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"drepInactivityPeriod")) ::
              Data.ProtoLens.FieldDescriptor PParams
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, coinsPerUtxoByte__field_descriptor),
           (Data.ProtoLens.Tag 2, maxTxSize__field_descriptor),
           (Data.ProtoLens.Tag 3, minFeeCoefficient__field_descriptor),
           (Data.ProtoLens.Tag 4, minFeeConstant__field_descriptor),
           (Data.ProtoLens.Tag 5, maxBlockBodySize__field_descriptor),
           (Data.ProtoLens.Tag 6, maxBlockHeaderSize__field_descriptor),
           (Data.ProtoLens.Tag 7, stakeKeyDeposit__field_descriptor),
           (Data.ProtoLens.Tag 8, poolDeposit__field_descriptor),
           (Data.ProtoLens.Tag 9, poolRetirementEpochBound__field_descriptor),
           (Data.ProtoLens.Tag 10, desiredNumberOfPools__field_descriptor),
           (Data.ProtoLens.Tag 11, poolInfluence__field_descriptor),
           (Data.ProtoLens.Tag 12, monetaryExpansion__field_descriptor),
           (Data.ProtoLens.Tag 13, treasuryExpansion__field_descriptor),
           (Data.ProtoLens.Tag 14, minPoolCost__field_descriptor),
           (Data.ProtoLens.Tag 15, protocolVersion__field_descriptor),
           (Data.ProtoLens.Tag 16, maxValueSize__field_descriptor),
           (Data.ProtoLens.Tag 17, collateralPercentage__field_descriptor),
           (Data.ProtoLens.Tag 18, maxCollateralInputs__field_descriptor),
           (Data.ProtoLens.Tag 19, costModels__field_descriptor),
           (Data.ProtoLens.Tag 20, prices__field_descriptor),
           (Data.ProtoLens.Tag 21, 
            maxExecutionUnitsPerTransaction__field_descriptor),
           (Data.ProtoLens.Tag 22, 
            maxExecutionUnitsPerBlock__field_descriptor),
           (Data.ProtoLens.Tag 23, 
            minFeeScriptRefCostPerByte__field_descriptor),
           (Data.ProtoLens.Tag 24, poolVotingThresholds__field_descriptor),
           (Data.ProtoLens.Tag 25, drepVotingThresholds__field_descriptor),
           (Data.ProtoLens.Tag 26, minCommitteeSize__field_descriptor),
           (Data.ProtoLens.Tag 27, committeeTermLimit__field_descriptor),
           (Data.ProtoLens.Tag 28, 
            governanceActionValidityPeriod__field_descriptor),
           (Data.ProtoLens.Tag 29, governanceActionDeposit__field_descriptor),
           (Data.ProtoLens.Tag 30, drepDeposit__field_descriptor),
           (Data.ProtoLens.Tag 31, drepInactivityPeriod__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _PParams'_unknownFields
        (\ x__ y__ -> x__ {_PParams'_unknownFields = y__})
  defMessage
    = PParams'_constructor
        {_PParams'coinsPerUtxoByte = Data.ProtoLens.fieldDefault,
         _PParams'maxTxSize = Data.ProtoLens.fieldDefault,
         _PParams'minFeeCoefficient = Data.ProtoLens.fieldDefault,
         _PParams'minFeeConstant = Data.ProtoLens.fieldDefault,
         _PParams'maxBlockBodySize = Data.ProtoLens.fieldDefault,
         _PParams'maxBlockHeaderSize = Data.ProtoLens.fieldDefault,
         _PParams'stakeKeyDeposit = Data.ProtoLens.fieldDefault,
         _PParams'poolDeposit = Data.ProtoLens.fieldDefault,
         _PParams'poolRetirementEpochBound = Data.ProtoLens.fieldDefault,
         _PParams'desiredNumberOfPools = Data.ProtoLens.fieldDefault,
         _PParams'poolInfluence = Prelude.Nothing,
         _PParams'monetaryExpansion = Prelude.Nothing,
         _PParams'treasuryExpansion = Prelude.Nothing,
         _PParams'minPoolCost = Data.ProtoLens.fieldDefault,
         _PParams'protocolVersion = Prelude.Nothing,
         _PParams'maxValueSize = Data.ProtoLens.fieldDefault,
         _PParams'collateralPercentage = Data.ProtoLens.fieldDefault,
         _PParams'maxCollateralInputs = Data.ProtoLens.fieldDefault,
         _PParams'costModels = Prelude.Nothing,
         _PParams'prices = Prelude.Nothing,
         _PParams'maxExecutionUnitsPerTransaction = Prelude.Nothing,
         _PParams'maxExecutionUnitsPerBlock = Prelude.Nothing,
         _PParams'minFeeScriptRefCostPerByte = Prelude.Nothing,
         _PParams'poolVotingThresholds = Prelude.Nothing,
         _PParams'drepVotingThresholds = Prelude.Nothing,
         _PParams'minCommitteeSize = Data.ProtoLens.fieldDefault,
         _PParams'committeeTermLimit = Data.ProtoLens.fieldDefault,
         _PParams'governanceActionValidityPeriod = Data.ProtoLens.fieldDefault,
         _PParams'governanceActionDeposit = Data.ProtoLens.fieldDefault,
         _PParams'drepDeposit = Data.ProtoLens.fieldDefault,
         _PParams'drepInactivityPeriod = Data.ProtoLens.fieldDefault,
         _PParams'_unknownFields = []}
  parseMessage
    = let
        loop :: PParams -> Data.ProtoLens.Encoding.Bytes.Parser PParams
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
                                       Data.ProtoLens.Encoding.Bytes.getVarInt "coins_per_utxo_byte"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"coinsPerUtxoByte") y x)
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getVarInt "max_tx_size"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"maxTxSize") y x)
                        24
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getVarInt "min_fee_coefficient"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"minFeeCoefficient") y x)
                        32
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getVarInt "min_fee_constant"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"minFeeConstant") y x)
                        40
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getVarInt "max_block_body_size"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"maxBlockBodySize") y x)
                        48
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getVarInt
                                       "max_block_header_size"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"maxBlockHeaderSize") y x)
                        56
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getVarInt "stake_key_deposit"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"stakeKeyDeposit") y x)
                        64
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getVarInt "pool_deposit"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"poolDeposit") y x)
                        72
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getVarInt
                                       "pool_retirement_epoch_bound"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"poolRetirementEpochBound") y x)
                        80
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getVarInt
                                       "desired_number_of_pools"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"desiredNumberOfPools") y x)
                        90
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "pool_influence"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"poolInfluence") y x)
                        98
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "monetary_expansion"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"monetaryExpansion") y x)
                        106
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "treasury_expansion"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"treasuryExpansion") y x)
                        112
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getVarInt "min_pool_cost"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"minPoolCost") y x)
                        122
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "protocol_version"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"protocolVersion") y x)
                        128
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getVarInt "max_value_size"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"maxValueSize") y x)
                        136
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getVarInt
                                       "collateral_percentage"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"collateralPercentage") y x)
                        144
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getVarInt
                                       "max_collateral_inputs"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"maxCollateralInputs") y x)
                        154
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "cost_models"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"costModels") y x)
                        162
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "prices"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"prices") y x)
                        170
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "max_execution_units_per_transaction"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"maxExecutionUnitsPerTransaction")
                                     y x)
                        178
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "max_execution_units_per_block"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"maxExecutionUnitsPerBlock") y x)
                        186
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "min_fee_script_ref_cost_per_byte"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"minFeeScriptRefCostPerByte") y x)
                        194
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "pool_voting_thresholds"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"poolVotingThresholds") y x)
                        202
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "drep_voting_thresholds"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"drepVotingThresholds") y x)
                        208
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "min_committee_size"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"minCommitteeSize") y x)
                        216
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getVarInt
                                       "committee_term_limit"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"committeeTermLimit") y x)
                        224
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getVarInt
                                       "governance_action_validity_period"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"governanceActionValidityPeriod")
                                     y x)
                        232
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getVarInt
                                       "governance_action_deposit"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"governanceActionDeposit") y x)
                        240
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getVarInt "drep_deposit"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"drepDeposit") y x)
                        248
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getVarInt
                                       "drep_inactivity_period"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"drepInactivityPeriod") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "PParams"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v
                  = Lens.Family2.view
                      (Data.ProtoLens.Field.field @"coinsPerUtxoByte") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt _v))
             ((Data.Monoid.<>)
                (let
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"maxTxSize") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 16)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt _v))
                ((Data.Monoid.<>)
                   (let
                      _v
                        = Lens.Family2.view
                            (Data.ProtoLens.Field.field @"minFeeCoefficient") _x
                    in
                      if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty
                      else
                          (Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 24)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt _v))
                   ((Data.Monoid.<>)
                      (let
                         _v
                           = Lens.Family2.view
                               (Data.ProtoLens.Field.field @"minFeeConstant") _x
                       in
                         if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                             Data.Monoid.mempty
                         else
                             (Data.Monoid.<>)
                               (Data.ProtoLens.Encoding.Bytes.putVarInt 32)
                               (Data.ProtoLens.Encoding.Bytes.putVarInt _v))
                      ((Data.Monoid.<>)
                         (let
                            _v
                              = Lens.Family2.view
                                  (Data.ProtoLens.Field.field @"maxBlockBodySize") _x
                          in
                            if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                Data.Monoid.mempty
                            else
                                (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt 40)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt _v))
                         ((Data.Monoid.<>)
                            (let
                               _v
                                 = Lens.Family2.view
                                     (Data.ProtoLens.Field.field @"maxBlockHeaderSize") _x
                             in
                               if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                   Data.Monoid.mempty
                               else
                                   (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt 48)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt _v))
                            ((Data.Monoid.<>)
                               (let
                                  _v
                                    = Lens.Family2.view
                                        (Data.ProtoLens.Field.field @"stakeKeyDeposit") _x
                                in
                                  if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                      Data.Monoid.mempty
                                  else
                                      (Data.Monoid.<>)
                                        (Data.ProtoLens.Encoding.Bytes.putVarInt 56)
                                        (Data.ProtoLens.Encoding.Bytes.putVarInt _v))
                               ((Data.Monoid.<>)
                                  (let
                                     _v
                                       = Lens.Family2.view
                                           (Data.ProtoLens.Field.field @"poolDeposit") _x
                                   in
                                     if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                         Data.Monoid.mempty
                                     else
                                         (Data.Monoid.<>)
                                           (Data.ProtoLens.Encoding.Bytes.putVarInt 64)
                                           (Data.ProtoLens.Encoding.Bytes.putVarInt _v))
                                  ((Data.Monoid.<>)
                                     (let
                                        _v
                                          = Lens.Family2.view
                                              (Data.ProtoLens.Field.field
                                                 @"poolRetirementEpochBound")
                                              _x
                                      in
                                        if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                            Data.Monoid.mempty
                                        else
                                            (Data.Monoid.<>)
                                              (Data.ProtoLens.Encoding.Bytes.putVarInt 72)
                                              (Data.ProtoLens.Encoding.Bytes.putVarInt _v))
                                     ((Data.Monoid.<>)
                                        (let
                                           _v
                                             = Lens.Family2.view
                                                 (Data.ProtoLens.Field.field
                                                    @"desiredNumberOfPools")
                                                 _x
                                         in
                                           if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                               Data.Monoid.mempty
                                           else
                                               (Data.Monoid.<>)
                                                 (Data.ProtoLens.Encoding.Bytes.putVarInt 80)
                                                 (Data.ProtoLens.Encoding.Bytes.putVarInt _v))
                                        ((Data.Monoid.<>)
                                           (case
                                                Lens.Family2.view
                                                  (Data.ProtoLens.Field.field
                                                     @"maybe'poolInfluence")
                                                  _x
                                            of
                                              Prelude.Nothing -> Data.Monoid.mempty
                                              (Prelude.Just _v)
                                                -> (Data.Monoid.<>)
                                                     (Data.ProtoLens.Encoding.Bytes.putVarInt 90)
                                                     ((Prelude..)
                                                        (\ bs
                                                           -> (Data.Monoid.<>)
                                                                (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                   (Prelude.fromIntegral
                                                                      (Data.ByteString.length bs)))
                                                                (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                   bs))
                                                        Data.ProtoLens.encodeMessage _v))
                                           ((Data.Monoid.<>)
                                              (case
                                                   Lens.Family2.view
                                                     (Data.ProtoLens.Field.field
                                                        @"maybe'monetaryExpansion")
                                                     _x
                                               of
                                                 Prelude.Nothing -> Data.Monoid.mempty
                                                 (Prelude.Just _v)
                                                   -> (Data.Monoid.<>)
                                                        (Data.ProtoLens.Encoding.Bytes.putVarInt 98)
                                                        ((Prelude..)
                                                           (\ bs
                                                              -> (Data.Monoid.<>)
                                                                   (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                      (Prelude.fromIntegral
                                                                         (Data.ByteString.length
                                                                            bs)))
                                                                   (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                      bs))
                                                           Data.ProtoLens.encodeMessage _v))
                                              ((Data.Monoid.<>)
                                                 (case
                                                      Lens.Family2.view
                                                        (Data.ProtoLens.Field.field
                                                           @"maybe'treasuryExpansion")
                                                        _x
                                                  of
                                                    Prelude.Nothing -> Data.Monoid.mempty
                                                    (Prelude.Just _v)
                                                      -> (Data.Monoid.<>)
                                                           (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                              106)
                                                           ((Prelude..)
                                                              (\ bs
                                                                 -> (Data.Monoid.<>)
                                                                      (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                         (Prelude.fromIntegral
                                                                            (Data.ByteString.length
                                                                               bs)))
                                                                      (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                         bs))
                                                              Data.ProtoLens.encodeMessage _v))
                                                 ((Data.Monoid.<>)
                                                    (let
                                                       _v
                                                         = Lens.Family2.view
                                                             (Data.ProtoLens.Field.field
                                                                @"minPoolCost")
                                                             _x
                                                     in
                                                       if (Prelude.==)
                                                            _v Data.ProtoLens.fieldDefault then
                                                           Data.Monoid.mempty
                                                       else
                                                           (Data.Monoid.<>)
                                                             (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                112)
                                                             (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                _v))
                                                    ((Data.Monoid.<>)
                                                       (case
                                                            Lens.Family2.view
                                                              (Data.ProtoLens.Field.field
                                                                 @"maybe'protocolVersion")
                                                              _x
                                                        of
                                                          Prelude.Nothing -> Data.Monoid.mempty
                                                          (Prelude.Just _v)
                                                            -> (Data.Monoid.<>)
                                                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                    122)
                                                                 ((Prelude..)
                                                                    (\ bs
                                                                       -> (Data.Monoid.<>)
                                                                            (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                               (Prelude.fromIntegral
                                                                                  (Data.ByteString.length
                                                                                     bs)))
                                                                            (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                               bs))
                                                                    Data.ProtoLens.encodeMessage
                                                                    _v))
                                                       ((Data.Monoid.<>)
                                                          (let
                                                             _v
                                                               = Lens.Family2.view
                                                                   (Data.ProtoLens.Field.field
                                                                      @"maxValueSize")
                                                                   _x
                                                           in
                                                             if (Prelude.==)
                                                                  _v
                                                                  Data.ProtoLens.fieldDefault then
                                                                 Data.Monoid.mempty
                                                             else
                                                                 (Data.Monoid.<>)
                                                                   (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                      128)
                                                                   (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                      _v))
                                                          ((Data.Monoid.<>)
                                                             (let
                                                                _v
                                                                  = Lens.Family2.view
                                                                      (Data.ProtoLens.Field.field
                                                                         @"collateralPercentage")
                                                                      _x
                                                              in
                                                                if (Prelude.==)
                                                                     _v
                                                                     Data.ProtoLens.fieldDefault then
                                                                    Data.Monoid.mempty
                                                                else
                                                                    (Data.Monoid.<>)
                                                                      (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                         136)
                                                                      (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                         _v))
                                                             ((Data.Monoid.<>)
                                                                (let
                                                                   _v
                                                                     = Lens.Family2.view
                                                                         (Data.ProtoLens.Field.field
                                                                            @"maxCollateralInputs")
                                                                         _x
                                                                 in
                                                                   if (Prelude.==)
                                                                        _v
                                                                        Data.ProtoLens.fieldDefault then
                                                                       Data.Monoid.mempty
                                                                   else
                                                                       (Data.Monoid.<>)
                                                                         (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                            144)
                                                                         (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                            _v))
                                                                ((Data.Monoid.<>)
                                                                   (case
                                                                        Lens.Family2.view
                                                                          (Data.ProtoLens.Field.field
                                                                             @"maybe'costModels")
                                                                          _x
                                                                    of
                                                                      Prelude.Nothing
                                                                        -> Data.Monoid.mempty
                                                                      (Prelude.Just _v)
                                                                        -> (Data.Monoid.<>)
                                                                             (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                154)
                                                                             ((Prelude..)
                                                                                (\ bs
                                                                                   -> (Data.Monoid.<>)
                                                                                        (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                           (Prelude.fromIntegral
                                                                                              (Data.ByteString.length
                                                                                                 bs)))
                                                                                        (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                           bs))
                                                                                Data.ProtoLens.encodeMessage
                                                                                _v))
                                                                   ((Data.Monoid.<>)
                                                                      (case
                                                                           Lens.Family2.view
                                                                             (Data.ProtoLens.Field.field
                                                                                @"maybe'prices")
                                                                             _x
                                                                       of
                                                                         Prelude.Nothing
                                                                           -> Data.Monoid.mempty
                                                                         (Prelude.Just _v)
                                                                           -> (Data.Monoid.<>)
                                                                                (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                   162)
                                                                                ((Prelude..)
                                                                                   (\ bs
                                                                                      -> (Data.Monoid.<>)
                                                                                           (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                              (Prelude.fromIntegral
                                                                                                 (Data.ByteString.length
                                                                                                    bs)))
                                                                                           (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                              bs))
                                                                                   Data.ProtoLens.encodeMessage
                                                                                   _v))
                                                                      ((Data.Monoid.<>)
                                                                         (case
                                                                              Lens.Family2.view
                                                                                (Data.ProtoLens.Field.field
                                                                                   @"maybe'maxExecutionUnitsPerTransaction")
                                                                                _x
                                                                          of
                                                                            Prelude.Nothing
                                                                              -> Data.Monoid.mempty
                                                                            (Prelude.Just _v)
                                                                              -> (Data.Monoid.<>)
                                                                                   (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                      170)
                                                                                   ((Prelude..)
                                                                                      (\ bs
                                                                                         -> (Data.Monoid.<>)
                                                                                              (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                 (Prelude.fromIntegral
                                                                                                    (Data.ByteString.length
                                                                                                       bs)))
                                                                                              (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                                 bs))
                                                                                      Data.ProtoLens.encodeMessage
                                                                                      _v))
                                                                         ((Data.Monoid.<>)
                                                                            (case
                                                                                 Lens.Family2.view
                                                                                   (Data.ProtoLens.Field.field
                                                                                      @"maybe'maxExecutionUnitsPerBlock")
                                                                                   _x
                                                                             of
                                                                               Prelude.Nothing
                                                                                 -> Data.Monoid.mempty
                                                                               (Prelude.Just _v)
                                                                                 -> (Data.Monoid.<>)
                                                                                      (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                         178)
                                                                                      ((Prelude..)
                                                                                         (\ bs
                                                                                            -> (Data.Monoid.<>)
                                                                                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                    (Prelude.fromIntegral
                                                                                                       (Data.ByteString.length
                                                                                                          bs)))
                                                                                                 (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                                    bs))
                                                                                         Data.ProtoLens.encodeMessage
                                                                                         _v))
                                                                            ((Data.Monoid.<>)
                                                                               (case
                                                                                    Lens.Family2.view
                                                                                      (Data.ProtoLens.Field.field
                                                                                         @"maybe'minFeeScriptRefCostPerByte")
                                                                                      _x
                                                                                of
                                                                                  Prelude.Nothing
                                                                                    -> Data.Monoid.mempty
                                                                                  (Prelude.Just _v)
                                                                                    -> (Data.Monoid.<>)
                                                                                         (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                            186)
                                                                                         ((Prelude..)
                                                                                            (\ bs
                                                                                               -> (Data.Monoid.<>)
                                                                                                    (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                       (Prelude.fromIntegral
                                                                                                          (Data.ByteString.length
                                                                                                             bs)))
                                                                                                    (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                                       bs))
                                                                                            Data.ProtoLens.encodeMessage
                                                                                            _v))
                                                                               ((Data.Monoid.<>)
                                                                                  (case
                                                                                       Lens.Family2.view
                                                                                         (Data.ProtoLens.Field.field
                                                                                            @"maybe'poolVotingThresholds")
                                                                                         _x
                                                                                   of
                                                                                     Prelude.Nothing
                                                                                       -> Data.Monoid.mempty
                                                                                     (Prelude.Just _v)
                                                                                       -> (Data.Monoid.<>)
                                                                                            (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                               194)
                                                                                            ((Prelude..)
                                                                                               (\ bs
                                                                                                  -> (Data.Monoid.<>)
                                                                                                       (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                          (Prelude.fromIntegral
                                                                                                             (Data.ByteString.length
                                                                                                                bs)))
                                                                                                       (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                                          bs))
                                                                                               Data.ProtoLens.encodeMessage
                                                                                               _v))
                                                                                  ((Data.Monoid.<>)
                                                                                     (case
                                                                                          Lens.Family2.view
                                                                                            (Data.ProtoLens.Field.field
                                                                                               @"maybe'drepVotingThresholds")
                                                                                            _x
                                                                                      of
                                                                                        Prelude.Nothing
                                                                                          -> Data.Monoid.mempty
                                                                                        (Prelude.Just _v)
                                                                                          -> (Data.Monoid.<>)
                                                                                               (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                  202)
                                                                                               ((Prelude..)
                                                                                                  (\ bs
                                                                                                     -> (Data.Monoid.<>)
                                                                                                          (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                             (Prelude.fromIntegral
                                                                                                                (Data.ByteString.length
                                                                                                                   bs)))
                                                                                                          (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                                             bs))
                                                                                                  Data.ProtoLens.encodeMessage
                                                                                                  _v))
                                                                                     ((Data.Monoid.<>)
                                                                                        (let
                                                                                           _v
                                                                                             = Lens.Family2.view
                                                                                                 (Data.ProtoLens.Field.field
                                                                                                    @"minCommitteeSize")
                                                                                                 _x
                                                                                         in
                                                                                           if (Prelude.==)
                                                                                                _v
                                                                                                Data.ProtoLens.fieldDefault then
                                                                                               Data.Monoid.mempty
                                                                                           else
                                                                                               (Data.Monoid.<>)
                                                                                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                    208)
                                                                                                 ((Prelude..)
                                                                                                    Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                    Prelude.fromIntegral
                                                                                                    _v))
                                                                                        ((Data.Monoid.<>)
                                                                                           (let
                                                                                              _v
                                                                                                = Lens.Family2.view
                                                                                                    (Data.ProtoLens.Field.field
                                                                                                       @"committeeTermLimit")
                                                                                                    _x
                                                                                            in
                                                                                              if (Prelude.==)
                                                                                                   _v
                                                                                                   Data.ProtoLens.fieldDefault then
                                                                                                  Data.Monoid.mempty
                                                                                              else
                                                                                                  (Data.Monoid.<>)
                                                                                                    (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                       216)
                                                                                                    (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                       _v))
                                                                                           ((Data.Monoid.<>)
                                                                                              (let
                                                                                                 _v
                                                                                                   = Lens.Family2.view
                                                                                                       (Data.ProtoLens.Field.field
                                                                                                          @"governanceActionValidityPeriod")
                                                                                                       _x
                                                                                               in
                                                                                                 if (Prelude.==)
                                                                                                      _v
                                                                                                      Data.ProtoLens.fieldDefault then
                                                                                                     Data.Monoid.mempty
                                                                                                 else
                                                                                                     (Data.Monoid.<>)
                                                                                                       (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                          224)
                                                                                                       (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                          _v))
                                                                                              ((Data.Monoid.<>)
                                                                                                 (let
                                                                                                    _v
                                                                                                      = Lens.Family2.view
                                                                                                          (Data.ProtoLens.Field.field
                                                                                                             @"governanceActionDeposit")
                                                                                                          _x
                                                                                                  in
                                                                                                    if (Prelude.==)
                                                                                                         _v
                                                                                                         Data.ProtoLens.fieldDefault then
                                                                                                        Data.Monoid.mempty
                                                                                                    else
                                                                                                        (Data.Monoid.<>)
                                                                                                          (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                             232)
                                                                                                          (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                             _v))
                                                                                                 ((Data.Monoid.<>)
                                                                                                    (let
                                                                                                       _v
                                                                                                         = Lens.Family2.view
                                                                                                             (Data.ProtoLens.Field.field
                                                                                                                @"drepDeposit")
                                                                                                             _x
                                                                                                     in
                                                                                                       if (Prelude.==)
                                                                                                            _v
                                                                                                            Data.ProtoLens.fieldDefault then
                                                                                                           Data.Monoid.mempty
                                                                                                       else
                                                                                                           (Data.Monoid.<>)
                                                                                                             (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                240)
                                                                                                             (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                _v))
                                                                                                    ((Data.Monoid.<>)
                                                                                                       (let
                                                                                                          _v
                                                                                                            = Lens.Family2.view
                                                                                                                (Data.ProtoLens.Field.field
                                                                                                                   @"drepInactivityPeriod")
                                                                                                                _x
                                                                                                        in
                                                                                                          if (Prelude.==)
                                                                                                               _v
                                                                                                               Data.ProtoLens.fieldDefault then
                                                                                                              Data.Monoid.mempty
                                                                                                          else
                                                                                                              (Data.Monoid.<>)
                                                                                                                (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                   248)
                                                                                                                (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                   _v))
                                                                                                       (Data.ProtoLens.Encoding.Wire.buildFieldSet
                                                                                                          (Lens.Family2.view
                                                                                                             Data.ProtoLens.unknownFields
                                                                                                             _x))))))))))))))))))))))))))))))))
instance Control.DeepSeq.NFData PParams where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_PParams'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_PParams'coinsPerUtxoByte x__)
                (Control.DeepSeq.deepseq
                   (_PParams'maxTxSize x__)
                   (Control.DeepSeq.deepseq
                      (_PParams'minFeeCoefficient x__)
                      (Control.DeepSeq.deepseq
                         (_PParams'minFeeConstant x__)
                         (Control.DeepSeq.deepseq
                            (_PParams'maxBlockBodySize x__)
                            (Control.DeepSeq.deepseq
                               (_PParams'maxBlockHeaderSize x__)
                               (Control.DeepSeq.deepseq
                                  (_PParams'stakeKeyDeposit x__)
                                  (Control.DeepSeq.deepseq
                                     (_PParams'poolDeposit x__)
                                     (Control.DeepSeq.deepseq
                                        (_PParams'poolRetirementEpochBound x__)
                                        (Control.DeepSeq.deepseq
                                           (_PParams'desiredNumberOfPools x__)
                                           (Control.DeepSeq.deepseq
                                              (_PParams'poolInfluence x__)
                                              (Control.DeepSeq.deepseq
                                                 (_PParams'monetaryExpansion x__)
                                                 (Control.DeepSeq.deepseq
                                                    (_PParams'treasuryExpansion x__)
                                                    (Control.DeepSeq.deepseq
                                                       (_PParams'minPoolCost x__)
                                                       (Control.DeepSeq.deepseq
                                                          (_PParams'protocolVersion x__)
                                                          (Control.DeepSeq.deepseq
                                                             (_PParams'maxValueSize x__)
                                                             (Control.DeepSeq.deepseq
                                                                (_PParams'collateralPercentage x__)
                                                                (Control.DeepSeq.deepseq
                                                                   (_PParams'maxCollateralInputs
                                                                      x__)
                                                                   (Control.DeepSeq.deepseq
                                                                      (_PParams'costModels x__)
                                                                      (Control.DeepSeq.deepseq
                                                                         (_PParams'prices x__)
                                                                         (Control.DeepSeq.deepseq
                                                                            (_PParams'maxExecutionUnitsPerTransaction
                                                                               x__)
                                                                            (Control.DeepSeq.deepseq
                                                                               (_PParams'maxExecutionUnitsPerBlock
                                                                                  x__)
                                                                               (Control.DeepSeq.deepseq
                                                                                  (_PParams'minFeeScriptRefCostPerByte
                                                                                     x__)
                                                                                  (Control.DeepSeq.deepseq
                                                                                     (_PParams'poolVotingThresholds
                                                                                        x__)
                                                                                     (Control.DeepSeq.deepseq
                                                                                        (_PParams'drepVotingThresholds
                                                                                           x__)
                                                                                        (Control.DeepSeq.deepseq
                                                                                           (_PParams'minCommitteeSize
                                                                                              x__)
                                                                                           (Control.DeepSeq.deepseq
                                                                                              (_PParams'committeeTermLimit
                                                                                                 x__)
                                                                                              (Control.DeepSeq.deepseq
                                                                                                 (_PParams'governanceActionValidityPeriod
                                                                                                    x__)
                                                                                                 (Control.DeepSeq.deepseq
                                                                                                    (_PParams'governanceActionDeposit
                                                                                                       x__)
                                                                                                    (Control.DeepSeq.deepseq
                                                                                                       (_PParams'drepDeposit
                                                                                                          x__)
                                                                                                       (Control.DeepSeq.deepseq
                                                                                                          (_PParams'drepInactivityPeriod
                                                                                                             x__)
                                                                                                          ())))))))))))))))))))))))))))))))
{- | Fields :
     
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.major' @:: Lens' ProtocolVersion Data.Word.Word32@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.minor' @:: Lens' ProtocolVersion Data.Word.Word32@ -}
data ProtocolVersion
  = ProtocolVersion'_constructor {_ProtocolVersion'major :: !Data.Word.Word32,
                                  _ProtocolVersion'minor :: !Data.Word.Word32,
                                  _ProtocolVersion'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ProtocolVersion where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ProtocolVersion "major" Data.Word.Word32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ProtocolVersion'major
           (\ x__ y__ -> x__ {_ProtocolVersion'major = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ProtocolVersion "minor" Data.Word.Word32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ProtocolVersion'minor
           (\ x__ y__ -> x__ {_ProtocolVersion'minor = y__}))
        Prelude.id
instance Data.ProtoLens.Message ProtocolVersion where
  messageName _
    = Data.Text.pack "utxorpc.v1alpha.cardano.ProtocolVersion"
  packedMessageDescriptor _
    = "\n\
      \\SIProtocolVersion\DC2\DC4\n\
      \\ENQmajor\CAN\SOH \SOH(\rR\ENQmajor\DC2\DC4\n\
      \\ENQminor\CAN\STX \SOH(\rR\ENQminor"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        major__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "major"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"major")) ::
              Data.ProtoLens.FieldDescriptor ProtocolVersion
        minor__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "minor"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"minor")) ::
              Data.ProtoLens.FieldDescriptor ProtocolVersion
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, major__field_descriptor),
           (Data.ProtoLens.Tag 2, minor__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ProtocolVersion'_unknownFields
        (\ x__ y__ -> x__ {_ProtocolVersion'_unknownFields = y__})
  defMessage
    = ProtocolVersion'_constructor
        {_ProtocolVersion'major = Data.ProtoLens.fieldDefault,
         _ProtocolVersion'minor = Data.ProtoLens.fieldDefault,
         _ProtocolVersion'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ProtocolVersion
          -> Data.ProtoLens.Encoding.Bytes.Parser ProtocolVersion
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
                                       "major"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"major") y x)
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "minor"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"minor") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "ProtocolVersion"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"major") _x
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
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"minor") _x
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
instance Control.DeepSeq.NFData ProtocolVersion where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ProtocolVersion'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ProtocolVersion'major x__)
                (Control.DeepSeq.deepseq (_ProtocolVersion'minor x__) ()))
{- | Fields :
     
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.numerator' @:: Lens' RationalNumber Data.Int.Int64@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.denominator' @:: Lens' RationalNumber Data.Word.Word64@ -}
data RationalNumber
  = RationalNumber'_constructor {_RationalNumber'numerator :: !Data.Int.Int64,
                                 _RationalNumber'denominator :: !Data.Word.Word64,
                                 _RationalNumber'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show RationalNumber where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField RationalNumber "numerator" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _RationalNumber'numerator
           (\ x__ y__ -> x__ {_RationalNumber'numerator = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField RationalNumber "denominator" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _RationalNumber'denominator
           (\ x__ y__ -> x__ {_RationalNumber'denominator = y__}))
        Prelude.id
instance Data.ProtoLens.Message RationalNumber where
  messageName _
    = Data.Text.pack "utxorpc.v1alpha.cardano.RationalNumber"
  packedMessageDescriptor _
    = "\n\
      \\SORationalNumber\DC2 \n\
      \\tnumerator\CAN\SOH \SOH(\ETXR\tnumeratorB\STX0\SOH\DC2$\n\
      \\vdenominator\CAN\STX \SOH(\EOTR\vdenominatorB\STX0\SOH"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        numerator__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "numerator"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"numerator")) ::
              Data.ProtoLens.FieldDescriptor RationalNumber
        denominator__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "denominator"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"denominator")) ::
              Data.ProtoLens.FieldDescriptor RationalNumber
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, numerator__field_descriptor),
           (Data.ProtoLens.Tag 2, denominator__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _RationalNumber'_unknownFields
        (\ x__ y__ -> x__ {_RationalNumber'_unknownFields = y__})
  defMessage
    = RationalNumber'_constructor
        {_RationalNumber'numerator = Data.ProtoLens.fieldDefault,
         _RationalNumber'denominator = Data.ProtoLens.fieldDefault,
         _RationalNumber'_unknownFields = []}
  parseMessage
    = let
        loop ::
          RationalNumber
          -> Data.ProtoLens.Encoding.Bytes.Parser RationalNumber
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
                                       "numerator"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"numerator") y x)
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getVarInt "denominator"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"denominator") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "RationalNumber"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"numerator") _x
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
                     = Lens.Family2.view (Data.ProtoLens.Field.field @"denominator") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 16)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData RationalNumber where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_RationalNumber'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_RationalNumber'numerator x__)
                (Control.DeepSeq.deepseq (_RationalNumber'denominator x__) ()))
{- | Fields :
     
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maybe'script' @:: Lens' Script (Prelude.Maybe Script'Script)@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maybe'native' @:: Lens' Script (Prelude.Maybe Data.ByteString.ByteString)@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.native' @:: Lens' Script Data.ByteString.ByteString@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maybe'plutusV1' @:: Lens' Script (Prelude.Maybe Data.ByteString.ByteString)@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.plutusV1' @:: Lens' Script Data.ByteString.ByteString@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maybe'plutusV2' @:: Lens' Script (Prelude.Maybe Data.ByteString.ByteString)@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.plutusV2' @:: Lens' Script Data.ByteString.ByteString@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maybe'plutusV3' @:: Lens' Script (Prelude.Maybe Data.ByteString.ByteString)@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.plutusV3' @:: Lens' Script Data.ByteString.ByteString@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maybe'plutusV4' @:: Lens' Script (Prelude.Maybe Data.ByteString.ByteString)@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.plutusV4' @:: Lens' Script Data.ByteString.ByteString@ -}
data Script
  = Script'_constructor {_Script'script :: !(Prelude.Maybe Script'Script),
                         _Script'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Script where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
data Script'Script
  = Script'Native !Data.ByteString.ByteString |
    Script'PlutusV1 !Data.ByteString.ByteString |
    Script'PlutusV2 !Data.ByteString.ByteString |
    Script'PlutusV3 !Data.ByteString.ByteString |
    Script'PlutusV4 !Data.ByteString.ByteString
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.Field.HasField Script "maybe'script" (Prelude.Maybe Script'Script) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Script'script (\ x__ y__ -> x__ {_Script'script = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Script "maybe'native" (Prelude.Maybe Data.ByteString.ByteString) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Script'script (\ x__ y__ -> x__ {_Script'script = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (Script'Native x__val)) -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap Script'Native y__))
instance Data.ProtoLens.Field.HasField Script "native" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Script'script (\ x__ y__ -> x__ {_Script'script = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (Script'Native x__val)) -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap Script'Native y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault))
instance Data.ProtoLens.Field.HasField Script "maybe'plutusV1" (Prelude.Maybe Data.ByteString.ByteString) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Script'script (\ x__ y__ -> x__ {_Script'script = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (Script'PlutusV1 x__val)) -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap Script'PlutusV1 y__))
instance Data.ProtoLens.Field.HasField Script "plutusV1" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Script'script (\ x__ y__ -> x__ {_Script'script = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (Script'PlutusV1 x__val)) -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap Script'PlutusV1 y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault))
instance Data.ProtoLens.Field.HasField Script "maybe'plutusV2" (Prelude.Maybe Data.ByteString.ByteString) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Script'script (\ x__ y__ -> x__ {_Script'script = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (Script'PlutusV2 x__val)) -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap Script'PlutusV2 y__))
instance Data.ProtoLens.Field.HasField Script "plutusV2" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Script'script (\ x__ y__ -> x__ {_Script'script = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (Script'PlutusV2 x__val)) -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap Script'PlutusV2 y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault))
instance Data.ProtoLens.Field.HasField Script "maybe'plutusV3" (Prelude.Maybe Data.ByteString.ByteString) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Script'script (\ x__ y__ -> x__ {_Script'script = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (Script'PlutusV3 x__val)) -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap Script'PlutusV3 y__))
instance Data.ProtoLens.Field.HasField Script "plutusV3" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Script'script (\ x__ y__ -> x__ {_Script'script = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (Script'PlutusV3 x__val)) -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap Script'PlutusV3 y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault))
instance Data.ProtoLens.Field.HasField Script "maybe'plutusV4" (Prelude.Maybe Data.ByteString.ByteString) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Script'script (\ x__ y__ -> x__ {_Script'script = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (Script'PlutusV4 x__val)) -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap Script'PlutusV4 y__))
instance Data.ProtoLens.Field.HasField Script "plutusV4" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Script'script (\ x__ y__ -> x__ {_Script'script = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (Script'PlutusV4 x__val)) -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap Script'PlutusV4 y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault))
instance Data.ProtoLens.Message Script where
  messageName _ = Data.Text.pack "utxorpc.v1alpha.cardano.Script"
  packedMessageDescriptor _
    = "\n\
      \\ACKScript\DC2\CAN\n\
      \\ACKnative\CAN\SOH \SOH(\fH\NULR\ACKnative\DC2\GS\n\
      \\tplutus_v1\CAN\STX \SOH(\fH\NULR\bplutusV1\DC2\GS\n\
      \\tplutus_v2\CAN\ETX \SOH(\fH\NULR\bplutusV2\DC2\GS\n\
      \\tplutus_v3\CAN\EOT \SOH(\fH\NULR\bplutusV3\DC2\GS\n\
      \\tplutus_v4\CAN\ENQ \SOH(\fH\NULR\bplutusV4B\b\n\
      \\ACKscript"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        native__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "native"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'native")) ::
              Data.ProtoLens.FieldDescriptor Script
        plutusV1__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "plutus_v1"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'plutusV1")) ::
              Data.ProtoLens.FieldDescriptor Script
        plutusV2__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "plutus_v2"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'plutusV2")) ::
              Data.ProtoLens.FieldDescriptor Script
        plutusV3__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "plutus_v3"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'plutusV3")) ::
              Data.ProtoLens.FieldDescriptor Script
        plutusV4__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "plutus_v4"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'plutusV4")) ::
              Data.ProtoLens.FieldDescriptor Script
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, native__field_descriptor),
           (Data.ProtoLens.Tag 2, plutusV1__field_descriptor),
           (Data.ProtoLens.Tag 3, plutusV2__field_descriptor),
           (Data.ProtoLens.Tag 4, plutusV3__field_descriptor),
           (Data.ProtoLens.Tag 5, plutusV4__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Script'_unknownFields
        (\ x__ y__ -> x__ {_Script'_unknownFields = y__})
  defMessage
    = Script'_constructor
        {_Script'script = Prelude.Nothing, _Script'_unknownFields = []}
  parseMessage
    = let
        loop :: Script -> Data.ProtoLens.Encoding.Bytes.Parser Script
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
                                       "native"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"native") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getBytes
                                             (Prelude.fromIntegral len))
                                       "plutus_v1"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"plutusV1") y x)
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getBytes
                                             (Prelude.fromIntegral len))
                                       "plutus_v2"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"plutusV2") y x)
                        34
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getBytes
                                             (Prelude.fromIntegral len))
                                       "plutus_v3"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"plutusV3") y x)
                        42
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getBytes
                                             (Prelude.fromIntegral len))
                                       "plutus_v4"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"plutusV4") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "Script"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'script") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just (Script'Native v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                       ((\ bs
                           -> (Data.Monoid.<>)
                                (Data.ProtoLens.Encoding.Bytes.putVarInt
                                   (Prelude.fromIntegral (Data.ByteString.length bs)))
                                (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          v)
                (Prelude.Just (Script'PlutusV1 v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                       ((\ bs
                           -> (Data.Monoid.<>)
                                (Data.ProtoLens.Encoding.Bytes.putVarInt
                                   (Prelude.fromIntegral (Data.ByteString.length bs)))
                                (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          v)
                (Prelude.Just (Script'PlutusV2 v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                       ((\ bs
                           -> (Data.Monoid.<>)
                                (Data.ProtoLens.Encoding.Bytes.putVarInt
                                   (Prelude.fromIntegral (Data.ByteString.length bs)))
                                (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          v)
                (Prelude.Just (Script'PlutusV3 v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                       ((\ bs
                           -> (Data.Monoid.<>)
                                (Data.ProtoLens.Encoding.Bytes.putVarInt
                                   (Prelude.fromIntegral (Data.ByteString.length bs)))
                                (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          v)
                (Prelude.Just (Script'PlutusV4 v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 42)
                       ((\ bs
                           -> (Data.Monoid.<>)
                                (Data.ProtoLens.Encoding.Bytes.putVarInt
                                   (Prelude.fromIntegral (Data.ByteString.length bs)))
                                (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          v))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData Script where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Script'_unknownFields x__)
             (Control.DeepSeq.deepseq (_Script'script x__) ())
instance Control.DeepSeq.NFData Script'Script where
  rnf (Script'Native x__) = Control.DeepSeq.rnf x__
  rnf (Script'PlutusV1 x__) = Control.DeepSeq.rnf x__
  rnf (Script'PlutusV2 x__) = Control.DeepSeq.rnf x__
  rnf (Script'PlutusV3 x__) = Control.DeepSeq.rnf x__
  rnf (Script'PlutusV4 x__) = Control.DeepSeq.rnf x__
_Script'Native ::
  Data.ProtoLens.Prism.Prism' Script'Script Data.ByteString.ByteString
_Script'Native
  = Data.ProtoLens.Prism.prism'
      Script'Native
      (\ p__
         -> case p__ of
              (Script'Native p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_Script'PlutusV1 ::
  Data.ProtoLens.Prism.Prism' Script'Script Data.ByteString.ByteString
_Script'PlutusV1
  = Data.ProtoLens.Prism.prism'
      Script'PlutusV1
      (\ p__
         -> case p__ of
              (Script'PlutusV1 p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_Script'PlutusV2 ::
  Data.ProtoLens.Prism.Prism' Script'Script Data.ByteString.ByteString
_Script'PlutusV2
  = Data.ProtoLens.Prism.prism'
      Script'PlutusV2
      (\ p__
         -> case p__ of
              (Script'PlutusV2 p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_Script'PlutusV3 ::
  Data.ProtoLens.Prism.Prism' Script'Script Data.ByteString.ByteString
_Script'PlutusV3
  = Data.ProtoLens.Prism.prism'
      Script'PlutusV3
      (\ p__
         -> case p__ of
              (Script'PlutusV3 p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_Script'PlutusV4 ::
  Data.ProtoLens.Prism.Prism' Script'Script Data.ByteString.ByteString
_Script'PlutusV4
  = Data.ProtoLens.Prism.prism'
      Script'PlutusV4
      (\ p__
         -> case p__ of
              (Script'PlutusV4 p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
{- | Fields :
     
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.address' @:: Lens' TxOutput Data.ByteString.ByteString@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.coin' @:: Lens' TxOutput Data.Word.Word64@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.assets' @:: Lens' TxOutput [MultiAsset]@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.vec'assets' @:: Lens' TxOutput (Data.Vector.Vector MultiAsset)@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.datum' @:: Lens' TxOutput Datum@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maybe'datum' @:: Lens' TxOutput (Prelude.Maybe Datum)@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.script' @:: Lens' TxOutput Script@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maybe'script' @:: Lens' TxOutput (Prelude.Maybe Script)@ -}
data TxOutput
  = TxOutput'_constructor {_TxOutput'address :: !Data.ByteString.ByteString,
                           _TxOutput'coin :: !Data.Word.Word64,
                           _TxOutput'assets :: !(Data.Vector.Vector MultiAsset),
                           _TxOutput'datum :: !(Prelude.Maybe Datum),
                           _TxOutput'script :: !(Prelude.Maybe Script),
                           _TxOutput'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show TxOutput where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField TxOutput "address" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TxOutput'address (\ x__ y__ -> x__ {_TxOutput'address = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField TxOutput "coin" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TxOutput'coin (\ x__ y__ -> x__ {_TxOutput'coin = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField TxOutput "assets" [MultiAsset] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TxOutput'assets (\ x__ y__ -> x__ {_TxOutput'assets = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField TxOutput "vec'assets" (Data.Vector.Vector MultiAsset) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TxOutput'assets (\ x__ y__ -> x__ {_TxOutput'assets = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField TxOutput "datum" Datum where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TxOutput'datum (\ x__ y__ -> x__ {_TxOutput'datum = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField TxOutput "maybe'datum" (Prelude.Maybe Datum) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TxOutput'datum (\ x__ y__ -> x__ {_TxOutput'datum = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField TxOutput "script" Script where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TxOutput'script (\ x__ y__ -> x__ {_TxOutput'script = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField TxOutput "maybe'script" (Prelude.Maybe Script) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TxOutput'script (\ x__ y__ -> x__ {_TxOutput'script = y__}))
        Prelude.id
instance Data.ProtoLens.Message TxOutput where
  messageName _ = Data.Text.pack "utxorpc.v1alpha.cardano.TxOutput"
  packedMessageDescriptor _
    = "\n\
      \\bTxOutput\DC2\CAN\n\
      \\aaddress\CAN\SOH \SOH(\fR\aaddress\DC2\SYN\n\
      \\EOTcoin\CAN\STX \SOH(\EOTR\EOTcoinB\STX0\SOH\DC2;\n\
      \\ACKassets\CAN\ETX \ETX(\v2#.utxorpc.v1alpha.cardano.MultiAssetR\ACKassets\DC24\n\
      \\ENQdatum\CAN\EOT \SOH(\v2\RS.utxorpc.v1alpha.cardano.DatumR\ENQdatum\DC27\n\
      \\ACKscript\CAN\ENQ \SOH(\v2\US.utxorpc.v1alpha.cardano.ScriptR\ACKscript"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        address__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "address"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"address")) ::
              Data.ProtoLens.FieldDescriptor TxOutput
        coin__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "coin"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"coin")) ::
              Data.ProtoLens.FieldDescriptor TxOutput
        assets__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "assets"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor MultiAsset)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"assets")) ::
              Data.ProtoLens.FieldDescriptor TxOutput
        datum__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "datum"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Datum)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'datum")) ::
              Data.ProtoLens.FieldDescriptor TxOutput
        script__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "script"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Script)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'script")) ::
              Data.ProtoLens.FieldDescriptor TxOutput
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, address__field_descriptor),
           (Data.ProtoLens.Tag 2, coin__field_descriptor),
           (Data.ProtoLens.Tag 3, assets__field_descriptor),
           (Data.ProtoLens.Tag 4, datum__field_descriptor),
           (Data.ProtoLens.Tag 5, script__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _TxOutput'_unknownFields
        (\ x__ y__ -> x__ {_TxOutput'_unknownFields = y__})
  defMessage
    = TxOutput'_constructor
        {_TxOutput'address = Data.ProtoLens.fieldDefault,
         _TxOutput'coin = Data.ProtoLens.fieldDefault,
         _TxOutput'assets = Data.Vector.Generic.empty,
         _TxOutput'datum = Prelude.Nothing,
         _TxOutput'script = Prelude.Nothing, _TxOutput'_unknownFields = []}
  parseMessage
    = let
        loop ::
          TxOutput
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld MultiAsset
             -> Data.ProtoLens.Encoding.Bytes.Parser TxOutput
        loop x mutable'assets
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'assets <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                         (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                            mutable'assets)
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
                              (Data.ProtoLens.Field.field @"vec'assets") frozen'assets x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getBytes
                                             (Prelude.fromIntegral len))
                                       "address"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"address") y x)
                                  mutable'assets
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getVarInt "coin"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"coin") y x)
                                  mutable'assets
                        26
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "assets"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'assets y)
                                loop x v
                        34
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "datum"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"datum") y x)
                                  mutable'assets
                        42
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "script"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"script") y x)
                                  mutable'assets
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'assets
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'assets <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                  Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'assets)
          "TxOutput"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"address") _x
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
                (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"coin") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 16)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt _v))
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
                      (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'assets") _x))
                   ((Data.Monoid.<>)
                      (case
                           Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'datum") _x
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
                              Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'script") _x
                          of
                            Prelude.Nothing -> Data.Monoid.mempty
                            (Prelude.Just _v)
                              -> (Data.Monoid.<>)
                                   (Data.ProtoLens.Encoding.Bytes.putVarInt 42)
                                   ((Prelude..)
                                      (\ bs
                                         -> (Data.Monoid.<>)
                                              (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                 (Prelude.fromIntegral (Data.ByteString.length bs)))
                                              (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                      Data.ProtoLens.encodeMessage _v))
                         (Data.ProtoLens.Encoding.Wire.buildFieldSet
                            (Lens.Family2.view Data.ProtoLens.unknownFields _x))))))
instance Control.DeepSeq.NFData TxOutput where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_TxOutput'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_TxOutput'address x__)
                (Control.DeepSeq.deepseq
                   (_TxOutput'coin x__)
                   (Control.DeepSeq.deepseq
                      (_TxOutput'assets x__)
                      (Control.DeepSeq.deepseq
                         (_TxOutput'datum x__)
                         (Control.DeepSeq.deepseq (_TxOutput'script x__) ())))))
{- | Fields :
     
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.thresholds' @:: Lens' VotingThresholds [RationalNumber]@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.vec'thresholds' @:: Lens' VotingThresholds (Data.Vector.Vector RationalNumber)@ -}
data VotingThresholds
  = VotingThresholds'_constructor {_VotingThresholds'thresholds :: !(Data.Vector.Vector RationalNumber),
                                   _VotingThresholds'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show VotingThresholds where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField VotingThresholds "thresholds" [RationalNumber] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _VotingThresholds'thresholds
           (\ x__ y__ -> x__ {_VotingThresholds'thresholds = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField VotingThresholds "vec'thresholds" (Data.Vector.Vector RationalNumber) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _VotingThresholds'thresholds
           (\ x__ y__ -> x__ {_VotingThresholds'thresholds = y__}))
        Prelude.id
instance Data.ProtoLens.Message VotingThresholds where
  messageName _
    = Data.Text.pack "utxorpc.v1alpha.cardano.VotingThresholds"
  packedMessageDescriptor _
    = "\n\
      \\DLEVotingThresholds\DC2G\n\
      \\n\
      \thresholds\CAN\SOH \ETX(\v2'.utxorpc.v1alpha.cardano.RationalNumberR\n\
      \thresholds"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        thresholds__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "thresholds"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor RationalNumber)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"thresholds")) ::
              Data.ProtoLens.FieldDescriptor VotingThresholds
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, thresholds__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _VotingThresholds'_unknownFields
        (\ x__ y__ -> x__ {_VotingThresholds'_unknownFields = y__})
  defMessage
    = VotingThresholds'_constructor
        {_VotingThresholds'thresholds = Data.Vector.Generic.empty,
         _VotingThresholds'_unknownFields = []}
  parseMessage
    = let
        loop ::
          VotingThresholds
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld RationalNumber
             -> Data.ProtoLens.Encoding.Bytes.Parser VotingThresholds
        loop x mutable'thresholds
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'thresholds <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                             (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                mutable'thresholds)
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
                              (Data.ProtoLens.Field.field @"vec'thresholds") frozen'thresholds
                              x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "thresholds"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'thresholds y)
                                loop x v
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'thresholds
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'thresholds <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                      Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'thresholds)
          "VotingThresholds"
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
                   (Data.ProtoLens.Field.field @"vec'thresholds") _x))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData VotingThresholds where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_VotingThresholds'_unknownFields x__)
             (Control.DeepSeq.deepseq (_VotingThresholds'thresholds x__) ())
packedFileDescriptor :: Data.ByteString.ByteString
packedFileDescriptor
  = "\n\
    \%utxorpc/v1alpha/cardano/cardano.proto\DC2\ETButxorpc.v1alpha.cardano\"\232\SOH\n\
    \\bTxOutput\DC2\CAN\n\
    \\aaddress\CAN\SOH \SOH(\fR\aaddress\DC2\SYN\n\
    \\EOTcoin\CAN\STX \SOH(\EOTR\EOTcoinB\STX0\SOH\DC2;\n\
    \\ACKassets\CAN\ETX \ETX(\v2#.utxorpc.v1alpha.cardano.MultiAssetR\ACKassets\DC24\n\
    \\ENQdatum\CAN\EOT \SOH(\v2\RS.utxorpc.v1alpha.cardano.DatumR\ENQdatum\DC27\n\
    \\ACKscript\CAN\ENQ \SOH(\v2\US.utxorpc.v1alpha.cardano.ScriptR\ACKscript\"$\n\
    \\fAddressArray\DC2\DC4\n\
    \\ENQitems\CAN\SOH \ETX(\fR\ENQitems\"@\n\
    \\ENQDatum\DC2\DC2\n\
    \\EOThash\CAN\SOH \SOH(\fR\EOThash\DC2#\n\
    \\roriginal_cbor\CAN\ETX \SOH(\fR\foriginalCbor\"q\n\
    \\ENQAsset\DC2\DC2\n\
    \\EOTname\CAN\SOH \SOH(\fR\EOTname\DC2%\n\
    \\voutput_coin\CAN\STX \SOH(\EOTH\NULR\n\
    \outputCoinB\STX0\SOH\DC2!\n\
    \\tmint_coin\CAN\ETX \SOH(\ETXH\NULR\bmintCoinB\STX0\SOHB\n\
    \\n\
    \\bquantity\"a\n\
    \\n\
    \MultiAsset\DC2\ESC\n\
    \\tpolicy_id\CAN\SOH \SOH(\fR\bpolicyId\DC26\n\
    \\ACKassets\CAN\STX \ETX(\v2\RS.utxorpc.v1alpha.cardano.AssetR\ACKassets\"\168\SOH\n\
    \\ACKScript\DC2\CAN\n\
    \\ACKnative\CAN\SOH \SOH(\fH\NULR\ACKnative\DC2\GS\n\
    \\tplutus_v1\CAN\STX \SOH(\fH\NULR\bplutusV1\DC2\GS\n\
    \\tplutus_v2\CAN\ETX \SOH(\fH\NULR\bplutusV2\DC2\GS\n\
    \\tplutus_v3\CAN\EOT \SOH(\fH\NULR\bplutusV3\DC2\GS\n\
    \\tplutus_v4\CAN\ENQ \SOH(\fH\NULR\bplutusV4B\b\n\
    \\ACKscript\"X\n\
    \\SORationalNumber\DC2 \n\
    \\tnumerator\CAN\SOH \SOH(\ETXR\tnumeratorB\STX0\SOH\DC2$\n\
    \\vdenominator\CAN\STX \SOH(\EOTR\vdenominatorB\STX0\SOH\"7\n\
    \\aExUnits\DC2\DC4\n\
    \\ENQsteps\CAN\SOH \SOH(\EOTR\ENQsteps\DC2\SYN\n\
    \\ACKmemory\CAN\STX \SOH(\EOTR\ACKmemory\"\138\SOH\n\
    \\bExPrices\DC2=\n\
    \\ENQsteps\CAN\SOH \SOH(\v2'.utxorpc.v1alpha.cardano.RationalNumberR\ENQsteps\DC2?\n\
    \\ACKmemory\CAN\STX \SOH(\v2'.utxorpc.v1alpha.cardano.RationalNumberR\ACKmemory\"=\n\
    \\SIProtocolVersion\DC2\DC4\n\
    \\ENQmajor\CAN\SOH \SOH(\rR\ENQmajor\DC2\DC4\n\
    \\ENQminor\CAN\STX \SOH(\rR\ENQminor\"#\n\
    \\tCostModel\DC2\SYN\n\
    \\ACKvalues\CAN\SOH \ETX(\ETXR\ACKvalues\"\144\STX\n\
    \\n\
    \CostModels\DC2?\n\
    \\tplutus_v1\CAN\SOH \SOH(\v2\".utxorpc.v1alpha.cardano.CostModelR\bplutusV1\DC2?\n\
    \\tplutus_v2\CAN\STX \SOH(\v2\".utxorpc.v1alpha.cardano.CostModelR\bplutusV2\DC2?\n\
    \\tplutus_v3\CAN\ETX \SOH(\v2\".utxorpc.v1alpha.cardano.CostModelR\bplutusV3\DC2?\n\
    \\tplutus_v4\CAN\EOT \SOH(\v2\".utxorpc.v1alpha.cardano.CostModelR\bplutusV4\"[\n\
    \\DLEVotingThresholds\DC2G\n\
    \\n\
    \thresholds\CAN\SOH \ETX(\v2'.utxorpc.v1alpha.cardano.RationalNumberR\n\
    \thresholds\"\223\SI\n\
    \\aPParams\DC21\n\
    \\DC3coins_per_utxo_byte\CAN\SOH \SOH(\EOTR\DLEcoinsPerUtxoByteB\STX0\SOH\DC2\"\n\
    \\vmax_tx_size\CAN\STX \SOH(\EOTR\tmaxTxSizeB\STX0\SOH\DC22\n\
    \\DC3min_fee_coefficient\CAN\ETX \SOH(\EOTR\DC1minFeeCoefficientB\STX0\SOH\DC2,\n\
    \\DLEmin_fee_constant\CAN\EOT \SOH(\EOTR\SOminFeeConstantB\STX0\SOH\DC21\n\
    \\DC3max_block_body_size\CAN\ENQ \SOH(\EOTR\DLEmaxBlockBodySizeB\STX0\SOH\DC25\n\
    \\NAKmax_block_header_size\CAN\ACK \SOH(\EOTR\DC2maxBlockHeaderSizeB\STX0\SOH\DC2.\n\
    \\DC1stake_key_deposit\CAN\a \SOH(\EOTR\SIstakeKeyDepositB\STX0\SOH\DC2%\n\
    \\fpool_deposit\CAN\b \SOH(\EOTR\vpoolDepositB\STX0\SOH\DC2=\n\
    \\ESCpool_retirement_epoch_bound\CAN\t \SOH(\EOTR\CANpoolRetirementEpochBound\DC25\n\
    \\ETBdesired_number_of_pools\CAN\n\
    \ \SOH(\EOTR\DC4desiredNumberOfPools\DC2N\n\
    \\SOpool_influence\CAN\v \SOH(\v2'.utxorpc.v1alpha.cardano.RationalNumberR\rpoolInfluence\DC2V\n\
    \\DC2monetary_expansion\CAN\f \SOH(\v2'.utxorpc.v1alpha.cardano.RationalNumberR\DC1monetaryExpansion\DC2V\n\
    \\DC2treasury_expansion\CAN\r \SOH(\v2'.utxorpc.v1alpha.cardano.RationalNumberR\DC1treasuryExpansion\DC2&\n\
    \\rmin_pool_cost\CAN\SO \SOH(\EOTR\vminPoolCostB\STX0\SOH\DC2S\n\
    \\DLEprotocol_version\CAN\SI \SOH(\v2(.utxorpc.v1alpha.cardano.ProtocolVersionR\SIprotocolVersion\DC2(\n\
    \\SOmax_value_size\CAN\DLE \SOH(\EOTR\fmaxValueSizeB\STX0\SOH\DC27\n\
    \\NAKcollateral_percentage\CAN\DC1 \SOH(\EOTR\DC4collateralPercentageB\STX0\SOH\DC26\n\
    \\NAKmax_collateral_inputs\CAN\DC2 \SOH(\EOTR\DC3maxCollateralInputsB\STX0\SOH\DC2D\n\
    \\vcost_models\CAN\DC3 \SOH(\v2#.utxorpc.v1alpha.cardano.CostModelsR\n\
    \costModels\DC29\n\
    \\ACKprices\CAN\DC4 \SOH(\v2!.utxorpc.v1alpha.cardano.ExPricesR\ACKprices\DC2n\n\
    \#max_execution_units_per_transaction\CAN\NAK \SOH(\v2 .utxorpc.v1alpha.cardano.ExUnitsR\USmaxExecutionUnitsPerTransaction\DC2b\n\
    \\GSmax_execution_units_per_block\CAN\SYN \SOH(\v2 .utxorpc.v1alpha.cardano.ExUnitsR\EMmaxExecutionUnitsPerBlock\DC2m\n\
    \ min_fee_script_ref_cost_per_byte\CAN\ETB \SOH(\v2'.utxorpc.v1alpha.cardano.RationalNumberR\SUBminFeeScriptRefCostPerByte\DC2_\n\
    \\SYNpool_voting_thresholds\CAN\CAN \SOH(\v2).utxorpc.v1alpha.cardano.VotingThresholdsR\DC4poolVotingThresholds\DC2_\n\
    \\SYNdrep_voting_thresholds\CAN\EM \SOH(\v2).utxorpc.v1alpha.cardano.VotingThresholdsR\DC4drepVotingThresholds\DC2,\n\
    \\DC2min_committee_size\CAN\SUB \SOH(\rR\DLEminCommitteeSize\DC20\n\
    \\DC4committee_term_limit\CAN\ESC \SOH(\EOTR\DC2committeeTermLimit\DC2I\n\
    \!governance_action_validity_period\CAN\FS \SOH(\EOTR\RSgovernanceActionValidityPeriod\DC2>\n\
    \\EMgovernance_action_deposit\CAN\GS \SOH(\EOTR\ETBgovernanceActionDepositB\STX0\SOH\DC2%\n\
    \\fdrep_deposit\CAN\RS \SOH(\EOTR\vdrepDepositB\STX0\SOH\DC24\n\
    \\SYNdrep_inactivity_period\CAN\US \SOH(\EOTR\DC4drepInactivityPeriodB\169\SOH\n\
    \\ESCcom.utxorpc.v1alpha.cardanoB\fCardanoProtoP\SOH\162\STX\ETXUVC\170\STX\ETBUtxorpc.V1alpha.Cardano\202\STX\ETBUtxorpc\\V1alpha\\Cardano\226\STX#Utxorpc\\V1alpha\\Cardano\\GPBMetadata\234\STX\EMUtxorpc::V1alpha::CardanoJ\180\&3\n\
    \\ACK\DC2\EOT\NUL\NUL|\SOH\n\
    \\b\n\
    \\SOH\f\DC2\ETX\NUL\NUL\DC2\n\
    \\b\n\
    \\SOH\STX\DC2\ETX\STX\NUL \n\
    \H\n\
    \\STX\EOT\NUL\DC2\EOT\ENQ\NUL\v\SOH\SUB< Represents a transaction output in the Cardano blockchain.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\NUL\SOH\DC2\ETX\ENQ\b\DLE\n\
    \,\n\
    \\EOT\EOT\NUL\STX\NUL\DC2\ETX\ACK\STX\DC4\"\US Address receiving the output.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ENQ\DC2\ETX\ACK\STX\a\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\SOH\DC2\ETX\ACK\b\SI\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ETX\DC2\ETX\ACK\DC2\DC3\n\
    \+\n\
    \\EOT\EOT\NUL\STX\SOH\DC2\ETX\a\STX'\"\RS Amount of ADA in the output.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\ENQ\DC2\ETX\a\STX\b\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\SOH\DC2\ETX\a\t\r\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\ETX\DC2\ETX\a\DLE\DC1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\b\DC2\ETX\a\DC2&\n\
    \\r\n\
    \\ACK\EOT\NUL\STX\SOH\b\ACK\DC2\ETX\a\DC3%\n\
    \@\n\
    \\EOT\EOT\NUL\STX\STX\DC2\ETX\b\STX!\"3 Additional native (non-ADA) assets in the output.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\STX\EOT\DC2\ETX\b\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\STX\ACK\DC2\ETX\b\v\NAK\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\STX\SOH\DC2\ETX\b\SYN\FS\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\STX\ETX\DC2\ETX\b\US \n\
    \6\n\
    \\EOT\EOT\NUL\STX\ETX\DC2\ETX\t\STX\DC2\") Plutus data associated with the output.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ETX\ACK\DC2\ETX\t\STX\a\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ETX\SOH\DC2\ETX\t\b\r\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ETX\ETX\DC2\ETX\t\DLE\DC1\n\
    \1\n\
    \\EOT\EOT\NUL\STX\EOT\DC2\ETX\n\
    \\STX\DC4\"$ Script associated with the output.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\EOT\ACK\DC2\ETX\n\
    \\STX\b\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\EOT\SOH\DC2\ETX\n\
    \\t\SI\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\EOT\ETX\DC2\ETX\n\
    \\DC2\DC3\n\
    \K\n\
    \\STX\EOT\SOH\DC2\EOT\SO\NUL\DLE\SOH\SUB? TODO u5c: new type - https://github.com/utxorpc/spec/pull/167\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\SOH\SOH\DC2\ETX\SO\b\DC4\n\
    \\v\n\
    \\EOT\EOT\SOH\STX\NUL\DC2\ETX\SI\STX\ESC\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\EOT\DC2\ETX\SI\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ENQ\DC2\ETX\SI\v\DLE\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\SOH\DC2\ETX\SI\DC1\SYN\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ETX\DC2\ETX\SI\EM\SUB\n\
    \<\n\
    \\STX\EOT\STX\DC2\EOT\DC3\NUL\SYN\SOH\SUB0 TODO u5c: replaced plutus_data with just bytes\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\STX\SOH\DC2\ETX\DC3\b\r\n\
    \2\n\
    \\EOT\EOT\STX\STX\NUL\DC2\ETX\DC4\STX\DC1\"% Hash of this datum as seen on-chain\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ENQ\DC2\ETX\DC4\STX\a\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\SOH\DC2\ETX\DC4\b\f\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ETX\DC2\ETX\DC4\SI\DLE\n\
    \:\n\
    \\EOT\EOT\STX\STX\SOH\DC2\ETX\NAK\STX\SUB\"- Original cbor-encoded data as seen on-chain\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\ENQ\DC2\ETX\NAK\STX\a\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\SOH\DC2\ETX\NAK\b\NAK\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\ETX\DC2\ETX\NAK\CAN\EM\n\
    \B\n\
    \\STX\EOT\ETX\DC2\EOT\EM\NUL\US\SOH\SUB6 Represents a custom asset in the Cardano blockchain.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\ETX\SOH\DC2\ETX\EM\b\r\n\
    \(\n\
    \\EOT\EOT\ETX\STX\NUL\DC2\ETX\SUB\STX\DC1\"\ESC Name of the custom asset.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\ENQ\DC2\ETX\SUB\STX\a\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\SOH\DC2\ETX\SUB\b\f\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\ETX\DC2\ETX\SUB\SI\DLE\n\
    \\f\n\
    \\EOT\EOT\ETX\b\NUL\DC2\EOT\ESC\STX\RS\ETX\n\
    \\f\n\
    \\ENQ\EOT\ETX\b\NUL\SOH\DC2\ETX\ESC\b\DLE\n\
    \A\n\
    \\EOT\EOT\ETX\STX\SOH\DC2\ETX\FS\EOT0\"4 Quantity of the custom asset in case of an output.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\SOH\ENQ\DC2\ETX\FS\EOT\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\SOH\SOH\DC2\ETX\FS\v\SYN\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\SOH\ETX\DC2\ETX\FS\EM\SUB\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\SOH\b\DC2\ETX\FS\ESC/\n\
    \\r\n\
    \\ACK\EOT\ETX\STX\SOH\b\ACK\DC2\ETX\FS\FS.\n\
    \>\n\
    \\EOT\EOT\ETX\STX\STX\DC2\ETX\GS\EOT-\"1 Quantity of the custom asset in case of a mint.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\STX\ENQ\DC2\ETX\GS\EOT\t\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\STX\SOH\DC2\ETX\GS\n\
    \\DC3\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\STX\ETX\DC2\ETX\GS\SYN\ETB\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\STX\b\DC2\ETX\GS\CAN,\n\
    \\r\n\
    \\ACK\EOT\ETX\STX\STX\b\ACK\DC2\ETX\GS\EM+\n\
    \g\n\
    \\STX\EOT\EOT\DC2\EOT#\NUL&\SOH\SUB[ TODO u5c: redeemer was removed\n\
    \ Represents a multi-asset group in the Cardano blockchain.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\EOT\SOH\DC2\ETX#\b\DC2\n\
    \5\n\
    \\EOT\EOT\EOT\STX\NUL\DC2\ETX$\STX\SYN\"( Policy ID governing the custom assets.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\ENQ\DC2\ETX$\STX\a\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\SOH\DC2\ETX$\b\DC1\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\ETX\DC2\ETX$\DC4\NAK\n\
    \%\n\
    \\EOT\EOT\EOT\STX\SOH\DC2\ETX%\STX\FS\"\CAN List of custom assets.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\SOH\EOT\DC2\ETX%\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\SOH\ACK\DC2\ETX%\v\DLE\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\SOH\SOH\DC2\ETX%\DC1\ETB\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\SOH\ETX\DC2\ETX%\SUB\ESC\n\
    \n\n\
    \\STX\EOT\ENQ\DC2\EOT*\NUL2\SOH\SUBb Represents a script in Cardano.\n\
    \ TODO u5c: removed native script representation, added plutus_v4\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\ENQ\SOH\DC2\ETX*\b\SO\n\
    \\f\n\
    \\EOT\EOT\ENQ\b\NUL\DC2\EOT+\STX1\ETX\n\
    \\f\n\
    \\ENQ\EOT\ENQ\b\NUL\SOH\DC2\ETX+\b\SO\n\
    \\GS\n\
    \\EOT\EOT\ENQ\STX\NUL\DC2\ETX,\EOT\NAK\"\DLE Native script.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\NUL\ENQ\DC2\ETX,\EOT\t\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\NUL\SOH\DC2\ETX,\n\
    \\DLE\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\NUL\ETX\DC2\ETX,\DC3\DC4\n\
    \ \n\
    \\EOT\EOT\ENQ\STX\SOH\DC2\ETX-\EOT\CAN\"\DC3 Plutus V1 script.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\SOH\ENQ\DC2\ETX-\EOT\t\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\SOH\SOH\DC2\ETX-\n\
    \\DC3\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\SOH\ETX\DC2\ETX-\SYN\ETB\n\
    \ \n\
    \\EOT\EOT\ENQ\STX\STX\DC2\ETX.\EOT\CAN\"\DC3 Plutus V2 script.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\STX\ENQ\DC2\ETX.\EOT\t\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\STX\SOH\DC2\ETX.\n\
    \\DC3\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\STX\ETX\DC2\ETX.\SYN\ETB\n\
    \ \n\
    \\EOT\EOT\ENQ\STX\ETX\DC2\ETX/\EOT\CAN\"\DC3 Plutus V3 script.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\ETX\ENQ\DC2\ETX/\EOT\t\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\ETX\SOH\DC2\ETX/\n\
    \\DC3\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\ETX\ETX\DC2\ETX/\SYN\ETB\n\
    \ \n\
    \\EOT\EOT\ENQ\STX\EOT\DC2\ETX0\EOT\CAN\"\DC3 Plutus V4 script.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\EOT\ENQ\DC2\ETX0\EOT\t\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\EOT\SOH\DC2\ETX0\n\
    \\DC3\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\EOT\ETX\DC2\ETX0\SYN\ETB\n\
    \b\n\
    \\STX\EOT\ACK\DC2\EOT6\NUL9\SOH\SUBV Represents a rational number as a fraction.\n\
    \ TODO u5c increased precision to 64 bits\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\ACK\SOH\DC2\ETX6\b\SYN\n\
    \\v\n\
    \\EOT\EOT\ACK\STX\NUL\DC2\ETX7\STX+\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\NUL\ENQ\DC2\ETX7\STX\a\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\NUL\SOH\DC2\ETX7\b\DC1\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\NUL\ETX\DC2\ETX7\DC4\NAK\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\NUL\b\DC2\ETX7\SYN*\n\
    \\r\n\
    \\ACK\EOT\ACK\STX\NUL\b\ACK\DC2\ETX7\ETB)\n\
    \\v\n\
    \\EOT\EOT\ACK\STX\SOH\DC2\ETX8\STX.\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\SOH\ENQ\DC2\ETX8\STX\b\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\SOH\SOH\DC2\ETX8\t\DC4\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\SOH\ETX\DC2\ETX8\ETB\CAN\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\SOH\b\DC2\ETX8\EM-\n\
    \\r\n\
    \\ACK\EOT\ACK\STX\SOH\b\ACK\DC2\ETX8\SUB,\n\
    \\FS\n\
    \\STX\EOT\a\DC2\EOT>\NULA\SOH2\DLE PARAMS\n\
    \ ======\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\a\SOH\DC2\ETX>\b\SI\n\
    \\v\n\
    \\EOT\EOT\a\STX\NUL\DC2\ETX?\STX\DC3\n\
    \\f\n\
    \\ENQ\EOT\a\STX\NUL\ENQ\DC2\ETX?\STX\b\n\
    \\f\n\
    \\ENQ\EOT\a\STX\NUL\SOH\DC2\ETX?\t\SO\n\
    \\f\n\
    \\ENQ\EOT\a\STX\NUL\ETX\DC2\ETX?\DC1\DC2\n\
    \\v\n\
    \\EOT\EOT\a\STX\SOH\DC2\ETX@\STX\DC4\n\
    \\f\n\
    \\ENQ\EOT\a\STX\SOH\ENQ\DC2\ETX@\STX\b\n\
    \\f\n\
    \\ENQ\EOT\a\STX\SOH\SOH\DC2\ETX@\t\SI\n\
    \\f\n\
    \\ENQ\EOT\a\STX\SOH\ETX\DC2\ETX@\DC2\DC3\n\
    \\n\
    \\n\
    \\STX\EOT\b\DC2\EOTC\NULF\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\b\SOH\DC2\ETXC\b\DLE\n\
    \\v\n\
    \\EOT\EOT\b\STX\NUL\DC2\ETXD\STX\ESC\n\
    \\f\n\
    \\ENQ\EOT\b\STX\NUL\ACK\DC2\ETXD\STX\DLE\n\
    \\f\n\
    \\ENQ\EOT\b\STX\NUL\SOH\DC2\ETXD\DC1\SYN\n\
    \\f\n\
    \\ENQ\EOT\b\STX\NUL\ETX\DC2\ETXD\EM\SUB\n\
    \\v\n\
    \\EOT\EOT\b\STX\SOH\DC2\ETXE\STX\FS\n\
    \\f\n\
    \\ENQ\EOT\b\STX\SOH\ACK\DC2\ETXE\STX\DLE\n\
    \\f\n\
    \\ENQ\EOT\b\STX\SOH\SOH\DC2\ETXE\DC1\ETB\n\
    \\f\n\
    \\ENQ\EOT\b\STX\SOH\ETX\DC2\ETXE\SUB\ESC\n\
    \\n\
    \\n\
    \\STX\EOT\t\DC2\EOTH\NULK\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\t\SOH\DC2\ETXH\b\ETB\n\
    \\v\n\
    \\EOT\EOT\t\STX\NUL\DC2\ETXI\STX\DC3\n\
    \\f\n\
    \\ENQ\EOT\t\STX\NUL\ENQ\DC2\ETXI\STX\b\n\
    \\f\n\
    \\ENQ\EOT\t\STX\NUL\SOH\DC2\ETXI\t\SO\n\
    \\f\n\
    \\ENQ\EOT\t\STX\NUL\ETX\DC2\ETXI\DC1\DC2\n\
    \\v\n\
    \\EOT\EOT\t\STX\SOH\DC2\ETXJ\STX\DC3\n\
    \\f\n\
    \\ENQ\EOT\t\STX\SOH\ENQ\DC2\ETXJ\STX\b\n\
    \\f\n\
    \\ENQ\EOT\t\STX\SOH\SOH\DC2\ETXJ\t\SO\n\
    \\f\n\
    \\ENQ\EOT\t\STX\SOH\ETX\DC2\ETXJ\DC1\DC2\n\
    \\n\
    \\n\
    \\STX\EOT\n\
    \\DC2\EOTM\NULO\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\n\
    \\SOH\DC2\ETXM\b\DC1\n\
    \\v\n\
    \\EOT\EOT\n\
    \\STX\NUL\DC2\ETXN\STX\FS\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\NUL\EOT\DC2\ETXN\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\NUL\ENQ\DC2\ETXN\v\DLE\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\NUL\SOH\DC2\ETXN\DC1\ETB\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\NUL\ETX\DC2\ETXN\SUB\ESC\n\
    \\n\
    \\n\
    \\STX\EOT\v\DC2\EOTQ\NULV\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\v\SOH\DC2\ETXQ\b\DC2\n\
    \\v\n\
    \\EOT\EOT\v\STX\NUL\DC2\ETXR\STX\SUB\n\
    \\f\n\
    \\ENQ\EOT\v\STX\NUL\ACK\DC2\ETXR\STX\v\n\
    \\f\n\
    \\ENQ\EOT\v\STX\NUL\SOH\DC2\ETXR\f\NAK\n\
    \\f\n\
    \\ENQ\EOT\v\STX\NUL\ETX\DC2\ETXR\CAN\EM\n\
    \\v\n\
    \\EOT\EOT\v\STX\SOH\DC2\ETXS\STX\SUB\n\
    \\f\n\
    \\ENQ\EOT\v\STX\SOH\ACK\DC2\ETXS\STX\v\n\
    \\f\n\
    \\ENQ\EOT\v\STX\SOH\SOH\DC2\ETXS\f\NAK\n\
    \\f\n\
    \\ENQ\EOT\v\STX\SOH\ETX\DC2\ETXS\CAN\EM\n\
    \\v\n\
    \\EOT\EOT\v\STX\STX\DC2\ETXT\STX\SUB\n\
    \\f\n\
    \\ENQ\EOT\v\STX\STX\ACK\DC2\ETXT\STX\v\n\
    \\f\n\
    \\ENQ\EOT\v\STX\STX\SOH\DC2\ETXT\f\NAK\n\
    \\f\n\
    \\ENQ\EOT\v\STX\STX\ETX\DC2\ETXT\CAN\EM\n\
    \\v\n\
    \\EOT\EOT\v\STX\ETX\DC2\ETXU\STX\SUB\n\
    \\f\n\
    \\ENQ\EOT\v\STX\ETX\ACK\DC2\ETXU\STX\v\n\
    \\f\n\
    \\ENQ\EOT\v\STX\ETX\SOH\DC2\ETXU\f\NAK\n\
    \\f\n\
    \\ENQ\EOT\v\STX\ETX\ETX\DC2\ETXU\CAN\EM\n\
    \\n\
    \\n\
    \\STX\EOT\f\DC2\EOTX\NULZ\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\f\SOH\DC2\ETXX\b\CAN\n\
    \\v\n\
    \\EOT\EOT\f\STX\NUL\DC2\ETXY\STX)\n\
    \\f\n\
    \\ENQ\EOT\f\STX\NUL\EOT\DC2\ETXY\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\f\STX\NUL\ACK\DC2\ETXY\v\EM\n\
    \\f\n\
    \\ENQ\EOT\f\STX\NUL\SOH\DC2\ETXY\SUB$\n\
    \\f\n\
    \\ENQ\EOT\f\STX\NUL\ETX\DC2\ETXY'(\n\
    \\n\
    \\n\
    \\STX\EOT\r\DC2\EOT\\\NUL|\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\r\SOH\DC2\ETX\\\b\SI\n\
    \1\n\
    \\EOT\EOT\r\STX\NUL\DC2\ETX]\STX6\"$ The number of coins per UTXO byte.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\r\STX\NUL\ENQ\DC2\ETX]\STX\b\n\
    \\f\n\
    \\ENQ\EOT\r\STX\NUL\SOH\DC2\ETX]\t\FS\n\
    \\f\n\
    \\ENQ\EOT\r\STX\NUL\ETX\DC2\ETX]\US \n\
    \\f\n\
    \\ENQ\EOT\r\STX\NUL\b\DC2\ETX]!5\n\
    \\r\n\
    \\ACK\EOT\r\STX\NUL\b\ACK\DC2\ETX]\"4\n\
    \,\n\
    \\EOT\EOT\r\STX\SOH\DC2\ETX^\STX.\"\US The maximum transaction size.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\r\STX\SOH\ENQ\DC2\ETX^\STX\b\n\
    \\f\n\
    \\ENQ\EOT\r\STX\SOH\SOH\DC2\ETX^\t\DC4\n\
    \\f\n\
    \\ENQ\EOT\r\STX\SOH\ETX\DC2\ETX^\ETB\CAN\n\
    \\f\n\
    \\ENQ\EOT\r\STX\SOH\b\DC2\ETX^\EM-\n\
    \\r\n\
    \\ACK\EOT\r\STX\SOH\b\ACK\DC2\ETX^\SUB,\n\
    \+\n\
    \\EOT\EOT\r\STX\STX\DC2\ETX_\STX6\"\RS The minimum fee coefficient.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\r\STX\STX\ENQ\DC2\ETX_\STX\b\n\
    \\f\n\
    \\ENQ\EOT\r\STX\STX\SOH\DC2\ETX_\t\FS\n\
    \\f\n\
    \\ENQ\EOT\r\STX\STX\ETX\DC2\ETX_\US \n\
    \\f\n\
    \\ENQ\EOT\r\STX\STX\b\DC2\ETX_!5\n\
    \\r\n\
    \\ACK\EOT\r\STX\STX\b\ACK\DC2\ETX_\"4\n\
    \(\n\
    \\EOT\EOT\r\STX\ETX\DC2\ETX`\STX3\"\ESC The minimum fee constant.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\r\STX\ETX\ENQ\DC2\ETX`\STX\b\n\
    \\f\n\
    \\ENQ\EOT\r\STX\ETX\SOH\DC2\ETX`\t\EM\n\
    \\f\n\
    \\ENQ\EOT\r\STX\ETX\ETX\DC2\ETX`\FS\GS\n\
    \\f\n\
    \\ENQ\EOT\r\STX\ETX\b\DC2\ETX`\RS2\n\
    \\r\n\
    \\ACK\EOT\r\STX\ETX\b\ACK\DC2\ETX`\US1\n\
    \+\n\
    \\EOT\EOT\r\STX\EOT\DC2\ETXa\STX6\"\RS The maximum block body size.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\r\STX\EOT\ENQ\DC2\ETXa\STX\b\n\
    \\f\n\
    \\ENQ\EOT\r\STX\EOT\SOH\DC2\ETXa\t\FS\n\
    \\f\n\
    \\ENQ\EOT\r\STX\EOT\ETX\DC2\ETXa\US \n\
    \\f\n\
    \\ENQ\EOT\r\STX\EOT\b\DC2\ETXa!5\n\
    \\r\n\
    \\ACK\EOT\r\STX\EOT\b\ACK\DC2\ETXa\"4\n\
    \-\n\
    \\EOT\EOT\r\STX\ENQ\DC2\ETXb\STX8\"  The maximum block header size.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\r\STX\ENQ\ENQ\DC2\ETXb\STX\b\n\
    \\f\n\
    \\ENQ\EOT\r\STX\ENQ\SOH\DC2\ETXb\t\RS\n\
    \\f\n\
    \\ENQ\EOT\r\STX\ENQ\ETX\DC2\ETXb!\"\n\
    \\f\n\
    \\ENQ\EOT\r\STX\ENQ\b\DC2\ETXb#7\n\
    \\r\n\
    \\ACK\EOT\r\STX\ENQ\b\ACK\DC2\ETXb$6\n\
    \%\n\
    \\EOT\EOT\r\STX\ACK\DC2\ETXc\STX4\"\CAN The stake key deposit.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\r\STX\ACK\ENQ\DC2\ETXc\STX\b\n\
    \\f\n\
    \\ENQ\EOT\r\STX\ACK\SOH\DC2\ETXc\t\SUB\n\
    \\f\n\
    \\ENQ\EOT\r\STX\ACK\ETX\DC2\ETXc\GS\RS\n\
    \\f\n\
    \\ENQ\EOT\r\STX\ACK\b\DC2\ETXc\US3\n\
    \\r\n\
    \\ACK\EOT\r\STX\ACK\b\ACK\DC2\ETXc 2\n\
    \ \n\
    \\EOT\EOT\r\STX\a\DC2\ETXd\STX/\"\DC3 The pool deposit.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\r\STX\a\ENQ\DC2\ETXd\STX\b\n\
    \\f\n\
    \\ENQ\EOT\r\STX\a\SOH\DC2\ETXd\t\NAK\n\
    \\f\n\
    \\ENQ\EOT\r\STX\a\ETX\DC2\ETXd\CAN\EM\n\
    \\f\n\
    \\ENQ\EOT\r\STX\a\b\DC2\ETXd\SUB.\n\
    \\r\n\
    \\ACK\EOT\r\STX\a\b\ACK\DC2\ETXd\ESC-\n\
    \/\n\
    \\EOT\EOT\r\STX\b\DC2\ETXe\STX)\"\" The pool retirement epoch bound.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\r\STX\b\ENQ\DC2\ETXe\STX\b\n\
    \\f\n\
    \\ENQ\EOT\r\STX\b\SOH\DC2\ETXe\t$\n\
    \\f\n\
    \\ENQ\EOT\r\STX\b\ETX\DC2\ETXe'(\n\
    \+\n\
    \\EOT\EOT\r\STX\t\DC2\ETXf\STX&\"\RS The desired number of pools.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\r\STX\t\ENQ\DC2\ETXf\STX\b\n\
    \\f\n\
    \\ENQ\EOT\r\STX\t\SOH\DC2\ETXf\t \n\
    \\f\n\
    \\ENQ\EOT\r\STX\t\ETX\DC2\ETXf#%\n\
    \\"\n\
    \\EOT\EOT\r\STX\n\
    \\DC2\ETXg\STX%\"\NAK The pool influence.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\r\STX\n\
    \\ACK\DC2\ETXg\STX\DLE\n\
    \\f\n\
    \\ENQ\EOT\r\STX\n\
    \\SOH\DC2\ETXg\DC1\US\n\
    \\f\n\
    \\ENQ\EOT\r\STX\n\
    \\ETX\DC2\ETXg\"$\n\
    \&\n\
    \\EOT\EOT\r\STX\v\DC2\ETXh\STX)\"\EM The monetary expansion.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\r\STX\v\ACK\DC2\ETXh\STX\DLE\n\
    \\f\n\
    \\ENQ\EOT\r\STX\v\SOH\DC2\ETXh\DC1#\n\
    \\f\n\
    \\ENQ\EOT\r\STX\v\ETX\DC2\ETXh&(\n\
    \&\n\
    \\EOT\EOT\r\STX\f\DC2\ETXi\STX)\"\EM The treasury expansion.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\r\STX\f\ACK\DC2\ETXi\STX\DLE\n\
    \\f\n\
    \\ENQ\EOT\r\STX\f\SOH\DC2\ETXi\DC1#\n\
    \\f\n\
    \\ENQ\EOT\r\STX\f\ETX\DC2\ETXi&(\n\
    \%\n\
    \\EOT\EOT\r\STX\r\DC2\ETXj\STX1\"\CAN The minimum pool cost.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\r\STX\r\ENQ\DC2\ETXj\STX\b\n\
    \\f\n\
    \\ENQ\EOT\r\STX\r\SOH\DC2\ETXj\t\SYN\n\
    \\f\n\
    \\ENQ\EOT\r\STX\r\ETX\DC2\ETXj\EM\ESC\n\
    \\f\n\
    \\ENQ\EOT\r\STX\r\b\DC2\ETXj\FS0\n\
    \\r\n\
    \\ACK\EOT\r\STX\r\b\ACK\DC2\ETXj\GS/\n\
    \$\n\
    \\EOT\EOT\r\STX\SO\DC2\ETXk\STX(\"\ETB The protocol version.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\r\STX\SO\ACK\DC2\ETXk\STX\DC1\n\
    \\f\n\
    \\ENQ\EOT\r\STX\SO\SOH\DC2\ETXk\DC2\"\n\
    \\f\n\
    \\ENQ\EOT\r\STX\SO\ETX\DC2\ETXk%'\n\
    \&\n\
    \\EOT\EOT\r\STX\SI\DC2\ETXl\STX2\"\EM The maximum value size.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\r\STX\SI\ENQ\DC2\ETXl\STX\b\n\
    \\f\n\
    \\ENQ\EOT\r\STX\SI\SOH\DC2\ETXl\t\ETB\n\
    \\f\n\
    \\ENQ\EOT\r\STX\SI\ETX\DC2\ETXl\SUB\FS\n\
    \\f\n\
    \\ENQ\EOT\r\STX\SI\b\DC2\ETXl\GS1\n\
    \\r\n\
    \\ACK\EOT\r\STX\SI\b\ACK\DC2\ETXl\RS0\n\
    \)\n\
    \\EOT\EOT\r\STX\DLE\DC2\ETXm\STX9\"\FS The collateral percentage.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\r\STX\DLE\ENQ\DC2\ETXm\STX\b\n\
    \\f\n\
    \\ENQ\EOT\r\STX\DLE\SOH\DC2\ETXm\t\RS\n\
    \\f\n\
    \\ENQ\EOT\r\STX\DLE\ETX\DC2\ETXm!#\n\
    \\f\n\
    \\ENQ\EOT\r\STX\DLE\b\DC2\ETXm$8\n\
    \\r\n\
    \\ACK\EOT\r\STX\DLE\b\ACK\DC2\ETXm%7\n\
    \-\n\
    \\EOT\EOT\r\STX\DC1\DC2\ETXn\STX9\"  The maximum collateral inputs.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\r\STX\DC1\ENQ\DC2\ETXn\STX\b\n\
    \\f\n\
    \\ENQ\EOT\r\STX\DC1\SOH\DC2\ETXn\t\RS\n\
    \\f\n\
    \\ENQ\EOT\r\STX\DC1\ETX\DC2\ETXn!#\n\
    \\f\n\
    \\ENQ\EOT\r\STX\DC1\b\DC2\ETXn$8\n\
    \\r\n\
    \\ACK\EOT\r\STX\DC1\b\ACK\DC2\ETXn%7\n\
    \\US\n\
    \\EOT\EOT\r\STX\DC2\DC2\ETXo\STX\RS\"\DC2 The cost models.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\r\STX\DC2\ACK\DC2\ETXo\STX\f\n\
    \\f\n\
    \\ENQ\EOT\r\STX\DC2\SOH\DC2\ETXo\r\CAN\n\
    \\f\n\
    \\ENQ\EOT\r\STX\DC2\ETX\DC2\ETXo\ESC\GS\n\
    \\SUB\n\
    \\EOT\EOT\r\STX\DC3\DC2\ETXp\STX\ETB\"\r The prices.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\r\STX\DC3\ACK\DC2\ETXp\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\r\STX\DC3\SOH\DC2\ETXp\v\DC1\n\
    \\f\n\
    \\ENQ\EOT\r\STX\DC3\ETX\DC2\ETXp\DC4\SYN\n\
    \;\n\
    \\EOT\EOT\r\STX\DC4\DC2\ETXq\STX3\". The maximum execution units per transaction.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\r\STX\DC4\ACK\DC2\ETXq\STX\t\n\
    \\f\n\
    \\ENQ\EOT\r\STX\DC4\SOH\DC2\ETXq\n\
    \-\n\
    \\f\n\
    \\ENQ\EOT\r\STX\DC4\ETX\DC2\ETXq02\n\
    \5\n\
    \\EOT\EOT\r\STX\NAK\DC2\ETXr\STX-\"( The maximum execution units per block.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\r\STX\NAK\ACK\DC2\ETXr\STX\t\n\
    \\f\n\
    \\ENQ\EOT\r\STX\NAK\SOH\DC2\ETXr\n\
    \'\n\
    \\f\n\
    \\ENQ\EOT\r\STX\NAK\ETX\DC2\ETXr*,\n\
    \9\n\
    \\EOT\EOT\r\STX\SYN\DC2\ETXs\STX7\", The minimum fee per script reference byte.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\r\STX\SYN\ACK\DC2\ETXs\STX\DLE\n\
    \\f\n\
    \\ENQ\EOT\r\STX\SYN\SOH\DC2\ETXs\DC11\n\
    \\f\n\
    \\ENQ\EOT\r\STX\SYN\ETX\DC2\ETXs46\n\
    \*\n\
    \\EOT\EOT\r\STX\ETB\DC2\ETXt\STX/\"\GS The pool voting thresholds.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\r\STX\ETB\ACK\DC2\ETXt\STX\DC2\n\
    \\f\n\
    \\ENQ\EOT\r\STX\ETB\SOH\DC2\ETXt\DC3)\n\
    \\f\n\
    \\ENQ\EOT\r\STX\ETB\ETX\DC2\ETXt,.\n\
    \*\n\
    \\EOT\EOT\r\STX\CAN\DC2\ETXu\STX/\"\GS The drep voting thresholds.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\r\STX\CAN\ACK\DC2\ETXu\STX\DC2\n\
    \\f\n\
    \\ENQ\EOT\r\STX\CAN\SOH\DC2\ETXu\DC3)\n\
    \\f\n\
    \\ENQ\EOT\r\STX\CAN\ETX\DC2\ETXu,.\n\
    \*\n\
    \\EOT\EOT\r\STX\EM\DC2\ETXv\STX!\"\GS The minimum committee size.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\r\STX\EM\ENQ\DC2\ETXv\STX\b\n\
    \\f\n\
    \\ENQ\EOT\r\STX\EM\SOH\DC2\ETXv\t\ESC\n\
    \\f\n\
    \\ENQ\EOT\r\STX\EM\ETX\DC2\ETXv\RS \n\
    \(\n\
    \\EOT\EOT\r\STX\SUB\DC2\ETXw\STX#\"\ESC The committee term limit.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\r\STX\SUB\ENQ\DC2\ETXw\STX\b\n\
    \\f\n\
    \\ENQ\EOT\r\STX\SUB\SOH\DC2\ETXw\t\GS\n\
    \\f\n\
    \\ENQ\EOT\r\STX\SUB\ETX\DC2\ETXw \"\n\
    \5\n\
    \\EOT\EOT\r\STX\ESC\DC2\ETXx\STX0\"( The governance action validity period.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\r\STX\ESC\ENQ\DC2\ETXx\STX\b\n\
    \\f\n\
    \\ENQ\EOT\r\STX\ESC\SOH\DC2\ETXx\t*\n\
    \\f\n\
    \\ENQ\EOT\r\STX\ESC\ETX\DC2\ETXx-/\n\
    \-\n\
    \\EOT\EOT\r\STX\FS\DC2\ETXy\STX=\"  The governance action deposit.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\r\STX\FS\ENQ\DC2\ETXy\STX\b\n\
    \\f\n\
    \\ENQ\EOT\r\STX\FS\SOH\DC2\ETXy\t\"\n\
    \\f\n\
    \\ENQ\EOT\r\STX\FS\ETX\DC2\ETXy%'\n\
    \\f\n\
    \\ENQ\EOT\r\STX\FS\b\DC2\ETXy(<\n\
    \\r\n\
    \\ACK\EOT\r\STX\FS\b\ACK\DC2\ETXy);\n\
    \ \n\
    \\EOT\EOT\r\STX\GS\DC2\ETXz\STX0\"\DC3 The drep deposit.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\r\STX\GS\ENQ\DC2\ETXz\STX\b\n\
    \\f\n\
    \\ENQ\EOT\r\STX\GS\SOH\DC2\ETXz\t\NAK\n\
    \\f\n\
    \\ENQ\EOT\r\STX\GS\ETX\DC2\ETXz\CAN\SUB\n\
    \\f\n\
    \\ENQ\EOT\r\STX\GS\b\DC2\ETXz\ESC/\n\
    \\r\n\
    \\ACK\EOT\r\STX\GS\b\ACK\DC2\ETXz\FS.\n\
    \*\n\
    \\EOT\EOT\r\STX\RS\DC2\ETX{\STX%\"\GS The drep inactivity period.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\r\STX\RS\ENQ\DC2\ETX{\STX\b\n\
    \\f\n\
    \\ENQ\EOT\r\STX\RS\SOH\DC2\ETX{\t\US\n\
    \\f\n\
    \\ENQ\EOT\r\STX\RS\ETX\DC2\ETX{\"$b\ACKproto3"