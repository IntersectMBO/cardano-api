{- This file was auto-generated from utxorpc/v1alpha/cardano/cardano.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Utxorpc.V1alpha.Cardano.Cardano (
        AddressArray(), Asset(), Asset'Quantity(..), _Asset'OutputCoin,
        _Asset'MintCoin, BigInt(), BigInt'BigInt(..), _BigInt'Int,
        _BigInt'BigUInt, _BigInt'BigNInt, Constr(), CostModel(),
        CostModels(), Datum(), ExPrices(), ExUnits(), MultiAsset(),
        NativeScript(), NativeScript'NativeScript(..),
        _NativeScript'ScriptPubkey, _NativeScript'ScriptAll,
        _NativeScript'ScriptAny, _NativeScript'ScriptNOfK,
        _NativeScript'InvalidBefore, _NativeScript'InvalidHereafter,
        NativeScriptList(), PParams(), PlutusData(),
        PlutusData'PlutusData(..), _PlutusData'Constr, _PlutusData'Map,
        _PlutusData'BigInt, _PlutusData'BoundedBytes, _PlutusData'Array,
        PlutusDataArray(), PlutusDataMap(), PlutusDataPair(),
        ProtocolVersion(), RationalNumber(), Script(), Script'Script(..),
        _Script'Native, _Script'PlutusV1, _Script'PlutusV2,
        _Script'PlutusV3, _Script'PlutusV4, ScriptNOfK(), TxOutput(),
        VotingThresholds()
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
     
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maybe'bigInt' @:: Lens' BigInt (Prelude.Maybe BigInt'BigInt)@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maybe'int' @:: Lens' BigInt (Prelude.Maybe Data.Int.Int64)@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.int' @:: Lens' BigInt Data.Int.Int64@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maybe'bigUInt' @:: Lens' BigInt (Prelude.Maybe Data.ByteString.ByteString)@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.bigUInt' @:: Lens' BigInt Data.ByteString.ByteString@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maybe'bigNInt' @:: Lens' BigInt (Prelude.Maybe Data.ByteString.ByteString)@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.bigNInt' @:: Lens' BigInt Data.ByteString.ByteString@ -}
data BigInt
  = BigInt'_constructor {_BigInt'bigInt :: !(Prelude.Maybe BigInt'BigInt),
                         _BigInt'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show BigInt where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
data BigInt'BigInt
  = BigInt'Int !Data.Int.Int64 |
    BigInt'BigUInt !Data.ByteString.ByteString |
    BigInt'BigNInt !Data.ByteString.ByteString
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.Field.HasField BigInt "maybe'bigInt" (Prelude.Maybe BigInt'BigInt) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _BigInt'bigInt (\ x__ y__ -> x__ {_BigInt'bigInt = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField BigInt "maybe'int" (Prelude.Maybe Data.Int.Int64) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _BigInt'bigInt (\ x__ y__ -> x__ {_BigInt'bigInt = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (BigInt'Int x__val)) -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap BigInt'Int y__))
instance Data.ProtoLens.Field.HasField BigInt "int" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _BigInt'bigInt (\ x__ y__ -> x__ {_BigInt'bigInt = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (BigInt'Int x__val)) -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap BigInt'Int y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault))
instance Data.ProtoLens.Field.HasField BigInt "maybe'bigUInt" (Prelude.Maybe Data.ByteString.ByteString) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _BigInt'bigInt (\ x__ y__ -> x__ {_BigInt'bigInt = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (BigInt'BigUInt x__val)) -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap BigInt'BigUInt y__))
instance Data.ProtoLens.Field.HasField BigInt "bigUInt" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _BigInt'bigInt (\ x__ y__ -> x__ {_BigInt'bigInt = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (BigInt'BigUInt x__val)) -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap BigInt'BigUInt y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault))
instance Data.ProtoLens.Field.HasField BigInt "maybe'bigNInt" (Prelude.Maybe Data.ByteString.ByteString) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _BigInt'bigInt (\ x__ y__ -> x__ {_BigInt'bigInt = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (BigInt'BigNInt x__val)) -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap BigInt'BigNInt y__))
instance Data.ProtoLens.Field.HasField BigInt "bigNInt" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _BigInt'bigInt (\ x__ y__ -> x__ {_BigInt'bigInt = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (BigInt'BigNInt x__val)) -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap BigInt'BigNInt y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault))
instance Data.ProtoLens.Message BigInt where
  messageName _ = Data.Text.pack "utxorpc.v1alpha.cardano.BigInt"
  packedMessageDescriptor _
    = "\n\
      \\ACKBigInt\DC2\SYN\n\
      \\ETXint\CAN\SOH \SOH(\ETXH\NULR\ETXintB\STX0\SOH\DC2\FS\n\
      \\tbig_u_int\CAN\STX \SOH(\fH\NULR\abigUInt\DC2\FS\n\
      \\tbig_n_int\CAN\ETX \SOH(\fH\NULR\abigNIntB\t\n\
      \\abig_int"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        int__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "int"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'int")) ::
              Data.ProtoLens.FieldDescriptor BigInt
        bigUInt__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "big_u_int"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'bigUInt")) ::
              Data.ProtoLens.FieldDescriptor BigInt
        bigNInt__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "big_n_int"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'bigNInt")) ::
              Data.ProtoLens.FieldDescriptor BigInt
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, int__field_descriptor),
           (Data.ProtoLens.Tag 2, bigUInt__field_descriptor),
           (Data.ProtoLens.Tag 3, bigNInt__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _BigInt'_unknownFields
        (\ x__ y__ -> x__ {_BigInt'_unknownFields = y__})
  defMessage
    = BigInt'_constructor
        {_BigInt'bigInt = Prelude.Nothing, _BigInt'_unknownFields = []}
  parseMessage
    = let
        loop :: BigInt -> Data.ProtoLens.Encoding.Bytes.Parser BigInt
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
                                       "int"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"int") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getBytes
                                             (Prelude.fromIntegral len))
                                       "big_u_int"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"bigUInt") y x)
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getBytes
                                             (Prelude.fromIntegral len))
                                       "big_n_int"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"bigNInt") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "BigInt"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'bigInt") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just (BigInt'Int v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                       ((Prelude..)
                          Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral v)
                (Prelude.Just (BigInt'BigUInt v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                       ((\ bs
                           -> (Data.Monoid.<>)
                                (Data.ProtoLens.Encoding.Bytes.putVarInt
                                   (Prelude.fromIntegral (Data.ByteString.length bs)))
                                (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          v)
                (Prelude.Just (BigInt'BigNInt v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                       ((\ bs
                           -> (Data.Monoid.<>)
                                (Data.ProtoLens.Encoding.Bytes.putVarInt
                                   (Prelude.fromIntegral (Data.ByteString.length bs)))
                                (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          v))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData BigInt where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_BigInt'_unknownFields x__)
             (Control.DeepSeq.deepseq (_BigInt'bigInt x__) ())
instance Control.DeepSeq.NFData BigInt'BigInt where
  rnf (BigInt'Int x__) = Control.DeepSeq.rnf x__
  rnf (BigInt'BigUInt x__) = Control.DeepSeq.rnf x__
  rnf (BigInt'BigNInt x__) = Control.DeepSeq.rnf x__
_BigInt'Int ::
  Data.ProtoLens.Prism.Prism' BigInt'BigInt Data.Int.Int64
_BigInt'Int
  = Data.ProtoLens.Prism.prism'
      BigInt'Int
      (\ p__
         -> case p__ of
              (BigInt'Int p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_BigInt'BigUInt ::
  Data.ProtoLens.Prism.Prism' BigInt'BigInt Data.ByteString.ByteString
_BigInt'BigUInt
  = Data.ProtoLens.Prism.prism'
      BigInt'BigUInt
      (\ p__
         -> case p__ of
              (BigInt'BigUInt p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_BigInt'BigNInt ::
  Data.ProtoLens.Prism.Prism' BigInt'BigInt Data.ByteString.ByteString
_BigInt'BigNInt
  = Data.ProtoLens.Prism.prism'
      BigInt'BigNInt
      (\ p__
         -> case p__ of
              (BigInt'BigNInt p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
{- | Fields :
     
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.tag' @:: Lens' Constr Data.Word.Word32@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.anyConstructor' @:: Lens' Constr Data.Word.Word64@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.fields' @:: Lens' Constr [PlutusData]@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.vec'fields' @:: Lens' Constr (Data.Vector.Vector PlutusData)@ -}
data Constr
  = Constr'_constructor {_Constr'tag :: !Data.Word.Word32,
                         _Constr'anyConstructor :: !Data.Word.Word64,
                         _Constr'fields :: !(Data.Vector.Vector PlutusData),
                         _Constr'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Constr where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Constr "tag" Data.Word.Word32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Constr'tag (\ x__ y__ -> x__ {_Constr'tag = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Constr "anyConstructor" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Constr'anyConstructor
           (\ x__ y__ -> x__ {_Constr'anyConstructor = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Constr "fields" [PlutusData] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Constr'fields (\ x__ y__ -> x__ {_Constr'fields = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField Constr "vec'fields" (Data.Vector.Vector PlutusData) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Constr'fields (\ x__ y__ -> x__ {_Constr'fields = y__}))
        Prelude.id
instance Data.ProtoLens.Message Constr where
  messageName _ = Data.Text.pack "utxorpc.v1alpha.cardano.Constr"
  packedMessageDescriptor _
    = "\n\
      \\ACKConstr\DC2\DLE\n\
      \\ETXtag\CAN\SOH \SOH(\rR\ETXtag\DC2'\n\
      \\SIany_constructor\CAN\STX \SOH(\EOTR\SOanyConstructor\DC2;\n\
      \\ACKfields\CAN\ETX \ETX(\v2#.utxorpc.v1alpha.cardano.PlutusDataR\ACKfields"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        tag__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "tag"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"tag")) ::
              Data.ProtoLens.FieldDescriptor Constr
        anyConstructor__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "any_constructor"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"anyConstructor")) ::
              Data.ProtoLens.FieldDescriptor Constr
        fields__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "fields"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor PlutusData)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"fields")) ::
              Data.ProtoLens.FieldDescriptor Constr
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, tag__field_descriptor),
           (Data.ProtoLens.Tag 2, anyConstructor__field_descriptor),
           (Data.ProtoLens.Tag 3, fields__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Constr'_unknownFields
        (\ x__ y__ -> x__ {_Constr'_unknownFields = y__})
  defMessage
    = Constr'_constructor
        {_Constr'tag = Data.ProtoLens.fieldDefault,
         _Constr'anyConstructor = Data.ProtoLens.fieldDefault,
         _Constr'fields = Data.Vector.Generic.empty,
         _Constr'_unknownFields = []}
  parseMessage
    = let
        loop ::
          Constr
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld PlutusData
             -> Data.ProtoLens.Encoding.Bytes.Parser Constr
        loop x mutable'fields
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'fields <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                         (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                            mutable'fields)
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
                              (Data.ProtoLens.Field.field @"vec'fields") frozen'fields x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        8 -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "tag"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"tag") y x)
                                  mutable'fields
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getVarInt "any_constructor"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"anyConstructor") y x)
                                  mutable'fields
                        26
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "fields"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'fields y)
                                loop x v
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'fields
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'fields <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                  Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'fields)
          "Constr"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"tag") _x
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
                     = Lens.Family2.view
                         (Data.ProtoLens.Field.field @"anyConstructor") _x
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
                      (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'fields") _x))
                   (Data.ProtoLens.Encoding.Wire.buildFieldSet
                      (Lens.Family2.view Data.ProtoLens.unknownFields _x))))
instance Control.DeepSeq.NFData Constr where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Constr'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_Constr'tag x__)
                (Control.DeepSeq.deepseq
                   (_Constr'anyConstructor x__)
                   (Control.DeepSeq.deepseq (_Constr'fields x__) ())))
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
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.payload' @:: Lens' Datum PlutusData@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maybe'payload' @:: Lens' Datum (Prelude.Maybe PlutusData)@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.originalCbor' @:: Lens' Datum Data.ByteString.ByteString@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maybe'originalCbor' @:: Lens' Datum (Prelude.Maybe Data.ByteString.ByteString)@ -}
data Datum
  = Datum'_constructor {_Datum'hash :: !Data.ByteString.ByteString,
                        _Datum'payload :: !(Prelude.Maybe PlutusData),
                        _Datum'originalCbor :: !(Prelude.Maybe Data.ByteString.ByteString),
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
instance Data.ProtoLens.Field.HasField Datum "payload" PlutusData where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Datum'payload (\ x__ y__ -> x__ {_Datum'payload = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField Datum "maybe'payload" (Prelude.Maybe PlutusData) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Datum'payload (\ x__ y__ -> x__ {_Datum'payload = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Datum "originalCbor" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Datum'originalCbor (\ x__ y__ -> x__ {_Datum'originalCbor = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Data.ProtoLens.Field.HasField Datum "maybe'originalCbor" (Prelude.Maybe Data.ByteString.ByteString) where
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
      \\EOThash\CAN\SOH \SOH(\fR\EOThash\DC2B\n\
      \\apayload\CAN\STX \SOH(\v2#.utxorpc.v1alpha.cardano.PlutusDataH\NULR\apayload\136\SOH\SOH\DC2(\n\
      \\roriginal_cbor\CAN\ETX \SOH(\fH\SOHR\foriginalCbor\136\SOH\SOHB\n\
      \\n\
      \\b_payloadB\DLE\n\
      \\SO_original_cbor"
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
        payload__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "payload"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor PlutusData)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'payload")) ::
              Data.ProtoLens.FieldDescriptor Datum
        originalCbor__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "original_cbor"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'originalCbor")) ::
              Data.ProtoLens.FieldDescriptor Datum
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, hash__field_descriptor),
           (Data.ProtoLens.Tag 2, payload__field_descriptor),
           (Data.ProtoLens.Tag 3, originalCbor__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Datum'_unknownFields
        (\ x__ y__ -> x__ {_Datum'_unknownFields = y__})
  defMessage
    = Datum'_constructor
        {_Datum'hash = Data.ProtoLens.fieldDefault,
         _Datum'payload = Prelude.Nothing,
         _Datum'originalCbor = Prelude.Nothing, _Datum'_unknownFields = []}
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
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "payload"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"payload") y x)
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
                (case
                     Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'payload") _x
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
                          (Data.ProtoLens.Field.field @"maybe'originalCbor") _x
                    of
                      Prelude.Nothing -> Data.Monoid.mempty
                      (Prelude.Just _v)
                        -> (Data.Monoid.<>)
                             (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                             ((\ bs
                                 -> (Data.Monoid.<>)
                                      (Data.ProtoLens.Encoding.Bytes.putVarInt
                                         (Prelude.fromIntegral (Data.ByteString.length bs)))
                                      (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                _v))
                   (Data.ProtoLens.Encoding.Wire.buildFieldSet
                      (Lens.Family2.view Data.ProtoLens.unknownFields _x))))
instance Control.DeepSeq.NFData Datum where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Datum'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_Datum'hash x__)
                (Control.DeepSeq.deepseq
                   (_Datum'payload x__)
                   (Control.DeepSeq.deepseq (_Datum'originalCbor x__) ())))
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
     
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maybe'nativeScript' @:: Lens' NativeScript (Prelude.Maybe NativeScript'NativeScript)@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maybe'scriptPubkey' @:: Lens' NativeScript (Prelude.Maybe Data.ByteString.ByteString)@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.scriptPubkey' @:: Lens' NativeScript Data.ByteString.ByteString@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maybe'scriptAll' @:: Lens' NativeScript (Prelude.Maybe NativeScriptList)@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.scriptAll' @:: Lens' NativeScript NativeScriptList@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maybe'scriptAny' @:: Lens' NativeScript (Prelude.Maybe NativeScriptList)@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.scriptAny' @:: Lens' NativeScript NativeScriptList@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maybe'scriptNOfK' @:: Lens' NativeScript (Prelude.Maybe ScriptNOfK)@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.scriptNOfK' @:: Lens' NativeScript ScriptNOfK@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maybe'invalidBefore' @:: Lens' NativeScript (Prelude.Maybe Data.Word.Word64)@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.invalidBefore' @:: Lens' NativeScript Data.Word.Word64@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maybe'invalidHereafter' @:: Lens' NativeScript (Prelude.Maybe Data.Word.Word64)@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.invalidHereafter' @:: Lens' NativeScript Data.Word.Word64@ -}
data NativeScript
  = NativeScript'_constructor {_NativeScript'nativeScript :: !(Prelude.Maybe NativeScript'NativeScript),
                               _NativeScript'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show NativeScript where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
data NativeScript'NativeScript
  = NativeScript'ScriptPubkey !Data.ByteString.ByteString |
    NativeScript'ScriptAll !NativeScriptList |
    NativeScript'ScriptAny !NativeScriptList |
    NativeScript'ScriptNOfK !ScriptNOfK |
    NativeScript'InvalidBefore !Data.Word.Word64 |
    NativeScript'InvalidHereafter !Data.Word.Word64
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.Field.HasField NativeScript "maybe'nativeScript" (Prelude.Maybe NativeScript'NativeScript) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _NativeScript'nativeScript
           (\ x__ y__ -> x__ {_NativeScript'nativeScript = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField NativeScript "maybe'scriptPubkey" (Prelude.Maybe Data.ByteString.ByteString) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _NativeScript'nativeScript
           (\ x__ y__ -> x__ {_NativeScript'nativeScript = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (NativeScript'ScriptPubkey x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap NativeScript'ScriptPubkey y__))
instance Data.ProtoLens.Field.HasField NativeScript "scriptPubkey" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _NativeScript'nativeScript
           (\ x__ y__ -> x__ {_NativeScript'nativeScript = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (NativeScript'ScriptPubkey x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap NativeScript'ScriptPubkey y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault))
instance Data.ProtoLens.Field.HasField NativeScript "maybe'scriptAll" (Prelude.Maybe NativeScriptList) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _NativeScript'nativeScript
           (\ x__ y__ -> x__ {_NativeScript'nativeScript = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (NativeScript'ScriptAll x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap NativeScript'ScriptAll y__))
instance Data.ProtoLens.Field.HasField NativeScript "scriptAll" NativeScriptList where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _NativeScript'nativeScript
           (\ x__ y__ -> x__ {_NativeScript'nativeScript = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (NativeScript'ScriptAll x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap NativeScript'ScriptAll y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Field.HasField NativeScript "maybe'scriptAny" (Prelude.Maybe NativeScriptList) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _NativeScript'nativeScript
           (\ x__ y__ -> x__ {_NativeScript'nativeScript = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (NativeScript'ScriptAny x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap NativeScript'ScriptAny y__))
instance Data.ProtoLens.Field.HasField NativeScript "scriptAny" NativeScriptList where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _NativeScript'nativeScript
           (\ x__ y__ -> x__ {_NativeScript'nativeScript = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (NativeScript'ScriptAny x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap NativeScript'ScriptAny y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Field.HasField NativeScript "maybe'scriptNOfK" (Prelude.Maybe ScriptNOfK) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _NativeScript'nativeScript
           (\ x__ y__ -> x__ {_NativeScript'nativeScript = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (NativeScript'ScriptNOfK x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap NativeScript'ScriptNOfK y__))
instance Data.ProtoLens.Field.HasField NativeScript "scriptNOfK" ScriptNOfK where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _NativeScript'nativeScript
           (\ x__ y__ -> x__ {_NativeScript'nativeScript = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (NativeScript'ScriptNOfK x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap NativeScript'ScriptNOfK y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Field.HasField NativeScript "maybe'invalidBefore" (Prelude.Maybe Data.Word.Word64) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _NativeScript'nativeScript
           (\ x__ y__ -> x__ {_NativeScript'nativeScript = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (NativeScript'InvalidBefore x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap NativeScript'InvalidBefore y__))
instance Data.ProtoLens.Field.HasField NativeScript "invalidBefore" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _NativeScript'nativeScript
           (\ x__ y__ -> x__ {_NativeScript'nativeScript = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (NativeScript'InvalidBefore x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap NativeScript'InvalidBefore y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault))
instance Data.ProtoLens.Field.HasField NativeScript "maybe'invalidHereafter" (Prelude.Maybe Data.Word.Word64) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _NativeScript'nativeScript
           (\ x__ y__ -> x__ {_NativeScript'nativeScript = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (NativeScript'InvalidHereafter x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap NativeScript'InvalidHereafter y__))
instance Data.ProtoLens.Field.HasField NativeScript "invalidHereafter" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _NativeScript'nativeScript
           (\ x__ y__ -> x__ {_NativeScript'nativeScript = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (NativeScript'InvalidHereafter x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap NativeScript'InvalidHereafter y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault))
instance Data.ProtoLens.Message NativeScript where
  messageName _
    = Data.Text.pack "utxorpc.v1alpha.cardano.NativeScript"
  packedMessageDescriptor _
    = "\n\
      \\fNativeScript\DC2%\n\
      \\rscript_pubkey\CAN\SOH \SOH(\fH\NULR\fscriptPubkey\DC2J\n\
      \\n\
      \script_all\CAN\STX \SOH(\v2).utxorpc.v1alpha.cardano.NativeScriptListH\NULR\tscriptAll\DC2J\n\
      \\n\
      \script_any\CAN\ETX \SOH(\v2).utxorpc.v1alpha.cardano.NativeScriptListH\NULR\tscriptAny\DC2H\n\
      \\rscript_n_of_k\CAN\EOT \SOH(\v2#.utxorpc.v1alpha.cardano.ScriptNOfKH\NULR\n\
      \scriptNOfK\DC2'\n\
      \\SOinvalid_before\CAN\ENQ \SOH(\EOTH\NULR\rinvalidBefore\DC2-\n\
      \\DC1invalid_hereafter\CAN\ACK \SOH(\EOTH\NULR\DLEinvalidHereafterB\SI\n\
      \\rnative_script"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        scriptPubkey__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "script_pubkey"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'scriptPubkey")) ::
              Data.ProtoLens.FieldDescriptor NativeScript
        scriptAll__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "script_all"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor NativeScriptList)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'scriptAll")) ::
              Data.ProtoLens.FieldDescriptor NativeScript
        scriptAny__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "script_any"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor NativeScriptList)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'scriptAny")) ::
              Data.ProtoLens.FieldDescriptor NativeScript
        scriptNOfK__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "script_n_of_k"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ScriptNOfK)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'scriptNOfK")) ::
              Data.ProtoLens.FieldDescriptor NativeScript
        invalidBefore__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "invalid_before"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'invalidBefore")) ::
              Data.ProtoLens.FieldDescriptor NativeScript
        invalidHereafter__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "invalid_hereafter"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'invalidHereafter")) ::
              Data.ProtoLens.FieldDescriptor NativeScript
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, scriptPubkey__field_descriptor),
           (Data.ProtoLens.Tag 2, scriptAll__field_descriptor),
           (Data.ProtoLens.Tag 3, scriptAny__field_descriptor),
           (Data.ProtoLens.Tag 4, scriptNOfK__field_descriptor),
           (Data.ProtoLens.Tag 5, invalidBefore__field_descriptor),
           (Data.ProtoLens.Tag 6, invalidHereafter__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _NativeScript'_unknownFields
        (\ x__ y__ -> x__ {_NativeScript'_unknownFields = y__})
  defMessage
    = NativeScript'_constructor
        {_NativeScript'nativeScript = Prelude.Nothing,
         _NativeScript'_unknownFields = []}
  parseMessage
    = let
        loop ::
          NativeScript -> Data.ProtoLens.Encoding.Bytes.Parser NativeScript
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
                                       "script_pubkey"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"scriptPubkey") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "script_all"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"scriptAll") y x)
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "script_any"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"scriptAny") y x)
                        34
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "script_n_of_k"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"scriptNOfK") y x)
                        40
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getVarInt "invalid_before"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"invalidBefore") y x)
                        48
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getVarInt "invalid_hereafter"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"invalidHereafter") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "NativeScript"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view
                    (Data.ProtoLens.Field.field @"maybe'nativeScript") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just (NativeScript'ScriptPubkey v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                       ((\ bs
                           -> (Data.Monoid.<>)
                                (Data.ProtoLens.Encoding.Bytes.putVarInt
                                   (Prelude.fromIntegral (Data.ByteString.length bs)))
                                (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          v)
                (Prelude.Just (NativeScript'ScriptAll v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage v)
                (Prelude.Just (NativeScript'ScriptAny v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage v)
                (Prelude.Just (NativeScript'ScriptNOfK v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage v)
                (Prelude.Just (NativeScript'InvalidBefore v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 40)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt v)
                (Prelude.Just (NativeScript'InvalidHereafter v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 48)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt v))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData NativeScript where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_NativeScript'_unknownFields x__)
             (Control.DeepSeq.deepseq (_NativeScript'nativeScript x__) ())
instance Control.DeepSeq.NFData NativeScript'NativeScript where
  rnf (NativeScript'ScriptPubkey x__) = Control.DeepSeq.rnf x__
  rnf (NativeScript'ScriptAll x__) = Control.DeepSeq.rnf x__
  rnf (NativeScript'ScriptAny x__) = Control.DeepSeq.rnf x__
  rnf (NativeScript'ScriptNOfK x__) = Control.DeepSeq.rnf x__
  rnf (NativeScript'InvalidBefore x__) = Control.DeepSeq.rnf x__
  rnf (NativeScript'InvalidHereafter x__) = Control.DeepSeq.rnf x__
_NativeScript'ScriptPubkey ::
  Data.ProtoLens.Prism.Prism' NativeScript'NativeScript Data.ByteString.ByteString
_NativeScript'ScriptPubkey
  = Data.ProtoLens.Prism.prism'
      NativeScript'ScriptPubkey
      (\ p__
         -> case p__ of
              (NativeScript'ScriptPubkey p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_NativeScript'ScriptAll ::
  Data.ProtoLens.Prism.Prism' NativeScript'NativeScript NativeScriptList
_NativeScript'ScriptAll
  = Data.ProtoLens.Prism.prism'
      NativeScript'ScriptAll
      (\ p__
         -> case p__ of
              (NativeScript'ScriptAll p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_NativeScript'ScriptAny ::
  Data.ProtoLens.Prism.Prism' NativeScript'NativeScript NativeScriptList
_NativeScript'ScriptAny
  = Data.ProtoLens.Prism.prism'
      NativeScript'ScriptAny
      (\ p__
         -> case p__ of
              (NativeScript'ScriptAny p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_NativeScript'ScriptNOfK ::
  Data.ProtoLens.Prism.Prism' NativeScript'NativeScript ScriptNOfK
_NativeScript'ScriptNOfK
  = Data.ProtoLens.Prism.prism'
      NativeScript'ScriptNOfK
      (\ p__
         -> case p__ of
              (NativeScript'ScriptNOfK p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_NativeScript'InvalidBefore ::
  Data.ProtoLens.Prism.Prism' NativeScript'NativeScript Data.Word.Word64
_NativeScript'InvalidBefore
  = Data.ProtoLens.Prism.prism'
      NativeScript'InvalidBefore
      (\ p__
         -> case p__ of
              (NativeScript'InvalidBefore p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_NativeScript'InvalidHereafter ::
  Data.ProtoLens.Prism.Prism' NativeScript'NativeScript Data.Word.Word64
_NativeScript'InvalidHereafter
  = Data.ProtoLens.Prism.prism'
      NativeScript'InvalidHereafter
      (\ p__
         -> case p__ of
              (NativeScript'InvalidHereafter p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
{- | Fields :
     
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.items' @:: Lens' NativeScriptList [NativeScript]@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.vec'items' @:: Lens' NativeScriptList (Data.Vector.Vector NativeScript)@ -}
data NativeScriptList
  = NativeScriptList'_constructor {_NativeScriptList'items :: !(Data.Vector.Vector NativeScript),
                                   _NativeScriptList'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show NativeScriptList where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField NativeScriptList "items" [NativeScript] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _NativeScriptList'items
           (\ x__ y__ -> x__ {_NativeScriptList'items = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField NativeScriptList "vec'items" (Data.Vector.Vector NativeScript) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _NativeScriptList'items
           (\ x__ y__ -> x__ {_NativeScriptList'items = y__}))
        Prelude.id
instance Data.ProtoLens.Message NativeScriptList where
  messageName _
    = Data.Text.pack "utxorpc.v1alpha.cardano.NativeScriptList"
  packedMessageDescriptor _
    = "\n\
      \\DLENativeScriptList\DC2;\n\
      \\ENQitems\CAN\SOH \ETX(\v2%.utxorpc.v1alpha.cardano.NativeScriptR\ENQitems"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        items__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "items"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor NativeScript)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"items")) ::
              Data.ProtoLens.FieldDescriptor NativeScriptList
      in
        Data.Map.fromList [(Data.ProtoLens.Tag 1, items__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _NativeScriptList'_unknownFields
        (\ x__ y__ -> x__ {_NativeScriptList'_unknownFields = y__})
  defMessage
    = NativeScriptList'_constructor
        {_NativeScriptList'items = Data.Vector.Generic.empty,
         _NativeScriptList'_unknownFields = []}
  parseMessage
    = let
        loop ::
          NativeScriptList
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld NativeScript
             -> Data.ProtoLens.Encoding.Bytes.Parser NativeScriptList
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
          "NativeScriptList"
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
instance Control.DeepSeq.NFData NativeScriptList where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_NativeScriptList'_unknownFields x__)
             (Control.DeepSeq.deepseq (_NativeScriptList'items x__) ())
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
     
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maybe'plutusData' @:: Lens' PlutusData (Prelude.Maybe PlutusData'PlutusData)@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maybe'constr' @:: Lens' PlutusData (Prelude.Maybe Constr)@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.constr' @:: Lens' PlutusData Constr@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maybe'map' @:: Lens' PlutusData (Prelude.Maybe PlutusDataMap)@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.map' @:: Lens' PlutusData PlutusDataMap@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maybe'bigInt' @:: Lens' PlutusData (Prelude.Maybe BigInt)@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.bigInt' @:: Lens' PlutusData BigInt@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maybe'boundedBytes' @:: Lens' PlutusData (Prelude.Maybe Data.ByteString.ByteString)@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.boundedBytes' @:: Lens' PlutusData Data.ByteString.ByteString@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maybe'array' @:: Lens' PlutusData (Prelude.Maybe PlutusDataArray)@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.array' @:: Lens' PlutusData PlutusDataArray@ -}
data PlutusData
  = PlutusData'_constructor {_PlutusData'plutusData :: !(Prelude.Maybe PlutusData'PlutusData),
                             _PlutusData'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show PlutusData where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
data PlutusData'PlutusData
  = PlutusData'Constr !Constr |
    PlutusData'Map !PlutusDataMap |
    PlutusData'BigInt !BigInt |
    PlutusData'BoundedBytes !Data.ByteString.ByteString |
    PlutusData'Array !PlutusDataArray
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.Field.HasField PlutusData "maybe'plutusData" (Prelude.Maybe PlutusData'PlutusData) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PlutusData'plutusData
           (\ x__ y__ -> x__ {_PlutusData'plutusData = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField PlutusData "maybe'constr" (Prelude.Maybe Constr) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PlutusData'plutusData
           (\ x__ y__ -> x__ {_PlutusData'plutusData = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (PlutusData'Constr x__val)) -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap PlutusData'Constr y__))
instance Data.ProtoLens.Field.HasField PlutusData "constr" Constr where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PlutusData'plutusData
           (\ x__ y__ -> x__ {_PlutusData'plutusData = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (PlutusData'Constr x__val)) -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap PlutusData'Constr y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Field.HasField PlutusData "maybe'map" (Prelude.Maybe PlutusDataMap) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PlutusData'plutusData
           (\ x__ y__ -> x__ {_PlutusData'plutusData = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (PlutusData'Map x__val)) -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap PlutusData'Map y__))
instance Data.ProtoLens.Field.HasField PlutusData "map" PlutusDataMap where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PlutusData'plutusData
           (\ x__ y__ -> x__ {_PlutusData'plutusData = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (PlutusData'Map x__val)) -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap PlutusData'Map y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Field.HasField PlutusData "maybe'bigInt" (Prelude.Maybe BigInt) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PlutusData'plutusData
           (\ x__ y__ -> x__ {_PlutusData'plutusData = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (PlutusData'BigInt x__val)) -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap PlutusData'BigInt y__))
instance Data.ProtoLens.Field.HasField PlutusData "bigInt" BigInt where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PlutusData'plutusData
           (\ x__ y__ -> x__ {_PlutusData'plutusData = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (PlutusData'BigInt x__val)) -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap PlutusData'BigInt y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Field.HasField PlutusData "maybe'boundedBytes" (Prelude.Maybe Data.ByteString.ByteString) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PlutusData'plutusData
           (\ x__ y__ -> x__ {_PlutusData'plutusData = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (PlutusData'BoundedBytes x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap PlutusData'BoundedBytes y__))
instance Data.ProtoLens.Field.HasField PlutusData "boundedBytes" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PlutusData'plutusData
           (\ x__ y__ -> x__ {_PlutusData'plutusData = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (PlutusData'BoundedBytes x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap PlutusData'BoundedBytes y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault))
instance Data.ProtoLens.Field.HasField PlutusData "maybe'array" (Prelude.Maybe PlutusDataArray) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PlutusData'plutusData
           (\ x__ y__ -> x__ {_PlutusData'plutusData = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (PlutusData'Array x__val)) -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap PlutusData'Array y__))
instance Data.ProtoLens.Field.HasField PlutusData "array" PlutusDataArray where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PlutusData'plutusData
           (\ x__ y__ -> x__ {_PlutusData'plutusData = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (PlutusData'Array x__val)) -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap PlutusData'Array y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Message PlutusData where
  messageName _ = Data.Text.pack "utxorpc.v1alpha.cardano.PlutusData"
  packedMessageDescriptor _
    = "\n\
      \\n\
      \PlutusData\DC29\n\
      \\ACKconstr\CAN\STX \SOH(\v2\US.utxorpc.v1alpha.cardano.ConstrH\NULR\ACKconstr\DC2:\n\
      \\ETXmap\CAN\ETX \SOH(\v2&.utxorpc.v1alpha.cardano.PlutusDataMapH\NULR\ETXmap\DC2:\n\
      \\abig_int\CAN\EOT \SOH(\v2\US.utxorpc.v1alpha.cardano.BigIntH\NULR\ACKbigInt\DC2%\n\
      \\rbounded_bytes\CAN\ENQ \SOH(\fH\NULR\fboundedBytes\DC2@\n\
      \\ENQarray\CAN\ACK \SOH(\v2(.utxorpc.v1alpha.cardano.PlutusDataArrayH\NULR\ENQarrayB\r\n\
      \\vplutus_data"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        constr__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "constr"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Constr)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'constr")) ::
              Data.ProtoLens.FieldDescriptor PlutusData
        map__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "map"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor PlutusDataMap)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'map")) ::
              Data.ProtoLens.FieldDescriptor PlutusData
        bigInt__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "big_int"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor BigInt)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'bigInt")) ::
              Data.ProtoLens.FieldDescriptor PlutusData
        boundedBytes__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "bounded_bytes"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'boundedBytes")) ::
              Data.ProtoLens.FieldDescriptor PlutusData
        array__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "array"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor PlutusDataArray)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'array")) ::
              Data.ProtoLens.FieldDescriptor PlutusData
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 2, constr__field_descriptor),
           (Data.ProtoLens.Tag 3, map__field_descriptor),
           (Data.ProtoLens.Tag 4, bigInt__field_descriptor),
           (Data.ProtoLens.Tag 5, boundedBytes__field_descriptor),
           (Data.ProtoLens.Tag 6, array__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _PlutusData'_unknownFields
        (\ x__ y__ -> x__ {_PlutusData'_unknownFields = y__})
  defMessage
    = PlutusData'_constructor
        {_PlutusData'plutusData = Prelude.Nothing,
         _PlutusData'_unknownFields = []}
  parseMessage
    = let
        loop ::
          PlutusData -> Data.ProtoLens.Encoding.Bytes.Parser PlutusData
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
                                       "constr"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"constr") y x)
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "map"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"map") y x)
                        34
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "big_int"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"bigInt") y x)
                        42
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getBytes
                                             (Prelude.fromIntegral len))
                                       "bounded_bytes"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"boundedBytes") y x)
                        50
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "array"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"array") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "PlutusData"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view
                    (Data.ProtoLens.Field.field @"maybe'plutusData") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just (PlutusData'Constr v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage v)
                (Prelude.Just (PlutusData'Map v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage v)
                (Prelude.Just (PlutusData'BigInt v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage v)
                (Prelude.Just (PlutusData'BoundedBytes v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 42)
                       ((\ bs
                           -> (Data.Monoid.<>)
                                (Data.ProtoLens.Encoding.Bytes.putVarInt
                                   (Prelude.fromIntegral (Data.ByteString.length bs)))
                                (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          v)
                (Prelude.Just (PlutusData'Array v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 50)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage v))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData PlutusData where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_PlutusData'_unknownFields x__)
             (Control.DeepSeq.deepseq (_PlutusData'plutusData x__) ())
instance Control.DeepSeq.NFData PlutusData'PlutusData where
  rnf (PlutusData'Constr x__) = Control.DeepSeq.rnf x__
  rnf (PlutusData'Map x__) = Control.DeepSeq.rnf x__
  rnf (PlutusData'BigInt x__) = Control.DeepSeq.rnf x__
  rnf (PlutusData'BoundedBytes x__) = Control.DeepSeq.rnf x__
  rnf (PlutusData'Array x__) = Control.DeepSeq.rnf x__
_PlutusData'Constr ::
  Data.ProtoLens.Prism.Prism' PlutusData'PlutusData Constr
_PlutusData'Constr
  = Data.ProtoLens.Prism.prism'
      PlutusData'Constr
      (\ p__
         -> case p__ of
              (PlutusData'Constr p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_PlutusData'Map ::
  Data.ProtoLens.Prism.Prism' PlutusData'PlutusData PlutusDataMap
_PlutusData'Map
  = Data.ProtoLens.Prism.prism'
      PlutusData'Map
      (\ p__
         -> case p__ of
              (PlutusData'Map p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_PlutusData'BigInt ::
  Data.ProtoLens.Prism.Prism' PlutusData'PlutusData BigInt
_PlutusData'BigInt
  = Data.ProtoLens.Prism.prism'
      PlutusData'BigInt
      (\ p__
         -> case p__ of
              (PlutusData'BigInt p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_PlutusData'BoundedBytes ::
  Data.ProtoLens.Prism.Prism' PlutusData'PlutusData Data.ByteString.ByteString
_PlutusData'BoundedBytes
  = Data.ProtoLens.Prism.prism'
      PlutusData'BoundedBytes
      (\ p__
         -> case p__ of
              (PlutusData'BoundedBytes p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_PlutusData'Array ::
  Data.ProtoLens.Prism.Prism' PlutusData'PlutusData PlutusDataArray
_PlutusData'Array
  = Data.ProtoLens.Prism.prism'
      PlutusData'Array
      (\ p__
         -> case p__ of
              (PlutusData'Array p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
{- | Fields :
     
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.items' @:: Lens' PlutusDataArray [PlutusData]@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.vec'items' @:: Lens' PlutusDataArray (Data.Vector.Vector PlutusData)@ -}
data PlutusDataArray
  = PlutusDataArray'_constructor {_PlutusDataArray'items :: !(Data.Vector.Vector PlutusData),
                                  _PlutusDataArray'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show PlutusDataArray where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField PlutusDataArray "items" [PlutusData] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PlutusDataArray'items
           (\ x__ y__ -> x__ {_PlutusDataArray'items = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField PlutusDataArray "vec'items" (Data.Vector.Vector PlutusData) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PlutusDataArray'items
           (\ x__ y__ -> x__ {_PlutusDataArray'items = y__}))
        Prelude.id
instance Data.ProtoLens.Message PlutusDataArray where
  messageName _
    = Data.Text.pack "utxorpc.v1alpha.cardano.PlutusDataArray"
  packedMessageDescriptor _
    = "\n\
      \\SIPlutusDataArray\DC29\n\
      \\ENQitems\CAN\SOH \ETX(\v2#.utxorpc.v1alpha.cardano.PlutusDataR\ENQitems"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        items__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "items"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor PlutusData)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"items")) ::
              Data.ProtoLens.FieldDescriptor PlutusDataArray
      in
        Data.Map.fromList [(Data.ProtoLens.Tag 1, items__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _PlutusDataArray'_unknownFields
        (\ x__ y__ -> x__ {_PlutusDataArray'_unknownFields = y__})
  defMessage
    = PlutusDataArray'_constructor
        {_PlutusDataArray'items = Data.Vector.Generic.empty,
         _PlutusDataArray'_unknownFields = []}
  parseMessage
    = let
        loop ::
          PlutusDataArray
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld PlutusData
             -> Data.ProtoLens.Encoding.Bytes.Parser PlutusDataArray
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
          "PlutusDataArray"
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
instance Control.DeepSeq.NFData PlutusDataArray where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_PlutusDataArray'_unknownFields x__)
             (Control.DeepSeq.deepseq (_PlutusDataArray'items x__) ())
{- | Fields :
     
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.pairs' @:: Lens' PlutusDataMap [PlutusDataPair]@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.vec'pairs' @:: Lens' PlutusDataMap (Data.Vector.Vector PlutusDataPair)@ -}
data PlutusDataMap
  = PlutusDataMap'_constructor {_PlutusDataMap'pairs :: !(Data.Vector.Vector PlutusDataPair),
                                _PlutusDataMap'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show PlutusDataMap where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField PlutusDataMap "pairs" [PlutusDataPair] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PlutusDataMap'pairs
           (\ x__ y__ -> x__ {_PlutusDataMap'pairs = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField PlutusDataMap "vec'pairs" (Data.Vector.Vector PlutusDataPair) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PlutusDataMap'pairs
           (\ x__ y__ -> x__ {_PlutusDataMap'pairs = y__}))
        Prelude.id
instance Data.ProtoLens.Message PlutusDataMap where
  messageName _
    = Data.Text.pack "utxorpc.v1alpha.cardano.PlutusDataMap"
  packedMessageDescriptor _
    = "\n\
      \\rPlutusDataMap\DC2=\n\
      \\ENQpairs\CAN\SOH \ETX(\v2'.utxorpc.v1alpha.cardano.PlutusDataPairR\ENQpairs"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        pairs__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "pairs"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor PlutusDataPair)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"pairs")) ::
              Data.ProtoLens.FieldDescriptor PlutusDataMap
      in
        Data.Map.fromList [(Data.ProtoLens.Tag 1, pairs__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _PlutusDataMap'_unknownFields
        (\ x__ y__ -> x__ {_PlutusDataMap'_unknownFields = y__})
  defMessage
    = PlutusDataMap'_constructor
        {_PlutusDataMap'pairs = Data.Vector.Generic.empty,
         _PlutusDataMap'_unknownFields = []}
  parseMessage
    = let
        loop ::
          PlutusDataMap
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld PlutusDataPair
             -> Data.ProtoLens.Encoding.Bytes.Parser PlutusDataMap
        loop x mutable'pairs
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'pairs <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                        (Data.ProtoLens.Encoding.Growing.unsafeFreeze mutable'pairs)
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
                              (Data.ProtoLens.Field.field @"vec'pairs") frozen'pairs x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "pairs"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'pairs y)
                                loop x v
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'pairs
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'pairs <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                 Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'pairs)
          "PlutusDataMap"
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
                (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'pairs") _x))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData PlutusDataMap where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_PlutusDataMap'_unknownFields x__)
             (Control.DeepSeq.deepseq (_PlutusDataMap'pairs x__) ())
{- | Fields :
     
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.key' @:: Lens' PlutusDataPair PlutusData@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maybe'key' @:: Lens' PlutusDataPair (Prelude.Maybe PlutusData)@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.value' @:: Lens' PlutusDataPair PlutusData@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maybe'value' @:: Lens' PlutusDataPair (Prelude.Maybe PlutusData)@ -}
data PlutusDataPair
  = PlutusDataPair'_constructor {_PlutusDataPair'key :: !(Prelude.Maybe PlutusData),
                                 _PlutusDataPair'value :: !(Prelude.Maybe PlutusData),
                                 _PlutusDataPair'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show PlutusDataPair where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField PlutusDataPair "key" PlutusData where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PlutusDataPair'key (\ x__ y__ -> x__ {_PlutusDataPair'key = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField PlutusDataPair "maybe'key" (Prelude.Maybe PlutusData) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PlutusDataPair'key (\ x__ y__ -> x__ {_PlutusDataPair'key = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField PlutusDataPair "value" PlutusData where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PlutusDataPair'value
           (\ x__ y__ -> x__ {_PlutusDataPair'value = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField PlutusDataPair "maybe'value" (Prelude.Maybe PlutusData) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PlutusDataPair'value
           (\ x__ y__ -> x__ {_PlutusDataPair'value = y__}))
        Prelude.id
instance Data.ProtoLens.Message PlutusDataPair where
  messageName _
    = Data.Text.pack "utxorpc.v1alpha.cardano.PlutusDataPair"
  packedMessageDescriptor _
    = "\n\
      \\SOPlutusDataPair\DC25\n\
      \\ETXkey\CAN\SOH \SOH(\v2#.utxorpc.v1alpha.cardano.PlutusDataR\ETXkey\DC29\n\
      \\ENQvalue\CAN\STX \SOH(\v2#.utxorpc.v1alpha.cardano.PlutusDataR\ENQvalue"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        key__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "key"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor PlutusData)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'key")) ::
              Data.ProtoLens.FieldDescriptor PlutusDataPair
        value__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "value"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor PlutusData)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'value")) ::
              Data.ProtoLens.FieldDescriptor PlutusDataPair
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, key__field_descriptor),
           (Data.ProtoLens.Tag 2, value__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _PlutusDataPair'_unknownFields
        (\ x__ y__ -> x__ {_PlutusDataPair'_unknownFields = y__})
  defMessage
    = PlutusDataPair'_constructor
        {_PlutusDataPair'key = Prelude.Nothing,
         _PlutusDataPair'value = Prelude.Nothing,
         _PlutusDataPair'_unknownFields = []}
  parseMessage
    = let
        loop ::
          PlutusDataPair
          -> Data.ProtoLens.Encoding.Bytes.Parser PlutusDataPair
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
                                       "key"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"key") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "value"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"value") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "PlutusDataPair"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'key") _x
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
                     Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'value") _x
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
instance Control.DeepSeq.NFData PlutusDataPair where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_PlutusDataPair'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_PlutusDataPair'key x__)
                (Control.DeepSeq.deepseq (_PlutusDataPair'value x__) ()))
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
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.maybe'native' @:: Lens' Script (Prelude.Maybe NativeScript)@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.native' @:: Lens' Script NativeScript@
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
  = Script'Native !NativeScript |
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
instance Data.ProtoLens.Field.HasField Script "maybe'native" (Prelude.Maybe NativeScript) where
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
instance Data.ProtoLens.Field.HasField Script "native" NativeScript where
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
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
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
      \\ACKScript\DC2?\n\
      \\ACKnative\CAN\SOH \SOH(\v2%.utxorpc.v1alpha.cardano.NativeScriptH\NULR\ACKnative\DC2\GS\n\
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
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor NativeScript)
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
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
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
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage v)
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
  Data.ProtoLens.Prism.Prism' Script'Script NativeScript
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
     
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.k' @:: Lens' ScriptNOfK Data.Word.Word32@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.scripts' @:: Lens' ScriptNOfK [NativeScript]@
         * 'Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields.vec'scripts' @:: Lens' ScriptNOfK (Data.Vector.Vector NativeScript)@ -}
data ScriptNOfK
  = ScriptNOfK'_constructor {_ScriptNOfK'k :: !Data.Word.Word32,
                             _ScriptNOfK'scripts :: !(Data.Vector.Vector NativeScript),
                             _ScriptNOfK'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ScriptNOfK where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ScriptNOfK "k" Data.Word.Word32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ScriptNOfK'k (\ x__ y__ -> x__ {_ScriptNOfK'k = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ScriptNOfK "scripts" [NativeScript] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ScriptNOfK'scripts (\ x__ y__ -> x__ {_ScriptNOfK'scripts = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ScriptNOfK "vec'scripts" (Data.Vector.Vector NativeScript) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ScriptNOfK'scripts (\ x__ y__ -> x__ {_ScriptNOfK'scripts = y__}))
        Prelude.id
instance Data.ProtoLens.Message ScriptNOfK where
  messageName _ = Data.Text.pack "utxorpc.v1alpha.cardano.ScriptNOfK"
  packedMessageDescriptor _
    = "\n\
      \\n\
      \ScriptNOfK\DC2\f\n\
      \\SOHk\CAN\SOH \SOH(\rR\SOHk\DC2?\n\
      \\ascripts\CAN\STX \ETX(\v2%.utxorpc.v1alpha.cardano.NativeScriptR\ascripts"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        k__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "k"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"k")) ::
              Data.ProtoLens.FieldDescriptor ScriptNOfK
        scripts__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "scripts"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor NativeScript)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"scripts")) ::
              Data.ProtoLens.FieldDescriptor ScriptNOfK
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, k__field_descriptor),
           (Data.ProtoLens.Tag 2, scripts__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ScriptNOfK'_unknownFields
        (\ x__ y__ -> x__ {_ScriptNOfK'_unknownFields = y__})
  defMessage
    = ScriptNOfK'_constructor
        {_ScriptNOfK'k = Data.ProtoLens.fieldDefault,
         _ScriptNOfK'scripts = Data.Vector.Generic.empty,
         _ScriptNOfK'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ScriptNOfK
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld NativeScript
             -> Data.ProtoLens.Encoding.Bytes.Parser ScriptNOfK
        loop x mutable'scripts
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'scripts <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                          (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                             mutable'scripts)
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
                              (Data.ProtoLens.Field.field @"vec'scripts") frozen'scripts x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        8 -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "k"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"k") y x)
                                  mutable'scripts
                        18
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "scripts"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'scripts y)
                                loop x v
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'scripts
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'scripts <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                   Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'scripts)
          "ScriptNOfK"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"k") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                      ((Prelude..)
                         Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
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
                   (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'scripts") _x))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData ScriptNOfK where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ScriptNOfK'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ScriptNOfK'k x__)
                (Control.DeepSeq.deepseq (_ScriptNOfK'scripts x__) ()))
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
    \\ENQitems\CAN\SOH \ETX(\fR\ENQitems\"\167\SOH\n\
    \\ENQDatum\DC2\DC2\n\
    \\EOThash\CAN\SOH \SOH(\fR\EOThash\DC2B\n\
    \\apayload\CAN\STX \SOH(\v2#.utxorpc.v1alpha.cardano.PlutusDataH\NULR\apayload\136\SOH\SOH\DC2(\n\
    \\roriginal_cbor\CAN\ETX \SOH(\fH\SOHR\foriginalCbor\136\SOH\SOHB\n\
    \\n\
    \\b_payloadB\DLE\n\
    \\SO_original_cbor\"q\n\
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
    \\ACKassets\CAN\STX \ETX(\v2\RS.utxorpc.v1alpha.cardano.AssetR\ACKassets\"\128\ETX\n\
    \\fNativeScript\DC2%\n\
    \\rscript_pubkey\CAN\SOH \SOH(\fH\NULR\fscriptPubkey\DC2J\n\
    \\n\
    \script_all\CAN\STX \SOH(\v2).utxorpc.v1alpha.cardano.NativeScriptListH\NULR\tscriptAll\DC2J\n\
    \\n\
    \script_any\CAN\ETX \SOH(\v2).utxorpc.v1alpha.cardano.NativeScriptListH\NULR\tscriptAny\DC2H\n\
    \\rscript_n_of_k\CAN\EOT \SOH(\v2#.utxorpc.v1alpha.cardano.ScriptNOfKH\NULR\n\
    \scriptNOfK\DC2'\n\
    \\SOinvalid_before\CAN\ENQ \SOH(\EOTH\NULR\rinvalidBefore\DC2-\n\
    \\DC1invalid_hereafter\CAN\ACK \SOH(\EOTH\NULR\DLEinvalidHereafterB\SI\n\
    \\rnative_script\"O\n\
    \\DLENativeScriptList\DC2;\n\
    \\ENQitems\CAN\SOH \ETX(\v2%.utxorpc.v1alpha.cardano.NativeScriptR\ENQitems\"[\n\
    \\n\
    \ScriptNOfK\DC2\f\n\
    \\SOHk\CAN\SOH \SOH(\rR\SOHk\DC2?\n\
    \\ascripts\CAN\STX \ETX(\v2%.utxorpc.v1alpha.cardano.NativeScriptR\ascripts\"\128\SOH\n\
    \\ACKConstr\DC2\DLE\n\
    \\ETXtag\CAN\SOH \SOH(\rR\ETXtag\DC2'\n\
    \\SIany_constructor\CAN\STX \SOH(\EOTR\SOanyConstructor\DC2;\n\
    \\ACKfields\CAN\ETX \ETX(\v2#.utxorpc.v1alpha.cardano.PlutusDataR\ACKfields\"g\n\
    \\ACKBigInt\DC2\SYN\n\
    \\ETXint\CAN\SOH \SOH(\ETXH\NULR\ETXintB\STX0\SOH\DC2\FS\n\
    \\tbig_u_int\CAN\STX \SOH(\fH\NULR\abigUInt\DC2\FS\n\
    \\tbig_n_int\CAN\ETX \SOH(\fH\NULR\abigNIntB\t\n\
    \\abig_int\"\130\SOH\n\
    \\SOPlutusDataPair\DC25\n\
    \\ETXkey\CAN\SOH \SOH(\v2#.utxorpc.v1alpha.cardano.PlutusDataR\ETXkey\DC29\n\
    \\ENQvalue\CAN\STX \SOH(\v2#.utxorpc.v1alpha.cardano.PlutusDataR\ENQvalue\"\183\STX\n\
    \\n\
    \PlutusData\DC29\n\
    \\ACKconstr\CAN\STX \SOH(\v2\US.utxorpc.v1alpha.cardano.ConstrH\NULR\ACKconstr\DC2:\n\
    \\ETXmap\CAN\ETX \SOH(\v2&.utxorpc.v1alpha.cardano.PlutusDataMapH\NULR\ETXmap\DC2:\n\
    \\abig_int\CAN\EOT \SOH(\v2\US.utxorpc.v1alpha.cardano.BigIntH\NULR\ACKbigInt\DC2%\n\
    \\rbounded_bytes\CAN\ENQ \SOH(\fH\NULR\fboundedBytes\DC2@\n\
    \\ENQarray\CAN\ACK \SOH(\v2(.utxorpc.v1alpha.cardano.PlutusDataArrayH\NULR\ENQarrayB\r\n\
    \\vplutus_data\"N\n\
    \\rPlutusDataMap\DC2=\n\
    \\ENQpairs\CAN\SOH \ETX(\v2'.utxorpc.v1alpha.cardano.PlutusDataPairR\ENQpairs\"L\n\
    \\SIPlutusDataArray\DC29\n\
    \\ENQitems\CAN\SOH \ETX(\v2#.utxorpc.v1alpha.cardano.PlutusDataR\ENQitems\"\207\SOH\n\
    \\ACKScript\DC2?\n\
    \\ACKnative\CAN\SOH \SOH(\v2%.utxorpc.v1alpha.cardano.NativeScriptH\NULR\ACKnative\DC2\GS\n\
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
    \\ESCcom.utxorpc.v1alpha.cardanoB\fCardanoProtoP\SOH\162\STX\ETXUVC\170\STX\ETBUtxorpc.V1alpha.Cardano\202\STX\ETBUtxorpc\\V1alpha\\Cardano\226\STX#Utxorpc\\V1alpha\\Cardano\\GPBMetadata\234\STX\EMUtxorpc::V1alpha::CardanoJ\185K\n\
    \\a\DC2\ENQ\NUL\NUL\192\SOH\SOH\n\
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
    \\n\
    \\n\
    \\STX\EOT\STX\DC2\EOT\DC2\NUL\SYN\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\STX\SOH\DC2\ETX\DC2\b\r\n\
    \2\n\
    \\EOT\EOT\STX\STX\NUL\DC2\ETX\DC3\STX\DC1\"% Hash of this datum as seen on-chain\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ENQ\DC2\ETX\DC3\STX\a\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\SOH\DC2\ETX\DC3\b\f\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ETX\DC2\ETX\DC3\SI\DLE\n\
    \)\n\
    \\EOT\EOT\STX\STX\SOH\DC2\ETX\DC4\STX\"\"\FS Parsed Plutus data payload\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\EOT\DC2\ETX\DC4\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\ACK\DC2\ETX\DC4\v\NAK\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\SOH\DC2\ETX\DC4\SYN\GS\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\ETX\DC2\ETX\DC4 !\n\
    \:\n\
    \\EOT\EOT\STX\STX\STX\DC2\ETX\NAK\STX#\"- Original cbor-encoded data as seen on-chain\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\STX\EOT\DC2\ETX\NAK\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\STX\ENQ\DC2\ETX\NAK\v\DLE\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\STX\SOH\DC2\ETX\NAK\DC1\RS\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\STX\ETX\DC2\ETX\NAK!\"\n\
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
    \4\n\
    \\STX\EOT\ENQ\DC2\EOT)\NUL2\SOH\SUB( Represents a native script in Cardano.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\ENQ\SOH\DC2\ETX)\b\DC4\n\
    \\f\n\
    \\EOT\EOT\ENQ\b\NUL\DC2\EOT*\STX1\ETX\n\
    \\f\n\
    \\ENQ\EOT\ENQ\b\NUL\SOH\DC2\ETX*\b\NAK\n\
    \3\n\
    \\EOT\EOT\ENQ\STX\NUL\DC2\ETX+\EOT\FS\"& Script based on an address key hash.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\NUL\ENQ\DC2\ETX+\EOT\t\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\NUL\SOH\DC2\ETX+\n\
    \\ETB\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\NUL\ETX\DC2\ETX+\SUB\ESC\n\
    \G\n\
    \\EOT\EOT\ENQ\STX\SOH\DC2\ETX,\EOT$\": Script that requires all nested scripts to be satisfied.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\SOH\ACK\DC2\ETX,\EOT\DC4\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\SOH\SOH\DC2\ETX,\NAK\US\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\SOH\ETX\DC2\ETX,\"#\n\
    \N\n\
    \\EOT\EOT\ENQ\STX\STX\DC2\ETX-\EOT$\"A Script that requires any of the nested scripts to be satisfied.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\STX\ACK\DC2\ETX-\EOT\DC4\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\STX\SOH\DC2\ETX-\NAK\US\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\STX\ETX\DC2\ETX-\"#\n\
    \N\n\
    \\EOT\EOT\ENQ\STX\ETX\DC2\ETX.\EOT!\"A Script that requires k out of n nested scripts to be satisfied.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\ETX\ACK\DC2\ETX.\EOT\SO\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\ETX\SOH\DC2\ETX.\SI\FS\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\ETX\ETX\DC2\ETX.\US \n\
    \>\n\
    \\EOT\EOT\ENQ\STX\EOT\DC2\ETX/\EOT\RS\"1 Slot number before which the script is invalid.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\EOT\ENQ\DC2\ETX/\EOT\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\EOT\SOH\DC2\ETX/\v\EM\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\EOT\ETX\DC2\ETX/\FS\GS\n\
    \=\n\
    \\EOT\EOT\ENQ\STX\ENQ\DC2\ETX0\EOT!\"0 Slot number after which the script is invalid.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\ENQ\ENQ\DC2\ETX0\EOT\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\ENQ\SOH\DC2\ETX0\v\FS\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\ENQ\ETX\DC2\ETX0\US \n\
    \2\n\
    \\STX\EOT\ACK\DC2\EOT5\NUL7\SOH\SUB& Represents a list of native scripts.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\ACK\SOH\DC2\ETX5\b\CAN\n\
    \&\n\
    \\EOT\EOT\ACK\STX\NUL\DC2\ETX6\STX\"\"\EM List of native scripts.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\NUL\EOT\DC2\ETX6\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\NUL\ACK\DC2\ETX6\v\ETB\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\NUL\SOH\DC2\ETX6\CAN\GS\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\NUL\ETX\DC2\ETX6 !\n\
    \6\n\
    \\STX\EOT\a\DC2\EOT:\NUL=\SOH\SUB* Represents a \"k out of n\" native script.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\a\SOH\DC2\ETX:\b\DC2\n\
    \8\n\
    \\EOT\EOT\a\STX\NUL\DC2\ETX;\STX\SI\"+ The number of required satisfied scripts.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\a\STX\NUL\ENQ\DC2\ETX;\STX\b\n\
    \\f\n\
    \\ENQ\EOT\a\STX\NUL\SOH\DC2\ETX;\t\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\a\STX\NUL\ETX\DC2\ETX;\r\SO\n\
    \&\n\
    \\EOT\EOT\a\STX\SOH\DC2\ETX<\STX$\"\EM List of native scripts.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\a\STX\SOH\EOT\DC2\ETX<\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\a\STX\SOH\ACK\DC2\ETX<\v\ETB\n\
    \\f\n\
    \\ENQ\EOT\a\STX\SOH\SOH\DC2\ETX<\CAN\US\n\
    \\f\n\
    \\ENQ\EOT\a\STX\SOH\ETX\DC2\ETX<\"#\n\
    \B\n\
    \\STX\EOT\b\DC2\EOT@\NULD\SOH\SUB6 Represents a constructor for Plutus data in Cardano.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\b\SOH\DC2\ETX@\b\SO\n\
    \\v\n\
    \\EOT\EOT\b\STX\NUL\DC2\ETXA\STX\DC1\n\
    \\f\n\
    \\ENQ\EOT\b\STX\NUL\ENQ\DC2\ETXA\STX\b\n\
    \\f\n\
    \\ENQ\EOT\b\STX\NUL\SOH\DC2\ETXA\t\f\n\
    \\f\n\
    \\ENQ\EOT\b\STX\NUL\ETX\DC2\ETXA\SI\DLE\n\
    \\v\n\
    \\EOT\EOT\b\STX\SOH\DC2\ETXB\STX\GS\n\
    \\f\n\
    \\ENQ\EOT\b\STX\SOH\ENQ\DC2\ETXB\STX\b\n\
    \\f\n\
    \\ENQ\EOT\b\STX\SOH\SOH\DC2\ETXB\t\CAN\n\
    \\f\n\
    \\ENQ\EOT\b\STX\SOH\ETX\DC2\ETXB\ESC\FS\n\
    \\v\n\
    \\EOT\EOT\b\STX\STX\DC2\ETXC\STX!\n\
    \\f\n\
    \\ENQ\EOT\b\STX\STX\EOT\DC2\ETXC\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\b\STX\STX\ACK\DC2\ETXC\v\NAK\n\
    \\f\n\
    \\ENQ\EOT\b\STX\STX\SOH\DC2\ETXC\SYN\FS\n\
    \\f\n\
    \\ENQ\EOT\b\STX\STX\ETX\DC2\ETXC\US \n\
    \B\n\
    \\STX\EOT\t\DC2\EOTG\NULM\SOH\SUB6 Represents a big integer for Plutus data in Cardano.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\t\SOH\DC2\ETXG\b\SO\n\
    \\f\n\
    \\EOT\EOT\t\b\NUL\DC2\EOTH\STXL\ETX\n\
    \\f\n\
    \\ENQ\EOT\t\b\NUL\SOH\DC2\ETXH\b\SI\n\
    \\v\n\
    \\EOT\EOT\t\STX\NUL\DC2\ETXI\EOT'\n\
    \\f\n\
    \\ENQ\EOT\t\STX\NUL\ENQ\DC2\ETXI\EOT\t\n\
    \\f\n\
    \\ENQ\EOT\t\STX\NUL\SOH\DC2\ETXI\n\
    \\r\n\
    \\f\n\
    \\ENQ\EOT\t\STX\NUL\ETX\DC2\ETXI\DLE\DC1\n\
    \\f\n\
    \\ENQ\EOT\t\STX\NUL\b\DC2\ETXI\DC2&\n\
    \\r\n\
    \\ACK\EOT\t\STX\NUL\b\ACK\DC2\ETXI\DC3%\n\
    \\v\n\
    \\EOT\EOT\t\STX\SOH\DC2\ETXJ\EOT\CAN\n\
    \\f\n\
    \\ENQ\EOT\t\STX\SOH\ENQ\DC2\ETXJ\EOT\t\n\
    \\f\n\
    \\ENQ\EOT\t\STX\SOH\SOH\DC2\ETXJ\n\
    \\DC3\n\
    \\f\n\
    \\ENQ\EOT\t\STX\SOH\ETX\DC2\ETXJ\SYN\ETB\n\
    \\v\n\
    \\EOT\EOT\t\STX\STX\DC2\ETXK\EOT\CAN\n\
    \\f\n\
    \\ENQ\EOT\t\STX\STX\ENQ\DC2\ETXK\EOT\t\n\
    \\f\n\
    \\ENQ\EOT\t\STX\STX\SOH\DC2\ETXK\n\
    \\DC3\n\
    \\f\n\
    \\ENQ\EOT\t\STX\STX\ETX\DC2\ETXK\SYN\ETB\n\
    \E\n\
    \\STX\EOT\n\
    \\DC2\EOTQ\NULT\SOH\SUB9 Represents a key-value pair for Plutus data in Cardano.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\n\
    \\SOH\DC2\ETXQ\b\SYN\n\
    \\US\n\
    \\EOT\EOT\n\
    \\STX\NUL\DC2\ETXR\STX\NAK\"\DC2 Key of the pair.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\NUL\ACK\DC2\ETXR\STX\f\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\NUL\SOH\DC2\ETXR\r\DLE\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\NUL\ETX\DC2\ETXR\DC3\DC4\n\
    \!\n\
    \\EOT\EOT\n\
    \\STX\SOH\DC2\ETXS\STX\ETB\"\DC4 Value of the pair.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\SOH\ACK\DC2\ETXS\STX\f\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\SOH\SOH\DC2\ETXS\r\DC2\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\SOH\ETX\DC2\ETXS\NAK\SYN\n\
    \7\n\
    \\STX\EOT\v\DC2\EOTW\NUL_\SOH\SUB+ Represents a Plutus data item in Cardano.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\v\SOH\DC2\ETXW\b\DC2\n\
    \\f\n\
    \\EOT\EOT\v\b\NUL\DC2\EOTX\STX^\ETX\n\
    \\f\n\
    \\ENQ\EOT\v\b\NUL\SOH\DC2\ETXX\b\DC3\n\
    \\ESC\n\
    \\EOT\EOT\v\STX\NUL\DC2\ETXY\EOT\SYN\"\SO Constructor.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\v\STX\NUL\ACK\DC2\ETXY\EOT\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\v\STX\NUL\SOH\DC2\ETXY\v\DC1\n\
    \\f\n\
    \\ENQ\EOT\v\STX\NUL\ETX\DC2\ETXY\DC4\NAK\n\
    \\"\n\
    \\EOT\EOT\v\STX\SOH\DC2\ETXZ\EOT\SUB\"\NAK Map of Plutus data.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\v\STX\SOH\ACK\DC2\ETXZ\EOT\DC1\n\
    \\f\n\
    \\ENQ\EOT\v\STX\SOH\SOH\DC2\ETXZ\DC2\NAK\n\
    \\f\n\
    \\ENQ\EOT\v\STX\SOH\ETX\DC2\ETXZ\CAN\EM\n\
    \\ESC\n\
    \\EOT\EOT\v\STX\STX\DC2\ETX[\EOT\ETB\"\SO Big integer.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\v\STX\STX\ACK\DC2\ETX[\EOT\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\v\STX\STX\SOH\DC2\ETX[\v\DC2\n\
    \\f\n\
    \\ENQ\EOT\v\STX\STX\ETX\DC2\ETX[\NAK\SYN\n\
    \\GS\n\
    \\EOT\EOT\v\STX\ETX\DC2\ETX\\\EOT\FS\"\DLE Bounded bytes.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\v\STX\ETX\ENQ\DC2\ETX\\\EOT\t\n\
    \\f\n\
    \\ENQ\EOT\v\STX\ETX\SOH\DC2\ETX\\\n\
    \\ETB\n\
    \\f\n\
    \\ENQ\EOT\v\STX\ETX\ETX\DC2\ETX\\\SUB\ESC\n\
    \$\n\
    \\EOT\EOT\v\STX\EOT\DC2\ETX]\EOT\RS\"\ETB Array of Plutus data.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\v\STX\EOT\ACK\DC2\ETX]\EOT\DC3\n\
    \\f\n\
    \\ENQ\EOT\v\STX\EOT\SOH\DC2\ETX]\DC4\EM\n\
    \\f\n\
    \\ENQ\EOT\v\STX\EOT\ETX\DC2\ETX]\FS\GS\n\
    \9\n\
    \\STX\EOT\f\DC2\EOTb\NULd\SOH\SUB- Represents a map of Plutus data in Cardano.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\f\SOH\DC2\ETXb\b\NAK\n\
    \'\n\
    \\EOT\EOT\f\STX\NUL\DC2\ETXc\STX$\"\SUB List of key-value pairs.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\f\STX\NUL\EOT\DC2\ETXc\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\f\STX\NUL\ACK\DC2\ETXc\v\EM\n\
    \\f\n\
    \\ENQ\EOT\f\STX\NUL\SOH\DC2\ETXc\SUB\US\n\
    \\f\n\
    \\ENQ\EOT\f\STX\NUL\ETX\DC2\ETXc\"#\n\
    \<\n\
    \\STX\EOT\r\DC2\EOTg\NULi\SOH\SUB0 Represents an array of Plutus data in Cardano.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\r\SOH\DC2\ETXg\b\ETB\n\
    \)\n\
    \\EOT\EOT\r\STX\NUL\DC2\ETXh\STX \"\FS List of Plutus data items.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\r\STX\NUL\EOT\DC2\ETXh\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\r\STX\NUL\ACK\DC2\ETXh\v\NAK\n\
    \\f\n\
    \\ENQ\EOT\r\STX\NUL\SOH\DC2\ETXh\SYN\ESC\n\
    \\f\n\
    \\ENQ\EOT\r\STX\NUL\ETX\DC2\ETXh\RS\US\n\
    \n\n\
    \\STX\EOT\SO\DC2\EOTn\NULv\SOH\SUBb Represents a script in Cardano.\n\
    \ TODO u5c: removed native script representation, added plutus_v4\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\SO\SOH\DC2\ETXn\b\SO\n\
    \\f\n\
    \\EOT\EOT\SO\b\NUL\DC2\EOTo\STXu\ETX\n\
    \\f\n\
    \\ENQ\EOT\SO\b\NUL\SOH\DC2\ETXo\b\SO\n\
    \\GS\n\
    \\EOT\EOT\SO\STX\NUL\DC2\ETXp\EOT\FS\"\DLE Native script.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\NUL\ACK\DC2\ETXp\EOT\DLE\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\NUL\SOH\DC2\ETXp\DC1\ETB\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\NUL\ETX\DC2\ETXp\SUB\ESC\n\
    \ \n\
    \\EOT\EOT\SO\STX\SOH\DC2\ETXq\EOT\CAN\"\DC3 Plutus V1 script.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\SOH\ENQ\DC2\ETXq\EOT\t\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\SOH\SOH\DC2\ETXq\n\
    \\DC3\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\SOH\ETX\DC2\ETXq\SYN\ETB\n\
    \ \n\
    \\EOT\EOT\SO\STX\STX\DC2\ETXr\EOT\CAN\"\DC3 Plutus V2 script.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\STX\ENQ\DC2\ETXr\EOT\t\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\STX\SOH\DC2\ETXr\n\
    \\DC3\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\STX\ETX\DC2\ETXr\SYN\ETB\n\
    \ \n\
    \\EOT\EOT\SO\STX\ETX\DC2\ETXs\EOT\CAN\"\DC3 Plutus V3 script.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\ETX\ENQ\DC2\ETXs\EOT\t\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\ETX\SOH\DC2\ETXs\n\
    \\DC3\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\ETX\ETX\DC2\ETXs\SYN\ETB\n\
    \ \n\
    \\EOT\EOT\SO\STX\EOT\DC2\ETXt\EOT\CAN\"\DC3 Plutus V4 script.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\EOT\ENQ\DC2\ETXt\EOT\t\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\EOT\SOH\DC2\ETXt\n\
    \\DC3\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\EOT\ETX\DC2\ETXt\SYN\ETB\n\
    \b\n\
    \\STX\EOT\SI\DC2\EOTz\NUL}\SOH\SUBV Represents a rational number as a fraction.\n\
    \ TODO u5c increased precision to 64 bits\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\SI\SOH\DC2\ETXz\b\SYN\n\
    \\v\n\
    \\EOT\EOT\SI\STX\NUL\DC2\ETX{\STX+\n\
    \\f\n\
    \\ENQ\EOT\SI\STX\NUL\ENQ\DC2\ETX{\STX\a\n\
    \\f\n\
    \\ENQ\EOT\SI\STX\NUL\SOH\DC2\ETX{\b\DC1\n\
    \\f\n\
    \\ENQ\EOT\SI\STX\NUL\ETX\DC2\ETX{\DC4\NAK\n\
    \\f\n\
    \\ENQ\EOT\SI\STX\NUL\b\DC2\ETX{\SYN*\n\
    \\r\n\
    \\ACK\EOT\SI\STX\NUL\b\ACK\DC2\ETX{\ETB)\n\
    \\v\n\
    \\EOT\EOT\SI\STX\SOH\DC2\ETX|\STX.\n\
    \\f\n\
    \\ENQ\EOT\SI\STX\SOH\ENQ\DC2\ETX|\STX\b\n\
    \\f\n\
    \\ENQ\EOT\SI\STX\SOH\SOH\DC2\ETX|\t\DC4\n\
    \\f\n\
    \\ENQ\EOT\SI\STX\SOH\ETX\DC2\ETX|\ETB\CAN\n\
    \\f\n\
    \\ENQ\EOT\SI\STX\SOH\b\DC2\ETX|\EM-\n\
    \\r\n\
    \\ACK\EOT\SI\STX\SOH\b\ACK\DC2\ETX|\SUB,\n\
    \\RS\n\
    \\STX\EOT\DLE\DC2\ACK\130\SOH\NUL\133\SOH\SOH2\DLE PARAMS\n\
    \ ======\n\
    \\n\
    \\v\n\
    \\ETX\EOT\DLE\SOH\DC2\EOT\130\SOH\b\SI\n\
    \\f\n\
    \\EOT\EOT\DLE\STX\NUL\DC2\EOT\131\SOH\STX\DC3\n\
    \\r\n\
    \\ENQ\EOT\DLE\STX\NUL\ENQ\DC2\EOT\131\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\DLE\STX\NUL\SOH\DC2\EOT\131\SOH\t\SO\n\
    \\r\n\
    \\ENQ\EOT\DLE\STX\NUL\ETX\DC2\EOT\131\SOH\DC1\DC2\n\
    \\f\n\
    \\EOT\EOT\DLE\STX\SOH\DC2\EOT\132\SOH\STX\DC4\n\
    \\r\n\
    \\ENQ\EOT\DLE\STX\SOH\ENQ\DC2\EOT\132\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\DLE\STX\SOH\SOH\DC2\EOT\132\SOH\t\SI\n\
    \\r\n\
    \\ENQ\EOT\DLE\STX\SOH\ETX\DC2\EOT\132\SOH\DC2\DC3\n\
    \\f\n\
    \\STX\EOT\DC1\DC2\ACK\135\SOH\NUL\138\SOH\SOH\n\
    \\v\n\
    \\ETX\EOT\DC1\SOH\DC2\EOT\135\SOH\b\DLE\n\
    \\f\n\
    \\EOT\EOT\DC1\STX\NUL\DC2\EOT\136\SOH\STX\ESC\n\
    \\r\n\
    \\ENQ\EOT\DC1\STX\NUL\ACK\DC2\EOT\136\SOH\STX\DLE\n\
    \\r\n\
    \\ENQ\EOT\DC1\STX\NUL\SOH\DC2\EOT\136\SOH\DC1\SYN\n\
    \\r\n\
    \\ENQ\EOT\DC1\STX\NUL\ETX\DC2\EOT\136\SOH\EM\SUB\n\
    \\f\n\
    \\EOT\EOT\DC1\STX\SOH\DC2\EOT\137\SOH\STX\FS\n\
    \\r\n\
    \\ENQ\EOT\DC1\STX\SOH\ACK\DC2\EOT\137\SOH\STX\DLE\n\
    \\r\n\
    \\ENQ\EOT\DC1\STX\SOH\SOH\DC2\EOT\137\SOH\DC1\ETB\n\
    \\r\n\
    \\ENQ\EOT\DC1\STX\SOH\ETX\DC2\EOT\137\SOH\SUB\ESC\n\
    \\f\n\
    \\STX\EOT\DC2\DC2\ACK\140\SOH\NUL\143\SOH\SOH\n\
    \\v\n\
    \\ETX\EOT\DC2\SOH\DC2\EOT\140\SOH\b\ETB\n\
    \\f\n\
    \\EOT\EOT\DC2\STX\NUL\DC2\EOT\141\SOH\STX\DC3\n\
    \\r\n\
    \\ENQ\EOT\DC2\STX\NUL\ENQ\DC2\EOT\141\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\DC2\STX\NUL\SOH\DC2\EOT\141\SOH\t\SO\n\
    \\r\n\
    \\ENQ\EOT\DC2\STX\NUL\ETX\DC2\EOT\141\SOH\DC1\DC2\n\
    \\f\n\
    \\EOT\EOT\DC2\STX\SOH\DC2\EOT\142\SOH\STX\DC3\n\
    \\r\n\
    \\ENQ\EOT\DC2\STX\SOH\ENQ\DC2\EOT\142\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\DC2\STX\SOH\SOH\DC2\EOT\142\SOH\t\SO\n\
    \\r\n\
    \\ENQ\EOT\DC2\STX\SOH\ETX\DC2\EOT\142\SOH\DC1\DC2\n\
    \\f\n\
    \\STX\EOT\DC3\DC2\ACK\145\SOH\NUL\147\SOH\SOH\n\
    \\v\n\
    \\ETX\EOT\DC3\SOH\DC2\EOT\145\SOH\b\DC1\n\
    \\f\n\
    \\EOT\EOT\DC3\STX\NUL\DC2\EOT\146\SOH\STX\FS\n\
    \\r\n\
    \\ENQ\EOT\DC3\STX\NUL\EOT\DC2\EOT\146\SOH\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\DC3\STX\NUL\ENQ\DC2\EOT\146\SOH\v\DLE\n\
    \\r\n\
    \\ENQ\EOT\DC3\STX\NUL\SOH\DC2\EOT\146\SOH\DC1\ETB\n\
    \\r\n\
    \\ENQ\EOT\DC3\STX\NUL\ETX\DC2\EOT\146\SOH\SUB\ESC\n\
    \\f\n\
    \\STX\EOT\DC4\DC2\ACK\149\SOH\NUL\154\SOH\SOH\n\
    \\v\n\
    \\ETX\EOT\DC4\SOH\DC2\EOT\149\SOH\b\DC2\n\
    \\f\n\
    \\EOT\EOT\DC4\STX\NUL\DC2\EOT\150\SOH\STX\SUB\n\
    \\r\n\
    \\ENQ\EOT\DC4\STX\NUL\ACK\DC2\EOT\150\SOH\STX\v\n\
    \\r\n\
    \\ENQ\EOT\DC4\STX\NUL\SOH\DC2\EOT\150\SOH\f\NAK\n\
    \\r\n\
    \\ENQ\EOT\DC4\STX\NUL\ETX\DC2\EOT\150\SOH\CAN\EM\n\
    \\f\n\
    \\EOT\EOT\DC4\STX\SOH\DC2\EOT\151\SOH\STX\SUB\n\
    \\r\n\
    \\ENQ\EOT\DC4\STX\SOH\ACK\DC2\EOT\151\SOH\STX\v\n\
    \\r\n\
    \\ENQ\EOT\DC4\STX\SOH\SOH\DC2\EOT\151\SOH\f\NAK\n\
    \\r\n\
    \\ENQ\EOT\DC4\STX\SOH\ETX\DC2\EOT\151\SOH\CAN\EM\n\
    \\f\n\
    \\EOT\EOT\DC4\STX\STX\DC2\EOT\152\SOH\STX\SUB\n\
    \\r\n\
    \\ENQ\EOT\DC4\STX\STX\ACK\DC2\EOT\152\SOH\STX\v\n\
    \\r\n\
    \\ENQ\EOT\DC4\STX\STX\SOH\DC2\EOT\152\SOH\f\NAK\n\
    \\r\n\
    \\ENQ\EOT\DC4\STX\STX\ETX\DC2\EOT\152\SOH\CAN\EM\n\
    \\f\n\
    \\EOT\EOT\DC4\STX\ETX\DC2\EOT\153\SOH\STX\SUB\n\
    \\r\n\
    \\ENQ\EOT\DC4\STX\ETX\ACK\DC2\EOT\153\SOH\STX\v\n\
    \\r\n\
    \\ENQ\EOT\DC4\STX\ETX\SOH\DC2\EOT\153\SOH\f\NAK\n\
    \\r\n\
    \\ENQ\EOT\DC4\STX\ETX\ETX\DC2\EOT\153\SOH\CAN\EM\n\
    \\f\n\
    \\STX\EOT\NAK\DC2\ACK\156\SOH\NUL\158\SOH\SOH\n\
    \\v\n\
    \\ETX\EOT\NAK\SOH\DC2\EOT\156\SOH\b\CAN\n\
    \\f\n\
    \\EOT\EOT\NAK\STX\NUL\DC2\EOT\157\SOH\STX)\n\
    \\r\n\
    \\ENQ\EOT\NAK\STX\NUL\EOT\DC2\EOT\157\SOH\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\NAK\STX\NUL\ACK\DC2\EOT\157\SOH\v\EM\n\
    \\r\n\
    \\ENQ\EOT\NAK\STX\NUL\SOH\DC2\EOT\157\SOH\SUB$\n\
    \\r\n\
    \\ENQ\EOT\NAK\STX\NUL\ETX\DC2\EOT\157\SOH'(\n\
    \\f\n\
    \\STX\EOT\SYN\DC2\ACK\160\SOH\NUL\192\SOH\SOH\n\
    \\v\n\
    \\ETX\EOT\SYN\SOH\DC2\EOT\160\SOH\b\SI\n\
    \2\n\
    \\EOT\EOT\SYN\STX\NUL\DC2\EOT\161\SOH\STX6\"$ The number of coins per UTXO byte.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\NUL\ENQ\DC2\EOT\161\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\NUL\SOH\DC2\EOT\161\SOH\t\FS\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\NUL\ETX\DC2\EOT\161\SOH\US \n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\NUL\b\DC2\EOT\161\SOH!5\n\
    \\SO\n\
    \\ACK\EOT\SYN\STX\NUL\b\ACK\DC2\EOT\161\SOH\"4\n\
    \-\n\
    \\EOT\EOT\SYN\STX\SOH\DC2\EOT\162\SOH\STX.\"\US The maximum transaction size.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\SOH\ENQ\DC2\EOT\162\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\SOH\SOH\DC2\EOT\162\SOH\t\DC4\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\SOH\ETX\DC2\EOT\162\SOH\ETB\CAN\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\SOH\b\DC2\EOT\162\SOH\EM-\n\
    \\SO\n\
    \\ACK\EOT\SYN\STX\SOH\b\ACK\DC2\EOT\162\SOH\SUB,\n\
    \,\n\
    \\EOT\EOT\SYN\STX\STX\DC2\EOT\163\SOH\STX6\"\RS The minimum fee coefficient.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\STX\ENQ\DC2\EOT\163\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\STX\SOH\DC2\EOT\163\SOH\t\FS\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\STX\ETX\DC2\EOT\163\SOH\US \n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\STX\b\DC2\EOT\163\SOH!5\n\
    \\SO\n\
    \\ACK\EOT\SYN\STX\STX\b\ACK\DC2\EOT\163\SOH\"4\n\
    \)\n\
    \\EOT\EOT\SYN\STX\ETX\DC2\EOT\164\SOH\STX3\"\ESC The minimum fee constant.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\ETX\ENQ\DC2\EOT\164\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\ETX\SOH\DC2\EOT\164\SOH\t\EM\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\ETX\ETX\DC2\EOT\164\SOH\FS\GS\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\ETX\b\DC2\EOT\164\SOH\RS2\n\
    \\SO\n\
    \\ACK\EOT\SYN\STX\ETX\b\ACK\DC2\EOT\164\SOH\US1\n\
    \,\n\
    \\EOT\EOT\SYN\STX\EOT\DC2\EOT\165\SOH\STX6\"\RS The maximum block body size.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\EOT\ENQ\DC2\EOT\165\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\EOT\SOH\DC2\EOT\165\SOH\t\FS\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\EOT\ETX\DC2\EOT\165\SOH\US \n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\EOT\b\DC2\EOT\165\SOH!5\n\
    \\SO\n\
    \\ACK\EOT\SYN\STX\EOT\b\ACK\DC2\EOT\165\SOH\"4\n\
    \.\n\
    \\EOT\EOT\SYN\STX\ENQ\DC2\EOT\166\SOH\STX8\"  The maximum block header size.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\ENQ\ENQ\DC2\EOT\166\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\ENQ\SOH\DC2\EOT\166\SOH\t\RS\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\ENQ\ETX\DC2\EOT\166\SOH!\"\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\ENQ\b\DC2\EOT\166\SOH#7\n\
    \\SO\n\
    \\ACK\EOT\SYN\STX\ENQ\b\ACK\DC2\EOT\166\SOH$6\n\
    \&\n\
    \\EOT\EOT\SYN\STX\ACK\DC2\EOT\167\SOH\STX4\"\CAN The stake key deposit.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\ACK\ENQ\DC2\EOT\167\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\ACK\SOH\DC2\EOT\167\SOH\t\SUB\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\ACK\ETX\DC2\EOT\167\SOH\GS\RS\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\ACK\b\DC2\EOT\167\SOH\US3\n\
    \\SO\n\
    \\ACK\EOT\SYN\STX\ACK\b\ACK\DC2\EOT\167\SOH 2\n\
    \!\n\
    \\EOT\EOT\SYN\STX\a\DC2\EOT\168\SOH\STX/\"\DC3 The pool deposit.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\a\ENQ\DC2\EOT\168\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\a\SOH\DC2\EOT\168\SOH\t\NAK\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\a\ETX\DC2\EOT\168\SOH\CAN\EM\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\a\b\DC2\EOT\168\SOH\SUB.\n\
    \\SO\n\
    \\ACK\EOT\SYN\STX\a\b\ACK\DC2\EOT\168\SOH\ESC-\n\
    \0\n\
    \\EOT\EOT\SYN\STX\b\DC2\EOT\169\SOH\STX)\"\" The pool retirement epoch bound.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\b\ENQ\DC2\EOT\169\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\b\SOH\DC2\EOT\169\SOH\t$\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\b\ETX\DC2\EOT\169\SOH'(\n\
    \,\n\
    \\EOT\EOT\SYN\STX\t\DC2\EOT\170\SOH\STX&\"\RS The desired number of pools.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\t\ENQ\DC2\EOT\170\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\t\SOH\DC2\EOT\170\SOH\t \n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\t\ETX\DC2\EOT\170\SOH#%\n\
    \#\n\
    \\EOT\EOT\SYN\STX\n\
    \\DC2\EOT\171\SOH\STX%\"\NAK The pool influence.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\n\
    \\ACK\DC2\EOT\171\SOH\STX\DLE\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\n\
    \\SOH\DC2\EOT\171\SOH\DC1\US\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\n\
    \\ETX\DC2\EOT\171\SOH\"$\n\
    \'\n\
    \\EOT\EOT\SYN\STX\v\DC2\EOT\172\SOH\STX)\"\EM The monetary expansion.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\v\ACK\DC2\EOT\172\SOH\STX\DLE\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\v\SOH\DC2\EOT\172\SOH\DC1#\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\v\ETX\DC2\EOT\172\SOH&(\n\
    \'\n\
    \\EOT\EOT\SYN\STX\f\DC2\EOT\173\SOH\STX)\"\EM The treasury expansion.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\f\ACK\DC2\EOT\173\SOH\STX\DLE\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\f\SOH\DC2\EOT\173\SOH\DC1#\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\f\ETX\DC2\EOT\173\SOH&(\n\
    \&\n\
    \\EOT\EOT\SYN\STX\r\DC2\EOT\174\SOH\STX1\"\CAN The minimum pool cost.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\r\ENQ\DC2\EOT\174\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\r\SOH\DC2\EOT\174\SOH\t\SYN\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\r\ETX\DC2\EOT\174\SOH\EM\ESC\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\r\b\DC2\EOT\174\SOH\FS0\n\
    \\SO\n\
    \\ACK\EOT\SYN\STX\r\b\ACK\DC2\EOT\174\SOH\GS/\n\
    \%\n\
    \\EOT\EOT\SYN\STX\SO\DC2\EOT\175\SOH\STX(\"\ETB The protocol version.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\SO\ACK\DC2\EOT\175\SOH\STX\DC1\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\SO\SOH\DC2\EOT\175\SOH\DC2\"\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\SO\ETX\DC2\EOT\175\SOH%'\n\
    \'\n\
    \\EOT\EOT\SYN\STX\SI\DC2\EOT\176\SOH\STX2\"\EM The maximum value size.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\SI\ENQ\DC2\EOT\176\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\SI\SOH\DC2\EOT\176\SOH\t\ETB\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\SI\ETX\DC2\EOT\176\SOH\SUB\FS\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\SI\b\DC2\EOT\176\SOH\GS1\n\
    \\SO\n\
    \\ACK\EOT\SYN\STX\SI\b\ACK\DC2\EOT\176\SOH\RS0\n\
    \*\n\
    \\EOT\EOT\SYN\STX\DLE\DC2\EOT\177\SOH\STX9\"\FS The collateral percentage.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\DLE\ENQ\DC2\EOT\177\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\DLE\SOH\DC2\EOT\177\SOH\t\RS\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\DLE\ETX\DC2\EOT\177\SOH!#\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\DLE\b\DC2\EOT\177\SOH$8\n\
    \\SO\n\
    \\ACK\EOT\SYN\STX\DLE\b\ACK\DC2\EOT\177\SOH%7\n\
    \.\n\
    \\EOT\EOT\SYN\STX\DC1\DC2\EOT\178\SOH\STX9\"  The maximum collateral inputs.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\DC1\ENQ\DC2\EOT\178\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\DC1\SOH\DC2\EOT\178\SOH\t\RS\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\DC1\ETX\DC2\EOT\178\SOH!#\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\DC1\b\DC2\EOT\178\SOH$8\n\
    \\SO\n\
    \\ACK\EOT\SYN\STX\DC1\b\ACK\DC2\EOT\178\SOH%7\n\
    \ \n\
    \\EOT\EOT\SYN\STX\DC2\DC2\EOT\179\SOH\STX\RS\"\DC2 The cost models.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\DC2\ACK\DC2\EOT\179\SOH\STX\f\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\DC2\SOH\DC2\EOT\179\SOH\r\CAN\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\DC2\ETX\DC2\EOT\179\SOH\ESC\GS\n\
    \\ESC\n\
    \\EOT\EOT\SYN\STX\DC3\DC2\EOT\180\SOH\STX\ETB\"\r The prices.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\DC3\ACK\DC2\EOT\180\SOH\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\DC3\SOH\DC2\EOT\180\SOH\v\DC1\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\DC3\ETX\DC2\EOT\180\SOH\DC4\SYN\n\
    \<\n\
    \\EOT\EOT\SYN\STX\DC4\DC2\EOT\181\SOH\STX3\". The maximum execution units per transaction.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\DC4\ACK\DC2\EOT\181\SOH\STX\t\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\DC4\SOH\DC2\EOT\181\SOH\n\
    \-\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\DC4\ETX\DC2\EOT\181\SOH02\n\
    \6\n\
    \\EOT\EOT\SYN\STX\NAK\DC2\EOT\182\SOH\STX-\"( The maximum execution units per block.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\NAK\ACK\DC2\EOT\182\SOH\STX\t\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\NAK\SOH\DC2\EOT\182\SOH\n\
    \'\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\NAK\ETX\DC2\EOT\182\SOH*,\n\
    \:\n\
    \\EOT\EOT\SYN\STX\SYN\DC2\EOT\183\SOH\STX7\", The minimum fee per script reference byte.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\SYN\ACK\DC2\EOT\183\SOH\STX\DLE\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\SYN\SOH\DC2\EOT\183\SOH\DC11\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\SYN\ETX\DC2\EOT\183\SOH46\n\
    \+\n\
    \\EOT\EOT\SYN\STX\ETB\DC2\EOT\184\SOH\STX/\"\GS The pool voting thresholds.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\ETB\ACK\DC2\EOT\184\SOH\STX\DC2\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\ETB\SOH\DC2\EOT\184\SOH\DC3)\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\ETB\ETX\DC2\EOT\184\SOH,.\n\
    \+\n\
    \\EOT\EOT\SYN\STX\CAN\DC2\EOT\185\SOH\STX/\"\GS The drep voting thresholds.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\CAN\ACK\DC2\EOT\185\SOH\STX\DC2\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\CAN\SOH\DC2\EOT\185\SOH\DC3)\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\CAN\ETX\DC2\EOT\185\SOH,.\n\
    \+\n\
    \\EOT\EOT\SYN\STX\EM\DC2\EOT\186\SOH\STX!\"\GS The minimum committee size.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\EM\ENQ\DC2\EOT\186\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\EM\SOH\DC2\EOT\186\SOH\t\ESC\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\EM\ETX\DC2\EOT\186\SOH\RS \n\
    \)\n\
    \\EOT\EOT\SYN\STX\SUB\DC2\EOT\187\SOH\STX#\"\ESC The committee term limit.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\SUB\ENQ\DC2\EOT\187\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\SUB\SOH\DC2\EOT\187\SOH\t\GS\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\SUB\ETX\DC2\EOT\187\SOH \"\n\
    \6\n\
    \\EOT\EOT\SYN\STX\ESC\DC2\EOT\188\SOH\STX0\"( The governance action validity period.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\ESC\ENQ\DC2\EOT\188\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\ESC\SOH\DC2\EOT\188\SOH\t*\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\ESC\ETX\DC2\EOT\188\SOH-/\n\
    \.\n\
    \\EOT\EOT\SYN\STX\FS\DC2\EOT\189\SOH\STX=\"  The governance action deposit.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\FS\ENQ\DC2\EOT\189\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\FS\SOH\DC2\EOT\189\SOH\t\"\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\FS\ETX\DC2\EOT\189\SOH%'\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\FS\b\DC2\EOT\189\SOH(<\n\
    \\SO\n\
    \\ACK\EOT\SYN\STX\FS\b\ACK\DC2\EOT\189\SOH);\n\
    \!\n\
    \\EOT\EOT\SYN\STX\GS\DC2\EOT\190\SOH\STX0\"\DC3 The drep deposit.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\GS\ENQ\DC2\EOT\190\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\GS\SOH\DC2\EOT\190\SOH\t\NAK\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\GS\ETX\DC2\EOT\190\SOH\CAN\SUB\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\GS\b\DC2\EOT\190\SOH\ESC/\n\
    \\SO\n\
    \\ACK\EOT\SYN\STX\GS\b\ACK\DC2\EOT\190\SOH\FS.\n\
    \+\n\
    \\EOT\EOT\SYN\STX\RS\DC2\EOT\191\SOH\STX%\"\GS The drep inactivity period.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\RS\ENQ\DC2\EOT\191\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\RS\SOH\DC2\EOT\191\SOH\t\US\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\RS\ETX\DC2\EOT\191\SOH\"$b\ACKproto3"