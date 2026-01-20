{- This file was auto-generated from utxorpc/v1beta/query/query.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Utxorpc.V1beta.Query.Query_Fields where
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
allOf ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "allOf" a) =>
  Lens.Family2.LensLike' f s a
allOf = Data.ProtoLens.Field.field @"allOf"
anyOf ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "anyOf" a) =>
  Lens.Family2.LensLike' f s a
anyOf = Data.ProtoLens.Field.field @"anyOf"
blockRef ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "blockRef" a) =>
  Lens.Family2.LensLike' f s a
blockRef = Data.ProtoLens.Field.field @"blockRef"
caip2 ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "caip2" a) =>
  Lens.Family2.LensLike' f s a
caip2 = Data.ProtoLens.Field.field @"caip2"
cardano ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "cardano" a) =>
  Lens.Family2.LensLike' f s a
cardano = Data.ProtoLens.Field.field @"cardano"
fieldMask ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "fieldMask" a) =>
  Lens.Family2.LensLike' f s a
fieldMask = Data.ProtoLens.Field.field @"fieldMask"
genesis ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "genesis" a) =>
  Lens.Family2.LensLike' f s a
genesis = Data.ProtoLens.Field.field @"genesis"
hash ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "hash" a) =>
  Lens.Family2.LensLike' f s a
hash = Data.ProtoLens.Field.field @"hash"
height ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "height" a) =>
  Lens.Family2.LensLike' f s a
height = Data.ProtoLens.Field.field @"height"
index ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "index" a) =>
  Lens.Family2.LensLike' f s a
index = Data.ProtoLens.Field.field @"index"
items ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "items" a) =>
  Lens.Family2.LensLike' f s a
items = Data.ProtoLens.Field.field @"items"
key ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "key" a) =>
  Lens.Family2.LensLike' f s a
key = Data.ProtoLens.Field.field @"key"
keys ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "keys" a) =>
  Lens.Family2.LensLike' f s a
keys = Data.ProtoLens.Field.field @"keys"
ledgerTip ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "ledgerTip" a) =>
  Lens.Family2.LensLike' f s a
ledgerTip = Data.ProtoLens.Field.field @"ledgerTip"
match ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "match" a) =>
  Lens.Family2.LensLike' f s a
match = Data.ProtoLens.Field.field @"match"
maxItems ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maxItems" a) =>
  Lens.Family2.LensLike' f s a
maxItems = Data.ProtoLens.Field.field @"maxItems"
maybe'blockRef ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'blockRef" a) =>
  Lens.Family2.LensLike' f s a
maybe'blockRef = Data.ProtoLens.Field.field @"maybe'blockRef"
maybe'cardano ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'cardano" a) =>
  Lens.Family2.LensLike' f s a
maybe'cardano = Data.ProtoLens.Field.field @"maybe'cardano"
maybe'chain ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'chain" a) =>
  Lens.Family2.LensLike' f s a
maybe'chain = Data.ProtoLens.Field.field @"maybe'chain"
maybe'config ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'config" a) =>
  Lens.Family2.LensLike' f s a
maybe'config = Data.ProtoLens.Field.field @"maybe'config"
maybe'fieldMask ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'fieldMask" a) =>
  Lens.Family2.LensLike' f s a
maybe'fieldMask = Data.ProtoLens.Field.field @"maybe'fieldMask"
maybe'ledgerTip ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'ledgerTip" a) =>
  Lens.Family2.LensLike' f s a
maybe'ledgerTip = Data.ProtoLens.Field.field @"maybe'ledgerTip"
maybe'match ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'match" a) =>
  Lens.Family2.LensLike' f s a
maybe'match = Data.ProtoLens.Field.field @"maybe'match"
maybe'params ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'params" a) =>
  Lens.Family2.LensLike' f s a
maybe'params = Data.ProtoLens.Field.field @"maybe'params"
maybe'parsedState ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'parsedState" a) =>
  Lens.Family2.LensLike' f s a
maybe'parsedState = Data.ProtoLens.Field.field @"maybe'parsedState"
maybe'predicate ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'predicate" a) =>
  Lens.Family2.LensLike' f s a
maybe'predicate = Data.ProtoLens.Field.field @"maybe'predicate"
maybe'summary ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'summary" a) =>
  Lens.Family2.LensLike' f s a
maybe'summary = Data.ProtoLens.Field.field @"maybe'summary"
maybe'tx ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'tx" a) =>
  Lens.Family2.LensLike' f s a
maybe'tx = Data.ProtoLens.Field.field @"maybe'tx"
maybe'txoRef ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'txoRef" a) =>
  Lens.Family2.LensLike' f s a
maybe'txoRef = Data.ProtoLens.Field.field @"maybe'txoRef"
maybe'utxoPattern ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'utxoPattern" a) =>
  Lens.Family2.LensLike' f s a
maybe'utxoPattern = Data.ProtoLens.Field.field @"maybe'utxoPattern"
maybe'values ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'values" a) =>
  Lens.Family2.LensLike' f s a
maybe'values = Data.ProtoLens.Field.field @"maybe'values"
nativeBytes ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "nativeBytes" a) =>
  Lens.Family2.LensLike' f s a
nativeBytes = Data.ProtoLens.Field.field @"nativeBytes"
nextToken ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "nextToken" a) =>
  Lens.Family2.LensLike' f s a
nextToken = Data.ProtoLens.Field.field @"nextToken"
not ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "not" a) =>
  Lens.Family2.LensLike' f s a
not = Data.ProtoLens.Field.field @"not"
predicate ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "predicate" a) =>
  Lens.Family2.LensLike' f s a
predicate = Data.ProtoLens.Field.field @"predicate"
slot ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "slot" a) =>
  Lens.Family2.LensLike' f s a
slot = Data.ProtoLens.Field.field @"slot"
startToken ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "startToken" a) =>
  Lens.Family2.LensLike' f s a
startToken = Data.ProtoLens.Field.field @"startToken"
timestamp ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "timestamp" a) =>
  Lens.Family2.LensLike' f s a
timestamp = Data.ProtoLens.Field.field @"timestamp"
tx ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "tx" a) =>
  Lens.Family2.LensLike' f s a
tx = Data.ProtoLens.Field.field @"tx"
txoRef ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "txoRef" a) =>
  Lens.Family2.LensLike' f s a
txoRef = Data.ProtoLens.Field.field @"txoRef"
values ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "values" a) =>
  Lens.Family2.LensLike' f s a
values = Data.ProtoLens.Field.field @"values"
vec'allOf ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'allOf" a) =>
  Lens.Family2.LensLike' f s a
vec'allOf = Data.ProtoLens.Field.field @"vec'allOf"
vec'anyOf ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'anyOf" a) =>
  Lens.Family2.LensLike' f s a
vec'anyOf = Data.ProtoLens.Field.field @"vec'anyOf"
vec'items ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'items" a) =>
  Lens.Family2.LensLike' f s a
vec'items = Data.ProtoLens.Field.field @"vec'items"
vec'keys ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'keys" a) =>
  Lens.Family2.LensLike' f s a
vec'keys = Data.ProtoLens.Field.field @"vec'keys"
vec'not ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "vec'not" a) =>
  Lens.Family2.LensLike' f s a
vec'not = Data.ProtoLens.Field.field @"vec'not"
vec'values ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'values" a) =>
  Lens.Family2.LensLike' f s a
vec'values = Data.ProtoLens.Field.field @"vec'values"