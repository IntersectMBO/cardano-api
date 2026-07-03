{- This file was auto-generated from utxorpc/v1beta/sync/sync.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Utxorpc.V1beta.Sync.Sync_Fields where
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
apply ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "apply" a) =>
  Lens.Family2.LensLike' f s a
apply = Data.ProtoLens.Field.field @"apply"
block ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "block" a) =>
  Lens.Family2.LensLike' f s a
block = Data.ProtoLens.Field.field @"block"
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
intersect ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "intersect" a) =>
  Lens.Family2.LensLike' f s a
intersect = Data.ProtoLens.Field.field @"intersect"
maxItems ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maxItems" a) =>
  Lens.Family2.LensLike' f s a
maxItems = Data.ProtoLens.Field.field @"maxItems"
maybe'action ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'action" a) =>
  Lens.Family2.LensLike' f s a
maybe'action = Data.ProtoLens.Field.field @"maybe'action"
maybe'apply ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'apply" a) =>
  Lens.Family2.LensLike' f s a
maybe'apply = Data.ProtoLens.Field.field @"maybe'apply"
maybe'block ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'block" a) =>
  Lens.Family2.LensLike' f s a
maybe'block = Data.ProtoLens.Field.field @"maybe'block"
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
maybe'fieldMask ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'fieldMask" a) =>
  Lens.Family2.LensLike' f s a
maybe'fieldMask = Data.ProtoLens.Field.field @"maybe'fieldMask"
maybe'nextToken ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'nextToken" a) =>
  Lens.Family2.LensLike' f s a
maybe'nextToken = Data.ProtoLens.Field.field @"maybe'nextToken"
maybe'ref ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'ref" a) =>
  Lens.Family2.LensLike' f s a
maybe'ref = Data.ProtoLens.Field.field @"maybe'ref"
maybe'reset ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'reset" a) =>
  Lens.Family2.LensLike' f s a
maybe'reset = Data.ProtoLens.Field.field @"maybe'reset"
maybe'startToken ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'startToken" a) =>
  Lens.Family2.LensLike' f s a
maybe'startToken = Data.ProtoLens.Field.field @"maybe'startToken"
maybe'tip ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'tip" a) =>
  Lens.Family2.LensLike' f s a
maybe'tip = Data.ProtoLens.Field.field @"maybe'tip"
maybe'undo ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'undo" a) =>
  Lens.Family2.LensLike' f s a
maybe'undo = Data.ProtoLens.Field.field @"maybe'undo"
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
ref ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "ref" a) =>
  Lens.Family2.LensLike' f s a
ref = Data.ProtoLens.Field.field @"ref"
reset ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "reset" a) =>
  Lens.Family2.LensLike' f s a
reset = Data.ProtoLens.Field.field @"reset"
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
tip ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "tip" a) =>
  Lens.Family2.LensLike' f s a
tip = Data.ProtoLens.Field.field @"tip"
undo ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "undo" a) =>
  Lens.Family2.LensLike' f s a
undo = Data.ProtoLens.Field.field @"undo"
vec'block ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'block" a) =>
  Lens.Family2.LensLike' f s a
vec'block = Data.ProtoLens.Field.field @"vec'block"
vec'intersect ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'intersect" a) =>
  Lens.Family2.LensLike' f s a
vec'intersect = Data.ProtoLens.Field.field @"vec'intersect"