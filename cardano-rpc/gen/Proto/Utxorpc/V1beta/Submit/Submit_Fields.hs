{- This file was auto-generated from utxorpc/v1beta/submit/submit.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Utxorpc.V1beta.Submit.Submit_Fields where
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
items ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "items" a) =>
  Lens.Family2.LensLike' f s a
items = Data.ProtoLens.Field.field @"items"
match ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "match" a) =>
  Lens.Family2.LensLike' f s a
match = Data.ProtoLens.Field.field @"match"
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
maybe'match ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'match" a) =>
  Lens.Family2.LensLike' f s a
maybe'match = Data.ProtoLens.Field.field @"maybe'match"
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
maybe'raw ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'raw" a) =>
  Lens.Family2.LensLike' f s a
maybe'raw = Data.ProtoLens.Field.field @"maybe'raw"
maybe'report ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'report" a) =>
  Lens.Family2.LensLike' f s a
maybe'report = Data.ProtoLens.Field.field @"maybe'report"
maybe'tx ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'tx" a) =>
  Lens.Family2.LensLike' f s a
maybe'tx = Data.ProtoLens.Field.field @"maybe'tx"
maybe'type' ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'type'" a) =>
  Lens.Family2.LensLike' f s a
maybe'type' = Data.ProtoLens.Field.field @"maybe'type'"
nativeBytes ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "nativeBytes" a) =>
  Lens.Family2.LensLike' f s a
nativeBytes = Data.ProtoLens.Field.field @"nativeBytes"
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
raw ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "raw" a) =>
  Lens.Family2.LensLike' f s a
raw = Data.ProtoLens.Field.field @"raw"
ref ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "ref" a) =>
  Lens.Family2.LensLike' f s a
ref = Data.ProtoLens.Field.field @"ref"
report ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "report" a) =>
  Lens.Family2.LensLike' f s a
report = Data.ProtoLens.Field.field @"report"
stage ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "stage" a) =>
  Lens.Family2.LensLike' f s a
stage = Data.ProtoLens.Field.field @"stage"
tx ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "tx" a) =>
  Lens.Family2.LensLike' f s a
tx = Data.ProtoLens.Field.field @"tx"
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
vec'not ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "vec'not" a) =>
  Lens.Family2.LensLike' f s a
vec'not = Data.ProtoLens.Field.field @"vec'not"
vec'ref ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "vec'ref" a) =>
  Lens.Family2.LensLike' f s a
vec'ref = Data.ProtoLens.Field.field @"vec'ref"