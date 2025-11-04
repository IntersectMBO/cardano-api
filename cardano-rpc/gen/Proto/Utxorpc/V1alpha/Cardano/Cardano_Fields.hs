{- This file was auto-generated from utxorpc/v1alpha/cardano/cardano.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields where
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
address ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "address" a) =>
  Lens.Family2.LensLike' f s a
address = Data.ProtoLens.Field.field @"address"
assets ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "assets" a) =>
  Lens.Family2.LensLike' f s a
assets = Data.ProtoLens.Field.field @"assets"
coin ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "coin" a) =>
  Lens.Family2.LensLike' f s a
coin = Data.ProtoLens.Field.field @"coin"
coinsPerUtxoByte ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "coinsPerUtxoByte" a) =>
  Lens.Family2.LensLike' f s a
coinsPerUtxoByte = Data.ProtoLens.Field.field @"coinsPerUtxoByte"
collateralPercentage ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "collateralPercentage" a) =>
  Lens.Family2.LensLike' f s a
collateralPercentage
  = Data.ProtoLens.Field.field @"collateralPercentage"
committeeTermLimit ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "committeeTermLimit" a) =>
  Lens.Family2.LensLike' f s a
committeeTermLimit
  = Data.ProtoLens.Field.field @"committeeTermLimit"
costModels ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "costModels" a) =>
  Lens.Family2.LensLike' f s a
costModels = Data.ProtoLens.Field.field @"costModels"
datum ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "datum" a) =>
  Lens.Family2.LensLike' f s a
datum = Data.ProtoLens.Field.field @"datum"
denominator ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "denominator" a) =>
  Lens.Family2.LensLike' f s a
denominator = Data.ProtoLens.Field.field @"denominator"
desiredNumberOfPools ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "desiredNumberOfPools" a) =>
  Lens.Family2.LensLike' f s a
desiredNumberOfPools
  = Data.ProtoLens.Field.field @"desiredNumberOfPools"
drepDeposit ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "drepDeposit" a) =>
  Lens.Family2.LensLike' f s a
drepDeposit = Data.ProtoLens.Field.field @"drepDeposit"
drepInactivityPeriod ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "drepInactivityPeriod" a) =>
  Lens.Family2.LensLike' f s a
drepInactivityPeriod
  = Data.ProtoLens.Field.field @"drepInactivityPeriod"
drepVotingThresholds ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "drepVotingThresholds" a) =>
  Lens.Family2.LensLike' f s a
drepVotingThresholds
  = Data.ProtoLens.Field.field @"drepVotingThresholds"
governanceActionDeposit ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "governanceActionDeposit" a) =>
  Lens.Family2.LensLike' f s a
governanceActionDeposit
  = Data.ProtoLens.Field.field @"governanceActionDeposit"
governanceActionValidityPeriod ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "governanceActionValidityPeriod" a) =>
  Lens.Family2.LensLike' f s a
governanceActionValidityPeriod
  = Data.ProtoLens.Field.field @"governanceActionValidityPeriod"
hash ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "hash" a) =>
  Lens.Family2.LensLike' f s a
hash = Data.ProtoLens.Field.field @"hash"
items ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "items" a) =>
  Lens.Family2.LensLike' f s a
items = Data.ProtoLens.Field.field @"items"
major ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "major" a) =>
  Lens.Family2.LensLike' f s a
major = Data.ProtoLens.Field.field @"major"
maxBlockBodySize ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maxBlockBodySize" a) =>
  Lens.Family2.LensLike' f s a
maxBlockBodySize = Data.ProtoLens.Field.field @"maxBlockBodySize"
maxBlockHeaderSize ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maxBlockHeaderSize" a) =>
  Lens.Family2.LensLike' f s a
maxBlockHeaderSize
  = Data.ProtoLens.Field.field @"maxBlockHeaderSize"
maxCollateralInputs ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maxCollateralInputs" a) =>
  Lens.Family2.LensLike' f s a
maxCollateralInputs
  = Data.ProtoLens.Field.field @"maxCollateralInputs"
maxExecutionUnitsPerBlock ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maxExecutionUnitsPerBlock" a) =>
  Lens.Family2.LensLike' f s a
maxExecutionUnitsPerBlock
  = Data.ProtoLens.Field.field @"maxExecutionUnitsPerBlock"
maxExecutionUnitsPerTransaction ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maxExecutionUnitsPerTransaction" a) =>
  Lens.Family2.LensLike' f s a
maxExecutionUnitsPerTransaction
  = Data.ProtoLens.Field.field @"maxExecutionUnitsPerTransaction"
maxTxSize ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maxTxSize" a) =>
  Lens.Family2.LensLike' f s a
maxTxSize = Data.ProtoLens.Field.field @"maxTxSize"
maxValueSize ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maxValueSize" a) =>
  Lens.Family2.LensLike' f s a
maxValueSize = Data.ProtoLens.Field.field @"maxValueSize"
maybe'costModels ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'costModels" a) =>
  Lens.Family2.LensLike' f s a
maybe'costModels = Data.ProtoLens.Field.field @"maybe'costModels"
maybe'datum ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'datum" a) =>
  Lens.Family2.LensLike' f s a
maybe'datum = Data.ProtoLens.Field.field @"maybe'datum"
maybe'drepVotingThresholds ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'drepVotingThresholds" a) =>
  Lens.Family2.LensLike' f s a
maybe'drepVotingThresholds
  = Data.ProtoLens.Field.field @"maybe'drepVotingThresholds"
maybe'maxExecutionUnitsPerBlock ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'maxExecutionUnitsPerBlock" a) =>
  Lens.Family2.LensLike' f s a
maybe'maxExecutionUnitsPerBlock
  = Data.ProtoLens.Field.field @"maybe'maxExecutionUnitsPerBlock"
maybe'maxExecutionUnitsPerTransaction ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'maxExecutionUnitsPerTransaction" a) =>
  Lens.Family2.LensLike' f s a
maybe'maxExecutionUnitsPerTransaction
  = Data.ProtoLens.Field.field
      @"maybe'maxExecutionUnitsPerTransaction"
maybe'memory ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'memory" a) =>
  Lens.Family2.LensLike' f s a
maybe'memory = Data.ProtoLens.Field.field @"maybe'memory"
maybe'minFeeScriptRefCostPerByte ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'minFeeScriptRefCostPerByte" a) =>
  Lens.Family2.LensLike' f s a
maybe'minFeeScriptRefCostPerByte
  = Data.ProtoLens.Field.field @"maybe'minFeeScriptRefCostPerByte"
maybe'mintCoin ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'mintCoin" a) =>
  Lens.Family2.LensLike' f s a
maybe'mintCoin = Data.ProtoLens.Field.field @"maybe'mintCoin"
maybe'monetaryExpansion ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'monetaryExpansion" a) =>
  Lens.Family2.LensLike' f s a
maybe'monetaryExpansion
  = Data.ProtoLens.Field.field @"maybe'monetaryExpansion"
maybe'native ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'native" a) =>
  Lens.Family2.LensLike' f s a
maybe'native = Data.ProtoLens.Field.field @"maybe'native"
maybe'outputCoin ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'outputCoin" a) =>
  Lens.Family2.LensLike' f s a
maybe'outputCoin = Data.ProtoLens.Field.field @"maybe'outputCoin"
maybe'plutusV1 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'plutusV1" a) =>
  Lens.Family2.LensLike' f s a
maybe'plutusV1 = Data.ProtoLens.Field.field @"maybe'plutusV1"
maybe'plutusV2 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'plutusV2" a) =>
  Lens.Family2.LensLike' f s a
maybe'plutusV2 = Data.ProtoLens.Field.field @"maybe'plutusV2"
maybe'plutusV3 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'plutusV3" a) =>
  Lens.Family2.LensLike' f s a
maybe'plutusV3 = Data.ProtoLens.Field.field @"maybe'plutusV3"
maybe'plutusV4 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'plutusV4" a) =>
  Lens.Family2.LensLike' f s a
maybe'plutusV4 = Data.ProtoLens.Field.field @"maybe'plutusV4"
maybe'poolInfluence ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'poolInfluence" a) =>
  Lens.Family2.LensLike' f s a
maybe'poolInfluence
  = Data.ProtoLens.Field.field @"maybe'poolInfluence"
maybe'poolVotingThresholds ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'poolVotingThresholds" a) =>
  Lens.Family2.LensLike' f s a
maybe'poolVotingThresholds
  = Data.ProtoLens.Field.field @"maybe'poolVotingThresholds"
maybe'prices ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'prices" a) =>
  Lens.Family2.LensLike' f s a
maybe'prices = Data.ProtoLens.Field.field @"maybe'prices"
maybe'protocolVersion ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'protocolVersion" a) =>
  Lens.Family2.LensLike' f s a
maybe'protocolVersion
  = Data.ProtoLens.Field.field @"maybe'protocolVersion"
maybe'quantity ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'quantity" a) =>
  Lens.Family2.LensLike' f s a
maybe'quantity = Data.ProtoLens.Field.field @"maybe'quantity"
maybe'script ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'script" a) =>
  Lens.Family2.LensLike' f s a
maybe'script = Data.ProtoLens.Field.field @"maybe'script"
maybe'steps ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'steps" a) =>
  Lens.Family2.LensLike' f s a
maybe'steps = Data.ProtoLens.Field.field @"maybe'steps"
maybe'treasuryExpansion ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'treasuryExpansion" a) =>
  Lens.Family2.LensLike' f s a
maybe'treasuryExpansion
  = Data.ProtoLens.Field.field @"maybe'treasuryExpansion"
memory ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "memory" a) =>
  Lens.Family2.LensLike' f s a
memory = Data.ProtoLens.Field.field @"memory"
minCommitteeSize ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "minCommitteeSize" a) =>
  Lens.Family2.LensLike' f s a
minCommitteeSize = Data.ProtoLens.Field.field @"minCommitteeSize"
minFeeCoefficient ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "minFeeCoefficient" a) =>
  Lens.Family2.LensLike' f s a
minFeeCoefficient = Data.ProtoLens.Field.field @"minFeeCoefficient"
minFeeConstant ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "minFeeConstant" a) =>
  Lens.Family2.LensLike' f s a
minFeeConstant = Data.ProtoLens.Field.field @"minFeeConstant"
minFeeScriptRefCostPerByte ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "minFeeScriptRefCostPerByte" a) =>
  Lens.Family2.LensLike' f s a
minFeeScriptRefCostPerByte
  = Data.ProtoLens.Field.field @"minFeeScriptRefCostPerByte"
minPoolCost ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "minPoolCost" a) =>
  Lens.Family2.LensLike' f s a
minPoolCost = Data.ProtoLens.Field.field @"minPoolCost"
minor ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "minor" a) =>
  Lens.Family2.LensLike' f s a
minor = Data.ProtoLens.Field.field @"minor"
mintCoin ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "mintCoin" a) =>
  Lens.Family2.LensLike' f s a
mintCoin = Data.ProtoLens.Field.field @"mintCoin"
monetaryExpansion ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "monetaryExpansion" a) =>
  Lens.Family2.LensLike' f s a
monetaryExpansion = Data.ProtoLens.Field.field @"monetaryExpansion"
name ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "name" a) =>
  Lens.Family2.LensLike' f s a
name = Data.ProtoLens.Field.field @"name"
native ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "native" a) =>
  Lens.Family2.LensLike' f s a
native = Data.ProtoLens.Field.field @"native"
numerator ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "numerator" a) =>
  Lens.Family2.LensLike' f s a
numerator = Data.ProtoLens.Field.field @"numerator"
originalCbor ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "originalCbor" a) =>
  Lens.Family2.LensLike' f s a
originalCbor = Data.ProtoLens.Field.field @"originalCbor"
outputCoin ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "outputCoin" a) =>
  Lens.Family2.LensLike' f s a
outputCoin = Data.ProtoLens.Field.field @"outputCoin"
plutusV1 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "plutusV1" a) =>
  Lens.Family2.LensLike' f s a
plutusV1 = Data.ProtoLens.Field.field @"plutusV1"
plutusV2 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "plutusV2" a) =>
  Lens.Family2.LensLike' f s a
plutusV2 = Data.ProtoLens.Field.field @"plutusV2"
plutusV3 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "plutusV3" a) =>
  Lens.Family2.LensLike' f s a
plutusV3 = Data.ProtoLens.Field.field @"plutusV3"
plutusV4 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "plutusV4" a) =>
  Lens.Family2.LensLike' f s a
plutusV4 = Data.ProtoLens.Field.field @"plutusV4"
policyId ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "policyId" a) =>
  Lens.Family2.LensLike' f s a
policyId = Data.ProtoLens.Field.field @"policyId"
poolDeposit ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "poolDeposit" a) =>
  Lens.Family2.LensLike' f s a
poolDeposit = Data.ProtoLens.Field.field @"poolDeposit"
poolInfluence ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "poolInfluence" a) =>
  Lens.Family2.LensLike' f s a
poolInfluence = Data.ProtoLens.Field.field @"poolInfluence"
poolRetirementEpochBound ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "poolRetirementEpochBound" a) =>
  Lens.Family2.LensLike' f s a
poolRetirementEpochBound
  = Data.ProtoLens.Field.field @"poolRetirementEpochBound"
poolVotingThresholds ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "poolVotingThresholds" a) =>
  Lens.Family2.LensLike' f s a
poolVotingThresholds
  = Data.ProtoLens.Field.field @"poolVotingThresholds"
prices ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "prices" a) =>
  Lens.Family2.LensLike' f s a
prices = Data.ProtoLens.Field.field @"prices"
protocolVersion ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "protocolVersion" a) =>
  Lens.Family2.LensLike' f s a
protocolVersion = Data.ProtoLens.Field.field @"protocolVersion"
script ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "script" a) =>
  Lens.Family2.LensLike' f s a
script = Data.ProtoLens.Field.field @"script"
stakeKeyDeposit ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "stakeKeyDeposit" a) =>
  Lens.Family2.LensLike' f s a
stakeKeyDeposit = Data.ProtoLens.Field.field @"stakeKeyDeposit"
steps ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "steps" a) =>
  Lens.Family2.LensLike' f s a
steps = Data.ProtoLens.Field.field @"steps"
thresholds ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "thresholds" a) =>
  Lens.Family2.LensLike' f s a
thresholds = Data.ProtoLens.Field.field @"thresholds"
treasuryExpansion ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "treasuryExpansion" a) =>
  Lens.Family2.LensLike' f s a
treasuryExpansion = Data.ProtoLens.Field.field @"treasuryExpansion"
values ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "values" a) =>
  Lens.Family2.LensLike' f s a
values = Data.ProtoLens.Field.field @"values"
vec'assets ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'assets" a) =>
  Lens.Family2.LensLike' f s a
vec'assets = Data.ProtoLens.Field.field @"vec'assets"
vec'items ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'items" a) =>
  Lens.Family2.LensLike' f s a
vec'items = Data.ProtoLens.Field.field @"vec'items"
vec'thresholds ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'thresholds" a) =>
  Lens.Family2.LensLike' f s a
vec'thresholds = Data.ProtoLens.Field.field @"vec'thresholds"
vec'values ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'values" a) =>
  Lens.Family2.LensLike' f s a
vec'values = Data.ProtoLens.Field.field @"vec'values"