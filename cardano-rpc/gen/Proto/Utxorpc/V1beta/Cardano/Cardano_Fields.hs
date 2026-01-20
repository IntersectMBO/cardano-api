{- This file was auto-generated from utxorpc/v1beta/cardano/cardano.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Utxorpc.V1beta.Cardano.Cardano_Fields where
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
abstain ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "abstain" a) =>
  Lens.Family2.LensLike' f s a
abstain = Data.ProtoLens.Field.field @"abstain"
activeSlotsCoeff ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "activeSlotsCoeff" a) =>
  Lens.Family2.LensLike' f s a
activeSlotsCoeff = Data.ProtoLens.Field.field @"activeSlotsCoeff"
addrKeyHash ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "addrKeyHash" a) =>
  Lens.Family2.LensLike' f s a
addrKeyHash = Data.ProtoLens.Field.field @"addrKeyHash"
address ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "address" a) =>
  Lens.Family2.LensLike' f s a
address = Data.ProtoLens.Field.field @"address"
anchor ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "anchor" a) =>
  Lens.Family2.LensLike' f s a
anchor = Data.ProtoLens.Field.field @"anchor"
anyConstructor ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "anyConstructor" a) =>
  Lens.Family2.LensLike' f s a
anyConstructor = Data.ProtoLens.Field.field @"anyConstructor"
anyDrep ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "anyDrep" a) =>
  Lens.Family2.LensLike' f s a
anyDrep = Data.ProtoLens.Field.field @"anyDrep"
anyPoolKeyhash ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "anyPoolKeyhash" a) =>
  Lens.Family2.LensLike' f s a
anyPoolKeyhash = Data.ProtoLens.Field.field @"anyPoolKeyhash"
anyStakeCredential ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "anyStakeCredential" a) =>
  Lens.Family2.LensLike' f s a
anyStakeCredential
  = Data.ProtoLens.Field.field @"anyStakeCredential"
array ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "array" a) =>
  Lens.Family2.LensLike' f s a
array = Data.ProtoLens.Field.field @"array"
asOutput ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "asOutput" a) =>
  Lens.Family2.LensLike' f s a
asOutput = Data.ProtoLens.Field.field @"asOutput"
asset ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "asset" a) =>
  Lens.Family2.LensLike' f s a
asset = Data.ProtoLens.Field.field @"asset"
assetName ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "assetName" a) =>
  Lens.Family2.LensLike' f s a
assetName = Data.ProtoLens.Field.field @"assetName"
assets ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "assets" a) =>
  Lens.Family2.LensLike' f s a
assets = Data.ProtoLens.Field.field @"assets"
attributes ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "attributes" a) =>
  Lens.Family2.LensLike' f s a
attributes = Data.ProtoLens.Field.field @"attributes"
authCommitteeHotCert ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "authCommitteeHotCert" a) =>
  Lens.Family2.LensLike' f s a
authCommitteeHotCert
  = Data.ProtoLens.Field.field @"authCommitteeHotCert"
auxiliary ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "auxiliary" a) =>
  Lens.Family2.LensLike' f s a
auxiliary = Data.ProtoLens.Field.field @"auxiliary"
avvmDistr ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "avvmDistr" a) =>
  Lens.Family2.LensLike' f s a
avvmDistr = Data.ProtoLens.Field.field @"avvmDistr"
bigInt ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "bigInt" a) =>
  Lens.Family2.LensLike' f s a
bigInt = Data.ProtoLens.Field.field @"bigInt"
bigNInt ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "bigNInt" a) =>
  Lens.Family2.LensLike' f s a
bigNInt = Data.ProtoLens.Field.field @"bigNInt"
bigUInt ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "bigUInt" a) =>
  Lens.Family2.LensLike' f s a
bigUInt = Data.ProtoLens.Field.field @"bigUInt"
blockVersionData ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "blockVersionData" a) =>
  Lens.Family2.LensLike' f s a
blockVersionData = Data.ProtoLens.Field.field @"blockVersionData"
body ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "body" a) =>
  Lens.Family2.LensLike' f s a
body = Data.ProtoLens.Field.field @"body"
bootStakeholders ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "bootStakeholders" a) =>
  Lens.Family2.LensLike' f s a
bootStakeholders = Data.ProtoLens.Field.field @"bootStakeholders"
bootstrapWitnesses ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "bootstrapWitnesses" a) =>
  Lens.Family2.LensLike' f s a
bootstrapWitnesses
  = Data.ProtoLens.Field.field @"bootstrapWitnesses"
boundedBytes ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "boundedBytes" a) =>
  Lens.Family2.LensLike' f s a
boundedBytes = Data.ProtoLens.Field.field @"boundedBytes"
bytes ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "bytes" a) =>
  Lens.Family2.LensLike' f s a
bytes = Data.ProtoLens.Field.field @"bytes"
cert ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "cert" a) =>
  Lens.Family2.LensLike' f s a
cert = Data.ProtoLens.Field.field @"cert"
certificates ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "certificates" a) =>
  Lens.Family2.LensLike' f s a
certificates = Data.ProtoLens.Field.field @"certificates"
chainCode ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "chainCode" a) =>
  Lens.Family2.LensLike' f s a
chainCode = Data.ProtoLens.Field.field @"chainCode"
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
collateral ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "collateral" a) =>
  Lens.Family2.LensLike' f s a
collateral = Data.ProtoLens.Field.field @"collateral"
collateralPercentage ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "collateralPercentage" a) =>
  Lens.Family2.LensLike' f s a
collateralPercentage
  = Data.ProtoLens.Field.field @"collateralPercentage"
collateralReturn ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "collateralReturn" a) =>
  Lens.Family2.LensLike' f s a
collateralReturn = Data.ProtoLens.Field.field @"collateralReturn"
committee ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "committee" a) =>
  Lens.Family2.LensLike' f s a
committee = Data.ProtoLens.Field.field @"committee"
committeeColdCredential ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "committeeColdCredential" a) =>
  Lens.Family2.LensLike' f s a
committeeColdCredential
  = Data.ProtoLens.Field.field @"committeeColdCredential"
committeeHotCredential ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "committeeHotCredential" a) =>
  Lens.Family2.LensLike' f s a
committeeHotCredential
  = Data.ProtoLens.Field.field @"committeeHotCredential"
committeeMaxTermLength ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "committeeMaxTermLength" a) =>
  Lens.Family2.LensLike' f s a
committeeMaxTermLength
  = Data.ProtoLens.Field.field @"committeeMaxTermLength"
committeeMinSize ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "committeeMinSize" a) =>
  Lens.Family2.LensLike' f s a
committeeMinSize = Data.ProtoLens.Field.field @"committeeMinSize"
committeeNoConfidence ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "committeeNoConfidence" a) =>
  Lens.Family2.LensLike' f s a
committeeNoConfidence
  = Data.ProtoLens.Field.field @"committeeNoConfidence"
committeeNormal ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "committeeNormal" a) =>
  Lens.Family2.LensLike' f s a
committeeNormal = Data.ProtoLens.Field.field @"committeeNormal"
committeeTermLimit ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "committeeTermLimit" a) =>
  Lens.Family2.LensLike' f s a
committeeTermLimit
  = Data.ProtoLens.Field.field @"committeeTermLimit"
constitution ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "constitution" a) =>
  Lens.Family2.LensLike' f s a
constitution = Data.ProtoLens.Field.field @"constitution"
constr ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "constr" a) =>
  Lens.Family2.LensLike' f s a
constr = Data.ProtoLens.Field.field @"constr"
consumes ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "consumes" a) =>
  Lens.Family2.LensLike' f s a
consumes = Data.ProtoLens.Field.field @"consumes"
contentHash ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "contentHash" a) =>
  Lens.Family2.LensLike' f s a
contentHash = Data.ProtoLens.Field.field @"contentHash"
cost ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "cost" a) =>
  Lens.Family2.LensLike' f s a
cost = Data.ProtoLens.Field.field @"cost"
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
delegate ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "delegate" a) =>
  Lens.Family2.LensLike' f s a
delegate = Data.ProtoLens.Field.field @"delegate"
delegatePk ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "delegatePk" a) =>
  Lens.Family2.LensLike' f s a
delegatePk = Data.ProtoLens.Field.field @"delegatePk"
delegationPart ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "delegationPart" a) =>
  Lens.Family2.LensLike' f s a
delegationPart = Data.ProtoLens.Field.field @"delegationPart"
deltaCoin ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "deltaCoin" a) =>
  Lens.Family2.LensLike' f s a
deltaCoin = Data.ProtoLens.Field.field @"deltaCoin"
denominator ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "denominator" a) =>
  Lens.Family2.LensLike' f s a
denominator = Data.ProtoLens.Field.field @"denominator"
deposit ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "deposit" a) =>
  Lens.Family2.LensLike' f s a
deposit = Data.ProtoLens.Field.field @"deposit"
desiredNumberOfPools ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "desiredNumberOfPools" a) =>
  Lens.Family2.LensLike' f s a
desiredNumberOfPools
  = Data.ProtoLens.Field.field @"desiredNumberOfPools"
dnsName ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "dnsName" a) =>
  Lens.Family2.LensLike' f s a
dnsName = Data.ProtoLens.Field.field @"dnsName"
drep ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "drep" a) =>
  Lens.Family2.LensLike' f s a
drep = Data.ProtoLens.Field.field @"drep"
drepActivity ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "drepActivity" a) =>
  Lens.Family2.LensLike' f s a
drepActivity = Data.ProtoLens.Field.field @"drepActivity"
drepCredential ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "drepCredential" a) =>
  Lens.Family2.LensLike' f s a
drepCredential = Data.ProtoLens.Field.field @"drepCredential"
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
end ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "end" a) =>
  Lens.Family2.LensLike' f s a
end = Data.ProtoLens.Field.field @"end"
epoch ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "epoch" a) =>
  Lens.Family2.LensLike' f s a
epoch = Data.ProtoLens.Field.field @"epoch"
epochLength ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "epochLength" a) =>
  Lens.Family2.LensLike' f s a
epochLength = Data.ProtoLens.Field.field @"epochLength"
errors ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "errors" a) =>
  Lens.Family2.LensLike' f s a
errors = Data.ProtoLens.Field.field @"errors"
exUnits ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "exUnits" a) =>
  Lens.Family2.LensLike' f s a
exUnits = Data.ProtoLens.Field.field @"exUnits"
exactAddress ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "exactAddress" a) =>
  Lens.Family2.LensLike' f s a
exactAddress = Data.ProtoLens.Field.field @"exactAddress"
executionPrices ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "executionPrices" a) =>
  Lens.Family2.LensLike' f s a
executionPrices = Data.ProtoLens.Field.field @"executionPrices"
expiresEpoch ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "expiresEpoch" a) =>
  Lens.Family2.LensLike' f s a
expiresEpoch = Data.ProtoLens.Field.field @"expiresEpoch"
expiryEpoch ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "expiryEpoch" a) =>
  Lens.Family2.LensLike' f s a
expiryEpoch = Data.ProtoLens.Field.field @"expiryEpoch"
fee ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "fee" a) =>
  Lens.Family2.LensLike' f s a
fee = Data.ProtoLens.Field.field @"fee"
fields ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "fields" a) =>
  Lens.Family2.LensLike' f s a
fields = Data.ProtoLens.Field.field @"fields"
from ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "from" a) =>
  Lens.Family2.LensLike' f s a
from = Data.ProtoLens.Field.field @"from"
ftsSeed ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "ftsSeed" a) =>
  Lens.Family2.LensLike' f s a
ftsSeed = Data.ProtoLens.Field.field @"ftsSeed"
genDelegs ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "genDelegs" a) =>
  Lens.Family2.LensLike' f s a
genDelegs = Data.ProtoLens.Field.field @"genDelegs"
genesisDelegateHash ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "genesisDelegateHash" a) =>
  Lens.Family2.LensLike' f s a
genesisDelegateHash
  = Data.ProtoLens.Field.field @"genesisDelegateHash"
genesisHash ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "genesisHash" a) =>
  Lens.Family2.LensLike' f s a
genesisHash = Data.ProtoLens.Field.field @"genesisHash"
genesisKeyDelegation ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "genesisKeyDelegation" a) =>
  Lens.Family2.LensLike' f s a
genesisKeyDelegation
  = Data.ProtoLens.Field.field @"genesisKeyDelegation"
govAction ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "govAction" a) =>
  Lens.Family2.LensLike' f s a
govAction = Data.ProtoLens.Field.field @"govAction"
govActionDeposit ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "govActionDeposit" a) =>
  Lens.Family2.LensLike' f s a
govActionDeposit = Data.ProtoLens.Field.field @"govActionDeposit"
govActionId ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "govActionId" a) =>
  Lens.Family2.LensLike' f s a
govActionId = Data.ProtoLens.Field.field @"govActionId"
govActionLifetime ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "govActionLifetime" a) =>
  Lens.Family2.LensLike' f s a
govActionLifetime = Data.ProtoLens.Field.field @"govActionLifetime"
governanceActionDeposit ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "governanceActionDeposit" a) =>
  Lens.Family2.LensLike' f s a
governanceActionDeposit
  = Data.ProtoLens.Field.field @"governanceActionDeposit"
governanceActionIndex ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "governanceActionIndex" a) =>
  Lens.Family2.LensLike' f s a
governanceActionIndex
  = Data.ProtoLens.Field.field @"governanceActionIndex"
governanceActionValidityPeriod ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "governanceActionValidityPeriod" a) =>
  Lens.Family2.LensLike' f s a
governanceActionValidityPeriod
  = Data.ProtoLens.Field.field @"governanceActionValidityPeriod"
hardForkInitiation ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "hardForkInitiation" a) =>
  Lens.Family2.LensLike' f s a
hardForkInitiation
  = Data.ProtoLens.Field.field @"hardForkInitiation"
hardForkInitiationAction ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "hardForkInitiationAction" a) =>
  Lens.Family2.LensLike' f s a
hardForkInitiationAction
  = Data.ProtoLens.Field.field @"hardForkInitiationAction"
hasAddress ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "hasAddress" a) =>
  Lens.Family2.LensLike' f s a
hasAddress = Data.ProtoLens.Field.field @"hasAddress"
hasCertificate ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "hasCertificate" a) =>
  Lens.Family2.LensLike' f s a
hasCertificate = Data.ProtoLens.Field.field @"hasCertificate"
hash ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "hash" a) =>
  Lens.Family2.LensLike' f s a
hash = Data.ProtoLens.Field.field @"hash"
header ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "header" a) =>
  Lens.Family2.LensLike' f s a
header = Data.ProtoLens.Field.field @"header"
heavyDelThd ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "heavyDelThd" a) =>
  Lens.Family2.LensLike' f s a
heavyDelThd = Data.ProtoLens.Field.field @"heavyDelThd"
heavyDelegation ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "heavyDelegation" a) =>
  Lens.Family2.LensLike' f s a
heavyDelegation = Data.ProtoLens.Field.field @"heavyDelegation"
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
infoAction ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "infoAction" a) =>
  Lens.Family2.LensLike' f s a
infoAction = Data.ProtoLens.Field.field @"infoAction"
initThd ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "initThd" a) =>
  Lens.Family2.LensLike' f s a
initThd = Data.ProtoLens.Field.field @"initThd"
initialFunds ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "initialFunds" a) =>
  Lens.Family2.LensLike' f s a
initialFunds = Data.ProtoLens.Field.field @"initialFunds"
inputs ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "inputs" a) =>
  Lens.Family2.LensLike' f s a
inputs = Data.ProtoLens.Field.field @"inputs"
int ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "int" a) =>
  Lens.Family2.LensLike' f s a
int = Data.ProtoLens.Field.field @"int"
invalidBefore ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "invalidBefore" a) =>
  Lens.Family2.LensLike' f s a
invalidBefore = Data.ProtoLens.Field.field @"invalidBefore"
invalidHereafter ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "invalidHereafter" a) =>
  Lens.Family2.LensLike' f s a
invalidHereafter = Data.ProtoLens.Field.field @"invalidHereafter"
ipV4 ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "ipV4" a) =>
  Lens.Family2.LensLike' f s a
ipV4 = Data.ProtoLens.Field.field @"ipV4"
ipV6 ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "ipV6" a) =>
  Lens.Family2.LensLike' f s a
ipV6 = Data.ProtoLens.Field.field @"ipV6"
issuerPk ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "issuerPk" a) =>
  Lens.Family2.LensLike' f s a
issuerPk = Data.ProtoLens.Field.field @"issuerPk"
items ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "items" a) =>
  Lens.Family2.LensLike' f s a
items = Data.ProtoLens.Field.field @"items"
k ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "k" a) =>
  Lens.Family2.LensLike' f s a
k = Data.ProtoLens.Field.field @"k"
key ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "key" a) =>
  Lens.Family2.LensLike' f s a
key = Data.ProtoLens.Field.field @"key"
label ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "label" a) =>
  Lens.Family2.LensLike' f s a
label = Data.ProtoLens.Field.field @"label"
lovelacePerUtxoWord ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "lovelacePerUtxoWord" a) =>
  Lens.Family2.LensLike' f s a
lovelacePerUtxoWord
  = Data.ProtoLens.Field.field @"lovelacePerUtxoWord"
major ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "major" a) =>
  Lens.Family2.LensLike' f s a
major = Data.ProtoLens.Field.field @"major"
map ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "map" a) =>
  Lens.Family2.LensLike' f s a
map = Data.ProtoLens.Field.field @"map"
margin ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "margin" a) =>
  Lens.Family2.LensLike' f s a
margin = Data.ProtoLens.Field.field @"margin"
maxBlockBodySize ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maxBlockBodySize" a) =>
  Lens.Family2.LensLike' f s a
maxBlockBodySize = Data.ProtoLens.Field.field @"maxBlockBodySize"
maxBlockExUnits ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maxBlockExUnits" a) =>
  Lens.Family2.LensLike' f s a
maxBlockExUnits = Data.ProtoLens.Field.field @"maxBlockExUnits"
maxBlockHeaderSize ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maxBlockHeaderSize" a) =>
  Lens.Family2.LensLike' f s a
maxBlockHeaderSize
  = Data.ProtoLens.Field.field @"maxBlockHeaderSize"
maxBlockSize ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maxBlockSize" a) =>
  Lens.Family2.LensLike' f s a
maxBlockSize = Data.ProtoLens.Field.field @"maxBlockSize"
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
maxHeaderSize ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maxHeaderSize" a) =>
  Lens.Family2.LensLike' f s a
maxHeaderSize = Data.ProtoLens.Field.field @"maxHeaderSize"
maxKesEvolutions ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maxKesEvolutions" a) =>
  Lens.Family2.LensLike' f s a
maxKesEvolutions = Data.ProtoLens.Field.field @"maxKesEvolutions"
maxLovelaceSupply ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maxLovelaceSupply" a) =>
  Lens.Family2.LensLike' f s a
maxLovelaceSupply = Data.ProtoLens.Field.field @"maxLovelaceSupply"
maxProposalSize ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maxProposalSize" a) =>
  Lens.Family2.LensLike' f s a
maxProposalSize = Data.ProtoLens.Field.field @"maxProposalSize"
maxTxExUnits ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maxTxExUnits" a) =>
  Lens.Family2.LensLike' f s a
maxTxExUnits = Data.ProtoLens.Field.field @"maxTxExUnits"
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
maybe'abstain ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'abstain" a) =>
  Lens.Family2.LensLike' f s a
maybe'abstain = Data.ProtoLens.Field.field @"maybe'abstain"
maybe'activeSlotsCoeff ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'activeSlotsCoeff" a) =>
  Lens.Family2.LensLike' f s a
maybe'activeSlotsCoeff
  = Data.ProtoLens.Field.field @"maybe'activeSlotsCoeff"
maybe'addrKeyHash ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'addrKeyHash" a) =>
  Lens.Family2.LensLike' f s a
maybe'addrKeyHash = Data.ProtoLens.Field.field @"maybe'addrKeyHash"
maybe'address ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'address" a) =>
  Lens.Family2.LensLike' f s a
maybe'address = Data.ProtoLens.Field.field @"maybe'address"
maybe'anchor ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'anchor" a) =>
  Lens.Family2.LensLike' f s a
maybe'anchor = Data.ProtoLens.Field.field @"maybe'anchor"
maybe'anyDrep ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'anyDrep" a) =>
  Lens.Family2.LensLike' f s a
maybe'anyDrep = Data.ProtoLens.Field.field @"maybe'anyDrep"
maybe'anyPoolKeyhash ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'anyPoolKeyhash" a) =>
  Lens.Family2.LensLike' f s a
maybe'anyPoolKeyhash
  = Data.ProtoLens.Field.field @"maybe'anyPoolKeyhash"
maybe'anyStakeCredential ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'anyStakeCredential" a) =>
  Lens.Family2.LensLike' f s a
maybe'anyStakeCredential
  = Data.ProtoLens.Field.field @"maybe'anyStakeCredential"
maybe'array ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'array" a) =>
  Lens.Family2.LensLike' f s a
maybe'array = Data.ProtoLens.Field.field @"maybe'array"
maybe'asOutput ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'asOutput" a) =>
  Lens.Family2.LensLike' f s a
maybe'asOutput = Data.ProtoLens.Field.field @"maybe'asOutput"
maybe'asset ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'asset" a) =>
  Lens.Family2.LensLike' f s a
maybe'asset = Data.ProtoLens.Field.field @"maybe'asset"
maybe'authCommitteeHotCert ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'authCommitteeHotCert" a) =>
  Lens.Family2.LensLike' f s a
maybe'authCommitteeHotCert
  = Data.ProtoLens.Field.field @"maybe'authCommitteeHotCert"
maybe'auxiliary ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'auxiliary" a) =>
  Lens.Family2.LensLike' f s a
maybe'auxiliary = Data.ProtoLens.Field.field @"maybe'auxiliary"
maybe'bigInt ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'bigInt" a) =>
  Lens.Family2.LensLike' f s a
maybe'bigInt = Data.ProtoLens.Field.field @"maybe'bigInt"
maybe'bigNInt ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'bigNInt" a) =>
  Lens.Family2.LensLike' f s a
maybe'bigNInt = Data.ProtoLens.Field.field @"maybe'bigNInt"
maybe'bigUInt ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'bigUInt" a) =>
  Lens.Family2.LensLike' f s a
maybe'bigUInt = Data.ProtoLens.Field.field @"maybe'bigUInt"
maybe'blockVersionData ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'blockVersionData" a) =>
  Lens.Family2.LensLike' f s a
maybe'blockVersionData
  = Data.ProtoLens.Field.field @"maybe'blockVersionData"
maybe'body ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'body" a) =>
  Lens.Family2.LensLike' f s a
maybe'body = Data.ProtoLens.Field.field @"maybe'body"
maybe'boundedBytes ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'boundedBytes" a) =>
  Lens.Family2.LensLike' f s a
maybe'boundedBytes
  = Data.ProtoLens.Field.field @"maybe'boundedBytes"
maybe'bytes ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'bytes" a) =>
  Lens.Family2.LensLike' f s a
maybe'bytes = Data.ProtoLens.Field.field @"maybe'bytes"
maybe'certificate ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'certificate" a) =>
  Lens.Family2.LensLike' f s a
maybe'certificate = Data.ProtoLens.Field.field @"maybe'certificate"
maybe'certificateType ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'certificateType" a) =>
  Lens.Family2.LensLike' f s a
maybe'certificateType
  = Data.ProtoLens.Field.field @"maybe'certificateType"
maybe'coin ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'coin" a) =>
  Lens.Family2.LensLike' f s a
maybe'coin = Data.ProtoLens.Field.field @"maybe'coin"
maybe'coinsPerUtxoByte ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'coinsPerUtxoByte" a) =>
  Lens.Family2.LensLike' f s a
maybe'coinsPerUtxoByte
  = Data.ProtoLens.Field.field @"maybe'coinsPerUtxoByte"
maybe'collateral ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'collateral" a) =>
  Lens.Family2.LensLike' f s a
maybe'collateral = Data.ProtoLens.Field.field @"maybe'collateral"
maybe'collateralReturn ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'collateralReturn" a) =>
  Lens.Family2.LensLike' f s a
maybe'collateralReturn
  = Data.ProtoLens.Field.field @"maybe'collateralReturn"
maybe'committee ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'committee" a) =>
  Lens.Family2.LensLike' f s a
maybe'committee = Data.ProtoLens.Field.field @"maybe'committee"
maybe'committeeColdCredential ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'committeeColdCredential" a) =>
  Lens.Family2.LensLike' f s a
maybe'committeeColdCredential
  = Data.ProtoLens.Field.field @"maybe'committeeColdCredential"
maybe'committeeHotCredential ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'committeeHotCredential" a) =>
  Lens.Family2.LensLike' f s a
maybe'committeeHotCredential
  = Data.ProtoLens.Field.field @"maybe'committeeHotCredential"
maybe'committeeNoConfidence ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'committeeNoConfidence" a) =>
  Lens.Family2.LensLike' f s a
maybe'committeeNoConfidence
  = Data.ProtoLens.Field.field @"maybe'committeeNoConfidence"
maybe'committeeNormal ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'committeeNormal" a) =>
  Lens.Family2.LensLike' f s a
maybe'committeeNormal
  = Data.ProtoLens.Field.field @"maybe'committeeNormal"
maybe'constitution ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'constitution" a) =>
  Lens.Family2.LensLike' f s a
maybe'constitution
  = Data.ProtoLens.Field.field @"maybe'constitution"
maybe'constr ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'constr" a) =>
  Lens.Family2.LensLike' f s a
maybe'constr = Data.ProtoLens.Field.field @"maybe'constr"
maybe'consumes ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'consumes" a) =>
  Lens.Family2.LensLike' f s a
maybe'consumes = Data.ProtoLens.Field.field @"maybe'consumes"
maybe'cost ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'cost" a) =>
  Lens.Family2.LensLike' f s a
maybe'cost = Data.ProtoLens.Field.field @"maybe'cost"
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
maybe'deltaCoin ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'deltaCoin" a) =>
  Lens.Family2.LensLike' f s a
maybe'deltaCoin = Data.ProtoLens.Field.field @"maybe'deltaCoin"
maybe'deposit ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'deposit" a) =>
  Lens.Family2.LensLike' f s a
maybe'deposit = Data.ProtoLens.Field.field @"maybe'deposit"
maybe'drep ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'drep" a) =>
  Lens.Family2.LensLike' f s a
maybe'drep = Data.ProtoLens.Field.field @"maybe'drep"
maybe'drepCredential ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'drepCredential" a) =>
  Lens.Family2.LensLike' f s a
maybe'drepCredential
  = Data.ProtoLens.Field.field @"maybe'drepCredential"
maybe'drepDeposit ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'drepDeposit" a) =>
  Lens.Family2.LensLike' f s a
maybe'drepDeposit = Data.ProtoLens.Field.field @"maybe'drepDeposit"
maybe'drepVotingThresholds ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'drepVotingThresholds" a) =>
  Lens.Family2.LensLike' f s a
maybe'drepVotingThresholds
  = Data.ProtoLens.Field.field @"maybe'drepVotingThresholds"
maybe'end ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'end" a) =>
  Lens.Family2.LensLike' f s a
maybe'end = Data.ProtoLens.Field.field @"maybe'end"
maybe'exUnits ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'exUnits" a) =>
  Lens.Family2.LensLike' f s a
maybe'exUnits = Data.ProtoLens.Field.field @"maybe'exUnits"
maybe'executionPrices ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'executionPrices" a) =>
  Lens.Family2.LensLike' f s a
maybe'executionPrices
  = Data.ProtoLens.Field.field @"maybe'executionPrices"
maybe'fee ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'fee" a) =>
  Lens.Family2.LensLike' f s a
maybe'fee = Data.ProtoLens.Field.field @"maybe'fee"
maybe'genesisKeyDelegation ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'genesisKeyDelegation" a) =>
  Lens.Family2.LensLike' f s a
maybe'genesisKeyDelegation
  = Data.ProtoLens.Field.field @"maybe'genesisKeyDelegation"
maybe'govAction ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'govAction" a) =>
  Lens.Family2.LensLike' f s a
maybe'govAction = Data.ProtoLens.Field.field @"maybe'govAction"
maybe'govActionDeposit ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'govActionDeposit" a) =>
  Lens.Family2.LensLike' f s a
maybe'govActionDeposit
  = Data.ProtoLens.Field.field @"maybe'govActionDeposit"
maybe'govActionId ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'govActionId" a) =>
  Lens.Family2.LensLike' f s a
maybe'govActionId = Data.ProtoLens.Field.field @"maybe'govActionId"
maybe'governanceAction ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'governanceAction" a) =>
  Lens.Family2.LensLike' f s a
maybe'governanceAction
  = Data.ProtoLens.Field.field @"maybe'governanceAction"
maybe'governanceActionDeposit ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'governanceActionDeposit" a) =>
  Lens.Family2.LensLike' f s a
maybe'governanceActionDeposit
  = Data.ProtoLens.Field.field @"maybe'governanceActionDeposit"
maybe'hardForkInitiation ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'hardForkInitiation" a) =>
  Lens.Family2.LensLike' f s a
maybe'hardForkInitiation
  = Data.ProtoLens.Field.field @"maybe'hardForkInitiation"
maybe'hardForkInitiationAction ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'hardForkInitiationAction" a) =>
  Lens.Family2.LensLike' f s a
maybe'hardForkInitiationAction
  = Data.ProtoLens.Field.field @"maybe'hardForkInitiationAction"
maybe'hasAddress ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'hasAddress" a) =>
  Lens.Family2.LensLike' f s a
maybe'hasAddress = Data.ProtoLens.Field.field @"maybe'hasAddress"
maybe'hasCertificate ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'hasCertificate" a) =>
  Lens.Family2.LensLike' f s a
maybe'hasCertificate
  = Data.ProtoLens.Field.field @"maybe'hasCertificate"
maybe'header ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'header" a) =>
  Lens.Family2.LensLike' f s a
maybe'header = Data.ProtoLens.Field.field @"maybe'header"
maybe'infoAction ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'infoAction" a) =>
  Lens.Family2.LensLike' f s a
maybe'infoAction = Data.ProtoLens.Field.field @"maybe'infoAction"
maybe'int ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'int" a) =>
  Lens.Family2.LensLike' f s a
maybe'int = Data.ProtoLens.Field.field @"maybe'int"
maybe'invalidBefore ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'invalidBefore" a) =>
  Lens.Family2.LensLike' f s a
maybe'invalidBefore
  = Data.ProtoLens.Field.field @"maybe'invalidBefore"
maybe'invalidHereafter ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'invalidHereafter" a) =>
  Lens.Family2.LensLike' f s a
maybe'invalidHereafter
  = Data.ProtoLens.Field.field @"maybe'invalidHereafter"
maybe'key ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'key" a) =>
  Lens.Family2.LensLike' f s a
maybe'key = Data.ProtoLens.Field.field @"maybe'key"
maybe'lovelacePerUtxoWord ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'lovelacePerUtxoWord" a) =>
  Lens.Family2.LensLike' f s a
maybe'lovelacePerUtxoWord
  = Data.ProtoLens.Field.field @"maybe'lovelacePerUtxoWord"
maybe'map ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'map" a) =>
  Lens.Family2.LensLike' f s a
maybe'map = Data.ProtoLens.Field.field @"maybe'map"
maybe'margin ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'margin" a) =>
  Lens.Family2.LensLike' f s a
maybe'margin = Data.ProtoLens.Field.field @"maybe'margin"
maybe'maxBlockExUnits ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'maxBlockExUnits" a) =>
  Lens.Family2.LensLike' f s a
maybe'maxBlockExUnits
  = Data.ProtoLens.Field.field @"maybe'maxBlockExUnits"
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
maybe'maxLovelaceSupply ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'maxLovelaceSupply" a) =>
  Lens.Family2.LensLike' f s a
maybe'maxLovelaceSupply
  = Data.ProtoLens.Field.field @"maybe'maxLovelaceSupply"
maybe'maxTxExUnits ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'maxTxExUnits" a) =>
  Lens.Family2.LensLike' f s a
maybe'maxTxExUnits
  = Data.ProtoLens.Field.field @"maybe'maxTxExUnits"
maybe'memory ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'memory" a) =>
  Lens.Family2.LensLike' f s a
maybe'memory = Data.ProtoLens.Field.field @"maybe'memory"
maybe'metadatum ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'metadatum" a) =>
  Lens.Family2.LensLike' f s a
maybe'metadatum = Data.ProtoLens.Field.field @"maybe'metadatum"
maybe'minFeeCoefficient ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'minFeeCoefficient" a) =>
  Lens.Family2.LensLike' f s a
maybe'minFeeCoefficient
  = Data.ProtoLens.Field.field @"maybe'minFeeCoefficient"
maybe'minFeeConstant ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'minFeeConstant" a) =>
  Lens.Family2.LensLike' f s a
maybe'minFeeConstant
  = Data.ProtoLens.Field.field @"maybe'minFeeConstant"
maybe'minFeeRefScriptCostPerByte ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'minFeeRefScriptCostPerByte" a) =>
  Lens.Family2.LensLike' f s a
maybe'minFeeRefScriptCostPerByte
  = Data.ProtoLens.Field.field @"maybe'minFeeRefScriptCostPerByte"
maybe'minFeeScriptRefCostPerByte ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'minFeeScriptRefCostPerByte" a) =>
  Lens.Family2.LensLike' f s a
maybe'minFeeScriptRefCostPerByte
  = Data.ProtoLens.Field.field @"maybe'minFeeScriptRefCostPerByte"
maybe'minPoolCost ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'minPoolCost" a) =>
  Lens.Family2.LensLike' f s a
maybe'minPoolCost = Data.ProtoLens.Field.field @"maybe'minPoolCost"
maybe'mintsAsset ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'mintsAsset" a) =>
  Lens.Family2.LensLike' f s a
maybe'mintsAsset = Data.ProtoLens.Field.field @"maybe'mintsAsset"
maybe'mirCert ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'mirCert" a) =>
  Lens.Family2.LensLike' f s a
maybe'mirCert = Data.ProtoLens.Field.field @"maybe'mirCert"
maybe'monetaryExpansion ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'monetaryExpansion" a) =>
  Lens.Family2.LensLike' f s a
maybe'monetaryExpansion
  = Data.ProtoLens.Field.field @"maybe'monetaryExpansion"
maybe'motionNoConfidence ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'motionNoConfidence" a) =>
  Lens.Family2.LensLike' f s a
maybe'motionNoConfidence
  = Data.ProtoLens.Field.field @"maybe'motionNoConfidence"
maybe'movesAsset ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'movesAsset" a) =>
  Lens.Family2.LensLike' f s a
maybe'movesAsset = Data.ProtoLens.Field.field @"maybe'movesAsset"
maybe'native ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'native" a) =>
  Lens.Family2.LensLike' f s a
maybe'native = Data.ProtoLens.Field.field @"maybe'native"
maybe'nativeScript ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'nativeScript" a) =>
  Lens.Family2.LensLike' f s a
maybe'nativeScript
  = Data.ProtoLens.Field.field @"maybe'nativeScript"
maybe'newCommitteeThreshold ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'newCommitteeThreshold" a) =>
  Lens.Family2.LensLike' f s a
maybe'newCommitteeThreshold
  = Data.ProtoLens.Field.field @"maybe'newCommitteeThreshold"
maybe'newConstitutionAction ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'newConstitutionAction" a) =>
  Lens.Family2.LensLike' f s a
maybe'newConstitutionAction
  = Data.ProtoLens.Field.field @"maybe'newConstitutionAction"
maybe'noConfidence ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'noConfidence" a) =>
  Lens.Family2.LensLike' f s a
maybe'noConfidence
  = Data.ProtoLens.Field.field @"maybe'noConfidence"
maybe'noConfidenceAction ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'noConfidenceAction" a) =>
  Lens.Family2.LensLike' f s a
maybe'noConfidenceAction
  = Data.ProtoLens.Field.field @"maybe'noConfidenceAction"
maybe'originalCbor ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'originalCbor" a) =>
  Lens.Family2.LensLike' f s a
maybe'originalCbor
  = Data.ProtoLens.Field.field @"maybe'originalCbor"
maybe'parameterChangeAction ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'parameterChangeAction" a) =>
  Lens.Family2.LensLike' f s a
maybe'parameterChangeAction
  = Data.ProtoLens.Field.field @"maybe'parameterChangeAction"
maybe'payload ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'payload" a) =>
  Lens.Family2.LensLike' f s a
maybe'payload = Data.ProtoLens.Field.field @"maybe'payload"
maybe'pledge ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'pledge" a) =>
  Lens.Family2.LensLike' f s a
maybe'pledge = Data.ProtoLens.Field.field @"maybe'pledge"
maybe'plutusData ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'plutusData" a) =>
  Lens.Family2.LensLike' f s a
maybe'plutusData = Data.ProtoLens.Field.field @"maybe'plutusData"
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
maybe'poolDeposit ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'poolDeposit" a) =>
  Lens.Family2.LensLike' f s a
maybe'poolDeposit = Data.ProtoLens.Field.field @"maybe'poolDeposit"
maybe'poolInfluence ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'poolInfluence" a) =>
  Lens.Family2.LensLike' f s a
maybe'poolInfluence
  = Data.ProtoLens.Field.field @"maybe'poolInfluence"
maybe'poolMetadata ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'poolMetadata" a) =>
  Lens.Family2.LensLike' f s a
maybe'poolMetadata
  = Data.ProtoLens.Field.field @"maybe'poolMetadata"
maybe'poolRegistration ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'poolRegistration" a) =>
  Lens.Family2.LensLike' f s a
maybe'poolRegistration
  = Data.ProtoLens.Field.field @"maybe'poolRegistration"
maybe'poolRetirement ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'poolRetirement" a) =>
  Lens.Family2.LensLike' f s a
maybe'poolRetirement
  = Data.ProtoLens.Field.field @"maybe'poolRetirement"
maybe'poolVotingThresholds ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'poolVotingThresholds" a) =>
  Lens.Family2.LensLike' f s a
maybe'poolVotingThresholds
  = Data.ProtoLens.Field.field @"maybe'poolVotingThresholds"
maybe'ppEconomicGroup ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'ppEconomicGroup" a) =>
  Lens.Family2.LensLike' f s a
maybe'ppEconomicGroup
  = Data.ProtoLens.Field.field @"maybe'ppEconomicGroup"
maybe'ppGovGroup ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'ppGovGroup" a) =>
  Lens.Family2.LensLike' f s a
maybe'ppGovGroup = Data.ProtoLens.Field.field @"maybe'ppGovGroup"
maybe'ppNetworkGroup ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'ppNetworkGroup" a) =>
  Lens.Family2.LensLike' f s a
maybe'ppNetworkGroup
  = Data.ProtoLens.Field.field @"maybe'ppNetworkGroup"
maybe'ppSecurityGroup ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'ppSecurityGroup" a) =>
  Lens.Family2.LensLike' f s a
maybe'ppSecurityGroup
  = Data.ProtoLens.Field.field @"maybe'ppSecurityGroup"
maybe'ppTechnicalGroup ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'ppTechnicalGroup" a) =>
  Lens.Family2.LensLike' f s a
maybe'ppTechnicalGroup
  = Data.ProtoLens.Field.field @"maybe'ppTechnicalGroup"
maybe'prices ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'prices" a) =>
  Lens.Family2.LensLike' f s a
maybe'prices = Data.ProtoLens.Field.field @"maybe'prices"
maybe'produces ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'produces" a) =>
  Lens.Family2.LensLike' f s a
maybe'produces = Data.ProtoLens.Field.field @"maybe'produces"
maybe'protocolConsts ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'protocolConsts" a) =>
  Lens.Family2.LensLike' f s a
maybe'protocolConsts
  = Data.ProtoLens.Field.field @"maybe'protocolConsts"
maybe'protocolParamUpdate ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'protocolParamUpdate" a) =>
  Lens.Family2.LensLike' f s a
maybe'protocolParamUpdate
  = Data.ProtoLens.Field.field @"maybe'protocolParamUpdate"
maybe'protocolParams ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'protocolParams" a) =>
  Lens.Family2.LensLike' f s a
maybe'protocolParams
  = Data.ProtoLens.Field.field @"maybe'protocolParams"
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
maybe'redeemer ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'redeemer" a) =>
  Lens.Family2.LensLike' f s a
maybe'redeemer = Data.ProtoLens.Field.field @"maybe'redeemer"
maybe'regCert ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'regCert" a) =>
  Lens.Family2.LensLike' f s a
maybe'regCert = Data.ProtoLens.Field.field @"maybe'regCert"
maybe'regDrepCert ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'regDrepCert" a) =>
  Lens.Family2.LensLike' f s a
maybe'regDrepCert = Data.ProtoLens.Field.field @"maybe'regDrepCert"
maybe'resignCommitteeColdCert ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'resignCommitteeColdCert" a) =>
  Lens.Family2.LensLike' f s a
maybe'resignCommitteeColdCert
  = Data.ProtoLens.Field.field @"maybe'resignCommitteeColdCert"
maybe'script ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'script" a) =>
  Lens.Family2.LensLike' f s a
maybe'script = Data.ProtoLens.Field.field @"maybe'script"
maybe'scriptAll ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'scriptAll" a) =>
  Lens.Family2.LensLike' f s a
maybe'scriptAll = Data.ProtoLens.Field.field @"maybe'scriptAll"
maybe'scriptAny ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'scriptAny" a) =>
  Lens.Family2.LensLike' f s a
maybe'scriptAny = Data.ProtoLens.Field.field @"maybe'scriptAny"
maybe'scriptHash ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'scriptHash" a) =>
  Lens.Family2.LensLike' f s a
maybe'scriptHash = Data.ProtoLens.Field.field @"maybe'scriptHash"
maybe'scriptNOfK ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'scriptNOfK" a) =>
  Lens.Family2.LensLike' f s a
maybe'scriptNOfK = Data.ProtoLens.Field.field @"maybe'scriptNOfK"
maybe'scriptPubkeyHash ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'scriptPubkeyHash" a) =>
  Lens.Family2.LensLike' f s a
maybe'scriptPubkeyHash
  = Data.ProtoLens.Field.field @"maybe'scriptPubkeyHash"
maybe'softforkRule ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'softforkRule" a) =>
  Lens.Family2.LensLike' f s a
maybe'softforkRule
  = Data.ProtoLens.Field.field @"maybe'softforkRule"
maybe'stakeCredential ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'stakeCredential" a) =>
  Lens.Family2.LensLike' f s a
maybe'stakeCredential
  = Data.ProtoLens.Field.field @"maybe'stakeCredential"
maybe'stakeDelegation ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'stakeDelegation" a) =>
  Lens.Family2.LensLike' f s a
maybe'stakeDelegation
  = Data.ProtoLens.Field.field @"maybe'stakeDelegation"
maybe'stakeDeregistration ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'stakeDeregistration" a) =>
  Lens.Family2.LensLike' f s a
maybe'stakeDeregistration
  = Data.ProtoLens.Field.field @"maybe'stakeDeregistration"
maybe'stakeKeyDeposit ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'stakeKeyDeposit" a) =>
  Lens.Family2.LensLike' f s a
maybe'stakeKeyDeposit
  = Data.ProtoLens.Field.field @"maybe'stakeKeyDeposit"
maybe'stakeRegDelegCert ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'stakeRegDelegCert" a) =>
  Lens.Family2.LensLike' f s a
maybe'stakeRegDelegCert
  = Data.ProtoLens.Field.field @"maybe'stakeRegDelegCert"
maybe'stakeRegistration ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'stakeRegistration" a) =>
  Lens.Family2.LensLike' f s a
maybe'stakeRegistration
  = Data.ProtoLens.Field.field @"maybe'stakeRegistration"
maybe'stakeVoteDelegCert ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'stakeVoteDelegCert" a) =>
  Lens.Family2.LensLike' f s a
maybe'stakeVoteDelegCert
  = Data.ProtoLens.Field.field @"maybe'stakeVoteDelegCert"
maybe'stakeVoteRegDelegCert ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'stakeVoteRegDelegCert" a) =>
  Lens.Family2.LensLike' f s a
maybe'stakeVoteRegDelegCert
  = Data.ProtoLens.Field.field @"maybe'stakeVoteRegDelegCert"
maybe'start ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'start" a) =>
  Lens.Family2.LensLike' f s a
maybe'start = Data.ProtoLens.Field.field @"maybe'start"
maybe'steps ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'steps" a) =>
  Lens.Family2.LensLike' f s a
maybe'steps = Data.ProtoLens.Field.field @"maybe'steps"
maybe'text ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'text" a) =>
  Lens.Family2.LensLike' f s a
maybe'text = Data.ProtoLens.Field.field @"maybe'text"
maybe'threshold ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'threshold" a) =>
  Lens.Family2.LensLike' f s a
maybe'threshold = Data.ProtoLens.Field.field @"maybe'threshold"
maybe'totalCollateral ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'totalCollateral" a) =>
  Lens.Family2.LensLike' f s a
maybe'totalCollateral
  = Data.ProtoLens.Field.field @"maybe'totalCollateral"
maybe'treasuryExpansion ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'treasuryExpansion" a) =>
  Lens.Family2.LensLike' f s a
maybe'treasuryExpansion
  = Data.ProtoLens.Field.field @"maybe'treasuryExpansion"
maybe'treasuryWithdrawal ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'treasuryWithdrawal" a) =>
  Lens.Family2.LensLike' f s a
maybe'treasuryWithdrawal
  = Data.ProtoLens.Field.field @"maybe'treasuryWithdrawal"
maybe'treasuryWithdrawalsAction ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'treasuryWithdrawalsAction" a) =>
  Lens.Family2.LensLike' f s a
maybe'treasuryWithdrawalsAction
  = Data.ProtoLens.Field.field @"maybe'treasuryWithdrawalsAction"
maybe'txFeePolicy ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'txFeePolicy" a) =>
  Lens.Family2.LensLike' f s a
maybe'txFeePolicy = Data.ProtoLens.Field.field @"maybe'txFeePolicy"
maybe'unregCert ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'unregCert" a) =>
  Lens.Family2.LensLike' f s a
maybe'unregCert = Data.ProtoLens.Field.field @"maybe'unregCert"
maybe'unregDrepCert ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'unregDrepCert" a) =>
  Lens.Family2.LensLike' f s a
maybe'unregDrepCert
  = Data.ProtoLens.Field.field @"maybe'unregDrepCert"
maybe'updateCommitteeAction ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'updateCommitteeAction" a) =>
  Lens.Family2.LensLike' f s a
maybe'updateCommitteeAction
  = Data.ProtoLens.Field.field @"maybe'updateCommitteeAction"
maybe'updateDrepCert ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'updateDrepCert" a) =>
  Lens.Family2.LensLike' f s a
maybe'updateDrepCert
  = Data.ProtoLens.Field.field @"maybe'updateDrepCert"
maybe'updateToConstitution ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'updateToConstitution" a) =>
  Lens.Family2.LensLike' f s a
maybe'updateToConstitution
  = Data.ProtoLens.Field.field @"maybe'updateToConstitution"
maybe'validity ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'validity" a) =>
  Lens.Family2.LensLike' f s a
maybe'validity = Data.ProtoLens.Field.field @"maybe'validity"
maybe'value ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'value" a) =>
  Lens.Family2.LensLike' f s a
maybe'value = Data.ProtoLens.Field.field @"maybe'value"
maybe'voteDelegCert ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'voteDelegCert" a) =>
  Lens.Family2.LensLike' f s a
maybe'voteDelegCert
  = Data.ProtoLens.Field.field @"maybe'voteDelegCert"
maybe'voteRegDelegCert ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'voteRegDelegCert" a) =>
  Lens.Family2.LensLike' f s a
maybe'voteRegDelegCert
  = Data.ProtoLens.Field.field @"maybe'voteRegDelegCert"
maybe'witnesses ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'witnesses" a) =>
  Lens.Family2.LensLike' f s a
maybe'witnesses = Data.ProtoLens.Field.field @"maybe'witnesses"
members ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "members" a) =>
  Lens.Family2.LensLike' f s a
members = Data.ProtoLens.Field.field @"members"
memory ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "memory" a) =>
  Lens.Family2.LensLike' f s a
memory = Data.ProtoLens.Field.field @"memory"
metadata ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "metadata" a) =>
  Lens.Family2.LensLike' f s a
metadata = Data.ProtoLens.Field.field @"metadata"
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
minFeeRefScriptCostPerByte ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "minFeeRefScriptCostPerByte" a) =>
  Lens.Family2.LensLike' f s a
minFeeRefScriptCostPerByte
  = Data.ProtoLens.Field.field @"minFeeRefScriptCostPerByte"
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
minThd ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "minThd" a) =>
  Lens.Family2.LensLike' f s a
minThd = Data.ProtoLens.Field.field @"minThd"
minor ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "minor" a) =>
  Lens.Family2.LensLike' f s a
minor = Data.ProtoLens.Field.field @"minor"
mint ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "mint" a) =>
  Lens.Family2.LensLike' f s a
mint = Data.ProtoLens.Field.field @"mint"
mintsAsset ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "mintsAsset" a) =>
  Lens.Family2.LensLike' f s a
mintsAsset = Data.ProtoLens.Field.field @"mintsAsset"
mirCert ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "mirCert" a) =>
  Lens.Family2.LensLike' f s a
mirCert = Data.ProtoLens.Field.field @"mirCert"
monetaryExpansion ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "monetaryExpansion" a) =>
  Lens.Family2.LensLike' f s a
monetaryExpansion = Data.ProtoLens.Field.field @"monetaryExpansion"
motionNoConfidence ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "motionNoConfidence" a) =>
  Lens.Family2.LensLike' f s a
motionNoConfidence
  = Data.ProtoLens.Field.field @"motionNoConfidence"
movesAsset ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "movesAsset" a) =>
  Lens.Family2.LensLike' f s a
movesAsset = Data.ProtoLens.Field.field @"movesAsset"
mpcThd ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "mpcThd" a) =>
  Lens.Family2.LensLike' f s a
mpcThd = Data.ProtoLens.Field.field @"mpcThd"
msg ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "msg" a) =>
  Lens.Family2.LensLike' f s a
msg = Data.ProtoLens.Field.field @"msg"
multiplier ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "multiplier" a) =>
  Lens.Family2.LensLike' f s a
multiplier = Data.ProtoLens.Field.field @"multiplier"
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
networkId ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "networkId" a) =>
  Lens.Family2.LensLike' f s a
networkId = Data.ProtoLens.Field.field @"networkId"
networkMagic ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "networkMagic" a) =>
  Lens.Family2.LensLike' f s a
networkMagic = Data.ProtoLens.Field.field @"networkMagic"
newCommitteeCredentials ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "newCommitteeCredentials" a) =>
  Lens.Family2.LensLike' f s a
newCommitteeCredentials
  = Data.ProtoLens.Field.field @"newCommitteeCredentials"
newCommitteeThreshold ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "newCommitteeThreshold" a) =>
  Lens.Family2.LensLike' f s a
newCommitteeThreshold
  = Data.ProtoLens.Field.field @"newCommitteeThreshold"
newConstitutionAction ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "newConstitutionAction" a) =>
  Lens.Family2.LensLike' f s a
newConstitutionAction
  = Data.ProtoLens.Field.field @"newConstitutionAction"
noConfidence ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "noConfidence" a) =>
  Lens.Family2.LensLike' f s a
noConfidence = Data.ProtoLens.Field.field @"noConfidence"
noConfidenceAction ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "noConfidenceAction" a) =>
  Lens.Family2.LensLike' f s a
noConfidenceAction
  = Data.ProtoLens.Field.field @"noConfidenceAction"
nonAvvmBalances ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "nonAvvmBalances" a) =>
  Lens.Family2.LensLike' f s a
nonAvvmBalances = Data.ProtoLens.Field.field @"nonAvvmBalances"
numerator ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "numerator" a) =>
  Lens.Family2.LensLike' f s a
numerator = Data.ProtoLens.Field.field @"numerator"
omega ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "omega" a) =>
  Lens.Family2.LensLike' f s a
omega = Data.ProtoLens.Field.field @"omega"
operator ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "operator" a) =>
  Lens.Family2.LensLike' f s a
operator = Data.ProtoLens.Field.field @"operator"
originalCbor ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "originalCbor" a) =>
  Lens.Family2.LensLike' f s a
originalCbor = Data.ProtoLens.Field.field @"originalCbor"
otherPot ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "otherPot" a) =>
  Lens.Family2.LensLike' f s a
otherPot = Data.ProtoLens.Field.field @"otherPot"
outputIndex ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "outputIndex" a) =>
  Lens.Family2.LensLike' f s a
outputIndex = Data.ProtoLens.Field.field @"outputIndex"
outputs ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "outputs" a) =>
  Lens.Family2.LensLike' f s a
outputs = Data.ProtoLens.Field.field @"outputs"
pairs ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "pairs" a) =>
  Lens.Family2.LensLike' f s a
pairs = Data.ProtoLens.Field.field @"pairs"
parameterChangeAction ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "parameterChangeAction" a) =>
  Lens.Family2.LensLike' f s a
parameterChangeAction
  = Data.ProtoLens.Field.field @"parameterChangeAction"
payload ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "payload" a) =>
  Lens.Family2.LensLike' f s a
payload = Data.ProtoLens.Field.field @"payload"
paymentPart ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "paymentPart" a) =>
  Lens.Family2.LensLike' f s a
paymentPart = Data.ProtoLens.Field.field @"paymentPart"
pledge ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "pledge" a) =>
  Lens.Family2.LensLike' f s a
pledge = Data.ProtoLens.Field.field @"pledge"
plutusDatums ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "plutusDatums" a) =>
  Lens.Family2.LensLike' f s a
plutusDatums = Data.ProtoLens.Field.field @"plutusDatums"
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
policyHash ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "policyHash" a) =>
  Lens.Family2.LensLike' f s a
policyHash = Data.ProtoLens.Field.field @"policyHash"
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
poolKeyhash ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "poolKeyhash" a) =>
  Lens.Family2.LensLike' f s a
poolKeyhash = Data.ProtoLens.Field.field @"poolKeyhash"
poolMetadata ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "poolMetadata" a) =>
  Lens.Family2.LensLike' f s a
poolMetadata = Data.ProtoLens.Field.field @"poolMetadata"
poolOwners ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "poolOwners" a) =>
  Lens.Family2.LensLike' f s a
poolOwners = Data.ProtoLens.Field.field @"poolOwners"
poolRegistration ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "poolRegistration" a) =>
  Lens.Family2.LensLike' f s a
poolRegistration = Data.ProtoLens.Field.field @"poolRegistration"
poolRetirement ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "poolRetirement" a) =>
  Lens.Family2.LensLike' f s a
poolRetirement = Data.ProtoLens.Field.field @"poolRetirement"
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
port ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "port" a) =>
  Lens.Family2.LensLike' f s a
port = Data.ProtoLens.Field.field @"port"
ppEconomicGroup ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "ppEconomicGroup" a) =>
  Lens.Family2.LensLike' f s a
ppEconomicGroup = Data.ProtoLens.Field.field @"ppEconomicGroup"
ppGovGroup ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "ppGovGroup" a) =>
  Lens.Family2.LensLike' f s a
ppGovGroup = Data.ProtoLens.Field.field @"ppGovGroup"
ppNetworkGroup ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "ppNetworkGroup" a) =>
  Lens.Family2.LensLike' f s a
ppNetworkGroup = Data.ProtoLens.Field.field @"ppNetworkGroup"
ppSecurityGroup ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "ppSecurityGroup" a) =>
  Lens.Family2.LensLike' f s a
ppSecurityGroup = Data.ProtoLens.Field.field @"ppSecurityGroup"
ppTechnicalGroup ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "ppTechnicalGroup" a) =>
  Lens.Family2.LensLike' f s a
ppTechnicalGroup = Data.ProtoLens.Field.field @"ppTechnicalGroup"
prices ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "prices" a) =>
  Lens.Family2.LensLike' f s a
prices = Data.ProtoLens.Field.field @"prices"
produces ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "produces" a) =>
  Lens.Family2.LensLike' f s a
produces = Data.ProtoLens.Field.field @"produces"
proposals ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "proposals" a) =>
  Lens.Family2.LensLike' f s a
proposals = Data.ProtoLens.Field.field @"proposals"
protocolConsts ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "protocolConsts" a) =>
  Lens.Family2.LensLike' f s a
protocolConsts = Data.ProtoLens.Field.field @"protocolConsts"
protocolMagic ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "protocolMagic" a) =>
  Lens.Family2.LensLike' f s a
protocolMagic = Data.ProtoLens.Field.field @"protocolMagic"
protocolParamUpdate ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "protocolParamUpdate" a) =>
  Lens.Family2.LensLike' f s a
protocolParamUpdate
  = Data.ProtoLens.Field.field @"protocolParamUpdate"
protocolParams ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "protocolParams" a) =>
  Lens.Family2.LensLike' f s a
protocolParams = Data.ProtoLens.Field.field @"protocolParams"
protocolVersion ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "protocolVersion" a) =>
  Lens.Family2.LensLike' f s a
protocolVersion = Data.ProtoLens.Field.field @"protocolVersion"
purpose ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "purpose" a) =>
  Lens.Family2.LensLike' f s a
purpose = Data.ProtoLens.Field.field @"purpose"
quantity ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "quantity" a) =>
  Lens.Family2.LensLike' f s a
quantity = Data.ProtoLens.Field.field @"quantity"
redeemer ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "redeemer" a) =>
  Lens.Family2.LensLike' f s a
redeemer = Data.ProtoLens.Field.field @"redeemer"
redeemers ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "redeemers" a) =>
  Lens.Family2.LensLike' f s a
redeemers = Data.ProtoLens.Field.field @"redeemers"
referenceInputs ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "referenceInputs" a) =>
  Lens.Family2.LensLike' f s a
referenceInputs = Data.ProtoLens.Field.field @"referenceInputs"
regCert ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "regCert" a) =>
  Lens.Family2.LensLike' f s a
regCert = Data.ProtoLens.Field.field @"regCert"
regDrepCert ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "regDrepCert" a) =>
  Lens.Family2.LensLike' f s a
regDrepCert = Data.ProtoLens.Field.field @"regDrepCert"
relays ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "relays" a) =>
  Lens.Family2.LensLike' f s a
relays = Data.ProtoLens.Field.field @"relays"
removeCommitteeCredentials ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "removeCommitteeCredentials" a) =>
  Lens.Family2.LensLike' f s a
removeCommitteeCredentials
  = Data.ProtoLens.Field.field @"removeCommitteeCredentials"
resignCommitteeColdCert ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "resignCommitteeColdCert" a) =>
  Lens.Family2.LensLike' f s a
resignCommitteeColdCert
  = Data.ProtoLens.Field.field @"resignCommitteeColdCert"
rewardAccount ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "rewardAccount" a) =>
  Lens.Family2.LensLike' f s a
rewardAccount = Data.ProtoLens.Field.field @"rewardAccount"
script ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "script" a) =>
  Lens.Family2.LensLike' f s a
script = Data.ProtoLens.Field.field @"script"
scriptAll ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "scriptAll" a) =>
  Lens.Family2.LensLike' f s a
scriptAll = Data.ProtoLens.Field.field @"scriptAll"
scriptAny ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "scriptAny" a) =>
  Lens.Family2.LensLike' f s a
scriptAny = Data.ProtoLens.Field.field @"scriptAny"
scriptHash ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "scriptHash" a) =>
  Lens.Family2.LensLike' f s a
scriptHash = Data.ProtoLens.Field.field @"scriptHash"
scriptNOfK ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "scriptNOfK" a) =>
  Lens.Family2.LensLike' f s a
scriptNOfK = Data.ProtoLens.Field.field @"scriptNOfK"
scriptPubkeyHash ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "scriptPubkeyHash" a) =>
  Lens.Family2.LensLike' f s a
scriptPubkeyHash = Data.ProtoLens.Field.field @"scriptPubkeyHash"
scriptVersion ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "scriptVersion" a) =>
  Lens.Family2.LensLike' f s a
scriptVersion = Data.ProtoLens.Field.field @"scriptVersion"
scripts ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "scripts" a) =>
  Lens.Family2.LensLike' f s a
scripts = Data.ProtoLens.Field.field @"scripts"
securityParam ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "securityParam" a) =>
  Lens.Family2.LensLike' f s a
securityParam = Data.ProtoLens.Field.field @"securityParam"
signature ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "signature" a) =>
  Lens.Family2.LensLike' f s a
signature = Data.ProtoLens.Field.field @"signature"
signingKey ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "signingKey" a) =>
  Lens.Family2.LensLike' f s a
signingKey = Data.ProtoLens.Field.field @"signingKey"
slot ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "slot" a) =>
  Lens.Family2.LensLike' f s a
slot = Data.ProtoLens.Field.field @"slot"
slotDuration ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "slotDuration" a) =>
  Lens.Family2.LensLike' f s a
slotDuration = Data.ProtoLens.Field.field @"slotDuration"
slotLength ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "slotLength" a) =>
  Lens.Family2.LensLike' f s a
slotLength = Data.ProtoLens.Field.field @"slotLength"
slotsPerKesPeriod ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "slotsPerKesPeriod" a) =>
  Lens.Family2.LensLike' f s a
slotsPerKesPeriod = Data.ProtoLens.Field.field @"slotsPerKesPeriod"
softforkRule ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "softforkRule" a) =>
  Lens.Family2.LensLike' f s a
softforkRule = Data.ProtoLens.Field.field @"softforkRule"
stakeCredential ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "stakeCredential" a) =>
  Lens.Family2.LensLike' f s a
stakeCredential = Data.ProtoLens.Field.field @"stakeCredential"
stakeDelegation ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "stakeDelegation" a) =>
  Lens.Family2.LensLike' f s a
stakeDelegation = Data.ProtoLens.Field.field @"stakeDelegation"
stakeDeregistration ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "stakeDeregistration" a) =>
  Lens.Family2.LensLike' f s a
stakeDeregistration
  = Data.ProtoLens.Field.field @"stakeDeregistration"
stakeKeyDeposit ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "stakeKeyDeposit" a) =>
  Lens.Family2.LensLike' f s a
stakeKeyDeposit = Data.ProtoLens.Field.field @"stakeKeyDeposit"
stakeRegDelegCert ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "stakeRegDelegCert" a) =>
  Lens.Family2.LensLike' f s a
stakeRegDelegCert = Data.ProtoLens.Field.field @"stakeRegDelegCert"
stakeRegistration ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "stakeRegistration" a) =>
  Lens.Family2.LensLike' f s a
stakeRegistration = Data.ProtoLens.Field.field @"stakeRegistration"
stakeVoteDelegCert ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "stakeVoteDelegCert" a) =>
  Lens.Family2.LensLike' f s a
stakeVoteDelegCert
  = Data.ProtoLens.Field.field @"stakeVoteDelegCert"
stakeVoteRegDelegCert ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "stakeVoteRegDelegCert" a) =>
  Lens.Family2.LensLike' f s a
stakeVoteRegDelegCert
  = Data.ProtoLens.Field.field @"stakeVoteRegDelegCert"
start ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "start" a) =>
  Lens.Family2.LensLike' f s a
start = Data.ProtoLens.Field.field @"start"
startTime ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "startTime" a) =>
  Lens.Family2.LensLike' f s a
startTime = Data.ProtoLens.Field.field @"startTime"
steps ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "steps" a) =>
  Lens.Family2.LensLike' f s a
steps = Data.ProtoLens.Field.field @"steps"
successful ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "successful" a) =>
  Lens.Family2.LensLike' f s a
successful = Data.ProtoLens.Field.field @"successful"
summand ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "summand" a) =>
  Lens.Family2.LensLike' f s a
summand = Data.ProtoLens.Field.field @"summand"
summaries ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "summaries" a) =>
  Lens.Family2.LensLike' f s a
summaries = Data.ProtoLens.Field.field @"summaries"
systemStart ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "systemStart" a) =>
  Lens.Family2.LensLike' f s a
systemStart = Data.ProtoLens.Field.field @"systemStart"
tag ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "tag" a) =>
  Lens.Family2.LensLike' f s a
tag = Data.ProtoLens.Field.field @"tag"
text ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "text" a) =>
  Lens.Family2.LensLike' f s a
text = Data.ProtoLens.Field.field @"text"
thdDecrement ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "thdDecrement" a) =>
  Lens.Family2.LensLike' f s a
thdDecrement = Data.ProtoLens.Field.field @"thdDecrement"
threshold ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "threshold" a) =>
  Lens.Family2.LensLike' f s a
threshold = Data.ProtoLens.Field.field @"threshold"
thresholds ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "thresholds" a) =>
  Lens.Family2.LensLike' f s a
thresholds = Data.ProtoLens.Field.field @"thresholds"
time ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "time" a) =>
  Lens.Family2.LensLike' f s a
time = Data.ProtoLens.Field.field @"time"
timestamp ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "timestamp" a) =>
  Lens.Family2.LensLike' f s a
timestamp = Data.ProtoLens.Field.field @"timestamp"
to ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "to" a) =>
  Lens.Family2.LensLike' f s a
to = Data.ProtoLens.Field.field @"to"
totalCollateral ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "totalCollateral" a) =>
  Lens.Family2.LensLike' f s a
totalCollateral = Data.ProtoLens.Field.field @"totalCollateral"
traces ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "traces" a) =>
  Lens.Family2.LensLike' f s a
traces = Data.ProtoLens.Field.field @"traces"
transactionId ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "transactionId" a) =>
  Lens.Family2.LensLike' f s a
transactionId = Data.ProtoLens.Field.field @"transactionId"
treasuryExpansion ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "treasuryExpansion" a) =>
  Lens.Family2.LensLike' f s a
treasuryExpansion = Data.ProtoLens.Field.field @"treasuryExpansion"
treasuryWithdrawal ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "treasuryWithdrawal" a) =>
  Lens.Family2.LensLike' f s a
treasuryWithdrawal
  = Data.ProtoLens.Field.field @"treasuryWithdrawal"
treasuryWithdrawalsAction ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "treasuryWithdrawalsAction" a) =>
  Lens.Family2.LensLike' f s a
treasuryWithdrawalsAction
  = Data.ProtoLens.Field.field @"treasuryWithdrawalsAction"
ttl ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "ttl" a) =>
  Lens.Family2.LensLike' f s a
ttl = Data.ProtoLens.Field.field @"ttl"
tx ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "tx" a) =>
  Lens.Family2.LensLike' f s a
tx = Data.ProtoLens.Field.field @"tx"
txFeePolicy ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "txFeePolicy" a) =>
  Lens.Family2.LensLike' f s a
txFeePolicy = Data.ProtoLens.Field.field @"txFeePolicy"
txHash ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "txHash" a) =>
  Lens.Family2.LensLike' f s a
txHash = Data.ProtoLens.Field.field @"txHash"
unlockStakeEpoch ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "unlockStakeEpoch" a) =>
  Lens.Family2.LensLike' f s a
unlockStakeEpoch = Data.ProtoLens.Field.field @"unlockStakeEpoch"
unregCert ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "unregCert" a) =>
  Lens.Family2.LensLike' f s a
unregCert = Data.ProtoLens.Field.field @"unregCert"
unregDrepCert ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "unregDrepCert" a) =>
  Lens.Family2.LensLike' f s a
unregDrepCert = Data.ProtoLens.Field.field @"unregDrepCert"
updateCommitteeAction ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "updateCommitteeAction" a) =>
  Lens.Family2.LensLike' f s a
updateCommitteeAction
  = Data.ProtoLens.Field.field @"updateCommitteeAction"
updateDrepCert ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "updateDrepCert" a) =>
  Lens.Family2.LensLike' f s a
updateDrepCert = Data.ProtoLens.Field.field @"updateDrepCert"
updateImplicit ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "updateImplicit" a) =>
  Lens.Family2.LensLike' f s a
updateImplicit = Data.ProtoLens.Field.field @"updateImplicit"
updateProposalThd ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "updateProposalThd" a) =>
  Lens.Family2.LensLike' f s a
updateProposalThd = Data.ProtoLens.Field.field @"updateProposalThd"
updateQuorum ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "updateQuorum" a) =>
  Lens.Family2.LensLike' f s a
updateQuorum = Data.ProtoLens.Field.field @"updateQuorum"
updateToConstitution ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "updateToConstitution" a) =>
  Lens.Family2.LensLike' f s a
updateToConstitution
  = Data.ProtoLens.Field.field @"updateToConstitution"
updateVoteThd ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "updateVoteThd" a) =>
  Lens.Family2.LensLike' f s a
updateVoteThd = Data.ProtoLens.Field.field @"updateVoteThd"
url ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "url" a) =>
  Lens.Family2.LensLike' f s a
url = Data.ProtoLens.Field.field @"url"
validity ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "validity" a) =>
  Lens.Family2.LensLike' f s a
validity = Data.ProtoLens.Field.field @"validity"
value ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "value" a) =>
  Lens.Family2.LensLike' f s a
value = Data.ProtoLens.Field.field @"value"
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
vec'bootstrapWitnesses ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'bootstrapWitnesses" a) =>
  Lens.Family2.LensLike' f s a
vec'bootstrapWitnesses
  = Data.ProtoLens.Field.field @"vec'bootstrapWitnesses"
vec'certificates ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'certificates" a) =>
  Lens.Family2.LensLike' f s a
vec'certificates = Data.ProtoLens.Field.field @"vec'certificates"
vec'collateral ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'collateral" a) =>
  Lens.Family2.LensLike' f s a
vec'collateral = Data.ProtoLens.Field.field @"vec'collateral"
vec'errors ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'errors" a) =>
  Lens.Family2.LensLike' f s a
vec'errors = Data.ProtoLens.Field.field @"vec'errors"
vec'fields ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'fields" a) =>
  Lens.Family2.LensLike' f s a
vec'fields = Data.ProtoLens.Field.field @"vec'fields"
vec'inputs ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'inputs" a) =>
  Lens.Family2.LensLike' f s a
vec'inputs = Data.ProtoLens.Field.field @"vec'inputs"
vec'items ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'items" a) =>
  Lens.Family2.LensLike' f s a
vec'items = Data.ProtoLens.Field.field @"vec'items"
vec'metadata ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'metadata" a) =>
  Lens.Family2.LensLike' f s a
vec'metadata = Data.ProtoLens.Field.field @"vec'metadata"
vec'mint ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'mint" a) =>
  Lens.Family2.LensLike' f s a
vec'mint = Data.ProtoLens.Field.field @"vec'mint"
vec'newCommitteeCredentials ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'newCommitteeCredentials" a) =>
  Lens.Family2.LensLike' f s a
vec'newCommitteeCredentials
  = Data.ProtoLens.Field.field @"vec'newCommitteeCredentials"
vec'outputs ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'outputs" a) =>
  Lens.Family2.LensLike' f s a
vec'outputs = Data.ProtoLens.Field.field @"vec'outputs"
vec'pairs ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'pairs" a) =>
  Lens.Family2.LensLike' f s a
vec'pairs = Data.ProtoLens.Field.field @"vec'pairs"
vec'plutusDatums ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'plutusDatums" a) =>
  Lens.Family2.LensLike' f s a
vec'plutusDatums = Data.ProtoLens.Field.field @"vec'plutusDatums"
vec'poolOwners ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'poolOwners" a) =>
  Lens.Family2.LensLike' f s a
vec'poolOwners = Data.ProtoLens.Field.field @"vec'poolOwners"
vec'proposals ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'proposals" a) =>
  Lens.Family2.LensLike' f s a
vec'proposals = Data.ProtoLens.Field.field @"vec'proposals"
vec'redeemers ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'redeemers" a) =>
  Lens.Family2.LensLike' f s a
vec'redeemers = Data.ProtoLens.Field.field @"vec'redeemers"
vec'referenceInputs ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'referenceInputs" a) =>
  Lens.Family2.LensLike' f s a
vec'referenceInputs
  = Data.ProtoLens.Field.field @"vec'referenceInputs"
vec'relays ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'relays" a) =>
  Lens.Family2.LensLike' f s a
vec'relays = Data.ProtoLens.Field.field @"vec'relays"
vec'removeCommitteeCredentials ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'removeCommitteeCredentials" a) =>
  Lens.Family2.LensLike' f s a
vec'removeCommitteeCredentials
  = Data.ProtoLens.Field.field @"vec'removeCommitteeCredentials"
vec'script ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'script" a) =>
  Lens.Family2.LensLike' f s a
vec'script = Data.ProtoLens.Field.field @"vec'script"
vec'scripts ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'scripts" a) =>
  Lens.Family2.LensLike' f s a
vec'scripts = Data.ProtoLens.Field.field @"vec'scripts"
vec'summaries ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'summaries" a) =>
  Lens.Family2.LensLike' f s a
vec'summaries = Data.ProtoLens.Field.field @"vec'summaries"
vec'thresholds ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'thresholds" a) =>
  Lens.Family2.LensLike' f s a
vec'thresholds = Data.ProtoLens.Field.field @"vec'thresholds"
vec'to ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "vec'to" a) =>
  Lens.Family2.LensLike' f s a
vec'to = Data.ProtoLens.Field.field @"vec'to"
vec'traces ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'traces" a) =>
  Lens.Family2.LensLike' f s a
vec'traces = Data.ProtoLens.Field.field @"vec'traces"
vec'tx ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "vec'tx" a) =>
  Lens.Family2.LensLike' f s a
vec'tx = Data.ProtoLens.Field.field @"vec'tx"
vec'values ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'values" a) =>
  Lens.Family2.LensLike' f s a
vec'values = Data.ProtoLens.Field.field @"vec'values"
vec'vkeywitness ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'vkeywitness" a) =>
  Lens.Family2.LensLike' f s a
vec'vkeywitness = Data.ProtoLens.Field.field @"vec'vkeywitness"
vec'withdrawals ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'withdrawals" a) =>
  Lens.Family2.LensLike' f s a
vec'withdrawals = Data.ProtoLens.Field.field @"vec'withdrawals"
vkey ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "vkey" a) =>
  Lens.Family2.LensLike' f s a
vkey = Data.ProtoLens.Field.field @"vkey"
vkeywitness ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vkeywitness" a) =>
  Lens.Family2.LensLike' f s a
vkeywitness = Data.ProtoLens.Field.field @"vkeywitness"
voteDelegCert ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "voteDelegCert" a) =>
  Lens.Family2.LensLike' f s a
voteDelegCert = Data.ProtoLens.Field.field @"voteDelegCert"
voteRegDelegCert ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "voteRegDelegCert" a) =>
  Lens.Family2.LensLike' f s a
voteRegDelegCert = Data.ProtoLens.Field.field @"voteRegDelegCert"
vrf ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "vrf" a) =>
  Lens.Family2.LensLike' f s a
vrf = Data.ProtoLens.Field.field @"vrf"
vrfKeyhash ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vrfKeyhash" a) =>
  Lens.Family2.LensLike' f s a
vrfKeyhash = Data.ProtoLens.Field.field @"vrfKeyhash"
vssCerts ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vssCerts" a) =>
  Lens.Family2.LensLike' f s a
vssCerts = Data.ProtoLens.Field.field @"vssCerts"
vssKey ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "vssKey" a) =>
  Lens.Family2.LensLike' f s a
vssKey = Data.ProtoLens.Field.field @"vssKey"
vssMaxTtl ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vssMaxTtl" a) =>
  Lens.Family2.LensLike' f s a
vssMaxTtl = Data.ProtoLens.Field.field @"vssMaxTtl"
vssMinTtl ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vssMinTtl" a) =>
  Lens.Family2.LensLike' f s a
vssMinTtl = Data.ProtoLens.Field.field @"vssMinTtl"
withdrawals ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "withdrawals" a) =>
  Lens.Family2.LensLike' f s a
withdrawals = Data.ProtoLens.Field.field @"withdrawals"
witnesses ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "witnesses" a) =>
  Lens.Family2.LensLike' f s a
witnesses = Data.ProtoLens.Field.field @"witnesses"