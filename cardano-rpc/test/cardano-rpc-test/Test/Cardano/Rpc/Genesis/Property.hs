{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

-- | Structural property tests for 'Cardano.Rpc.Server.Internal.UtxoRpc.Type.Genesis'.
--
-- These generate arbitrary per-era genesis values (not fixtures) and check
-- that the per-era mapper wires every field to the right place: map sizes and
-- key sets are preserved, scalars survive their documented conversion
-- unchanged, and bounded rationals carry the exact numerator\/denominator of
-- the ledger value. They deliberately do not re-derive the low-level
-- encodings (BigInt varint layout, RationalNumber lossy approximation) that
-- the fixture-based tests already pin down; where those encodings are
-- exercised, the test reuses the production 'inject' helper rather than
-- reimplementing it.
module Test.Cardano.Rpc.Genesis.Property where

import Cardano.Api.Era (Inject (..))
import Cardano.Api.Ledger qualified as L
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as U5c
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.Genesis
  ( alonzoGenesisToProto
  , byronGenesisToProto
  , conwayGenesisToProto
  , shelleyGenesisToProto
  )

import Cardano.Chain.Common qualified as Byron
  ( KeyHash
  , LovelacePortion
  , TxFeePolicy (..)
  , TxSizeLinear (..)
  , lovelacePortionToRational
  , lovelaceToInteger
  , unBlockCount
  , unKeyHash
  )
import Cardano.Chain.Genesis qualified as Byron
  ( GenesisAvvmBalances (..)
  , GenesisData (..)
  , GenesisDelegation (..)
  , GenesisKeyHashes (..)
  , GenesisNonAvvmBalances (..)
  )
import Cardano.Chain.Slotting qualified as Byron (getEpochNumber, unSlotNumber)
import Cardano.Chain.Update qualified as Byron (ProtocolParameters (..), SoftforkRule (..))
import Cardano.Crypto qualified as Byron (hashHexF, unProtocolMagicId)
import Cardano.Ledger.Alonzo.Genesis qualified as L
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.BaseTypes qualified as L
import Cardano.Ledger.Conway.PParams qualified as L
import Cardano.Ledger.Shelley.Genesis qualified as L

import RIO

import Data.Bits (Bits, shiftR)
import Data.Map.Strict qualified as Map
import Data.ProtoLens (defMessage)
import Data.Ratio (denominator, numerator)
import Data.Set qualified as Set
import Data.Time.Calendar (Day (ModifiedJulianDay))
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Formatting (sformat)
import GHC.Exts qualified as Exts (toList)
import Network.GRPC.Spec

import Test.Cardano.Chain.Genesis.Gen (genGenesisData)
import Test.Cardano.Crypto.Gen (genProtocolMagicId)
import Test.Cardano.Ledger.Alonzo.Arbitrary ()
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Shelley.Arbitrary ()
import Test.Cardano.Rpc.ProtocolParameters (clipI, clipIBr, prMemL, prStepsL, pvMinorL)

import Hedgehog as H
import Hedgehog.Extras qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Gen.QuickCheck qualified as Q
import Hedgehog.Range qualified as Range

--------------------------------------------------------------------------------
-- Byron
--------------------------------------------------------------------------------

hprop_genesis_byron :: Property
hprop_genesis_byron = H.property $ do
  protocolMagicId <- H.forAll genProtocolMagicId
  genesisData <- H.forAll (genGenesisData protocolMagicId)

  let proto = byronGenesisToProto genesisData defMessage
      pp = Byron.gdProtocolParameters genesisData
      softfork = Byron.ppSoftforkRule pp

  -- Map sizes: nothing dropped, nothing spuriously merged.
  Map.size (proto ^. U5c.avvmDistr)
    === length (Byron.unGenesisAvvmBalances (Byron.gdAvvmDistr genesisData))
  Map.size (proto ^. U5c.nonAvvmBalances)
    === length (Byron.unGenesisNonAvvmBalances (Byron.gdNonAvvmBalances genesisData))
  Map.size (proto ^. U5c.heavyDelegation)
    === length (Byron.unGenesisDelegation (Byron.gdHeavyDelegation genesisData))

  -- bootStakeholders: every genesis key hash appears with a synthesised weight of 1.
  let genesisKeyHashHexes =
        Set.fromList $
          map byronKeyHashHex (toList (Byron.unGenesisKeyHashes (Byron.gdGenesisKeyHashes genesisData)))
  Map.keysSet (proto ^. U5c.bootStakeholders) === genesisKeyHashHexes
  H.assertWith (proto ^. U5c.bootStakeholders) (all (== 1) . Map.elems)

  -- Fields with no Byron ledger source stay at the proto default.
  H.assertWith (proto ^. U5c.vssCerts) Map.null

  -- Scalars, after the documented conversions.
  proto ^. U5c.startTime === round (utcTimeToPOSIXSeconds (Byron.gdStartTime genesisData))
  proto ^. U5c.protocolConsts . U5c.k === fromIntegral (Byron.unBlockCount (Byron.gdK genesisData))
  proto
    ^. U5c.protocolConsts
      . U5c.protocolMagic
    === Byron.unProtocolMagicId (Byron.gdProtocolMagicId genesisData)

  -- BlockVersionData sub-message.
  proto ^. U5c.blockVersionData . U5c.scriptVersion === fromIntegral (Byron.ppScriptVersion pp)
  proto ^. U5c.blockVersionData . U5c.slotDuration === tshow (Byron.ppSlotDuration pp)
  proto ^. U5c.blockVersionData . U5c.maxBlockSize === tshow (Byron.ppMaxBlockSize pp)
  proto ^. U5c.blockVersionData . U5c.maxHeaderSize === tshow (Byron.ppMaxHeaderSize pp)
  proto ^. U5c.blockVersionData . U5c.maxTxSize === tshow (Byron.ppMaxTxSize pp)
  proto ^. U5c.blockVersionData . U5c.maxProposalSize === tshow (Byron.ppMaxProposalSize pp)
  proto ^. U5c.blockVersionData . U5c.mpcThd === tshow (lovelacePortionWord (Byron.ppMpcThd pp))
  proto
    ^. U5c.blockVersionData
      . U5c.heavyDelThd
    === tshow (lovelacePortionWord (Byron.ppHeavyDelThd pp))
  proto
    ^. U5c.blockVersionData
      . U5c.updateVoteThd
    === tshow (lovelacePortionWord (Byron.ppUpdateVoteThd pp))
  proto
    ^. U5c.blockVersionData
      . U5c.updateProposalThd
    === tshow (lovelacePortionWord (Byron.ppUpdateProposalThd pp))
  proto
    ^. U5c.blockVersionData
      . U5c.updateImplicit
    === tshow (Byron.unSlotNumber (Byron.ppUpdateProposalTTL pp))
  proto
    ^. U5c.blockVersionData
      . U5c.unlockStakeEpoch
    === tshow (Byron.getEpochNumber (Byron.ppUnlockStakeEpoch pp))

  -- SoftforkRule sub-message.
  proto
    ^. U5c.blockVersionData
      . U5c.softforkRule
      . U5c.initThd
    === tshow (lovelacePortionWord (Byron.srInitThd softfork))
  proto
    ^. U5c.blockVersionData
      . U5c.softforkRule
      . U5c.minThd
    === tshow (lovelacePortionWord (Byron.srMinThd softfork))
  proto
    ^. U5c.blockVersionData
      . U5c.softforkRule
      . U5c.thdDecrement
    === tshow (lovelacePortionWord (Byron.srThdDecrement softfork))

  -- TxFeePolicy sub-message ('TxFeePolicy' has a single constructor).
  case Byron.ppTxFeePolicy pp of
    Byron.TxFeePolicyTxSizeLinear (Byron.TxSizeLinear constant multiplier) -> do
      proto
        ^. U5c.blockVersionData
          . U5c.txFeePolicy
          . U5c.summand
        === tshow (1_000_000_000 * Byron.lovelaceToInteger constant)
      proto
        ^. U5c.blockVersionData
          . U5c.txFeePolicy
          . U5c.multiplier
        === tshow (floor (1_000_000_000 * multiplier) :: Integer)

--------------------------------------------------------------------------------
-- Shelley
--------------------------------------------------------------------------------

hprop_genesis_shelley :: Property
hprop_genesis_shelley = H.property $ do
  systemStart <- H.forAll genSmallUTCTime
  rawGenesis <- H.forAll (Gen.resize genesisGenSize (Q.arbitrary @L.ShelleyGenesis))
  let genesis = (clipShelleyGenesis rawGenesis){L.sgSystemStart = systemStart}
      proto = shelleyGenesisToProto genesis defMessage
      pp = L.sgProtocolParams genesis

  -- Map sizes.
  Map.size (proto ^. U5c.genDelegs) === Map.size (L.sgGenDelegs genesis)
  Map.size (proto ^. U5c.initialFunds) === length (Exts.toList (L.sgInitialFunds genesis))

  -- Scalars, after the documented conversions.
  proto ^. U5c.epochLength === fromIntegral (L.unEpochSize (L.sgEpochLength genesis))
  proto ^. U5c.maxKesEvolutions === fromIntegral (L.sgMaxKESEvolutions genesis)
  proto ^. U5c.slotsPerKesPeriod === fromIntegral (L.sgSlotsPerKESPeriod genesis)
  proto ^. U5c.updateQuorum === fromIntegral (L.sgUpdateQuorum genesis)
  proto ^. U5c.securityParam === fromIntegral (L.unNonZero (L.sgSecurityParam genesis))
  proto ^. U5c.networkMagic === L.sgNetworkMagic genesis
  proto ^. U5c.maxLovelaceSupply === inject (fromIntegral (L.sgMaxLovelaceSupply genesis) :: Integer)
  proto ^. U5c.slotLength === round (1000 * L.fromNominalDiffTimeMicro (L.sgSlotLength genesis))
  proto
    ^. U5c.networkId
    === case L.sgNetworkId genesis of
      L.Mainnet -> "Mainnet"
      L.Testnet -> "Testnet"

  -- Rationals carry the exact (clipped) numerator/denominator.
  assertRational (L.sgActiveSlotsCoeff genesis) (proto ^. U5c.activeSlotsCoeff)

  -- Protocol params sub-message: every field the era-generic 'EraPParams'
  -- lenses expose.
  proto
    ^. U5c.protocolParams
      . U5c.minFeeCoefficient
    === inject (L.fromCompact (L.unCoinPerByte (pp ^. L.ppTxFeePerByteL)))
  proto ^. U5c.protocolParams . U5c.minFeeConstant === inject (pp ^. L.ppTxFeeFixedL)
  proto ^. U5c.protocolParams . U5c.maxBlockBodySize === fromIntegral (pp ^. L.ppMaxBBSizeL)
  proto ^. U5c.protocolParams . U5c.maxTxSize === fromIntegral (pp ^. L.ppMaxTxSizeL)
  proto ^. U5c.protocolParams . U5c.maxBlockHeaderSize === fromIntegral (pp ^. L.ppMaxBHSizeL)
  proto ^. U5c.protocolParams . U5c.stakeKeyDeposit === inject (pp ^. L.ppKeyDepositL)
  proto ^. U5c.protocolParams . U5c.poolDeposit === inject (pp ^. L.ppPoolDepositL)
  proto
    ^. U5c.protocolParams
      . U5c.poolRetirementEpochBound
    === fromIntegral (L.unEpochInterval (pp ^. L.ppEMaxL))
  proto ^. U5c.protocolParams . U5c.desiredNumberOfPools === fromIntegral (pp ^. L.ppNOptL)
  proto ^. U5c.protocolParams . U5c.minPoolCost === inject (pp ^. L.ppMinPoolCostL)
  proto ^. U5c.protocolParams . U5c.protocolVersion === inject (pp ^. L.ppProtocolVersionL)

  assertRational (pp ^. L.ppA0L) (proto ^. U5c.protocolParams . U5c.poolInfluence)
  assertRational (pp ^. L.ppRhoL) (proto ^. U5c.protocolParams . U5c.monetaryExpansion)
  assertRational (pp ^. L.ppTauL) (proto ^. U5c.protocolParams . U5c.treasuryExpansion)

--------------------------------------------------------------------------------
-- Alonzo
--------------------------------------------------------------------------------

hprop_genesis_alonzo :: Property
hprop_genesis_alonzo = H.property $ do
  -- Capped generator size; see 'genesisGenSize'.
  genesis <- clipAlonzoGenesis <$> H.forAll (Gen.resize genesisGenSize (Q.arbitrary @L.AlonzoGenesis))
  let proto = alonzoGenesisToProto genesis defMessage
      prices = L.agPrices genesis

  proto ^. U5c.lovelacePerUtxoWord === inject (L.unCoinPerWord (L.agCoinsPerUTxOWord genesis))
  proto ^. U5c.maxTxExUnits === inject (L.agMaxTxExUnits genesis)
  proto ^. U5c.maxBlockExUnits === inject (L.agMaxBlockExUnits genesis)
  proto ^. U5c.maxValueSize === L.agMaxValSize genesis
  proto ^. U5c.collateralPercentage === fromIntegral (L.agCollateralPercentage genesis)
  proto ^. U5c.maxCollateralInputs === fromIntegral (L.agMaxCollateralInputs genesis)

  assertRational (L.prSteps prices) (proto ^. U5c.executionPrices . U5c.steps)
  assertRational (L.prMem prices) (proto ^. U5c.executionPrices . U5c.memory)

  -- Only PlutusV1 is wired from the Alonzo genesis; the other cost model
  -- slots stay unset.
  proto
    ^. U5c.costModels
      . U5c.plutusV1
      . U5c.values
    === L.getCostModelParams (L.agPlutusV1CostModel genesis)
  H.assertWith proto (isNothing . (^. U5c.costModels . U5c.maybe'plutusV2))
  H.assertWith proto (isNothing . (^. U5c.costModels . U5c.maybe'plutusV3))
  H.assertWith proto (isNothing . (^. U5c.costModels . U5c.maybe'plutusV4))

--------------------------------------------------------------------------------
-- Conway
--------------------------------------------------------------------------------

hprop_genesis_conway :: Property
hprop_genesis_conway = H.property $ do
  -- Capped generator size; see 'genesisGenSize'.
  genesis <- clipConwayGenesis <$> H.forAll (Gen.resize genesisGenSize (Q.arbitrary @L.ConwayGenesis))
  let proto = conwayGenesisToProto genesis defMessage
      upgrade = L.cgUpgradePParams genesis
      committee = L.cgCommittee genesis

  -- Committee member count.
  Map.size (proto ^. U5c.committee . U5c.members) === Map.size (L.committeeMembers committee)

  -- Committee threshold, exact numerator/denominator.
  assertRational (L.committeeThreshold committee) (proto ^. U5c.committee . U5c.threshold)

  -- Scalars, after the documented conversions.
  proto ^. U5c.committeeMinSize === fromIntegral (L.ucppCommitteeMinSize upgrade)
  proto
    ^. U5c.committeeMaxTermLength
    === fromIntegral (L.unEpochInterval (L.ucppCommitteeMaxTermLength upgrade))
  proto
    ^. U5c.govActionLifetime
    === fromIntegral (L.unEpochInterval (L.ucppGovActionLifetime upgrade))
  proto ^. U5c.drepActivity === fromIntegral (L.unEpochInterval (L.ucppDRepActivity upgrade))
  proto ^. U5c.govActionDeposit === inject (L.ucppGovActionDeposit upgrade)
  proto ^. U5c.drepDeposit === inject (L.ucppDRepDeposit upgrade)

  assertRational
    (L.ucppMinFeeRefScriptCostPerByte upgrade)
    (proto ^. U5c.minFeeRefScriptCostPerByte)

  -- Voting thresholds, exact numerator/denominator per field.
  let poolThresholds = L.ucppPoolVotingThresholds upgrade
      drepThresholds = L.ucppDRepVotingThresholds upgrade
  assertRational
    (poolThresholds ^. L.pvtMotionNoConfidenceL)
    (proto ^. U5c.poolVotingThresholds . U5c.motionNoConfidence)
  assertRational
    (poolThresholds ^. L.pvtCommitteeNormalL)
    (proto ^. U5c.poolVotingThresholds . U5c.committeeNormal)
  assertRational
    (poolThresholds ^. L.pvtCommitteeNoConfidenceL)
    (proto ^. U5c.poolVotingThresholds . U5c.committeeNoConfidence)
  assertRational
    (poolThresholds ^. L.pvtHardForkInitiationL)
    (proto ^. U5c.poolVotingThresholds . U5c.hardForkInitiation)
  assertRational
    (poolThresholds ^. L.pvtPPSecurityGroupL)
    (proto ^. U5c.poolVotingThresholds . U5c.ppSecurityGroup)

  assertRational
    (drepThresholds ^. L.dvtMotionNoConfidenceL)
    (proto ^. U5c.drepVotingThresholds . U5c.motionNoConfidence)
  assertRational
    (drepThresholds ^. L.dvtCommitteeNormalL)
    (proto ^. U5c.drepVotingThresholds . U5c.committeeNormal)
  assertRational
    (drepThresholds ^. L.dvtCommitteeNoConfidenceL)
    (proto ^. U5c.drepVotingThresholds . U5c.committeeNoConfidence)
  assertRational
    (drepThresholds ^. L.dvtUpdateToConstitutionL)
    (proto ^. U5c.drepVotingThresholds . U5c.updateToConstitution)
  assertRational
    (drepThresholds ^. L.dvtHardForkInitiationL)
    (proto ^. U5c.drepVotingThresholds . U5c.hardForkInitiation)
  assertRational
    (drepThresholds ^. L.dvtPPNetworkGroupL)
    (proto ^. U5c.drepVotingThresholds . U5c.ppNetworkGroup)
  assertRational
    (drepThresholds ^. L.dvtPPEconomicGroupL)
    (proto ^. U5c.drepVotingThresholds . U5c.ppEconomicGroup)
  assertRational
    (drepThresholds ^. L.dvtPPTechnicalGroupL)
    (proto ^. U5c.drepVotingThresholds . U5c.ppTechnicalGroup)
  assertRational
    (drepThresholds ^. L.dvtPPGovGroupL)
    (proto ^. U5c.drepVotingThresholds . U5c.ppGovGroup)
  assertRational
    (drepThresholds ^. L.dvtTreasuryWithdrawalL)
    (proto ^. U5c.drepVotingThresholds . U5c.treasuryWithdrawal)

  -- Only PlutusV3 is wired from the Conway genesis.
  proto
    ^. U5c.costModels
      . U5c.plutusV3
      . U5c.values
    === L.getCostModelParams (L.ucppPlutusV3CostModel upgrade)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Assert that a bounded rational's exact numerator and denominator survive
-- the conversion to a proto 'U5c.RationalNumber'. Only valid for values whose
-- numerator and denominator already fit an @int32@\/@uint32@ respectively
-- (see 'clipIBr') - otherwise the production 'inject' encodes a lossy
-- continued-fraction approximation instead.
assertRational :: (MonadTest m, L.BoundedRational a) => a -> Proto U5c.RationalNumber -> m ()
assertRational value protoValue = do
  let r = L.unboundRational value
  protoValue ^. U5c.numerator === fromInteger (numerator r)
  protoValue ^. U5c.denominator === fromInteger (denominator r)

-- | Clip a non-negative Integral value (@Word64@, @Natural@, ...) down to
-- fit within @n@ bits. 'clipI' (from "Test.Cardano.Rpc.ProtocolParameters")
-- is unsuitable for unsigned types: its lower-bound check also compares
-- against a negated literal, which either throws an arithmetic underflow
-- ('Natural') or silently wraps around to an enormous positive threshold
-- that is almost never exceeded ('Word64') - in the latter case the
-- recursive halving never reaches a value below that threshold and loops
-- forever.
clipUnsigned :: (Integral a, Bits a) => Int -> a -> a
clipUnsigned n v
  | v > 2 ^ (n - 1) - 1 = clipUnsigned n (shiftR v 1)
  | otherwise = v

-- | Hedgehog scales the QuickCheck bridge's size up to 99 across a test run,
-- and at that size the ledger's collection-valued 'Arbitrary' instances
-- (genesis delegates, initial funds, stake pools, DReps, ...) generate enough
-- entries that a full run over hundreds of tests takes minutes; structural
-- correspondence only needs a handful of entries per collection to catch
-- wrong-field wiring, so the generator size is capped (mirrors the small
-- bounded ranges the native Hedgehog Byron generator already uses, e.g.
-- @Range.linear 1 10@).
genesisGenSize :: Size
genesisGenSize = 20

-- | Lowercase base16 rendering of a Byron key hash, matching genesis JSON
-- hash keys (mirrors the private @byronKeyHashHex@ in the module under test).
byronKeyHashHex :: Byron.KeyHash -> Text
byronKeyHashHex = sformat Byron.hashHexF . Byron.unKeyHash

-- | The raw Word64 numerator a Byron genesis JSON emits for a
-- 'Byron.LovelacePortion' over a fixed 1e15 denominator (mirrors the private
-- @lovelacePortionWord@ in the module under test).
lovelacePortionWord :: Byron.LovelacePortion -> Word64
lovelacePortionWord = round . (* 1_000_000_000_000_000) . Byron.lovelacePortionToRational

-- | 'Test.Cardano.Ledger.Shelley.Arbitrary's @Arbitrary UTCTime@ (inherited
-- from @quickcheck-instances@) draws the Julian day number from an
-- 'Arbitrary Integer' whose value is cheap to construct but can be extremely
-- expensive to force (large size values yield a huge, lazily-represented
-- magnitude). 'Data.Time''s Gregorian calendar conversion - used by both
-- 'Prelude.show' and 'shelleyGenesisToProto' itself, via @iso8601Show@ - is
-- consequently extremely slow to run on it. Arithmetically clipping the
-- generated value (e.g. via @mod@) does not help, since computing @mod@
-- itself has to force the same expensive magnitude; the only fix is to
-- never generate it in the first place. Mirrors the bounded range the
-- native Hedgehog Byron generator already uses for the same field,
-- @Range.linear 0 1000000@ and @Range.linear 0 86401@.
genSmallUTCTime :: Gen UTCTime
genSmallUTCTime =
  (UTCTime . ModifiedJulianDay <$> Gen.integral (Range.linear 0 1_000_000))
    <*> (secondsToDiffTime <$> Gen.integral (Range.linear 0 86_401))

-- | Clip a freshly-generated 'L.ShelleyGenesis' so every scalar and rational
-- field fits the width of its proto target exactly. Mirrors the clipping
-- done in 'Test.Cardano.Rpc.ProtocolParameters.hprop_roundtrip_protocol_parameters'
-- for the same reason: several Shelley genesis fields are @Word64@ on the
-- ledger side but narrower (@uint32@, or a rational's @int32@\/@uint32@
-- numerator\/denominator) on the proto side. The system start date is
-- overridden by the caller with 'genSmallUTCTime' rather than clipped here;
-- see its Haddock for why.
clipShelleyGenesis :: L.ShelleyGenesis -> L.ShelleyGenesis
clipShelleyGenesis genesis =
  genesis
    { L.sgActiveSlotsCoeff = clipIBr (L.sgActiveSlotsCoeff genesis)
    , L.sgSecurityParam =
        L.unsafeNonZero (max 1 (clipUnsigned 31 (L.unNonZero (L.sgSecurityParam genesis))))
    , L.sgEpochLength = L.EpochSize (clipUnsigned 31 (L.unEpochSize (L.sgEpochLength genesis)))
    , L.sgSlotsPerKESPeriod = clipUnsigned 31 (L.sgSlotsPerKESPeriod genesis)
    , L.sgMaxKESEvolutions = clipUnsigned 31 (L.sgMaxKESEvolutions genesis)
    , L.sgUpdateQuorum = clipUnsigned 31 (L.sgUpdateQuorum genesis)
    , L.sgSlotLength =
        L.secondsToNominalDiffTimeMicro . fromInteger . clipI 20 $
          (round (L.fromNominalDiffTimeMicro (L.sgSlotLength genesis)) :: Integer)
    , L.sgProtocolParams =
        L.sgProtocolParams genesis
          & L.ppA0L %~ clipIBr
          & L.ppRhoL %~ clipIBr
          & L.ppTauL %~ clipIBr
          & L.ppProtocolVersionL . pvMinorL %~ clipUnsigned 20
    }

-- | Clip the ex-unit 'Natural' fields (unbounded on the ledger side) down to
-- the proto's @uint64@ width.
clipExUnits :: L.ExUnits -> L.ExUnits
clipExUnits exUnits =
  L.ExUnits
    { L.exUnitsMem = clipUnsigned 63 (L.exUnitsMem exUnits)
    , L.exUnitsSteps = clipUnsigned 63 (L.exUnitsSteps exUnits)
    }

clipAlonzoGenesis :: L.AlonzoGenesis -> L.AlonzoGenesis
clipAlonzoGenesis genesis =
  genesis
    { L.agPrices = L.agPrices genesis & prStepsL %~ clipIBr & prMemL %~ clipIBr
    , L.agMaxTxExUnits = clipExUnits (L.agMaxTxExUnits genesis)
    , L.agMaxBlockExUnits = clipExUnits (L.agMaxBlockExUnits genesis)
    }

clipVotingThresholds
  :: L.PoolVotingThresholds -> L.PoolVotingThresholds
clipVotingThresholds thresholds =
  thresholds
    & L.pvtMotionNoConfidenceL %~ clipIBr
    & L.pvtCommitteeNormalL %~ clipIBr
    & L.pvtCommitteeNoConfidenceL %~ clipIBr
    & L.pvtHardForkInitiationL %~ clipIBr
    & L.pvtPPSecurityGroupL %~ clipIBr

clipDRepVotingThresholds
  :: L.DRepVotingThresholds -> L.DRepVotingThresholds
clipDRepVotingThresholds thresholds =
  thresholds
    & L.dvtMotionNoConfidenceL %~ clipIBr
    & L.dvtCommitteeNormalL %~ clipIBr
    & L.dvtCommitteeNoConfidenceL %~ clipIBr
    & L.dvtUpdateToConstitutionL %~ clipIBr
    & L.dvtHardForkInitiationL %~ clipIBr
    & L.dvtPPNetworkGroupL %~ clipIBr
    & L.dvtPPEconomicGroupL %~ clipIBr
    & L.dvtPPTechnicalGroupL %~ clipIBr
    & L.dvtPPGovGroupL %~ clipIBr
    & L.dvtTreasuryWithdrawalL %~ clipIBr

clipConwayGenesis :: L.ConwayGenesis -> L.ConwayGenesis
clipConwayGenesis genesis =
  genesis
    { L.cgCommittee =
        (L.cgCommittee genesis)
          { L.committeeThreshold = clipIBr (L.committeeThreshold (L.cgCommittee genesis))
          }
    , L.cgUpgradePParams =
        (L.cgUpgradePParams genesis)
          { L.ucppMinFeeRefScriptCostPerByte = clipIBr (L.ucppMinFeeRefScriptCostPerByte upgrade)
          , L.ucppPoolVotingThresholds = clipVotingThresholds (L.ucppPoolVotingThresholds upgrade)
          , L.ucppDRepVotingThresholds = clipDRepVotingThresholds (L.ucppDRepVotingThresholds upgrade)
          }
    }
 where
  upgrade = L.cgUpgradePParams genesis
