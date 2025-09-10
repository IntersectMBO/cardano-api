{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Gen.Cardano.Api.Orphans (obtainArbitraryConstraints) where

import Cardano.Api hiding (txIns)
import Cardano.Api.Ledger qualified as L

import Cardano.Crypto.Hash.Class (hashWith)
import Cardano.Crypto.Hash.Class qualified as Crypto
import Cardano.Ledger.Address ()
import Cardano.Ledger.Alonzo.PParams qualified as Ledger
import Cardano.Ledger.Alonzo.Scripts qualified as Alonzo
import Cardano.Ledger.Babbage.PParams qualified as Ledger
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Coin qualified as L
import Cardano.Ledger.Conway.Governance qualified as Ledger
import Cardano.Ledger.Conway.PParams qualified as Ledger
import Cardano.Ledger.Core qualified as Ledger
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.PParams
import Cardano.Ledger.HKD (HKD, NoUpdate (..))
import Cardano.Ledger.Keys (VRFVerKeyHash (..))
import Cardano.Ledger.Mary.Value qualified as ConcreteValue
import Cardano.Ledger.Mary.Value qualified as Ledger
import Cardano.Ledger.Plutus.CostModels qualified as Ledger
import Cardano.Ledger.Plutus.Language qualified as L
import Cardano.Ledger.Plutus.Language qualified as Ledger
import Cardano.Ledger.Shelley.PParams (ShelleyPParams)

import Control.Monad (replicateM)
import Control.Monad.Trans.Fail.String (errorFail)
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Short qualified as SBS
import Data.Foldable qualified as F
import Data.Functor.Identity
import Data.IP (IPv4, IPv6, toIPv4w, toIPv6w)
import Data.Int (Int64)
import Data.Map.Strict qualified as Map
import Data.Ratio
import Data.Sequence.Strict qualified as SSeq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (IsString (fromString))
import Data.Text qualified as T
import Data.Word
import GHC.Stack (HasCallStack)
import System.Random.Stateful
  ( StatefulGen (..)
  , runStateGen_
  )

import Test.QuickCheck
  ( Arbitrary (..)
  , Gen
  , NonNegative (..)
  , Positive (..)
  , arbitraryBoundedEnum
  , choose
  , chooseInt
  , elements
  , frequency
  , genericShrink
  , getNonNegative
  , liftArbitrary
  , listOf
  , listOf1
  , oneof
  , scale
  , shrinkBoundedEnum
  , sublistOf
  , suchThatMap
  , vectorOf
  )
import Test.QuickCheck.Gen (Gen (MkGen))
import Test.QuickCheck.Instances.Natural ()

import Generic.Random (genericArbitraryU)

-- | Maximum decimal precision for Word64-based rational types.
maxDecimalsWord64 :: Int
maxDecimalsWord64 = 19

-------------------------------------------------------------------------------

-- * Basic Type Instances

-------------------------------------------------------------------------------

instance Arbitrary Ledger.EpochInterval where
  arbitrary = Ledger.EpochInterval <$> arbitrary

instance Arbitrary Ledger.NonNegativeInterval where
  arbitrary = do
    p <- chooseInt (0, maxDecimalsWord64)
    let y = 10 ^ p :: Word64
    x <- choose (0, 10 ^ maxDecimalsWord64)
    pure $ unsafeBoundedRational $ Ledger.promoteRatio (x % y)

instance Arbitrary Ledger.UnitInterval where
  arbitrary = do
    p <- chooseInt (0, maxDecimalsWord64)
    let y = 10 ^ p :: Word64
    x <- choose (0, y)
    pure $ unsafeBoundedRational $ Ledger.promoteRatio (x % y)

instance Arbitrary Ledger.Nonce where
  arbitrary =
    oneof
      [ return Ledger.NeutralNonce
      , Ledger.mkNonceFromNumber <$> arbitrary
      ]

instance Arbitrary Ledger.Version where
  arbitrary = genVersion minBound maxBound

genVersion :: HasCallStack => Ledger.Version -> Ledger.Version -> Gen Ledger.Version
genVersion minVersion maxVersion =
  genVersion64 (Ledger.getVersion64 minVersion) (Ledger.getVersion64 maxVersion)
 where
  genVersion64 minVersion64 maxVersion64 = do
    v64 <- choose (minVersion64, maxVersion64)
    case Ledger.mkVersion64 v64 of
      Nothing -> error $ "Impossible: Invalid version generated: " ++ show v64
      Just v -> pure v

instance Arbitrary Ledger.ProtVer where
  arbitrary = Ledger.ProtVer <$> arbitrary <*> arbitrary

instance Arbitrary Coin where
  arbitrary = Coin <$> oneof [choose (0, 1000000), getNonNegative <$> arbitrary]
  shrink (Coin i) = Coin <$> shrink i

deriving instance Arbitrary L.CoinPerByte

deriving instance Arbitrary L.CoinPerWord

deriving instance Arbitrary Ledger.OrdExUnits

deriving instance Arbitrary EpochNo

-------------------------------------------------------------------------------

-- * Hash & Crypto Instances

-------------------------------------------------------------------------------

instance Arbitrary Ledger.ScriptHash where
  arbitrary = Ledger.ScriptHash <$> genHash

instance Arbitrary (L.KeyHash r) where
  arbitrary = L.KeyHash <$> genHash

instance Arbitrary (VRFVerKeyHash r) where
  arbitrary = VRFVerKeyHash <$> genHash

instance Arbitrary (L.SafeHash i) where
  arbitrary = L.unsafeMakeSafeHash <$> genHash

-- | Generate a hash of fixed size using the specified hash algorithm.
genHash :: forall h a. Ledger.HashAlgorithm h => Gen (Crypto.Hash h a)
genHash = Crypto.UnsafeHash <$> genShortByteString (fromIntegral (Crypto.sizeHash (Proxy @h)))

-- | Generate a short byte string of given length.
genShortByteString :: Int -> Gen SBS.ShortByteString
genShortByteString n = uniformShortByteString (fromIntegral n) QC

-- | Pseudo random generator compatible with QuickCheck's stateful interface.
data QC = QC

instance StatefulGen QC Gen where
  uniformWord32 QC = MkGen (\r _n -> runStateGen_ r uniformWord32)
  {-# INLINE uniformWord32 #-}
  uniformWord64 QC = MkGen (\r _n -> runStateGen_ r uniformWord64)
  {-# INLINE uniformWord64 #-}
  uniformShortByteString k QC =
    MkGen (\r _n -> runStateGen_ r (uniformShortByteString k))
  {-# INLINE uniformShortByteString #-}

-------------------------------------------------------------------------------

-- * Container & Wrapper Types

-------------------------------------------------------------------------------

instance Arbitrary e => Arbitrary (Ledger.StrictMaybe e) where
  arbitrary = Ledger.maybeToStrictMaybe <$> arbitrary
  shrink = fmap Ledger.maybeToStrictMaybe . shrink . Ledger.strictMaybeToMaybe

instance Arbitrary e => Arbitrary (SSeq.StrictSeq e) where
  arbitrary = SSeq.fromList <$> arbitrary
  shrink = fmap SSeq.fromList . shrink . F.toList

instance Arbitrary (HKD f a) => Arbitrary (Ledger.THKD t f a) where
  arbitrary = Ledger.THKD <$> arbitrary

instance Arbitrary (NoUpdate a) where
  arbitrary = pure NoUpdate

-------------------------------------------------------------------------------

-- * Core Ledger Types

-------------------------------------------------------------------------------

instance Arbitrary MIRPot where
  arbitrary = genericArbitraryU

instance Arbitrary MIRTarget where
  arbitrary =
    oneof
      [ L.StakeAddressesMIR <$> arbitrary
      , L.SendToOppositePotMIR <$> arbitrary
      ]

instance Arbitrary L.DeltaCoin where
  arbitrary = L.DeltaCoin <$> choose (-1000000, 1000000)
  shrink (L.DeltaCoin i) = L.DeltaCoin <$> shrink i

instance Arbitrary L.Vote where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

deriving instance Arbitrary L.GovActionIx

instance Arbitrary L.TxId where
  arbitrary = L.TxId <$> arbitrary

instance Arbitrary L.RewardAccount where
  arbitrary = L.RewardAccount <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary (L.Credential r) where
  arbitrary =
    oneof
      [ L.ScriptHashObj <$> arbitrary
      , L.KeyHashObj <$> arbitrary
      ]

instance Arbitrary L.Network where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Ledger.Anchor where
  arbitrary = L.Anchor <$> arbitrary <*> arbitrary

instance Arbitrary L.Url where
  arbitrary = do
    let prefix = "https://"
    n <- chooseInt (5, 64 - T.length prefix)
    txt <- genDnsName n
    pure $! guardLength n txt $ textToUrl 64 (prefix <> txt)

genDnsName :: Int -> Gen T.Text
genDnsName n = do
  str <- vectorOf (n - 4) $ elements $ '.' : '-' : ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9']
  pure $ T.pack str <> ".com"

guardLength :: HasCallStack => Int -> T.Text -> Maybe a -> a
guardLength n txt = \case
  Nothing -> error $ "Unexpected! Generated length: (" ++ show n ++ ") " ++ show txt
  Just t -> t

instance Arbitrary L.PoolParams where
  arbitrary =
    L.PoolParams
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary L.StakePoolRelay where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary L.DnsName where
  arbitrary = do
    n <- chooseInt (5, 64)
    txt <- genDnsName n
    pure $! guardLength n txt $ textToDns 64 txt

instance Arbitrary Ledger.Port where
  arbitrary = fromIntegral @Word16 @Ledger.Port <$> arbitrary

instance Arbitrary IPv4 where
  arbitrary = toIPv4w <$> arbitrary

instance Arbitrary IPv6 where
  arbitrary = do
    t <- (,,,) <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    pure $ toIPv6w t

instance Arbitrary L.PoolMetadata where
  arbitrary = L.PoolMetadata <$> arbitrary <*> arbitrary

instance Arbitrary Crypto.ByteString where
  arbitrary = C8.pack <$> arbitrary

instance Arbitrary L.ExUnits where
  arbitrary = L.ExUnits <$> genUnit <*> genUnit
   where
    genUnit = fromIntegral <$> choose (0, maxBound :: Int64)

instance Arbitrary L.Prices where
  arbitrary = L.Prices <$> arbitrary <*> arbitrary

-------------------------------------------------------------------------------

-- * MultiAsset & Token Instances

-------------------------------------------------------------------------------

instance Arbitrary Ledger.AssetName where
  arbitrary =
    Ledger.AssetName
      <$> frequency
        [ (3, elements digitByteStrings)
        , (7, genShortByteString =<< choose (1, 32))
        ]

instance Arbitrary Ledger.PolicyID where
  arbitrary =
    Ledger.PolicyID . Ledger.ScriptHash
      <$> oneof
        [ genHash
        , elements hashOfDigitByteStrings
        ]

-- | Byte strings "0" through "9"
digitByteStrings :: IsString s => [s]
digitByteStrings = [fromString [x] | x <- ['0' .. '9']]

hashOfDigitByteStrings :: Ledger.HashAlgorithm h => [Crypto.Hash h a]
hashOfDigitByteStrings = Crypto.castHash . hashWith id <$> digitByteStrings

instance Arbitrary L.MultiAsset where
  arbitrary =
    genMultiAsset $
      toInteger
        <$> oneof
          [ choose (1 :: Int, maxBound)
          , choose (minBound :: Int, -1)
          ]

genMultiAsset :: Gen Integer -> Gen L.MultiAsset
genMultiAsset genAmount = do
  ma <-
    oneof
      [ L.MultiAsset <$> genNonEmptyMap arbitrary (genNonEmptyMap arbitrary genAmount)
      , multiAssetFromListBounded <$> listOf1 (genMultiAssetTriple $ fromIntegral <$> genAmount)
      ]
  if Ledger.isMultiAssetSmallEnough ma
    then pure ma
    else scale (`div` 2) $ genMultiAsset genAmount

genMultiAssetTriple :: Gen Int64 -> Gen (Ledger.PolicyID, Ledger.AssetName, Int64)
genMultiAssetTriple genAmount = (,,) <$> arbitrary <*> arbitrary <*> genAmount

multiAssetFromListBounded
  :: forall i
   . (Bounded i, Integral i)
  => [(Ledger.PolicyID, Ledger.AssetName, i)]
  -> L.MultiAsset
multiAssetFromListBounded =
  foldr
    (\(p, n, fromIntegral -> i) ans -> ConcreteValue.insertMultiAsset comb p n i ans)
    mempty
 where
  comb :: Integer -> Integer -> Integer
  comb a b =
    max
      (fromIntegral $ minBound @i)
      (min (fromIntegral $ maxBound @i) (a + b))

-------------------------------------------------------------------------------

-- * Governance & Voting

-------------------------------------------------------------------------------

instance Arbitrary L.GovActionId where
  arbitrary = L.GovActionId <$> arbitrary <*> arbitrary

deriving instance Arbitrary (L.GovPurposeId p)

instance Arbitrary L.DRep where
  arbitrary =
    oneof
      [ L.DRepCredential <$> arbitrary
      , pure L.DRepAlwaysAbstain
      , pure L.DRepAlwaysNoConfidence
      ]

instance Arbitrary L.Delegatee where
  arbitrary =
    oneof
      [ L.DelegStake <$> arbitrary
      , L.DelegVote <$> arbitrary
      , L.DelegStakeVote <$> arbitrary <*> arbitrary
      ]

instance Arbitrary L.Voter where
  arbitrary =
    oneof
      [ L.CommitteeVoter <$> arbitrary
      , L.DRepVoter <$> arbitrary
      , L.StakePoolVoter <$> arbitrary
      ]
  shrink = genericShrink

instance (L.Era era, Arbitrary (L.PParamsUpdate era)) => Arbitrary (L.ProposalProcedure era) where
  arbitrary =
    L.ProposalProcedure
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
  shrink (L.ProposalProcedure dep ret gov anch) =
    [ L.ProposalProcedure dep' ret' gov' anch'
    | (dep', ret', gov', anch') <- shrink (dep, ret, gov, anch)
    ]

instance (L.Era era, Arbitrary (L.PParamsUpdate era)) => Arbitrary (L.GovAction era) where
  arbitrary = oneof govActionGenerators

govActionGenerators
  :: (L.Era era, Arbitrary (L.PParamsUpdate era))
  => [Gen (L.GovAction era)]
govActionGenerators =
  [ genParameterChange
  , genHardForkInitiation
  , genTreasuryWithdrawals
  , genNoConfidence
  , genUpdateCommittee
  , genNewConstitution
  , pure L.InfoAction
  ]

genParameterChange :: Arbitrary (L.PParamsUpdate era) => Gen (L.GovAction era)
genParameterChange = L.ParameterChange <$> arbitrary <*> arbitrary <*> arbitrary

genHardForkInitiation :: Gen (L.GovAction era)
genHardForkInitiation = L.HardForkInitiation <$> arbitrary <*> arbitrary

genTreasuryWithdrawals :: Gen (L.GovAction era)
genTreasuryWithdrawals = L.TreasuryWithdrawals <$> arbitrary <*> arbitrary

genNoConfidence :: Gen (L.GovAction era)
genNoConfidence = L.NoConfidence <$> arbitrary

genUpdateCommittee :: Gen (L.GovAction era)
genUpdateCommittee =
  L.UpdateCommittee
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

genNewConstitution :: L.Era era => Gen (L.GovAction era)
genNewConstitution = L.NewConstitution <$> arbitrary <*> arbitrary

-------------------------------------------------------------------------------

-- * Protocol Parameters & PParams Updates

-------------------------------------------------------------------------------

deriving instance
  (Ledger.Era era, Arbitrary (Ledger.PParamsHKD Identity era)) => Arbitrary (Ledger.PParams era)

deriving instance
  (L.Era era, Arbitrary (L.PParamsHKD L.StrictMaybe era)) => Arbitrary (L.PParamsUpdate era)

instance Ledger.Era era => Arbitrary (L.VotingProcedures era) where
  arbitrary = L.VotingProcedures <$> liftArbitrary (genNonEmptyMap arbitrary arbitrary)

instance Ledger.Era era => Arbitrary (L.VotingProcedure era) where
  arbitrary = L.VotingProcedure <$> arbitrary <*> arbitrary

genNonEmptyMap :: Ord k => Gen k -> Gen v -> Gen (Map.Map k v)
genNonEmptyMap genKey genVal = Map.fromList <$> listOf1 ((,) <$> genKey <*> genVal)

-- | Shelley-based PParams
instance Ledger.Era era => Arbitrary (ShelleyPParams Identity era) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Ledger.Era era => Arbitrary (ShelleyPParams Ledger.StrictMaybe era) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

-- | Alonzo PParams
instance Arbitrary (Ledger.AlonzoPParams Identity era) where
  arbitrary =
    Ledger.AlonzoPParams
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> genValidCostModels (Set.fromList [L.PlutusV1, L.PlutusV2])
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary (Ledger.AlonzoPParams Ledger.StrictMaybe era) where
  arbitrary =
    Ledger.AlonzoPParams
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> oneof
        [ pure Ledger.SNothing
        , Ledger.SJust <$> genValidCostModels (Set.fromList [L.PlutusV1, L.PlutusV2])
        ]
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

-- | Babbage PParams
instance Arbitrary (Ledger.BabbagePParams Identity era) where
  arbitrary =
    Ledger.BabbagePParams
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> genValidCostModels (Set.fromList [L.PlutusV1, L.PlutusV2])
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary (Ledger.BabbagePParams Ledger.StrictMaybe era) where
  arbitrary =
    Ledger.BabbagePParams
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> oneof
        [ pure Ledger.SNothing
        , Ledger.SJust <$> genValidCostModels (Set.fromList [L.PlutusV1, L.PlutusV2])
        ]
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

-- | Conway PParams
instance Ledger.Era era => Arbitrary (Ledger.ConwayPParams Identity era) where
  arbitrary =
    Ledger.ConwayPParams
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> (Ledger.THKD <$> arbitrary)
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
  shrink = genericShrink

instance Ledger.Era era => Arbitrary (Ledger.ConwayPParams Ledger.StrictMaybe era) where
  arbitrary =
    Ledger.ConwayPParams
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> pure NoUpdate
      <*> arbitrary
      <*> arbitrary
      <*> (Ledger.THKD <$> oneof [Ledger.SJust <$> genValidAndUnknownCostModels, pure Ledger.SNothing])
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
  shrink = genericShrink

-- | Thresholds
instance Arbitrary Ledger.PoolVotingThresholds where
  arbitrary =
    Ledger.PoolVotingThresholds
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
  shrink = genericShrink

instance Arbitrary Ledger.DRepVotingThresholds where
  arbitrary =
    Ledger.DRepVotingThresholds
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
  shrink = genericShrink

instance Ledger.Era era => Arbitrary (Ledger.Constitution era) where
  arbitrary = Ledger.Constitution <$> arbitrary <*> arbitrary

-------------------------------------------------------------------------------

-- * Cost Models

-------------------------------------------------------------------------------

instance Arbitrary Alonzo.CostModel where
  arbitrary = elements Ledger.nonNativeLanguages >>= genValidCostModel

instance Arbitrary Alonzo.CostModels where
  arbitrary = do
    known <- genKnownCostModels
    unknown <- genUnknownCostModels
    let cms = known `Map.union` unknown
    pure . errorFail $ Ledger.mkCostModelsLenient cms

genValidCostModel :: Ledger.Language -> Gen Ledger.CostModel
genValidCostModel lang = do
  newParamValues <- vectorOf (Ledger.costModelInitParamCount lang) arbitrary
  either (\err -> error $ "Corrupt cost model: " ++ show err) pure $
    Ledger.mkCostModel lang newParamValues

genValidCostModels :: Set Ledger.Language -> Gen L.CostModels
genValidCostModels = fmap Alonzo.mkCostModels . sequence . Map.fromSet genValidCostModel

genValidAndUnknownCostModels :: Gen L.CostModels
genValidAndUnknownCostModels = do
  langs <- sublistOf Ledger.nonNativeLanguages
  validCms <- genValidCostModels $ Set.fromList langs
  unknownCms <- errorFail . Ledger.mkCostModelsLenient <$> genUnknownCostModels
  pure $ Ledger.updateCostModels validCms unknownCms

genUnknownCostModels :: Gen (Map.Map Word8 [Int64])
genUnknownCostModels = Map.fromList <$> listOf genUnknownCostModelValues

genUnknownCostModelValues :: Gen (Word8, [Int64])
genUnknownCostModelValues = do
  lang <- chooseInt (firstInvalid, fromIntegral (maxBound :: Word8))
  vs <- arbitrary
  return (fromIntegral . fromEnum $ lang, vs)
 where
  firstInvalid = fromEnum (maxBound :: Ledger.Language) + 1

genKnownCostModels :: Gen (Map.Map Word8 [Int64])
genKnownCostModels = do
  langs <- sublistOf L.nonNativeLanguages
  cms <- mapM genCostModelValues langs
  return $ Map.fromList cms

genCostModelValues :: L.Language -> Gen (Word8, [Int64])
genCostModelValues lang = do
  Positive sub <- arbitrary
  (,) lang'
    <$> oneof
      [ listAtLeast (Ledger.costModelInitParamCount lang)
      , take (tooFew sub) <$> arbitrary
      ]
 where
  lang' = fromIntegral (fromEnum lang)
  tooFew sub = Ledger.costModelInitParamCount lang - sub
  listAtLeast :: Int -> Gen [Int64]
  listAtLeast x = do
    NonNegative y <- arbitrary
    replicateM (x + y) arbitrary

-------------------------------------------------------------------------------

-- * Helper: Era-Specific Arbitraries

-------------------------------------------------------------------------------

obtainArbitraryConstraints
  :: ShelleyBasedEra era
  -> ( ( Arbitrary (ShelleyPParams Identity (ShelleyLedgerEra era))
       , Arbitrary (L.VotingProcedures (ShelleyLedgerEra era))
       , Arbitrary (L.ProposalProcedure (ShelleyLedgerEra era))
       )
       => a
     )
  -> a
obtainArbitraryConstraints era f = case era of
  ShelleyBasedEraShelley -> f
  ShelleyBasedEraAllegra -> f
  ShelleyBasedEraMary -> f
  ShelleyBasedEraAlonzo -> f
  ShelleyBasedEraBabbage -> f
  ShelleyBasedEraConway -> f
  ShelleyBasedEraDijkstra -> f

instance Arbitrary (DijkstraPParams Identity DijkstraEra) where
  arbitrary = genericArbitraryU

instance Arbitrary (DijkstraPParams StrictMaybe DijkstraEra) where
  arbitrary = genericArbitraryU

instance Arbitrary PositiveInterval where
  arbitrary = do
    p <- chooseInt (0, maxDecimalsWord64)
    let y = 10 ^ p :: Word64
    x <- choose (1, 10 ^ (maxDecimalsWord64 :: Int))
    pure $ unsafeBoundedRational $ promoteRatio (x % y)

instance (Arbitrary a, HasZero a) => Arbitrary (NonZero a) where
  arbitrary = arbitrary `suchThatMap` nonZero

instance Arbitrary (L.CompactForm Coin) where
  arbitrary =
    L.CompactCoin <$> oneof [choose (0, 1000000), fromIntegral <$> (arbitrary :: Gen Word), arbitrary]
  shrink (L.CompactCoin i) = L.CompactCoin <$> shrink i
