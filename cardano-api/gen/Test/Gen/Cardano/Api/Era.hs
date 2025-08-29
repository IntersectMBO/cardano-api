{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Gen.Cardano.Api.Era
  ( shelleyBasedEraTestConstraints
  , shelleyToBabbageEraTestConstraints
  , conwayEraOnwardsTestConstraints
  )
where

import Cardano.Api hiding (txIns)
-- import Test.Cardano.Ledger.Conway.Arbitrary ()
-- import Test.Cardano.Ledger.Core.Arbitrary ()
import Cardano.Api.Genesis (unsafeBoundedRational)

import Cardano.Ledger.Alonzo qualified as Ledger
import Cardano.Ledger.BaseTypes (promoteRatio)
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Conway qualified as Ledger
import Cardano.Ledger.Core qualified as Ledger
import Cardano.Ledger.Shelley qualified as Ledger
import Cardano.Ledger.Shelley.PParams (ShelleyPParams)

import Data.Functor.Identity qualified as Ledger
import Data.Ratio
import Data.Word
import GHC.Num
import GHC.Stack (HasCallStack)

import Test.QuickCheck
  ( Arbitrary (..)
  , Gen
  , choose
  , chooseInt
  , genericShrink
  , getNonNegative
  , oneof
  )

import Generic.Random (genericArbitraryU)

instance Arbitrary (ShelleyPParams Ledger.StrictMaybe Ledger.ShelleyEra) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary (ShelleyPParams Ledger.StrictMaybe Ledger.AlonzoEra) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary (ShelleyPParams Ledger.StrictMaybe Ledger.ConwayEra) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary Ledger.EpochInterval where
  arbitrary = Ledger.EpochInterval <$> arbitrary

instance Arbitrary Ledger.NonNegativeInterval where
  arbitrary = do
    p <- chooseInt (0, maxDecimalsWord64)
    let y = 10 ^ p :: Word64
    x <- choose (0, 10 ^ (maxDecimalsWord64 :: Int))
    pure $ unsafeBoundedRational $ promoteRatio (x % y)

instance Arbitrary Ledger.UnitInterval where
  arbitrary = do
    p <- chooseInt (0, maxDecimalsWord64)
    let y = 10 ^ p :: Word64
    x <- choose (0, y)
    pure $ unsafeBoundedRational $ promoteRatio (x % y)

instance Arbitrary Ledger.Nonce where
  arbitrary =
    oneof
      [ return Ledger.NeutralNonce
      , Ledger.mkNonceFromNumber <$> arbitrary
      ]

instance Arbitrary Ledger.ProtVer where
  arbitrary = Ledger.ProtVer <$> arbitrary <*> arbitrary

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

instance Arbitrary Natural

maxDecimalsWord64 :: Int
maxDecimalsWord64 = 19

instance Arbitrary Coin where
  -- Cannot be negative even though it is an 'Integer'
  arbitrary = Coin <$> oneof [choose (0, 1000000), getNonNegative <$> arbitrary]
  shrink (Coin i) = Coin <$> shrink i

instance Arbitrary e => Arbitrary (Ledger.StrictMaybe e) where
  arbitrary = Ledger.maybeToStrictMaybe <$> arbitrary
  shrink = fmap Ledger.maybeToStrictMaybe . shrink . Ledger.strictMaybeToMaybe

shelleyBasedEraTestConstraints
  :: ()
  => ShelleyBasedEra era
  -> ( Ledger.Era (ShelleyLedgerEra era)
       => a
     )
  -> a
shelleyBasedEraTestConstraints = \case
  ShelleyBasedEraShelley -> id
  ShelleyBasedEraAllegra -> id
  ShelleyBasedEraMary -> id
  ShelleyBasedEraAlonzo -> id
  ShelleyBasedEraBabbage -> id
  ShelleyBasedEraConway -> id

shelleyToBabbageEraTestConstraints
  :: ()
  => ShelleyToBabbageEra era
  -> ( Ledger.Era (ShelleyLedgerEra era)
       => a
     )
  -> a
shelleyToBabbageEraTestConstraints = \case
  ShelleyToBabbageEraShelley -> id
  ShelleyToBabbageEraAllegra -> id
  ShelleyToBabbageEraMary -> id
  ShelleyToBabbageEraAlonzo -> id
  ShelleyToBabbageEraBabbage -> id

conwayEraOnwardsTestConstraints
  :: ()
  => ConwayEraOnwards era
  -> ( Ledger.Era (ShelleyLedgerEra era)
       => a
     )
  -> a
conwayEraOnwardsTestConstraints = \case
  ConwayEraOnwardsConway -> id
