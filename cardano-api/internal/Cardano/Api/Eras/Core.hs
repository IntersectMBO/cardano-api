{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}

-- | Cardano eras, sometimes we have to distinguish them.
--
module Cardano.Api.Eras.Core
  ( -- * Eras
    ByronEra
  , ShelleyEra
  , AllegraEra
  , MaryEra
  , AlonzoEra
  , BabbageEra
  , ConwayEra

    -- * CardanoEra
  , CardanoEra(..)
  , IsCardanoEra(..)
  , AnyCardanoEra(..)
  , anyCardanoEra
  , InAnyCardanoEra(..)
  , CardanoLedgerEra
  , ToCardanoEra(..)

    -- * FeatureInEra
  , FeatureInEra(..)
  , inEraFeature
  , inEraFeatureMaybe
  , maybeFeatureInEra
  , justFeatureInEra
  , justInEraFeature
  , featureInShelleyBasedEra
  , inShelleyBasedEraFeature
  , inShelleyBasedEraFeatureMaybe
  , maybeFeatureInShelleyBasedEra

    -- * Shelley-based eras
  , ShelleyBasedEra(..)
  , IsShelleyBasedEra(..)
  , AnyShelleyBasedEra(..)
  , InAnyShelleyBasedEra(..)
  , shelleyBasedToCardanoEra

    -- ** Mapping to era types from the Shelley ledger library
  , ShelleyLedgerEra
  , eraProtVerLow

    -- * Cardano eras, as Byron vs Shelley-based
  , CardanoEraStyle(..)
  , cardanoEraStyle

    -- * Data family instances
  , AsType(AsByronEra, AsShelleyEra, AsAllegraEra, AsMaryEra, AsAlonzoEra, AsBabbageEra, AsConwayEra)

    -- * Assertions on era
  , requireShelleyBasedEra

  ) where

import           Cardano.Api.HasTypeProxy

import qualified Cardano.Ledger.Api as L
import qualified Cardano.Ledger.BaseTypes as L
import           Ouroboros.Consensus.Shelley.Eras as Consensus (StandardAllegra, StandardAlonzo,
                   StandardBabbage, StandardConway, StandardMary, StandardShelley)

import           Control.DeepSeq
import           Data.Aeson (FromJSON (..), ToJSON, toJSON, withText)
import           Data.Kind
import qualified Data.Text as Text
import           Data.Type.Equality (TestEquality (..), (:~:) (Refl))

-- ----------------------------------------------------------------------------
-- Eras

-- | A type used as a tag to distinguish the Byron era.
data ByronEra

-- | A type used as a tag to distinguish the Shelley era.
data ShelleyEra

-- | A type used as a tag to distinguish the Allegra era.
data AllegraEra

-- | A type used as a tag to distinguish the Mary era.
data MaryEra

-- | A type used as a tag to distinguish the Alonzo era.
data AlonzoEra

-- | A type used as a tag to distinguish the Babbage era.
data BabbageEra

-- | A type used as a tag to distinguish the Conway era.
data ConwayEra

instance HasTypeProxy ByronEra where
    data AsType ByronEra = AsByronEra
    proxyToAsType _ = AsByronEra

instance HasTypeProxy ShelleyEra where
    data AsType ShelleyEra = AsShelleyEra
    proxyToAsType _ = AsShelleyEra

instance HasTypeProxy AllegraEra where
    data AsType AllegraEra = AsAllegraEra
    proxyToAsType _ = AsAllegraEra

instance HasTypeProxy MaryEra where
    data AsType MaryEra = AsMaryEra
    proxyToAsType _ = AsMaryEra

instance HasTypeProxy AlonzoEra where
    data AsType AlonzoEra = AsAlonzoEra
    proxyToAsType _ = AsAlonzoEra

instance HasTypeProxy BabbageEra where
   data AsType BabbageEra = AsBabbageEra
   proxyToAsType _ = AsBabbageEra

instance HasTypeProxy ConwayEra where
   data AsType ConwayEra = AsConwayEra
   proxyToAsType _ = AsConwayEra

-- ----------------------------------------------------------------------------
-- FeatureInEra

-- | A class for producing values for features that are supported in some eras
-- but not others.
class FeatureInEra (feature :: Type -> Type) where
  -- | Determine the value to use for a feature in a given 'CardanoEra'.
  -- Note that the negative case is the first argument, and the positive case is the second as per
  -- the 'either' function convention.
  featureInEra :: ()
    => a                    -- ^ Value to use if the feature is not supported in the era
    -> (feature era -> a)   -- ^ Function to get thealue to use if the feature is supported in the era
    -> CardanoEra era       -- ^ Era to check
    -> a                    -- ^ The value to use

inEraFeature :: ()
  => FeatureInEra feature
  => CardanoEra era       -- ^ Era to check
  -> a                    -- ^ Value to use if the feature is not supported in the era
  -> (feature era -> a)   -- ^ Function to get thealue to use if the feature is supported in the era
  -> a                    -- ^ The value to use
inEraFeature era no yes =
  featureInEra no yes era

inEraFeatureMaybe :: ()
  => FeatureInEra feature
  => CardanoEra era       -- ^ Era to check
  -> (feature era -> a)   -- ^ Function to get thealue to use if the feature is supported in the era
  -> Maybe a              -- ^ The value to use
inEraFeatureMaybe era yes =
  inEraFeature era Nothing (Just . yes)

maybeFeatureInEra :: ()
  => FeatureInEra feature
  => CardanoEra era       -- ^ Era to check
  -> Maybe (feature era)  -- ^ The feature if supported in the era
maybeFeatureInEra =
  featureInEra Nothing Just

justFeatureInEra :: ()
  => FeatureInEra feature
  => (feature era -> a)   -- ^ Function to get thealue to use if the feature is supported in the era
  -> CardanoEra era       -- ^ Era to check
  -> Maybe a              -- ^ The value to use
justFeatureInEra f = featureInEra Nothing (Just . f)

justInEraFeature :: ()
  => FeatureInEra feature
  => CardanoEra era       -- ^ Era to check
  -> (feature era -> a)   -- ^ Function to get thealue to use if the feature is supported in the era
  -> Maybe a              -- ^ The value to use
justInEraFeature era f = inEraFeature era Nothing (Just . f)

-- | Determine the value to use for a feature in a given 'ShelleyBasedEra'.
featureInShelleyBasedEra :: ()
  => FeatureInEra feature
  => a
  -> (feature era -> a)
  -> ShelleyBasedEra era
  -> a
featureInShelleyBasedEra no yes =
  featureInEra no yes . shelleyBasedToCardanoEra

maybeFeatureInShelleyBasedEra :: ()
  => FeatureInEra feature
  => ShelleyBasedEra era
  -> Maybe (feature era)
maybeFeatureInShelleyBasedEra =
  featureInEra Nothing Just . shelleyBasedToCardanoEra

inShelleyBasedEraFeature :: ()
  => FeatureInEra feature
  => ShelleyBasedEra era
  -> a
  -> (feature era -> a)
  -> a
inShelleyBasedEraFeature era no yes =
  featureInShelleyBasedEra no yes era

inShelleyBasedEraFeatureMaybe :: ()
  => FeatureInEra feature
  => ShelleyBasedEra era
  -> (feature era -> a)
  -> Maybe a
inShelleyBasedEraFeatureMaybe era yes =
  inShelleyBasedEraFeature era Nothing (Just . yes)

-- ----------------------------------------------------------------------------
-- ToCardanoEra

class ToCardanoEra (feature :: Type -> Type) where
  toCardanoEra :: ()
    => feature era
    -> CardanoEra era

-- ----------------------------------------------------------------------------
-- Value level representation for Cardano eras
--

-- | This GADT provides a value-level representation of all the Cardano eras.
-- This enables pattern matching on the era to allow them to be treated in a
-- non-uniform way.
--
-- This can be used in combination with the 'IsCardanoEra' class to get access
-- to this value.
--
-- In combination this can often enable code that handles all eras, and does
-- so uniformly where possible, and non-uniformly where necessary.
--
data CardanoEra era where
     ByronEra   :: CardanoEra ByronEra
     ShelleyEra :: CardanoEra ShelleyEra
     AllegraEra :: CardanoEra AllegraEra
     MaryEra    :: CardanoEra MaryEra
     AlonzoEra  :: CardanoEra AlonzoEra
     BabbageEra :: CardanoEra BabbageEra
     ConwayEra  :: CardanoEra ConwayEra
     -- when you add era here, change `instance Bounded AnyCardanoEra`

deriving instance Eq   (CardanoEra era)
deriving instance Ord  (CardanoEra era)
deriving instance Show (CardanoEra era)

instance ToJSON (CardanoEra era) where
   toJSON ByronEra   = "Byron"
   toJSON ShelleyEra = "Shelley"
   toJSON AllegraEra = "Allegra"
   toJSON MaryEra    = "Mary"
   toJSON AlonzoEra  = "Alonzo"
   toJSON BabbageEra = "Babbage"
   toJSON ConwayEra  = "Conway"

instance TestEquality CardanoEra where
    testEquality ByronEra   ByronEra   = Just Refl
    testEquality ShelleyEra ShelleyEra = Just Refl
    testEquality AllegraEra AllegraEra = Just Refl
    testEquality MaryEra    MaryEra    = Just Refl
    testEquality AlonzoEra  AlonzoEra  = Just Refl
    testEquality BabbageEra BabbageEra = Just Refl
    testEquality ConwayEra  ConwayEra  = Just Refl
    testEquality _          _          = Nothing

instance FeatureInEra CardanoEra where
  featureInEra _ yes = yes

instance ToCardanoEra CardanoEra where
  toCardanoEra = id

-- | The class of Cardano eras. This allows uniform handling of all Cardano
-- eras, but also non-uniform by making case distinctions on the 'CardanoEra'
-- constructors, or the 'CardanoEraStyle' constructors via `cardanoEraStyle`.
--
class HasTypeProxy era => IsCardanoEra era where
   cardanoEra      :: CardanoEra era

instance IsCardanoEra ByronEra where
   cardanoEra      = ByronEra

instance IsCardanoEra ShelleyEra where
   cardanoEra      = ShelleyEra

instance IsCardanoEra AllegraEra where
   cardanoEra      = AllegraEra

instance IsCardanoEra MaryEra where
   cardanoEra      = MaryEra

instance IsCardanoEra AlonzoEra where
   cardanoEra      = AlonzoEra

instance IsCardanoEra BabbageEra where
   cardanoEra      = BabbageEra

instance IsCardanoEra ConwayEra where
   cardanoEra      = ConwayEra

data AnyCardanoEra where
     AnyCardanoEra :: IsCardanoEra era  -- Provide class constraint
                   => CardanoEra era    -- and explicit value.
                   -> AnyCardanoEra

deriving instance Show AnyCardanoEra

instance Eq AnyCardanoEra where
    AnyCardanoEra era == AnyCardanoEra era' =
      case testEquality era era' of
        Nothing   -> False
        Just Refl -> True -- since no constructors share types

instance Bounded AnyCardanoEra where
   minBound = AnyCardanoEra ByronEra
   maxBound = AnyCardanoEra ConwayEra

instance Enum AnyCardanoEra where

   -- [e..] = [e..maxBound]
   enumFrom e = enumFromTo e maxBound

   fromEnum = \case
      AnyCardanoEra ByronEra    -> 0
      AnyCardanoEra ShelleyEra  -> 1
      AnyCardanoEra AllegraEra  -> 2
      AnyCardanoEra MaryEra     -> 3
      AnyCardanoEra AlonzoEra   -> 4
      AnyCardanoEra BabbageEra  -> 5
      AnyCardanoEra ConwayEra   -> 6

   toEnum = \case
      0 -> AnyCardanoEra ByronEra
      1 -> AnyCardanoEra ShelleyEra
      2 -> AnyCardanoEra AllegraEra
      3 -> AnyCardanoEra MaryEra
      4 -> AnyCardanoEra AlonzoEra
      5 -> AnyCardanoEra BabbageEra
      6 -> AnyCardanoEra ConwayEra
      n ->
         error $
            "AnyCardanoEra.toEnum: " <> show n
            <> " does not correspond to any known enumerated era."

instance ToJSON AnyCardanoEra where
   toJSON (AnyCardanoEra era) = toJSON era

instance FromJSON AnyCardanoEra where
   parseJSON = withText "AnyCardanoEra"
     $ \case
        "Byron" -> pure $ AnyCardanoEra ByronEra
        "Shelley" -> pure $ AnyCardanoEra ShelleyEra
        "Allegra" -> pure $ AnyCardanoEra AllegraEra
        "Mary" -> pure $ AnyCardanoEra MaryEra
        "Alonzo" -> pure $ AnyCardanoEra AlonzoEra
        "Babbage" -> pure $ AnyCardanoEra BabbageEra
        "Conway" -> pure $ AnyCardanoEra ConwayEra
        wrong -> fail $ "Failed to parse unknown era: " <> Text.unpack wrong


-- | Like the 'AnyCardanoEra' constructor but does not demand a 'IsCardanoEra'
-- class constraint.
--
anyCardanoEra :: CardanoEra era -> AnyCardanoEra
anyCardanoEra ByronEra   = AnyCardanoEra ByronEra
anyCardanoEra ShelleyEra = AnyCardanoEra ShelleyEra
anyCardanoEra AllegraEra = AnyCardanoEra AllegraEra
anyCardanoEra MaryEra    = AnyCardanoEra MaryEra
anyCardanoEra AlonzoEra  = AnyCardanoEra AlonzoEra
anyCardanoEra BabbageEra = AnyCardanoEra BabbageEra
anyCardanoEra ConwayEra  = AnyCardanoEra ConwayEra

-- | This pairs up some era-dependent type with a 'CardanoEra' value that tells
-- us what era it is, but hides the era type. This is useful when the era is
-- not statically known, for example when deserialising from a file.
--
data InAnyCardanoEra thing where
     InAnyCardanoEra :: IsCardanoEra era  -- Provide class constraint
                     => CardanoEra era    -- and explicit value.
                     -> thing era
                     -> InAnyCardanoEra thing

-- ----------------------------------------------------------------------------
-- Shelley-based eras
--

-- | While the Byron and Shelley eras are quite different, there are several
-- eras that are based on Shelley with only minor differences. It is useful
-- to be able to treat the Shelley-based eras in a mostly-uniform way.
--
-- Values of this type witness the fact that the era is Shelley-based. This
-- can be used to constrain the era to being a Shelley-based on. It allows
-- non-uniform handling making case distinctions on the constructor.
--
data ShelleyBasedEra era where
     ShelleyBasedEraShelley :: ShelleyBasedEra ShelleyEra
     ShelleyBasedEraAllegra :: ShelleyBasedEra AllegraEra
     ShelleyBasedEraMary    :: ShelleyBasedEra MaryEra
     ShelleyBasedEraAlonzo  :: ShelleyBasedEra AlonzoEra
     ShelleyBasedEraBabbage :: ShelleyBasedEra BabbageEra
     ShelleyBasedEraConway  :: ShelleyBasedEra ConwayEra

instance NFData (ShelleyBasedEra era) where
  rnf = \case
    ShelleyBasedEraShelley -> ()
    ShelleyBasedEraAllegra -> ()
    ShelleyBasedEraMary    -> ()
    ShelleyBasedEraAlonzo  -> ()
    ShelleyBasedEraBabbage -> ()
    ShelleyBasedEraConway  -> ()

deriving instance Eq   (ShelleyBasedEra era)
deriving instance Ord  (ShelleyBasedEra era)
deriving instance Show (ShelleyBasedEra era)

instance ToJSON (ShelleyBasedEra era) where
   toJSON = toJSON . shelleyBasedToCardanoEra

instance TestEquality ShelleyBasedEra where
    testEquality ShelleyBasedEraShelley ShelleyBasedEraShelley = Just Refl
    testEquality ShelleyBasedEraAllegra ShelleyBasedEraAllegra = Just Refl
    testEquality ShelleyBasedEraMary    ShelleyBasedEraMary    = Just Refl
    testEquality ShelleyBasedEraAlonzo  ShelleyBasedEraAlonzo  = Just Refl
    testEquality ShelleyBasedEraBabbage ShelleyBasedEraBabbage = Just Refl
    testEquality ShelleyBasedEraConway  ShelleyBasedEraConway  = Just Refl
    testEquality _                      _                      = Nothing

instance FeatureInEra ShelleyBasedEra where
  featureInEra no yes = \case
    ByronEra    -> no
    ShelleyEra  -> yes ShelleyBasedEraShelley
    AllegraEra  -> yes ShelleyBasedEraAllegra
    MaryEra     -> yes ShelleyBasedEraMary
    AlonzoEra   -> yes ShelleyBasedEraAlonzo
    BabbageEra  -> yes ShelleyBasedEraBabbage
    ConwayEra   -> yes ShelleyBasedEraConway

instance ToCardanoEra ShelleyBasedEra where
  toCardanoEra = \case
    ShelleyBasedEraShelley -> ShelleyEra
    ShelleyBasedEraAllegra -> AllegraEra
    ShelleyBasedEraMary    -> MaryEra
    ShelleyBasedEraAlonzo  -> AlonzoEra
    ShelleyBasedEraBabbage -> BabbageEra
    ShelleyBasedEraConway  -> ConwayEra

-- | The class of eras that are based on Shelley. This allows uniform handling
-- of Shelley-based eras, but also non-uniform by making case distinctions on
-- the 'ShelleyBasedEra' constructors.
--
class IsCardanoEra era => IsShelleyBasedEra era where
   shelleyBasedEra :: ShelleyBasedEra era

instance IsShelleyBasedEra ShelleyEra where
   shelleyBasedEra = ShelleyBasedEraShelley

instance IsShelleyBasedEra AllegraEra where
   shelleyBasedEra = ShelleyBasedEraAllegra

instance IsShelleyBasedEra MaryEra where
   shelleyBasedEra = ShelleyBasedEraMary

instance IsShelleyBasedEra AlonzoEra where
   shelleyBasedEra = ShelleyBasedEraAlonzo

instance IsShelleyBasedEra BabbageEra where
   shelleyBasedEra = ShelleyBasedEraBabbage

instance IsShelleyBasedEra ConwayEra where
   shelleyBasedEra = ShelleyBasedEraConway

data AnyShelleyBasedEra where
  AnyShelleyBasedEra
    :: IsShelleyBasedEra era  -- Provide class constraint
    => ShelleyBasedEra era    -- and explicit value.
    -> AnyShelleyBasedEra

deriving instance Show AnyShelleyBasedEra

instance Eq AnyShelleyBasedEra where
    AnyShelleyBasedEra sbe == AnyShelleyBasedEra sbe' =
      case testEquality sbe sbe' of
        Nothing   -> False
        Just Refl -> True -- since no constructors share types

instance Bounded AnyShelleyBasedEra where
   minBound = AnyShelleyBasedEra ShelleyBasedEraShelley
   maxBound = AnyShelleyBasedEra ShelleyBasedEraConway

instance Enum AnyShelleyBasedEra where
   enumFrom e = enumFromTo e maxBound

   fromEnum = \case
      AnyShelleyBasedEra ShelleyBasedEraShelley  -> 1
      AnyShelleyBasedEra ShelleyBasedEraAllegra  -> 2
      AnyShelleyBasedEra ShelleyBasedEraMary     -> 3
      AnyShelleyBasedEra ShelleyBasedEraAlonzo   -> 4
      AnyShelleyBasedEra ShelleyBasedEraBabbage  -> 5
      AnyShelleyBasedEra ShelleyBasedEraConway   -> 6

   toEnum = \case
      1 -> AnyShelleyBasedEra ShelleyBasedEraShelley
      2 -> AnyShelleyBasedEra ShelleyBasedEraAllegra
      3 -> AnyShelleyBasedEra ShelleyBasedEraMary
      4 -> AnyShelleyBasedEra ShelleyBasedEraAlonzo
      5 -> AnyShelleyBasedEra ShelleyBasedEraBabbage
      6 -> AnyShelleyBasedEra ShelleyBasedEraConway
      n ->
         error $
            "AnyShelleyBasedEra.toEnum: " <> show n
            <> " does not correspond to any known enumerated era."

instance ToJSON AnyShelleyBasedEra where
   toJSON (AnyShelleyBasedEra sbe) = toJSON sbe

instance FromJSON AnyShelleyBasedEra where
   parseJSON = withText "AnyShelleyBasedEra"
     $ \case
        "Shelley" -> pure $ AnyShelleyBasedEra ShelleyBasedEraShelley
        "Allegra" -> pure $ AnyShelleyBasedEra ShelleyBasedEraAllegra
        "Mary" -> pure $ AnyShelleyBasedEra ShelleyBasedEraMary
        "Alonzo" -> pure $ AnyShelleyBasedEra ShelleyBasedEraAlonzo
        "Babbage" -> pure $ AnyShelleyBasedEra ShelleyBasedEraBabbage
        "Conway" -> pure $ AnyShelleyBasedEra ShelleyBasedEraConway
        wrong -> fail $ "Failed to parse unknown shelley-based era: " <> Text.unpack wrong


-- | This pairs up some era-dependent type with a 'ShelleyBasedEra' value that
-- tells us what era it is, but hides the era type. This is useful when the era
-- is not statically known, for example when deserialising from a file.
--
data InAnyShelleyBasedEra thing where
     InAnyShelleyBasedEra :: IsShelleyBasedEra era -- Provide class constraint
                          => ShelleyBasedEra era   -- and explicit value.
                          -> thing era
                          -> InAnyShelleyBasedEra thing


-- | Converts a 'ShelleyBasedEra' to the broader 'CardanoEra'.
shelleyBasedToCardanoEra :: ShelleyBasedEra era -> CardanoEra era
shelleyBasedToCardanoEra ShelleyBasedEraShelley = ShelleyEra
shelleyBasedToCardanoEra ShelleyBasedEraAllegra = AllegraEra
shelleyBasedToCardanoEra ShelleyBasedEraMary    = MaryEra
shelleyBasedToCardanoEra ShelleyBasedEraAlonzo  = AlonzoEra
shelleyBasedToCardanoEra ShelleyBasedEraBabbage = BabbageEra
shelleyBasedToCardanoEra ShelleyBasedEraConway  = ConwayEra

-- ----------------------------------------------------------------------------
-- Cardano eras factored as Byron vs Shelley-based
--

-- | This is the same essential information as 'CardanoEra' but instead of a
-- flat set of alternative eras, it is factored into the legcy Byron era and
-- the current Shelley-based eras.
--
-- This way of factoring the eras is useful because in many cases the
-- major differences are between the Byron and Shelley-based eras, and
-- the Shelley-based eras can often be treated uniformly.
--
data CardanoEraStyle era where
     LegacyByronEra  :: CardanoEraStyle ByronEra
     ShelleyBasedEra :: IsShelleyBasedEra era -- Also provide class constraint
                     => ShelleyBasedEra era
                     -> CardanoEraStyle era

deriving instance Eq   (CardanoEraStyle era)
deriving instance Ord  (CardanoEraStyle era)
deriving instance Show (CardanoEraStyle era)

-- | The 'CardanoEraStyle' for a 'CardanoEra'.
--
cardanoEraStyle :: CardanoEra era -> CardanoEraStyle era
cardanoEraStyle ByronEra   = LegacyByronEra
cardanoEraStyle ShelleyEra = ShelleyBasedEra ShelleyBasedEraShelley
cardanoEraStyle AllegraEra = ShelleyBasedEra ShelleyBasedEraAllegra
cardanoEraStyle MaryEra    = ShelleyBasedEra ShelleyBasedEraMary
cardanoEraStyle AlonzoEra  = ShelleyBasedEra ShelleyBasedEraAlonzo
cardanoEraStyle BabbageEra = ShelleyBasedEra ShelleyBasedEraBabbage
cardanoEraStyle ConwayEra  = ShelleyBasedEra ShelleyBasedEraConway

-- ----------------------------------------------------------------------------
-- Conversion to Shelley ledger library types
--

-- | A type family that connects our era type tags to equivalent type tags used
-- in the Shelley ledger library.
--
-- This type mapping  connect types from this API with types in the Shelley
-- ledger library which allows writing conversion functions in a more generic
-- way.
--
type family ShelleyLedgerEra era = ledgerera | ledgerera -> era where
  ShelleyLedgerEra ShelleyEra = Consensus.StandardShelley
  ShelleyLedgerEra AllegraEra = Consensus.StandardAllegra
  ShelleyLedgerEra MaryEra    = Consensus.StandardMary
  ShelleyLedgerEra AlonzoEra  = Consensus.StandardAlonzo
  ShelleyLedgerEra BabbageEra = Consensus.StandardBabbage
  ShelleyLedgerEra ConwayEra  = Consensus.StandardConway

type family CardanoLedgerEra era = ledgerera | ledgerera -> era where
  CardanoLedgerEra ByronEra   = L.ByronEra   L.StandardCrypto
  CardanoLedgerEra ShelleyEra = L.ShelleyEra L.StandardCrypto
  CardanoLedgerEra AllegraEra = L.AllegraEra L.StandardCrypto
  CardanoLedgerEra MaryEra    = L.MaryEra    L.StandardCrypto
  CardanoLedgerEra AlonzoEra  = L.AlonzoEra  L.StandardCrypto
  CardanoLedgerEra BabbageEra = L.BabbageEra L.StandardCrypto
  CardanoLedgerEra ConwayEra  = L.ConwayEra  L.StandardCrypto

-- | Lookup the lower major protocol version for the shelley based era. In other words
-- this is the major protocol version that the era has started in.
eraProtVerLow :: ShelleyBasedEra era -> L.Version
eraProtVerLow = \case
  ShelleyBasedEraShelley -> L.eraProtVerLow @L.Shelley
  ShelleyBasedEraAllegra -> L.eraProtVerLow @L.Allegra
  ShelleyBasedEraMary    -> L.eraProtVerLow @L.Mary
  ShelleyBasedEraAlonzo  -> L.eraProtVerLow @L.Alonzo
  ShelleyBasedEraBabbage -> L.eraProtVerLow @L.Babbage
  ShelleyBasedEraConway  -> L.eraProtVerLow @L.Conway

requireShelleyBasedEra :: ()
  => Applicative m
  => CardanoEra era
  -> m (Maybe (ShelleyBasedEra era))
requireShelleyBasedEra era =
  case cardanoEraStyle era of
    LegacyByronEra -> pure Nothing
    ShelleyBasedEra sbe -> pure (Just sbe)
