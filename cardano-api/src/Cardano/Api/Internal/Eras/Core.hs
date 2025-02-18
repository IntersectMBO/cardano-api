{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilyDependencies #-}

-- | Cardano eras, sometimes we have to distinguish them.
module Cardano.Api.Internal.Eras.Core
  ( -- * Eras
    ByronEra
  , ShelleyEra
  , AllegraEra
  , MaryEra
  , AlonzoEra
  , BabbageEra
  , ConwayEra

    -- * CardanoEra
  , CardanoEra (..)
  , IsCardanoEra (..)
  , AnyCardanoEra (..)
  , anyCardanoEra
  , InAnyCardanoEra (..)
  , inAnyCardanoEra
  , CardanoLedgerEra
  , ToCardanoEra (..)

    -- * IsEon
  , Eon (..)
  , EraInEon (..)
  , inEonForEraMaybe
  , forEraInEon
  , forEraInEonMaybe
  , forEraMaybeEon
  , maybeEon
  , monoidForEraInEon
  , monoidForEraInEonA
  , Inject (..)

    -- * Data family instances
  , AsType (AsByronEra, AsShelleyEra, AsAllegraEra, AsMaryEra, AsAlonzoEra, AsBabbageEra, AsConwayEra)
  , CardanoEraConstraints
  , cardanoEraConstraints
  )
where

import Cardano.Api.Internal.HasTypeProxy
import Cardano.Api.Internal.Pretty

import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.BaseTypes (Inject (..))

import Data.Aeson (FromJSON (..), ToJSON, toJSON, withText)
import Data.Kind
import Data.Maybe (isJust)
import Data.String (IsString)
import Data.Text qualified as Text
import Data.Type.Equality (TestEquality (..), (:~:) (Refl))
import Data.Typeable (Typeable, showsTypeRep, typeOf)

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
-- Eon

-- | An Eon is a span of multiple eras.  Eons are used to scope functionality to
-- particular eras such that it isn't possible construct code that uses functionality
-- that is outside of given eras.
class Eon (eon :: Type -> Type) where
  -- | Determine the value to use in an eon (a span of multiple eras).
  -- Note that the negative case is the first argument, and the positive case is the second as per
  -- the 'either' function convention.
  inEonForEra
    :: ()
    => a
    -- ^ Value to use if the eon does not include the era
    -> (eon era -> a)
    -- ^ Function to get the value to use if the eon includes the era
    -> CardanoEra era
    -- ^ Era to check
    -> a
    -- ^ The value to use

inEonForEraMaybe
  :: ()
  => Eon eon
  => (eon era -> a)
  -- ^ Function to get the value to use if the eon includes the era
  -> CardanoEra era
  -- ^ Era to check
  -> Maybe a
  -- ^ The value to use
inEonForEraMaybe yes =
  inEonForEra Nothing (Just . yes)

forEraInEon
  :: ()
  => Eon eon
  => CardanoEra era
  -- ^ Era to check
  -> a
  -- ^ Value to use if the eon does not include the era
  -> (eon era -> a)
  -- ^ Function to get the value to use if the eon includes the era
  -> a
  -- ^ The value to use
forEraInEon era no yes =
  inEonForEra no yes era

forEraInEonMaybe
  :: ()
  => Eon eon
  => CardanoEra era
  -- ^ Era to check
  -> (eon era -> a)
  -- ^ Function to get the value to use if the eon includes the era
  -> Maybe a
  -- ^ The value to use
forEraInEonMaybe era yes =
  forEraInEon era Nothing (Just . yes)

forEraMaybeEon
  :: ()
  => Eon eon
  => CardanoEra era
  -- ^ Era to check
  -> Maybe (eon era)
  -- ^ The eon if supported in the era
forEraMaybeEon =
  inEonForEra Nothing Just

maybeEon
  :: ()
  => Eon eon
  => IsCardanoEra era
  => Maybe (eon era)
  -- ^ The eon if supported in the era
maybeEon =
  inEonForEra Nothing Just cardanoEra

monoidForEraInEon
  :: ()
  => Eon eon
  => Monoid a
  => CardanoEra era
  -> (eon era -> a)
  -> a
monoidForEraInEon sbe = forEraInEon sbe mempty

monoidForEraInEonA
  :: ()
  => Eon eon
  => Applicative f
  => Monoid a
  => CardanoEra era
  -> (eon era -> f a)
  -> f a
monoidForEraInEonA sbe = forEraInEon sbe (pure mempty)

-- ----------------------------------------------------------------------------
-- Era and eon existential types

data EraInEon eon where
  EraInEon
    :: ( Typeable era
       , Typeable (eon era)
       , Eon eon
       )
    => eon era
    -> EraInEon eon

-- | Assumes that eons are singletons
instance Show (EraInEon eon) where
  showsPrec _ (EraInEon eonEra) = showsTypeRep (typeOf eonEra)

-- | Assumes that eons are singletons
instance TestEquality eon => Eq (EraInEon eon) where
  EraInEon era1 == EraInEon era2 =
    isJust $ testEquality era1 era2

-- ----------------------------------------------------------------------------
-- ToCardanoEra

class ToCardanoEra (eon :: Type -> Type) where
  toCardanoEra
    :: ()
    => eon era
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
data CardanoEra era where
  ByronEra :: CardanoEra ByronEra
  ShelleyEra :: CardanoEra ShelleyEra
  AllegraEra :: CardanoEra AllegraEra
  MaryEra :: CardanoEra MaryEra
  AlonzoEra :: CardanoEra AlonzoEra
  BabbageEra :: CardanoEra BabbageEra
  ConwayEra :: CardanoEra ConwayEra

-- when you add era here, change `instance Bounded AnyCardanoEra`

deriving instance Eq (CardanoEra era)

deriving instance Ord (CardanoEra era)

deriving instance Show (CardanoEra era)

instance Pretty (CardanoEra era) where
  pretty = cardanoEraToStringLike

instance ToJSON (CardanoEra era) where
  toJSON = cardanoEraToStringLike

instance TestEquality CardanoEra where
  testEquality ByronEra ByronEra = Just Refl
  testEquality ShelleyEra ShelleyEra = Just Refl
  testEquality AllegraEra AllegraEra = Just Refl
  testEquality MaryEra MaryEra = Just Refl
  testEquality AlonzoEra AlonzoEra = Just Refl
  testEquality BabbageEra BabbageEra = Just Refl
  testEquality ConwayEra ConwayEra = Just Refl
  testEquality _ _ = Nothing

instance Eon CardanoEra where
  inEonForEra _ yes = yes

instance ToCardanoEra CardanoEra where
  toCardanoEra = id

-- | The class of Cardano eras. This allows uniform handling of all Cardano
-- eras, but also non-uniform by making case distinctions on the 'CardanoEra'
-- constructors.
class HasTypeProxy era => IsCardanoEra era where
  cardanoEra :: CardanoEra era

instance IsCardanoEra ByronEra where
  cardanoEra = ByronEra

instance IsCardanoEra ShelleyEra where
  cardanoEra = ShelleyEra

instance IsCardanoEra AllegraEra where
  cardanoEra = AllegraEra

instance IsCardanoEra MaryEra where
  cardanoEra = MaryEra

instance IsCardanoEra AlonzoEra where
  cardanoEra = AlonzoEra

instance IsCardanoEra BabbageEra where
  cardanoEra = BabbageEra

instance IsCardanoEra ConwayEra where
  cardanoEra = ConwayEra

type CardanoEraConstraints era =
  ( Typeable era
  , IsCardanoEra era
  )

cardanoEraConstraints
  :: ()
  => CardanoEra era
  -> (CardanoEraConstraints era => a)
  -> a
cardanoEraConstraints = \case
  ByronEra -> id
  ShelleyEra -> id
  AllegraEra -> id
  MaryEra -> id
  AlonzoEra -> id
  BabbageEra -> id
  ConwayEra -> id

data AnyCardanoEra where
  AnyCardanoEra
    :: Typeable era
    => CardanoEra era
    -> AnyCardanoEra

deriving instance Show AnyCardanoEra

instance Pretty AnyCardanoEra where
  pretty (AnyCardanoEra e) = pretty e

-- | Assumes that 'CardanoEra era' are singletons
instance Eq AnyCardanoEra where
  AnyCardanoEra era == AnyCardanoEra era' =
    isJust $ testEquality era era'

instance Bounded AnyCardanoEra where
  minBound = AnyCardanoEra ByronEra
  maxBound = AnyCardanoEra ConwayEra

instance Enum AnyCardanoEra where
  -- [e..] = [e..maxBound]
  enumFrom e = enumFromTo e maxBound

  fromEnum = \case
    AnyCardanoEra ByronEra -> 0
    AnyCardanoEra ShelleyEra -> 1
    AnyCardanoEra AllegraEra -> 2
    AnyCardanoEra MaryEra -> 3
    AnyCardanoEra AlonzoEra -> 4
    AnyCardanoEra BabbageEra -> 5
    AnyCardanoEra ConwayEra -> 6

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
        "AnyCardanoEra.toEnum: "
          <> show n
          <> " does not correspond to any known enumerated era."

instance ToJSON AnyCardanoEra where
  toJSON (AnyCardanoEra era) = toJSON era

instance FromJSON AnyCardanoEra where
  parseJSON =
    withText "AnyCardanoEra" $
      ( \case
          Right era -> pure era
          Left era -> fail $ "Failed to parse unknown era: " <> Text.unpack era
      )
        . anyCardanoEraFromStringLike

cardanoEraToStringLike :: IsString a => CardanoEra era -> a
{-# INLINE cardanoEraToStringLike #-}
cardanoEraToStringLike = \case
  ByronEra -> "Byron"
  ShelleyEra -> "Shelley"
  AllegraEra -> "Allegra"
  MaryEra -> "Mary"
  AlonzoEra -> "Alonzo"
  BabbageEra -> "Babbage"
  ConwayEra -> "Conway"

anyCardanoEraFromStringLike :: (IsString a, Eq a) => a -> Either a AnyCardanoEra
{-# INLINE anyCardanoEraFromStringLike #-}
anyCardanoEraFromStringLike = \case
  "Byron" -> pure $ AnyCardanoEra ByronEra
  "Shelley" -> pure $ AnyCardanoEra ShelleyEra
  "Allegra" -> pure $ AnyCardanoEra AllegraEra
  "Mary" -> pure $ AnyCardanoEra MaryEra
  "Alonzo" -> pure $ AnyCardanoEra AlonzoEra
  "Babbage" -> pure $ AnyCardanoEra BabbageEra
  "Conway" -> pure $ AnyCardanoEra ConwayEra
  wrong -> Left wrong

-- | Like the 'AnyCardanoEra' constructor but does not demand a 'IsCardanoEra'
-- class constraint.
anyCardanoEra :: CardanoEra era -> AnyCardanoEra
anyCardanoEra = \case
  ByronEra -> AnyCardanoEra ByronEra
  ShelleyEra -> AnyCardanoEra ShelleyEra
  AllegraEra -> AnyCardanoEra AllegraEra
  MaryEra -> AnyCardanoEra MaryEra
  AlonzoEra -> AnyCardanoEra AlonzoEra
  BabbageEra -> AnyCardanoEra BabbageEra
  ConwayEra -> AnyCardanoEra ConwayEra

-- | This pairs up some era-dependent type with a 'CardanoEra' value that tells
-- us what era it is, but hides the era type. This is useful when the era is
-- not statically known, for example when deserialising from a file.
data InAnyCardanoEra thing where
  InAnyCardanoEra
    :: Typeable era
    => CardanoEra era
    -> thing era
    -> InAnyCardanoEra thing

inAnyCardanoEra
  :: ()
  => CardanoEra era
  -> thing era
  -> InAnyCardanoEra thing
inAnyCardanoEra era a =
  cardanoEraConstraints era $ InAnyCardanoEra era a

-- ----------------------------------------------------------------------------
-- Conversion to ledger library types
--

-- | A type family that connects our era type tags to equivalent type tags used
-- in the ledger library.
--
-- This type mapping  connect types from this API with types in the
-- ledger library which allows writing conversion functions in a more generic
-- way.
type family CardanoLedgerEra era = ledgerera | ledgerera -> era where
  CardanoLedgerEra ByronEra = L.ByronEra L.StandardCrypto
  CardanoLedgerEra ShelleyEra = L.ShelleyEra L.StandardCrypto
  CardanoLedgerEra AllegraEra = L.AllegraEra L.StandardCrypto
  CardanoLedgerEra MaryEra = L.MaryEra L.StandardCrypto
  CardanoLedgerEra AlonzoEra = L.AlonzoEra L.StandardCrypto
  CardanoLedgerEra BabbageEra = L.BabbageEra L.StandardCrypto
  CardanoLedgerEra ConwayEra = L.ConwayEra L.StandardCrypto
