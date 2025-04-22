{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Api.Internal.Eon.ShelleyBasedEra
  ( -- * Shelley-based eras
    ShelleyBasedEra (..)
  , IsShelleyBasedEra (..)
  , AnyShelleyBasedEra (..)
  , InAnyShelleyBasedEra (..)
  , inAnyShelleyBasedEra
  , inEonForShelleyBasedEra
  , inEonForShelleyBasedEraMaybe
  , forShelleyBasedEraInEon
  , forShelleyBasedEraInEonMaybe
  , forShelleyBasedEraMaybeEon

    -- * Assertions on era
  , requireShelleyBasedEra

    -- ** Mapping to era types from the Shelley ledger library
  , ShelleyLedgerEra
  , eraProtVerLow
  , ShelleyBasedEraConstraints
  , shelleyBasedEraConstraints
  )
where

import Cardano.Api.Internal.Eon.Convert
import Cardano.Api.Internal.Eras.Core
import Cardano.Api.Internal.Modes
import Cardano.Api.Internal.Orphans ()
import Cardano.Api.Internal.Pretty (Pretty)

import Cardano.Crypto.Hash.Blake2b qualified as Blake2b
import Cardano.Crypto.Hash.Class qualified as C
import Cardano.Crypto.VRF qualified as C
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.BaseTypes qualified as L
import Cardano.Ledger.Binary (FromCBOR)
import Cardano.Ledger.Core qualified as L
import Cardano.Ledger.Shelley.Rules qualified as L
import Cardano.Ledger.State qualified as L
import Cardano.Protocol.Crypto qualified as L
import Ouroboros.Consensus.Protocol.Abstract qualified as Consensus
import Ouroboros.Consensus.Protocol.Praos.Common qualified as Consensus
import Ouroboros.Consensus.Shelley.Ledger qualified as Consensus

import Control.DeepSeq
import Data.Aeson (FromJSON (..), ToJSON, toJSON, withText)
import Data.Text qualified as Text
import Data.Type.Equality (TestEquality (..), (:~:) (Refl))
import Data.Typeable (Typeable)
import Text.Pretty (Pretty (..))

-- | Determine the value to use for a feature in a given 'ShelleyBasedEra'.
inEonForShelleyBasedEra
  :: ()
  => Eon eon
  => a
  -> (eon era -> a)
  -> ShelleyBasedEra era
  -> a
inEonForShelleyBasedEra no yes =
  inEonForEra no yes . toCardanoEra

inEonForShelleyBasedEraMaybe
  :: ()
  => Eon eon
  => (eon era -> a)
  -> ShelleyBasedEra era
  -> Maybe a
inEonForShelleyBasedEraMaybe yes =
  inEonForShelleyBasedEra Nothing (Just . yes)

forShelleyBasedEraMaybeEon
  :: ()
  => Eon eon
  => ShelleyBasedEra era
  -> Maybe (eon era)
forShelleyBasedEraMaybeEon =
  inEonForEra Nothing Just . toCardanoEra

forShelleyBasedEraInEon
  :: ()
  => Eon eon
  => ShelleyBasedEra era
  -> a
  -> (eon era -> a)
  -> a
forShelleyBasedEraInEon era no yes =
  inEonForShelleyBasedEra no yes era

forShelleyBasedEraInEonMaybe
  :: ()
  => Eon eon
  => ShelleyBasedEra era
  -> (eon era -> a)
  -> Maybe a
forShelleyBasedEraInEonMaybe era yes =
  forShelleyBasedEraInEon era Nothing (Just . yes)

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
data ShelleyBasedEra era where
  ShelleyBasedEraShelley :: ShelleyBasedEra ShelleyEra
  ShelleyBasedEraAllegra :: ShelleyBasedEra AllegraEra
  ShelleyBasedEraMary :: ShelleyBasedEra MaryEra
  ShelleyBasedEraAlonzo :: ShelleyBasedEra AlonzoEra
  ShelleyBasedEraBabbage :: ShelleyBasedEra BabbageEra
  ShelleyBasedEraConway :: ShelleyBasedEra ConwayEra

instance NFData (ShelleyBasedEra era) where
  rnf = \case
    ShelleyBasedEraShelley -> ()
    ShelleyBasedEraAllegra -> ()
    ShelleyBasedEraMary -> ()
    ShelleyBasedEraAlonzo -> ()
    ShelleyBasedEraBabbage -> ()
    ShelleyBasedEraConway -> ()

deriving instance Eq (ShelleyBasedEra era)

deriving instance Ord (ShelleyBasedEra era)

deriving instance Show (ShelleyBasedEra era)

instance Pretty (ShelleyBasedEra era) where
  pretty = pretty . toCardanoEra

instance ToJSON (ShelleyBasedEra era) where
  toJSON = toJSON . toCardanoEra

instance TestEquality ShelleyBasedEra where
  testEquality ShelleyBasedEraShelley ShelleyBasedEraShelley = Just Refl
  testEquality ShelleyBasedEraAllegra ShelleyBasedEraAllegra = Just Refl
  testEquality ShelleyBasedEraMary ShelleyBasedEraMary = Just Refl
  testEquality ShelleyBasedEraAlonzo ShelleyBasedEraAlonzo = Just Refl
  testEquality ShelleyBasedEraBabbage ShelleyBasedEraBabbage = Just Refl
  testEquality ShelleyBasedEraConway ShelleyBasedEraConway = Just Refl
  testEquality _ _ = Nothing

instance Eon ShelleyBasedEra where
  inEonForEra no yes = \case
    ByronEra -> no
    ShelleyEra -> yes ShelleyBasedEraShelley
    AllegraEra -> yes ShelleyBasedEraAllegra
    MaryEra -> yes ShelleyBasedEraMary
    AlonzoEra -> yes ShelleyBasedEraAlonzo
    BabbageEra -> yes ShelleyBasedEraBabbage
    ConwayEra -> yes ShelleyBasedEraConway

instance ToCardanoEra ShelleyBasedEra where
  toCardanoEra = \case
    ShelleyBasedEraShelley -> ShelleyEra
    ShelleyBasedEraAllegra -> AllegraEra
    ShelleyBasedEraMary -> MaryEra
    ShelleyBasedEraAlonzo -> AlonzoEra
    ShelleyBasedEraBabbage -> BabbageEra
    ShelleyBasedEraConway -> ConwayEra

instance Convert ShelleyBasedEra CardanoEra where
  convert = toCardanoEra

-- | The class of eras that are based on Shelley. This allows uniform handling
-- of Shelley-based eras, but also non-uniform by making case distinctions on
-- the 'ShelleyBasedEra' constructors.
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

type ShelleyBasedEraConstraints era =
  ( C.HashAlgorithm L.HASH
  , C.Signable (L.VRF L.StandardCrypto) L.Seed
  , Consensus.PraosProtocolSupportsNode (ConsensusProtocol era)
  , Consensus.ShelleyBlock (ConsensusProtocol era) (ShelleyLedgerEra era) ~ ConsensusBlockForEra era
  , Consensus.ShelleyCompatible (ConsensusProtocol era) (ShelleyLedgerEra era)
  , FromCBOR (Consensus.ChainDepState (ConsensusProtocol era))
  , IsCardanoEra era
  , IsShelleyBasedEra era
  , L.ADDRHASH ~ Blake2b.Blake2b_224
  , L.Era (ShelleyLedgerEra era)
  , L.EraPParams (ShelleyLedgerEra era)
  , L.EraTx (ShelleyLedgerEra era)
  , L.EraTxBody (ShelleyLedgerEra era)
  , L.EraTxOut (ShelleyLedgerEra era)
  , L.EraUTxO (ShelleyLedgerEra era)
  , L.EraTxWits (ShelleyLedgerEra era)
  , L.HashAnnotated (L.TxBody (ShelleyLedgerEra era)) L.EraIndependentTxBody
  , L.ShelleyEraTxCert (ShelleyLedgerEra era)
  , FromCBOR (Consensus.ChainDepState (ConsensusProtocol era))
  , IsCardanoEra era
  , IsShelleyBasedEra era
  , ToJSON (Consensus.ChainDepState (ConsensusProtocol era))
  , ToJSON (L.PredicateFailure (L.EraRule "LEDGER" (ShelleyLedgerEra era)))
  , Typeable era
  )

shelleyBasedEraConstraints
  :: ()
  => ShelleyBasedEra era
  -> (ShelleyBasedEraConstraints era => a)
  -> a
shelleyBasedEraConstraints = \case
  ShelleyBasedEraShelley -> id
  ShelleyBasedEraAllegra -> id
  ShelleyBasedEraMary -> id
  ShelleyBasedEraAlonzo -> id
  ShelleyBasedEraBabbage -> id
  ShelleyBasedEraConway -> id

data AnyShelleyBasedEra where
  AnyShelleyBasedEra
    :: Typeable era
    => ShelleyBasedEra era
    -> AnyShelleyBasedEra

deriving instance Show AnyShelleyBasedEra

instance Eq AnyShelleyBasedEra where
  AnyShelleyBasedEra sbe == AnyShelleyBasedEra sbe' =
    case testEquality sbe sbe' of
      Nothing -> False
      Just Refl -> True -- since no constructors share types

instance Bounded AnyShelleyBasedEra where
  minBound = AnyShelleyBasedEra ShelleyBasedEraShelley
  maxBound = AnyShelleyBasedEra ShelleyBasedEraConway

instance Enum AnyShelleyBasedEra where
  enumFrom e = enumFromTo e maxBound

  fromEnum = \case
    AnyShelleyBasedEra ShelleyBasedEraShelley -> 1
    AnyShelleyBasedEra ShelleyBasedEraAllegra -> 2
    AnyShelleyBasedEra ShelleyBasedEraMary -> 3
    AnyShelleyBasedEra ShelleyBasedEraAlonzo -> 4
    AnyShelleyBasedEra ShelleyBasedEraBabbage -> 5
    AnyShelleyBasedEra ShelleyBasedEraConway -> 6

  toEnum = \case
    1 -> AnyShelleyBasedEra ShelleyBasedEraShelley
    2 -> AnyShelleyBasedEra ShelleyBasedEraAllegra
    3 -> AnyShelleyBasedEra ShelleyBasedEraMary
    4 -> AnyShelleyBasedEra ShelleyBasedEraAlonzo
    5 -> AnyShelleyBasedEra ShelleyBasedEraBabbage
    6 -> AnyShelleyBasedEra ShelleyBasedEraConway
    n ->
      error $
        "AnyShelleyBasedEra.toEnum: "
          <> show n
          <> " does not correspond to any known enumerated era."

instance ToJSON AnyShelleyBasedEra where
  toJSON (AnyShelleyBasedEra sbe) = toJSON sbe

instance FromJSON AnyShelleyBasedEra where
  parseJSON = withText "AnyShelleyBasedEra" $
    \case
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
data InAnyShelleyBasedEra thing where
  InAnyShelleyBasedEra
    :: Typeable era
    => ShelleyBasedEra era
    -> thing era
    -> InAnyShelleyBasedEra thing

inAnyShelleyBasedEra
  :: ()
  => ShelleyBasedEra era
  -> thing era
  -> InAnyShelleyBasedEra thing
inAnyShelleyBasedEra sbe a =
  shelleyBasedEraConstraints sbe $ InAnyShelleyBasedEra sbe a

-- ----------------------------------------------------------------------------
-- Conversion to Shelley ledger library types
--

-- | A type family that connects our era type tags to equivalent type tags used
-- in the Shelley ledger library.
--
-- This type mapping  connect types from this API with types in the Shelley
-- ledger library which allows writing conversion functions in a more generic
-- way.
type family ShelleyLedgerEra era = ledgerera | ledgerera -> era where
  ShelleyLedgerEra ShelleyEra = L.ShelleyEra
  ShelleyLedgerEra AllegraEra = L.AllegraEra
  ShelleyLedgerEra MaryEra = L.MaryEra
  ShelleyLedgerEra AlonzoEra = L.AlonzoEra
  ShelleyLedgerEra BabbageEra = L.BabbageEra
  ShelleyLedgerEra ConwayEra = L.ConwayEra

-- | Lookup the lower major protocol version for the shelley based era. In other words
-- this is the major protocol version that the era has started in.
eraProtVerLow :: ShelleyBasedEra era -> L.Version
eraProtVerLow = \case
  ShelleyBasedEraShelley -> L.eraProtVerLow @L.ShelleyEra
  ShelleyBasedEraAllegra -> L.eraProtVerLow @L.AllegraEra
  ShelleyBasedEraMary -> L.eraProtVerLow @L.MaryEra
  ShelleyBasedEraAlonzo -> L.eraProtVerLow @L.AlonzoEra
  ShelleyBasedEraBabbage -> L.eraProtVerLow @L.BabbageEra
  ShelleyBasedEraConway -> L.eraProtVerLow @L.ConwayEra

requireShelleyBasedEra
  :: ()
  => Applicative m
  => CardanoEra era
  -> m (Maybe (ShelleyBasedEra era))
requireShelleyBasedEra = inEonForEra (pure Nothing) (pure . Just)
