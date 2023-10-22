{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Api.Eon.ShelleyBasedEra
  ( -- * Shelley-based eras
    ShelleyBasedEra(..)
  , IsShelleyBasedEra(..)
  , AnyShelleyBasedEra(..)
  , anyShelleyBasedEra
  , InAnyShelleyBasedEra(..)
  , inAnyShelleyBasedEra
  , shelleyBasedToCardanoEra
  , inEonForShelleyBasedEra
  , inEonForShelleyBasedEraMaybe
  , forShelleyBasedEraInEon
  , forShelleyBasedEraInEonMaybe
  , forShelleyBasedEraMaybeEon

    -- * Cardano eras, as Byron vs Shelley-based
  , CardanoEraStyle(..)
  , cardanoEraStyle

    -- * Assertions on era
  , requireShelleyBasedEra

    -- ** Mapping to era types from the Shelley ledger library
  , ShelleyLedgerEra
  , eraProtVerLow

  , ShelleyBasedEraConstraints
  , shelleyBasedEraConstraints
  ) where

import           Cardano.Api.Eras.Core
import           Cardano.Api.Modes
import           Cardano.Api.Orphans ()

import qualified Cardano.Crypto.Hash.Blake2b as Blake2b
import qualified Cardano.Crypto.Hash.Class as C
import qualified Cardano.Crypto.VRF as C
import qualified Cardano.Ledger.Api as L
import qualified Cardano.Ledger.BaseTypes as L
import           Cardano.Ledger.Binary (FromCBOR)
import qualified Cardano.Ledger.Core as L
import qualified Cardano.Ledger.SafeHash as L
import qualified Ouroboros.Consensus.Protocol.Abstract as Consensus
import qualified Ouroboros.Consensus.Protocol.Praos.Common as Consensus
import           Ouroboros.Consensus.Shelley.Eras as Consensus (StandardAllegra, StandardAlonzo,
                   StandardBabbage, StandardConway, StandardMary, StandardShelley)
import qualified Ouroboros.Consensus.Shelley.Ledger as Consensus

import           Control.DeepSeq
import           Data.Aeson (FromJSON (..), ToJSON, toJSON, withText)
import qualified Data.Text as Text
import           Data.Type.Equality (TestEquality (..), (:~:) (Refl))
import           Data.Typeable (Typeable)

-- | Determine the value to use for a feature in a given 'ShelleyBasedEra'.
inEonForShelleyBasedEra :: ()
  => Eon eon
  => a
  -> (eon era -> a)
  -> ShelleyBasedEra era
  -> a
inEonForShelleyBasedEra no yes =
  inEonForEra no yes . shelleyBasedToCardanoEra

inEonForShelleyBasedEraMaybe :: ()
  => Eon eon
  => (eon era -> a)
  -> ShelleyBasedEra era
  -> Maybe a
inEonForShelleyBasedEraMaybe yes =
  inEonForShelleyBasedEra Nothing (Just . yes)

forShelleyBasedEraMaybeEon :: ()
  => Eon eon
  => ShelleyBasedEra era
  -> Maybe (eon era)
forShelleyBasedEraMaybeEon =
  inEonForEra Nothing Just . shelleyBasedToCardanoEra

forShelleyBasedEraInEon :: ()
  => Eon eon
  => ShelleyBasedEra era
  -> a
  -> (eon era -> a)
  -> a
forShelleyBasedEraInEon era no yes =
  inEonForShelleyBasedEra no yes era

forShelleyBasedEraInEonMaybe :: ()
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

instance Eon ShelleyBasedEra where
  inEonForEra no yes = \case
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

type ShelleyBasedEraConstraints era =
  ( C.HashAlgorithm (L.HASH (L.EraCrypto (ShelleyLedgerEra era)))
  , C.Signable (L.VRF (L.EraCrypto (ShelleyLedgerEra era))) L.Seed
  , Consensus.PraosProtocolSupportsNode (ConsensusProtocol era)
  , Consensus.ShelleyCompatible (ConsensusProtocol era) (ShelleyLedgerEra era)
  , L.ADDRHASH (Consensus.PraosProtocolSupportsNodeCrypto (ConsensusProtocol era)) ~ Blake2b.Blake2b_224
  , L.Crypto (L.EraCrypto (ShelleyLedgerEra era))
  , L.Era (ShelleyLedgerEra era)
  , L.EraCrypto (ShelleyLedgerEra era) ~ L.StandardCrypto
  , L.EraPParams (ShelleyLedgerEra era)
  , L.EraTx (ShelleyLedgerEra era)
  , L.EraTxBody (ShelleyLedgerEra era)
  , L.HashAnnotated (L.TxBody (ShelleyLedgerEra era)) L.EraIndependentTxBody L.StandardCrypto
  , L.ShelleyEraTxBody (ShelleyLedgerEra era)
  , L.ShelleyEraTxCert (ShelleyLedgerEra era)
  , FromCBOR (Consensus.ChainDepState (ConsensusProtocol era))
  , IsCardanoEra era
  , IsShelleyBasedEra era
  , ToJSON (Consensus.ChainDepState (ConsensusProtocol era))
  , Typeable era
  )

shelleyBasedEraConstraints :: ()
  => ShelleyBasedEra era
  -> (ShelleyBasedEraConstraints era => a)
  -> a
shelleyBasedEraConstraints = \case
  ShelleyBasedEraShelley -> id
  ShelleyBasedEraAllegra -> id
  ShelleyBasedEraMary    -> id
  ShelleyBasedEraAlonzo  -> id
  ShelleyBasedEraBabbage -> id
  ShelleyBasedEraConway  -> id

data AnyShelleyBasedEra where
  AnyShelleyBasedEra
    :: ShelleyBasedEra era
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

anyShelleyBasedEra :: ()
  => ShelleyBasedEra era
  -> AnyShelleyBasedEra
anyShelleyBasedEra sbe =
  AnyShelleyBasedEra sbe

-- | This pairs up some era-dependent type with a 'ShelleyBasedEra' value that
-- tells us what era it is, but hides the era type. This is useful when the era
-- is not statically known, for example when deserialising from a file.
--
data InAnyShelleyBasedEra thing where
  InAnyShelleyBasedEra
    :: IsShelleyBasedEra era
    => ShelleyBasedEra era
    -> thing era
    -> InAnyShelleyBasedEra thing

inAnyShelleyBasedEra :: ()
  => ShelleyBasedEra era
  -> thing era
  -> InAnyShelleyBasedEra thing
inAnyShelleyBasedEra sbe a =
  shelleyBasedEraConstraints sbe $ InAnyShelleyBasedEra sbe a

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

  ShelleyBasedEra
    :: ShelleyBasedEra era
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
requireShelleyBasedEra = inEonForEra (pure Nothing) (pure . Just)
