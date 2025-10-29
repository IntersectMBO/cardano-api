{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}

-- | This module defines the protocol versions corresponding to the eras in the Cardano blockchain.
module Cardano.Api.Experimental.Era
  ( BabbageEra
  , ConwayEra
  , Era (..)
  , IsEra (..)
  , Some (..)
  , Inject (..)
  , Convert (..)
  , LedgerEra
  , DeprecatedEra (..)
  , EraCommonConstraints
  , obtainCommonConstraints
  , obtainConwayConstraints
  , eraToSbe
  , eraToBabbageEraOnwards
  , sbeToEra
  )
where

import Cardano.Api.Consensus.Internal.Mode
import Cardano.Api.Consensus.Internal.Reexport
import Cardano.Api.Era qualified as Api
import Cardano.Api.Era.Internal.Core (BabbageEra, ConwayEra, DijkstraEra, Eon (..))
import Cardano.Api.Era.Internal.Eon.AlonzoEraOnwards
import Cardano.Api.Era.Internal.Eon.BabbageEraOnwards
import Cardano.Api.Era.Internal.Eon.Convert
import Cardano.Api.Era.Internal.Eon.ConwayEraOnwards
import Cardano.Api.Era.Internal.Eon.MaryEraOnwards
import Cardano.Api.Era.Internal.Eon.ShelleyBasedEra (ShelleyBasedEra (..), ShelleyLedgerEra)
import Cardano.Api.Error
import Cardano.Api.Ledger.Internal.Reexport qualified as L
import Cardano.Api.Pretty.Internal.ShowOf

import Cardano.Binary
import Cardano.Ledger.Allegra.Scripts qualified as L
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.BaseTypes (Inject (..))
import Cardano.Ledger.Conway qualified as Ledger
import Cardano.Ledger.Core qualified as Ledger
import Cardano.Ledger.Hashes qualified as L
import Cardano.Ledger.State qualified as L

import Control.Monad.Error.Class
import Data.Aeson (FromJSON (..), ToJSON, withText)
import Data.Aeson.Types (ToJSON (..))
import Data.Kind
import Data.Maybe (isJust)
import Data.Text qualified as Text
import Data.Type.Equality
import Data.Typeable
import GHC.Exts (IsString)
import Prettyprinter

-- | Users typically interact with the latest features on the mainnet or experiment with features
-- from the upcoming era. Therefore, protocol versions are limited to the current mainnet era
-- and the next (upcoming) era.
type family LedgerEra era = (r :: Type) | r -> era where
  LedgerEra ConwayEra = Ledger.ConwayEra
  LedgerEra DijkstraEra = L.DijkstraEra

-- | An existential wrapper for types of kind @k -> Type@. It can hold any
-- era, for example, @Some Era@. The era witness can be brought back into scope,
-- for example, using this pattern:
--
-- @
-- anyEra = Some ConwayEra
-- -- then later in the code
-- Some era <- pure anyEra
-- obtainCommonConstraints era foo
-- @
data Some (f :: k -> Type) where
  Some
    :: forall f a
     . (Typeable a, Typeable (f a))
    => f a
    -> Some f

-- | Represents the latest Cardano blockchain eras, including
-- the one currently on mainnet and the upcoming one.
--
-- After a hard fork takes place, the era on mainnet before the hard fork
-- is deprecated and, after a deprecation period, removed from @cardano-api@.
-- During the deprecation period, @cardano-api@ users should update their
-- codebase to the new mainnet era.
data Era era where
  -- | The currently active era on the Cardano mainnet.
  ConwayEra :: Era ConwayEra
  DijkstraEra :: Era DijkstraEra

deriving instance Show (Era era)

deriving instance Eq (Era era)

instance Pretty (Era era) where
  pretty = eraToStringLike

instance TestEquality Era where
  testEquality ConwayEra ConwayEra = Just Refl
  testEquality DijkstraEra DijkstraEra = Just Refl
  testEquality _ _ = Nothing

instance ToJSON (Era era) where
  toJSON = eraToStringLike

instance Show (Some Era) where
  showsPrec _ (Some era) = shows era

instance Eq (Some Era) where
  Some era1 == Some era2 = isJust $ testEquality era1 era2

instance Bounded (Some Era) where
  minBound = Some ConwayEra
  maxBound = Some ConwayEra

instance Enum (Some Era) where
  toEnum 0 = Some ConwayEra
  toEnum i = error $ "Enum.toEnum: invalid argument " <> show i <> " - does not correspond to any era"
  fromEnum (Some ConwayEra) = 0
  fromEnum (Some DijkstraEra) = 1

instance Ord (Some Era) where
  compare e1 e2 = compare (fromEnum e1) (fromEnum e2)

instance Pretty (Some Era) where
  pretty (Some era) = pretty era

instance ToJSON (Some Era) where
  toJSON (Some era) = toJSON era

instance FromJSON (Some Era) where
  parseJSON =
    withText "Some Era" $
      ( \case
          Right era -> pure era
          Left era -> fail $ "Failed to parse unknown era: " <> Text.unpack era
      )
        . eraFromStringLike

-- | A temporary compatibility instance for easier conversion between the experimental and old APIs.
instance Eon Era where
  inEonForEra v f = \case
    Api.ConwayEra -> f ConwayEra
    _ -> v

-- | A temporary compatibility instance for easier conversion between the experimental and old APIs.
instance Api.ToCardanoEra Era where
  toCardanoEra = \case
    ConwayEra -> Api.ConwayEra
    DijkstraEra -> Api.DijkstraEra

eraToStringLike :: IsString a => Era era -> a
{-# INLINE eraToStringLike #-}
eraToStringLike = \case
  ConwayEra -> "Conway"
  DijkstraEra -> "Dijkstra"

eraFromStringLike :: (IsString a, Eq a) => a -> Either a (Some Era)
{-# INLINE eraFromStringLike #-}
eraFromStringLike = \case
  "Conway" -> pure $ Some ConwayEra
  "Dijkstra" -> pure $ Some DijkstraEra
  wrong -> Left wrong

-- | How to deprecate an era:
--
--   1. Add the DEPRECATED pragma to the era type tag and constructor at the same time:
--
-- @
-- {-# DEPRECATED BabbageEra "BabbageEra no longer supported, use ConwayEra" #-}
-- data BabbageEra
-- @
--
--   2. Update the Haddock documentation for the constructor of the deprecated era, mentioning the deprecation.
--
-- @
-- data Era era where
--   {-# DEPRECATED BabbageEra "BabbageEra no longer supported, use ConwayEra" #-}
--   BabbageEra :: Era BabbageEra
--   -- | The era currently active on Cardano's mainnet.
--   ConwayEra :: Era ConwayEra
-- @
--
--   3. Add a new 'IsEra' instance and update the deprecated era instance to
--   produce a compile-time error:
--
-- @
-- instance TypeError ('Text "IsEra BabbageEra: Deprecated. Update to ConwayEra") => IsEra BabbageEra where
--   useEra = error "unreachable"
--
-- instance IsEra ConwayEra where
--   useEra = ConwayEra
-- @
{-# DEPRECATED eraToSbe "Use 'convert' instead." #-}
eraToSbe
  :: Era era
  -> ShelleyBasedEra era
eraToSbe = convert

instance Convert Era Api.CardanoEra where
  convert = \case
    ConwayEra -> Api.ConwayEra
    DijkstraEra -> Api.DijkstraEra

instance Convert Era ShelleyBasedEra where
  convert = \case
    ConwayEra -> ShelleyBasedEraConway
    DijkstraEra -> ShelleyBasedEraDijkstra

instance Convert Era AlonzoEraOnwards where
  convert = \case
    ConwayEra -> AlonzoEraOnwardsConway
    DijkstraEra -> AlonzoEraOnwardsDijkstra

instance Convert Era BabbageEraOnwards where
  convert = \case
    ConwayEra -> BabbageEraOnwardsConway
    DijkstraEra -> BabbageEraOnwardsDijkstra

instance Convert Era MaryEraOnwards where
  convert = \case
    ConwayEra -> MaryEraOnwardsConway
    DijkstraEra -> MaryEraOnwardsDijkstra

instance Convert Era ConwayEraOnwards where
  convert = \case
    ConwayEra -> ConwayEraOnwardsConway
    DijkstraEra -> ConwayEraOnwardsDijkstra

instance Convert ConwayEraOnwards Era where
  convert = \case
    ConwayEraOnwardsConway -> ConwayEra
    ConwayEraOnwardsDijkstra -> DijkstraEra

newtype DeprecatedEra era
  = DeprecatedEra (ShelleyBasedEra era)
  deriving Show

deriving via (ShowOf (DeprecatedEra era)) instance Pretty (DeprecatedEra era)

instance Error (DeprecatedEra era) where
  prettyError (DeprecatedEra era) =
    "The era " <> pretty (show era) <> " is deprecated and no longer supported."

sbeToEra
  :: MonadError (DeprecatedEra era) m
  => ShelleyBasedEra era
  -> m (Era era)
sbeToEra ShelleyBasedEraConway = return ConwayEra
sbeToEra ShelleyBasedEraDijkstra = return DijkstraEra
sbeToEra e@ShelleyBasedEraBabbage = throwError $ DeprecatedEra e
sbeToEra e@ShelleyBasedEraAlonzo = throwError $ DeprecatedEra e
sbeToEra e@ShelleyBasedEraMary = throwError $ DeprecatedEra e
sbeToEra e@ShelleyBasedEraAllegra = throwError $ DeprecatedEra e
sbeToEra e@ShelleyBasedEraShelley = throwError $ DeprecatedEra e

{-# DEPRECATED eraToBabbageEraOnwards "Use 'convert' instead." #-}
eraToBabbageEraOnwards :: Era era -> BabbageEraOnwards era
eraToBabbageEraOnwards = convert

-------------------------------------------------------------------------

-- | Type class interface for the 'Era' type.
class IsEra era where
  useEra :: Era era

instance IsEra ConwayEra where
  useEra = ConwayEra

instance IsEra DijkstraEra where
  useEra = DijkstraEra

obtainCommonConstraints
  :: Era era
  -> (EraCommonConstraints era => a)
  -> a
obtainCommonConstraints = \case
  ConwayEra -> id
  DijkstraEra -> id

obtainConwayConstraints :: Era ConwayEra -> (EraConwayConstraints => a) -> a
obtainConwayConstraints ConwayEra a = a

type EraCommonConstraints era =
  ( L.AllegraEraScript (LedgerEra era)
  , L.AlonzoEraTx (LedgerEra era)
  , L.BabbageEraPParams (LedgerEra era)
  , L.BabbageEraTxBody (LedgerEra era)
  , L.ConwayEraTxBody (LedgerEra era)
  , L.ConwayEraTxCert (LedgerEra era)
  , L.Era (LedgerEra era)
  , L.EraScript (LedgerEra era)
  , L.EraTx (LedgerEra era)
  , L.EraTxCert (LedgerEra era)
  , L.EraTxOut (LedgerEra era)
  , L.EraUTxO (LedgerEra era)
  , FromCBOR (ChainDepState (ConsensusProtocol era))
  , PraosProtocolSupportsNode (ConsensusProtocol era)
  , ShelleyLedgerEra era ~ LedgerEra era
  , ToJSON (ChainDepState (ConsensusProtocol era))
  , L.HashAnnotated (Ledger.TxBody (LedgerEra era)) L.EraIndependentTxBody
  , Api.IsCardanoEra era
  , Api.IsShelleyBasedEra era
  , IsEra era
  )

type EraConwayConstraints =
  ( EraCommonConstraints ConwayEra
  , L.TxCert (LedgerEra ConwayEra) ~ L.ConwayTxCert (LedgerEra ConwayEra)
  , L.ShelleyEraTxCert (LedgerEra ConwayEra)
  )
