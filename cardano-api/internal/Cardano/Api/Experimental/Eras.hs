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
module Cardano.Api.Experimental.Eras
  ( BabbageEra
  , ConwayEra
  , Era (..)
  , IsEra (..)
  , Some (..)
  , Inject (..)
  , LedgerEra
  , DeprecatedEra (..)
  , EraCommonConstraints
  , obtainCommonConstraints
  , eraToSbe
  , babbageEraOnwardsToEra
  , eraToBabbageEraOnwards
  , sbeToEra
  )
where

import           Cardano.Api.Eon.BabbageEraOnwards
import           Cardano.Api.Eon.ShelleyBasedEra (ShelleyBasedEra (..), ShelleyLedgerEra)
import qualified Cardano.Api.Eras as Api
import           Cardano.Api.Eras.Core (BabbageEra, ConwayEra, Eon (..))
import qualified Cardano.Api.ReexposeLedger as L
import           Cardano.Api.Via.ShowOf

import qualified Cardano.Ledger.Api as L
import qualified Cardano.Ledger.Babbage as Ledger
import           Cardano.Ledger.BaseTypes (Inject (..))
import qualified Cardano.Ledger.Conway as Ledger
import qualified Cardano.Ledger.Core as Ledger
import           Cardano.Ledger.Hashes
import qualified Cardano.Ledger.SafeHash as L
import qualified Cardano.Ledger.UTxO as L

import           Control.Monad.Error.Class
import           Data.Aeson (FromJSON (..), ToJSON, withText)
import           Data.Aeson.Types (ToJSON (..))
import           Data.Kind
import           Data.Maybe (isJust)
import qualified Data.Text as Text
import           Data.Type.Equality
import           Data.Typeable
import           GHC.Exts (IsString)
import           Prettyprinter

-- | Users typically interact with the latest features on the mainnet or experiment with features
-- from the upcoming era. Hence, the protocol versions are limited to the current mainnet era
-- and the next era (upcoming era).
type family LedgerEra era = (r :: Type) | r -> era where
  LedgerEra BabbageEra = Ledger.Babbage
  LedgerEra ConwayEra = Ledger.Conway

-- | An existential wrapper for types of kind @k -> Types@. Use it to hold any era e.g. @Some Era@. One can
-- then bring the era witness back into scope for example using this pattern:
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

-- | Represents the eras in Cardano's blockchain.
-- This type represents eras currently on mainnet and new eras which are
-- in development.
--
-- After a hardfork, the era from which we hardfork from gets deprecated and
-- after deprecation period, gets removed. During deprecation period,
-- consumers of cardano-api should update their codebase to the mainnet era.
data Era era where
  -- | The era currently active on Cardano's mainnet.
  BabbageEra :: Era BabbageEra
  -- | The upcoming era in development.
  ConwayEra :: Era ConwayEra

deriving instance Show (Era era)

deriving instance Eq (Era era)

instance Pretty (Era era) where
  pretty = eraToStringLike

instance TestEquality Era where
  testEquality BabbageEra BabbageEra = Just Refl
  testEquality BabbageEra _ = Nothing
  testEquality ConwayEra ConwayEra = Just Refl
  testEquality ConwayEra _ = Nothing

instance ToJSON (Era era) where
  toJSON = eraToStringLike

instance Show (Some Era) where
  showsPrec _ (Some era) = shows era

instance Eq (Some Era) where
  Some era1 == Some era2 = isJust $ testEquality era1 era2

instance Bounded (Some Era) where
  minBound = Some BabbageEra
  maxBound = Some ConwayEra

instance Enum (Some Era) where
  toEnum 0 = Some BabbageEra
  toEnum 1 = Some ConwayEra
  toEnum i = error $ "Enum.toEnum: invalid argument " <> show i <> " - does not correspond to any era"
  fromEnum (Some BabbageEra) = 0
  fromEnum (Some ConwayEra) = 1

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

eraToStringLike :: IsString a => Era era -> a
{-# INLINE eraToStringLike #-}
eraToStringLike = \case
  BabbageEra -> "Babbage"
  ConwayEra -> "Conway"

eraFromStringLike :: (IsString a, Eq a) => a -> Either a (Some Era)
{-# INLINE eraFromStringLike #-}
eraFromStringLike = \case
  "Babbage" -> pure $ Some BabbageEra
  "Conway" -> pure $ Some ConwayEra
  wrong -> Left wrong

-- | How to deprecate an era
--
--   1. Add DEPRECATED pragma to the era type tag and the era constructor at the same time:
-- @
-- {-# DEPRECATED BabbageEra "BabbageEra no longer supported, use ConwayEra" #-}
-- data BabbageEra
-- @
--
--   2. Update haddock for the constructor of the deprecated era, mentioning deprecation.
--
-- @
-- data Era era where
--   {-# DEPRECATED BabbageEra "BabbageEra no longer supported, use ConwayEra" #-}
--   BabbageEra :: Era BabbageEra
--   -- | The era currently active on Cardano's mainnet.
--   ConwayEra :: Era ConwayEra
-- @
--
--   3. Add new 'IsEra' instance and update the deprecated era instance to produce a compile-time error:
-- @
-- instance TypeError ('Text "IsEra BabbageEra: Deprecated. Update to ConwayEra") => IsEra BabbageEra where
--   useEra = error "unreachable"
--
-- instance IsEra ConwayEra where
--   useEra = ConwayEra
-- @
{-# DEPRECATED eraToSbe "Use 'inject' instead." #-}
eraToSbe
  :: Era era
  -> ShelleyBasedEra era
eraToSbe = inject

instance Inject (Era era) (Api.CardanoEra era) where
  inject = \case
    BabbageEra -> Api.BabbageEra
    ConwayEra -> Api.ConwayEra

instance Inject (Era era) (ShelleyBasedEra era) where
  inject = \case
    BabbageEra -> ShelleyBasedEraBabbage
    ConwayEra -> ShelleyBasedEraConway

instance Inject (Era era) (BabbageEraOnwards era) where
  inject = \case
    BabbageEra -> BabbageEraOnwardsBabbage
    ConwayEra -> BabbageEraOnwardsConway

instance Inject (BabbageEraOnwards era) (Era era) where
  inject = \case
    BabbageEraOnwardsBabbage -> BabbageEra
    BabbageEraOnwardsConway -> ConwayEra

newtype DeprecatedEra era
  = DeprecatedEra (ShelleyBasedEra era)
  deriving Show

deriving via (ShowOf (DeprecatedEra era)) instance Pretty (DeprecatedEra era)

sbeToEra
  :: MonadError (DeprecatedEra era) m
  => ShelleyBasedEra era
  -> m (Era era)
sbeToEra ShelleyBasedEraConway = return ConwayEra
sbeToEra ShelleyBasedEraBabbage = return BabbageEra
sbeToEra e@ShelleyBasedEraAlonzo = throwError $ DeprecatedEra e
sbeToEra e@ShelleyBasedEraMary = throwError $ DeprecatedEra e
sbeToEra e@ShelleyBasedEraAllegra = throwError $ DeprecatedEra e
sbeToEra e@ShelleyBasedEraShelley = throwError $ DeprecatedEra e

{-# DEPRECATED babbageEraOnwardsToEra "Use 'inject' instead." #-}
babbageEraOnwardsToEra :: BabbageEraOnwards era -> Era era
babbageEraOnwardsToEra = inject

{-# DEPRECATED eraToBabbageEraOnwards "Use 'inject' instead." #-}
eraToBabbageEraOnwards :: Era era -> BabbageEraOnwards era
eraToBabbageEraOnwards = inject

-------------------------------------------------------------------------

-- | Type class interface for the 'Era' type.
class IsEra era where
  useEra :: Era era

instance IsEra BabbageEra where
  useEra = BabbageEra

instance IsEra ConwayEra where
  useEra = ConwayEra

-- | A temporary compatibility instance, for easier conversion between experimental and old API.
instance Eon Era where
  inEonForEra v f = \case
    Api.ConwayEra -> f ConwayEra
    Api.BabbageEra -> f BabbageEra
    _ -> v

obtainCommonConstraints
  :: Era era
  -> (EraCommonConstraints era => a)
  -> a
obtainCommonConstraints BabbageEra x = x
obtainCommonConstraints ConwayEra x = x

type EraCommonConstraints era =
  ( L.AlonzoEraTx (LedgerEra era)
  , L.BabbageEraTxBody (LedgerEra era)
  , L.EraTx (LedgerEra era)
  , L.EraUTxO (LedgerEra era)
  , Ledger.EraCrypto (LedgerEra era) ~ L.StandardCrypto
  , ShelleyLedgerEra era ~ LedgerEra era
  , L.HashAnnotated (Ledger.TxBody (LedgerEra era)) EraIndependentTxBody L.StandardCrypto
  , IsEra era
  )
