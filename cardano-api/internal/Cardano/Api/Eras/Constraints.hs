{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}


-- | Cardano eras, sometimes we have to distinguish them.
--
module Cardano.Api.Eras.Constraints
  ( withShelleyBasedEraConstraintsForLedger
  , cardanoEraConstraints
  , shelleyBasedEraConstraints
  ) where

import           Cardano.Api.Eras.Core

import qualified Cardano.Crypto.Hash.Class as C
import qualified Cardano.Ledger.Api as L

import           Data.Typeable (Typeable)

withShelleyBasedEraConstraintsForLedger :: ()
  => ShelleyLedgerEra era ~ ledgerera
  => ShelleyBasedEra era
  ->  ( ()
      => L.EraCrypto ledgerera ~ L.StandardCrypto
      => L.EraTx ledgerera
      => L.EraTxBody ledgerera
      => L.Era ledgerera
      => a
      )
  -> a
withShelleyBasedEraConstraintsForLedger = \case
  ShelleyBasedEraShelley -> id
  ShelleyBasedEraAllegra -> id
  ShelleyBasedEraMary    -> id
  ShelleyBasedEraAlonzo  -> id
  ShelleyBasedEraBabbage -> id
  ShelleyBasedEraConway  -> id

cardanoEraConstraints :: CardanoEra era -> (Typeable era => IsCardanoEra era => a) -> a
cardanoEraConstraints = \case
  ByronEra   -> id
  ShelleyEra -> id
  AllegraEra -> id
  MaryEra    -> id
  AlonzoEra  -> id
  BabbageEra -> id
  ConwayEra  -> id

shelleyBasedEraConstraints :: ()
  => ShelleyBasedEra era
  -> (()
      => Typeable era
      => IsShelleyBasedEra era
      => L.EraCrypto (ShelleyLedgerEra era) ~ L.StandardCrypto
      => L.Crypto (L.EraCrypto (ShelleyLedgerEra era))
      => L.EraPParams (ShelleyLedgerEra era)
      => IsShelleyBasedEra era
      => L.Era (ShelleyLedgerEra era)
      => C.HashAlgorithm (L.HASH (L.EraCrypto (ShelleyLedgerEra era)))
      => a)
  -> a
shelleyBasedEraConstraints = \case
  ShelleyBasedEraShelley -> id
  ShelleyBasedEraAllegra -> id
  ShelleyBasedEraMary    -> id
  ShelleyBasedEraAlonzo  -> id
  ShelleyBasedEraBabbage -> id
  ShelleyBasedEraConway  -> id
