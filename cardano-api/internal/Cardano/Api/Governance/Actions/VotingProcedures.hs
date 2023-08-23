{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.Governance.Actions.VotingProcedures where

import           Cardano.Api.Address
import           Cardano.Api.Eras.Constraints
import           Cardano.Api.Eras.Core
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.SerialiseCBOR
import           Cardano.Api.SerialiseTextEnvelope

import qualified Cardano.Binary as CBOR
import qualified Cardano.Ledger.Api as L
import qualified Cardano.Ledger.Core as L

import           GHC.Generics

newtype VotingProcedures era = VotingProcedures
  { unVotingProcedures  :: L.VotingProcedures (ShelleyLedgerEra era)
  }

deriving instance Eq (VotingProcedures era)
deriving instance Generic (VotingProcedures era)
deriving instance Show (VotingProcedures era)

instance IsShelleyBasedEra era => ToCBOR (VotingProcedures era) where
  toCBOR = \case
    VotingProcedures vp ->
      shelleyBasedEraConstraints (shelleyBasedEra @era)
        $ L.toEraCBOR @(ShelleyLedgerEra era) vp

instance IsShelleyBasedEra era => FromCBOR (VotingProcedures era) where
  fromCBOR =
    shelleyBasedEraConstraints (shelleyBasedEra @era)
      $ VotingProcedures <$> L.fromEraCBOR @(ShelleyLedgerEra era)

instance IsShelleyBasedEra era => SerialiseAsCBOR (VotingProcedures era) where
  serialiseToCBOR = shelleyBasedEraConstraints (shelleyBasedEra @era) CBOR.serialize'
  deserialiseFromCBOR _proxy = shelleyBasedEraConstraints (shelleyBasedEra @era) CBOR.decodeFull'

instance IsShelleyBasedEra era => HasTextEnvelope (VotingProcedures era) where
  textEnvelopeType _ = "Governance voting procedures"

instance HasTypeProxy era => HasTypeProxy (VotingProcedures era) where
  data AsType (VotingProcedures era) = AsVotingProcedures
  proxyToAsType _ = AsVotingProcedures
