{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.Governance.Actions.VotingEntry where

import           Cardano.Api.Address
import           Cardano.Api.Eras
import           Cardano.Api.Governance.Actions.VotingProcedure
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.SerialiseCBOR (FromCBOR (fromCBOR), SerialiseAsCBOR (..),
                   ToCBOR (toCBOR))
import           Cardano.Api.SerialiseTextEnvelope

import qualified Cardano.Binary as CBOR

-- TODO Conway: Ledger needs to provide this triplet as a type.  Once that is done,
-- this needs to be converted to a newtype wrapper around that type.  This will allow
-- us to delegate the ToCBOR and FromCBOR instances to the ledger where it can be
-- specified in the cddl spec.
data VotingEntry era = VotingEntry
  { votingEntryVoter            :: Voter era
  , votingEntryGovActionId      :: GovernanceActionId era
  , votingEntryVotingProcedure  :: VotingProcedure era
  } deriving (Show, Eq)

instance IsShelleyBasedEra era => ToCBOR (VotingEntry era) where
  toCBOR = \case
    VotingEntry a b c ->
      CBOR.encodeListLen 3
        <> toCBOR a
        <> toCBOR b
        <> toCBOR c

  encodedSizeExpr size _ =
    1
    + size (Proxy @(Voter era))
    + size (Proxy @(GovernanceActionId era))
    + size (Proxy @(VotingProcedure era))

instance IsShelleyBasedEra era => FromCBOR (VotingEntry era) where
  fromCBOR = do
    CBOR.decodeListLenOf 3
    !votingEntryVoter <- fromCBOR
    !votingEntryGovActionId <- fromCBOR
    !votingEntryVotingProcedure <- fromCBOR
    return VotingEntry
      { votingEntryVoter
      , votingEntryGovActionId
      , votingEntryVotingProcedure
      }

instance IsShelleyBasedEra era => SerialiseAsCBOR (VotingEntry era) where
  serialiseToCBOR = shelleyBasedEraConstraints (shelleyBasedEra @era) CBOR.serialize'
  deserialiseFromCBOR _proxy = shelleyBasedEraConstraints (shelleyBasedEra @era) CBOR.decodeFull'

instance IsShelleyBasedEra era => HasTextEnvelope (VotingEntry era) where
  textEnvelopeType _ = "Governance voting entry"

instance HasTypeProxy era => HasTypeProxy (VotingEntry era) where
  data AsType (VotingEntry era) = AsVotingEntry
  proxyToAsType _ = AsVotingEntry
