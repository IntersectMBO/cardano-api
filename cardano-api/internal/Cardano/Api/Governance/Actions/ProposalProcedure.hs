{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Api.Governance.Actions.ProposalProcedure where

import           Cardano.Api.Eras
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.Keys.Shelley
import           Cardano.Api.SerialiseCBOR
import           Cardano.Api.SerialiseTextEnvelope
import           Cardano.Api.Utils
import           Cardano.Api.Value

import qualified Cardano.Binary as CBOR
import qualified Cardano.Ledger.Conway as Conway
import qualified Cardano.Ledger.Conway.Governance as Gov
import           Cardano.Ledger.Core (EraCrypto)
import qualified Cardano.Ledger.Core as Shelley
import           Cardano.Ledger.Crypto (StandardCrypto)
import           Cardano.Ledger.SafeHash

import           Data.ByteString (ByteString)
import           Data.Maybe.Strict

-- | A representation of whether the era supports tx governance actions.
--
-- The Conway and subsequent eras support governance actions.
--
data TxGovernanceActions era where
  TxGovernanceActionsNone :: TxGovernanceActions era

  TxGovernanceActions
    :: TxGovernanceActionSupportedInEra era
    -- (deposit, return address, governance action)
    -> [(Lovelace, Hash StakeKey, GovernanceAction)]
    -> TxGovernanceActions era

deriving instance Show (TxGovernanceActions era)
deriving instance Eq (TxGovernanceActions era)


-- | A representation of whether the era supports transactions with governance
-- actions.
--
-- The Conway and subsequent eras support governance actions.
--
data TxGovernanceActionSupportedInEra era where

     GovernanceActionsSupportedInConwayEra  :: TxGovernanceActionSupportedInEra ConwayEra

deriving instance Show (TxGovernanceActionSupportedInEra era)
deriving instance Eq (TxGovernanceActionSupportedInEra era)


data AnyGovernanceAction = forall era. AnyGovernanceAction (Gov.GovernanceAction era)

-- TODO: Conway - fill in remaining actions
data GovernanceAction
  = MotionOfNoConfidence
  | ProposeNewConstitution ByteString
  deriving (Eq, Show)

toSafeHash :: ByteString -> SafeHash StandardCrypto ByteString
toSafeHash = makeHashWithExplicitProxys (Proxy :: Proxy  StandardCrypto) (Proxy :: Proxy ByteString)

toGovernanceAction
  :: EraCrypto ledgerera ~ StandardCrypto
  => GovernanceAction
  -> Gov.GovernanceAction ledgerera
toGovernanceAction MotionOfNoConfidence = Gov.NoConfidence
toGovernanceAction (ProposeNewConstitution bs) =
  Gov.NewConstitution $ toSafeHash bs

newtype Proposal era = Proposal { unProposal :: Gov.ProposalProcedure (ShelleyLedgerEra era) }

instance (IsShelleyBasedEra era, Shelley.EraPParams (ShelleyLedgerEra era)) => ToCBOR (Proposal era) where
  toCBOR (Proposal vp) = Shelley.toEraCBOR @Conway.Conway vp

instance ( IsShelleyBasedEra era
         , Shelley.EraPParams (ShelleyLedgerEra era)
         ) => FromCBOR (Proposal era) where
  fromCBOR = Proposal <$> Shelley.fromEraCBOR @Conway.Conway

instance ( IsShelleyBasedEra era
         , Shelley.EraPParams (ShelleyLedgerEra era)
         ) => SerialiseAsCBOR (Proposal era) where

  serialiseToCBOR = CBOR.serialize'
  deserialiseFromCBOR _proxy = CBOR.decodeFull'


instance ( IsShelleyBasedEra era
         , Shelley.EraPParams (ShelleyLedgerEra era)
         ) => HasTextEnvelope (Proposal era) where
  textEnvelopeType _ = "Governance proposal"

instance HasTypeProxy era => HasTypeProxy (Proposal era) where
    data AsType (Proposal era) = AsProposal
    proxyToAsType _ = AsProposal


createProposalProcedure
  :: ShelleyBasedEra era
  -> Lovelace -- ^ Deposit
  -> Hash StakeKey -- ^ Return address
  -> GovernanceAction
  -> Proposal era
createProposalProcedure sbe dep (StakeKeyHash retAddrh) govAct =
  Proposal $ obtainEraCryptoConstraints sbe $
    Gov.ProposalProcedure
      { Gov.pProcDeposit = toShelleyLovelace dep
      , Gov.pProcReturnAddr = retAddrh
      , Gov.pProcGovernanceAction = toGovernanceAction govAct
      , Gov.pProcAnchor = SNothing -- TODO: Conway
      }
