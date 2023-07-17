{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Api.Governance.Actions.ProposalProcedure where

import           Cardano.Api.Address
import           Cardano.Api.Eras
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.Keys.Shelley
import           Cardano.Api.ProtocolParameters
import           Cardano.Api.SerialiseCBOR
import           Cardano.Api.SerialiseTextEnvelope
import           Cardano.Api.Utils
import           Cardano.Api.Value

import qualified Cardano.Binary as CBOR
import           Cardano.Ledger.BaseTypes
import qualified Cardano.Ledger.Conway as Conway
import qualified Cardano.Ledger.Conway.Governance as Gov
import           Cardano.Ledger.Core (EraCrypto)
import qualified Cardano.Ledger.Core as Shelley
import           Cardano.Ledger.Crypto (StandardCrypto)
import           Cardano.Ledger.Keys (HasKeyRole (coerceKeyRole))
import           Cardano.Ledger.SafeHash

import           Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | A representation of whether the era supports tx governance actions.
--
-- The Conway and subsequent eras support governance actions.
--
data TxGovernanceActions era where
  TxGovernanceActionsNone :: TxGovernanceActions era

  TxGovernanceActions
    :: TxGovernanceActionSupportedInEra era
    -> [Proposal era]
    -> TxGovernanceActions era

deriving instance IsShelleyBasedEra era => Show (TxGovernanceActions era)
deriving instance IsShelleyBasedEra era => Eq (TxGovernanceActions era)


-- | A representation of whether the era supports transactions with governance
-- actions.
--
-- The Conway and subsequent eras support governance actions.
--
data TxGovernanceActionSupportedInEra era where

     GovernanceActionsSupportedInConwayEra  :: TxGovernanceActionSupportedInEra ConwayEra

deriving instance Show (TxGovernanceActionSupportedInEra era)
deriving instance Eq (TxGovernanceActionSupportedInEra era)

governanceActionsSupportedInEra :: ShelleyBasedEra era -> Maybe (TxGovernanceActionSupportedInEra era)
governanceActionsSupportedInEra ShelleyBasedEraShelley = Nothing
governanceActionsSupportedInEra ShelleyBasedEraAllegra = Nothing
governanceActionsSupportedInEra ShelleyBasedEraMary    = Nothing
governanceActionsSupportedInEra ShelleyBasedEraAlonzo  = Nothing
governanceActionsSupportedInEra ShelleyBasedEraBabbage = Nothing
governanceActionsSupportedInEra ShelleyBasedEraConway  = Just GovernanceActionsSupportedInConwayEra


data AnyGovernanceAction = forall era. AnyGovernanceAction (Gov.GovernanceAction era)

-- TODO: Conway - fill in remaining actions
data GovernanceAction
  = MotionOfNoConfidence
  | ProposeNewConstitution ByteString
  | ProposeNewCommittee [Hash StakeKey] Rational -- NB: This also includes stake pool keys
  | InfoAct
  | TreasuryWithdrawal [(StakeCredential, Lovelace)]
  | InitiateHardfork ProtVer
  | UpdatePParams ProtocolParametersUpdate
  deriving (Eq, Show)


toSafeHash :: ByteString -> SafeHash StandardCrypto ByteString
toSafeHash = makeHashWithExplicitProxys (Proxy :: Proxy  StandardCrypto) (Proxy :: Proxy ByteString)

toGovernanceAction
  :: EraCrypto ledgerera ~ StandardCrypto
  => ShelleyLedgerEra era ~ ledgerera
  => ShelleyBasedEra era
  -> GovernanceAction
  -> Gov.GovernanceAction ledgerera
toGovernanceAction _ MotionOfNoConfidence = Gov.NoConfidence
toGovernanceAction _ (ProposeNewConstitution bs) =
  Gov.NewConstitution $ toSafeHash bs
toGovernanceAction _ (ProposeNewCommittee stakeKeys quor) =
  Gov.NewCommittee (Set.fromList $ map (\(StakeKeyHash sk) -> coerceKeyRole sk) stakeKeys) quor
toGovernanceAction _ InfoAct = Gov.InfoAction
toGovernanceAction _ (TreasuryWithdrawal withdrawals) =
  let m = Map.fromList [(toShelleyStakeCredential sc, toShelleyLovelace l) | (sc,l) <- withdrawals]
  in Gov.TreasuryWithdrawals m
toGovernanceAction _ (InitiateHardfork pVer) = Gov.HardForkInitiation pVer
toGovernanceAction sbe (UpdatePParams ppup) =
  case toLedgerPParamsUpdate sbe ppup of
    Left e -> error $ "toGovernanceAction: " <> show e
    -- TODO: Conway era - remove use of error. Ideally we will use the ledger's PParams type
    -- in place of ProtocolParametersUpdate
    Right ppup' -> Gov.ParameterChange ppup'

fromGovernanceAction
  :: EraCrypto (ShelleyLedgerEra era) ~ StandardCrypto
  => ShelleyBasedEra era
  -> Gov.GovernanceAction (ShelleyLedgerEra era)
  -> GovernanceAction
fromGovernanceAction _ Gov.NoConfidence = MotionOfNoConfidence
fromGovernanceAction sbe (Gov.NewConstitution h) =
  ProposeNewConstitution $ obtainSafeToHashConstraint sbe $ originalBytes h
fromGovernanceAction sbe (Gov.ParameterChange pparams) =
  UpdatePParams $ fromLedgerPParamsUpdate sbe pparams
fromGovernanceAction _ (Gov.HardForkInitiation pVer) =
  InitiateHardfork pVer
fromGovernanceAction _ (Gov.TreasuryWithdrawals withdrawlMap) =
  let res = [ (fromShelleyStakeCredential lScred , fromShelleyLovelace coin)
            | (lScred, coin) <- Map.toList withdrawlMap
            ]
  in TreasuryWithdrawal res
fromGovernanceAction _ (Gov.NewCommittee proposedMembers quor) =
  let stakeCred = map (StakeKeyHash . coerceKeyRole) $ Set.toList proposedMembers
  in ProposeNewCommittee stakeCred quor
fromGovernanceAction _ Gov.InfoAction = InfoAct

newtype Proposal era = Proposal { unProposal :: Gov.ProposalProcedure (ShelleyLedgerEra era) }

instance IsShelleyBasedEra era => Show (Proposal era) where
  show (Proposal pp) = do
    let ppStr = withShelleyBasedEraConstraintsForLedger (shelleyBasedEra @era) $ show pp
    "Proposal {unProposal = " <> ppStr <> "}"

instance IsShelleyBasedEra era => Eq (Proposal era) where
  (Proposal pp1) == (Proposal pp2) = withShelleyBasedEraConstraintsForLedger (shelleyBasedEra @era) $ pp1 == pp2

instance IsShelleyBasedEra era => ToCBOR (Proposal era) where
  toCBOR (Proposal vp) = withShelleyBasedEraConstraintsForLedger (shelleyBasedEra @era) $ Shelley.toEraCBOR @Conway.Conway vp

instance IsShelleyBasedEra era => FromCBOR (Proposal era) where
  fromCBOR = Proposal <$> withShelleyBasedEraConstraintsForLedger (shelleyBasedEra @era) (Shelley.fromEraCBOR @Conway.Conway)

instance IsShelleyBasedEra era => SerialiseAsCBOR (Proposal era) where
  serialiseToCBOR = withShelleyBasedEraConstraintsForLedger (shelleyBasedEra @era) CBOR.serialize'
  deserialiseFromCBOR _proxy = withShelleyBasedEraConstraintsForLedger (shelleyBasedEra @era) CBOR.decodeFull'

instance IsShelleyBasedEra era => HasTextEnvelope (Proposal era) where
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
      , Gov.pProcGovernanceAction = toGovernanceAction sbe govAct
      , Gov.pProcAnchor = SNothing -- TODO: Conway
      }

fromProposalProcedure
  :: ShelleyBasedEra era
  -> Proposal era
  -> (Lovelace, Hash StakeKey, GovernanceAction)
fromProposalProcedure sbe (Proposal pp) =
  ( fromShelleyLovelace $ Gov.pProcDeposit pp
  , StakeKeyHash (obtainEraCryptoConstraints sbe (Gov.pProcReturnAddr pp))
  , obtainEraCryptoConstraints sbe $ fromGovernanceAction sbe (Gov.pProcGovernanceAction pp)
  )
