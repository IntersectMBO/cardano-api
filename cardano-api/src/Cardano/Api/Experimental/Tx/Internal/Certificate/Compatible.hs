{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Api.Experimental.Tx.Internal.Certificate.Compatible
  ( Delegatee

    -- * Registering stake address and delegating
  , makeStakeAddressDelegationCertificate
  , makeStakeAddressRegistrationCertificate
  , makeStakeAddressUnregistrationCertificate
  , StakeCredentialAndDeposit (..)
  , StakeRegistrationRequirements

    -- * Registering stake pools
  , makeStakePoolRegistrationCertificate
  , makeStakePoolRetirementCertificate

    -- * Special certificates
  , makeMIRCertificate
  , makeGenesisKeyDelegationCertificate
  , Ledger.MIRTarget (..)
  , Ledger.MIRPot (..)
  , selectStakeCredentialWitness

    -- * Internal
  , getTxCertWitness
  )
where

import Cardano.Api.Address
import Cardano.Api.Era.Internal.Core
import Cardano.Api.Era.Internal.Eon.ShelleyBasedEra
import Cardano.Api.Experimental.Tx.Internal.Certificate.Type
import Cardano.Api.Hash qualified as Api
import Cardano.Api.Key.Internal qualified as Api
import Cardano.Api.Key.Internal.Praos qualified as Api
import Cardano.Api.Ledger.Internal.Reexport qualified as Ledger
import Cardano.Api.Plutus.Internal.Script

import Cardano.Ledger.Keys qualified as Ledger

type family Delegatee era where
  Delegatee DijkstraEra = Ledger.Delegatee
  Delegatee ConwayEra = Ledger.Delegatee
  Delegatee BabbageEra = Api.Hash Api.StakePoolKey
  Delegatee AlonzoEra = Api.Hash Api.StakePoolKey
  Delegatee MaryEra = Api.Hash Api.StakePoolKey
  Delegatee AllegraEra = Api.Hash Api.StakePoolKey
  Delegatee ShelleyEra = Api.Hash Api.StakePoolKey

makeStakeAddressDelegationCertificate
  :: forall era
   . IsShelleyBasedEra era
  => StakeCredential
  -> Delegatee era
  -> Certificate (ShelleyLedgerEra era)
makeStakeAddressDelegationCertificate sCred delegatee =
  case shelleyBasedEra @era of
    ShelleyBasedEraConway ->
      Certificate $
        Ledger.mkDelegTxCert (toShelleyStakeCredential sCred) delegatee
    e@ShelleyBasedEraBabbage -> cert e delegatee
    e@ShelleyBasedEraAlonzo -> cert e delegatee
    e@ShelleyBasedEraMary -> cert e delegatee
    e@ShelleyBasedEraAllegra -> cert e delegatee
    e@ShelleyBasedEraShelley -> cert e delegatee
    ShelleyBasedEraDijkstra -> error "TODO: makeStakeAddressDelegationCertificate DijkstraEra"
 where
  cert
    :: Delegatee era ~ Api.Hash Api.StakePoolKey
    => ShelleyBasedEra era -> Delegatee era -> Certificate (ShelleyLedgerEra era)
  cert e delegatee' =
    shelleyBasedEraConstraints e $
      Certificate $
        Ledger.mkDelegStakeTxCert (toShelleyStakeCredential sCred) (Api.unStakePoolKeyHash delegatee')

data StakeCredentialAndDeposit = StakeCredentialAndDeposit StakeCredential Ledger.Coin

type family StakeRegistrationRequirements era where
  StakeRegistrationRequirements DijkstraEra = StakeCredentialAndDeposit
  StakeRegistrationRequirements ConwayEra = StakeCredentialAndDeposit
  StakeRegistrationRequirements BabbageEra = StakeCredential
  StakeRegistrationRequirements AlonzoEra = StakeCredential
  StakeRegistrationRequirements MaryEra = StakeCredential
  StakeRegistrationRequirements AllegraEra = StakeCredential
  StakeRegistrationRequirements ShelleyEra = StakeCredential

makeStakeAddressRegistrationCertificate
  :: forall era
   . IsShelleyBasedEra era
  => StakeRegistrationRequirements era
  -> Certificate (ShelleyLedgerEra era)
makeStakeAddressRegistrationCertificate scred =
  case shelleyBasedEra @era of
    ShelleyBasedEraDijkstra ->
      createRegCertWithDeposit scred
    ShelleyBasedEraConway ->
      createRegCertWithDeposit scred
    ShelleyBasedEraBabbage ->
      createRegCertNoDeposit scred
    ShelleyBasedEraAlonzo ->
      createRegCertNoDeposit scred
    ShelleyBasedEraMary ->
      createRegCertNoDeposit scred
    ShelleyBasedEraAllegra ->
      createRegCertNoDeposit scred
    ShelleyBasedEraShelley ->
      createRegCertNoDeposit scred
 where
  createRegCertWithDeposit stakeCredWithDeposit =
    let StakeCredentialAndDeposit cred dep = stakeCredWithDeposit
     in Certificate $
          Ledger.mkRegDepositTxCert (toShelleyStakeCredential cred) dep
  createRegCertNoDeposit stakeCredential =
    Certificate $
      Ledger.mkRegTxCert $
        toShelleyStakeCredential stakeCredential

makeStakeAddressUnregistrationCertificate
  :: forall era
   . IsShelleyBasedEra era
  => StakeCredential -> Certificate (ShelleyLedgerEra era)
makeStakeAddressUnregistrationCertificate scred =
  shelleyBasedEraConstraints (shelleyBasedEra @era) $
    Certificate $
      Ledger.mkUnRegTxCert $
        toShelleyStakeCredential scred

makeStakePoolRegistrationCertificate
  :: forall era
   . IsShelleyBasedEra era
  => Ledger.PoolParams
  -> Certificate (ShelleyLedgerEra era)
makeStakePoolRegistrationCertificate poolParams =
  shelleyBasedEraConstraints (shelleyBasedEra @era) $
    Certificate $
      Ledger.mkRegPoolTxCert poolParams

makeStakePoolRetirementCertificate
  :: forall era
   . IsShelleyBasedEra era
  => Api.Hash Api.StakePoolKey
  -> Ledger.EpochNo
  -> Certificate (ShelleyLedgerEra era)
makeStakePoolRetirementCertificate poolId retirementEpoch =
  shelleyBasedEraConstraints (shelleyBasedEra @era) $
    Certificate $
      Ledger.mkRetirePoolTxCert (Api.unStakePoolKeyHash poolId) retirementEpoch

-- This is only used by QA and only exists up until the Babbage era.
-- The serialization does not change from Shelley -> Babbage therefore
-- we hardcode the Babbage era here to simplify the type signature.
makeMIRCertificate
  :: Ledger.MIRPot
  -> Ledger.MIRTarget
  -> Certificate (ShelleyLedgerEra BabbageEra)
makeMIRCertificate mirPot mirTarget =
  Certificate $
    Ledger.ShelleyTxCertMir $
      Ledger.MIRCert mirPot mirTarget

-- This is only used by QA and only exists up until the Babbage era.
-- The serialization does not change from Shelley -> Babbage therefore
-- we hardcode the Babbage era here to simplify the type signature.
makeGenesisKeyDelegationCertificate
  :: Api.Hash Api.GenesisKey
  -> Api.Hash Api.GenesisDelegateKey
  -> Api.Hash Api.VrfKey
  -> Certificate (ShelleyLedgerEra BabbageEra)
makeGenesisKeyDelegationCertificate
  (Api.GenesisKeyHash hGenKey)
  (Api.GenesisDelegateKeyHash hGenDelegKey)
  (Api.VrfKeyHash hVrfKey) =
    Certificate $
      Ledger.mkGenesisDelegTxCert $
        Ledger.GenesisDelegCert hGenKey hGenDelegKey (Ledger.toVRFVerKeyHash hVrfKey)

-- | Get the stake credential witness for a certificate that requires it.
-- Only stake address deregistration and delegation requires witnessing (witness can be script or key).
selectStakeCredentialWitness
  :: IsShelleyBasedEra era
  => Certificate (ShelleyLedgerEra era)
  -> Maybe StakeCredential
selectStakeCredentialWitness (Certificate cert) =
  getTxCertWitness shelleyBasedEra cert

getTxCertWitness
  :: ShelleyBasedEra era
  -> Ledger.TxCert (ShelleyLedgerEra era)
  -> Maybe StakeCredential
getTxCertWitness sbe ledgerCert = shelleyBasedEraConstraints sbe $
  case Ledger.getVKeyWitnessTxCert ledgerCert of
    Just keyHash -> Just $ StakeCredentialByKey $ Api.StakeKeyHash $ Ledger.coerceKeyRole keyHash
    Nothing ->
      StakeCredentialByScript . fromShelleyScriptHash
        <$> Ledger.getScriptWitnessTxCert ledgerCert
