{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Api.Feature.ShelleyToBabbageEra
  ( ShelleyToBabbageEra(..)
  , AnyShelleyToBabbageEra(..)
  , shelleyToBabbageEraConstraints
  , shelleyToBabbageEraToCardanoEra
  , shelleyToBabbageEraToShelleyBasedEra
  , IsShelleyToBabbageEra
  ) where

import           Cardano.Api.Eras.Core
import           Cardano.Api.Modes
import           Cardano.Api.Query.Types

import           Cardano.Binary
import qualified Cardano.Crypto.Hash.Blake2b as Blake2b
import qualified Cardano.Crypto.Hash.Class as C
import qualified Cardano.Crypto.VRF as C
import qualified Cardano.Ledger.Api as L
import qualified Cardano.Ledger.BaseTypes as L
import qualified Cardano.Ledger.Core as L
import qualified Cardano.Ledger.SafeHash as L
import qualified Cardano.Ledger.Shelley.TxCert as L
import qualified Ouroboros.Consensus.Protocol.Abstract as Consensus
import qualified Ouroboros.Consensus.Protocol.Praos.Common as Consensus
import qualified Ouroboros.Consensus.Shelley.Ledger as Consensus

import           Data.Aeson
import           Data.Typeable (Typeable)

data ShelleyToBabbageEra era where
  ShelleyToBabbageEraShelley :: ShelleyToBabbageEra ShelleyEra
  ShelleyToBabbageEraAllegra :: ShelleyToBabbageEra AllegraEra
  ShelleyToBabbageEraMary :: ShelleyToBabbageEra MaryEra
  ShelleyToBabbageEraAlonzo :: ShelleyToBabbageEra AlonzoEra
  ShelleyToBabbageEraBabbage :: ShelleyToBabbageEra BabbageEra

deriving instance Show (ShelleyToBabbageEra era)
deriving instance Eq (ShelleyToBabbageEra era)

instance FeatureInEra ShelleyToBabbageEra where
  featureInEra no yes = \case
    ByronEra    -> no
    ShelleyEra  -> yes ShelleyToBabbageEraShelley
    AllegraEra  -> yes ShelleyToBabbageEraAllegra
    MaryEra     -> yes ShelleyToBabbageEraMary
    AlonzoEra   -> yes ShelleyToBabbageEraAlonzo
    BabbageEra  -> yes ShelleyToBabbageEraBabbage
    ConwayEra   -> no

type ShelleyToBabbageEraConstraints era ledgerera =
  ( C.HashAlgorithm (L.HASH (L.EraCrypto ledgerera))
  , C.Signable (L.VRF (L.EraCrypto ledgerera)) L.Seed
  , Consensus.PraosProtocolSupportsNode (ConsensusProtocol era)
  , Consensus.ShelleyCompatible (ConsensusProtocol era) ledgerera
  , L.ADDRHASH (Consensus.PraosProtocolSupportsNodeCrypto (ConsensusProtocol era)) ~ Blake2b.Blake2b_224
  , L.Crypto (L.EraCrypto ledgerera)
  , L.Era ledgerera
  , L.EraCrypto ledgerera ~ L.StandardCrypto
  , L.EraPParams ledgerera
  , L.EraTx ledgerera
  , L.EraTxBody ledgerera
  , L.HashAnnotated (L.TxBody ledgerera) L.EraIndependentTxBody L.StandardCrypto
  , L.ShelleyEraTxBody ledgerera
  , L.ShelleyEraTxCert ledgerera
  , L.TxCert ledgerera ~ L.ShelleyTxCert ledgerera
  , FromCBOR (Consensus.ChainDepState (ConsensusProtocol era))
  , FromCBOR (DebugLedgerState era)
  , IsShelleyBasedEra era
  , ToJSON (DebugLedgerState era)
  , Typeable era
  )

data AnyShelleyToBabbageEra where
  AnyShelleyToBabbageEra :: ShelleyToBabbageEra era -> AnyShelleyToBabbageEra

deriving instance Show AnyShelleyToBabbageEra

shelleyToBabbageEraConstraints :: ()
  => ShelleyLedgerEra era ~ ledgerera
  => ShelleyToBabbageEra era
  -> (ShelleyToBabbageEraConstraints era ledgerera => a)
  -> a
shelleyToBabbageEraConstraints = \case
  ShelleyToBabbageEraShelley -> id
  ShelleyToBabbageEraAllegra -> id
  ShelleyToBabbageEraMary    -> id
  ShelleyToBabbageEraAlonzo  -> id
  ShelleyToBabbageEraBabbage -> id

shelleyToBabbageEraToCardanoEra :: ShelleyToBabbageEra era -> CardanoEra era
shelleyToBabbageEraToCardanoEra = shelleyBasedToCardanoEra . shelleyToBabbageEraToShelleyBasedEra

shelleyToBabbageEraToShelleyBasedEra :: ShelleyToBabbageEra era -> ShelleyBasedEra era
shelleyToBabbageEraToShelleyBasedEra = \case
  ShelleyToBabbageEraShelley -> ShelleyBasedEraShelley
  ShelleyToBabbageEraAllegra -> ShelleyBasedEraAllegra
  ShelleyToBabbageEraMary    -> ShelleyBasedEraMary
  ShelleyToBabbageEraAlonzo  -> ShelleyBasedEraAlonzo
  ShelleyToBabbageEraBabbage -> ShelleyBasedEraBabbage

instance Is ShelleyToBabbageEra ShelleyEra where
  featureEra = ShelleyToBabbageEraShelley

instance Is ShelleyToBabbageEra AllegraEra where
  featureEra = ShelleyToBabbageEraAllegra

instance Is ShelleyToBabbageEra MaryEra where
  featureEra = ShelleyToBabbageEraMary

instance Is ShelleyToBabbageEra AlonzoEra where
  featureEra = ShelleyToBabbageEraAlonzo

instance Is ShelleyToBabbageEra BabbageEra where
  featureEra = ShelleyToBabbageEraBabbage

type IsShelleyToBabbageEra = Is ShelleyToBabbageEra
