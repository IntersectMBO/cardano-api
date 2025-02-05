{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Api.Eon.ShelleyEraOnly
  ( ShelleyEraOnly (..)
  , shelleyEraOnlyConstraints
  , shelleyEraOnlyToShelleyBasedEra
  , ShelleyEraOnlyConstraints
  )
where

import           Cardano.Api.Eon.Convert
import           Cardano.Api.Eon.ShelleyBasedEra
import           Cardano.Api.Eras.Core
import           Cardano.Api.Modes
import           Cardano.Api.Query.Types

import           Cardano.Binary
import qualified Cardano.Crypto.Hash.Blake2b as Blake2b
import qualified Cardano.Crypto.Hash.Class as C
import qualified Cardano.Crypto.VRF as C
import qualified Cardano.Ledger.Api as L
import qualified Cardano.Ledger.BaseTypes as L
import qualified Cardano.Ledger.Coin as L
import qualified Cardano.Ledger.Core as L
import qualified Cardano.Ledger.SafeHash as L
import qualified Cardano.Ledger.Shelley.TxCert as L
import qualified Ouroboros.Consensus.Protocol.Abstract as Consensus
import qualified Ouroboros.Consensus.Protocol.Praos.Common as Consensus
import qualified Ouroboros.Consensus.Shelley.Ledger as Consensus

import           Data.Aeson
import           Data.Typeable (Typeable)

data ShelleyEraOnly era where
  ShelleyEraOnlyShelley :: ShelleyEraOnly ShelleyEra

deriving instance Show (ShelleyEraOnly era)

deriving instance Eq (ShelleyEraOnly era)

instance Eon ShelleyEraOnly where
  inEonForEra no yes = \case
    ByronEra -> no
    ShelleyEra -> yes ShelleyEraOnlyShelley
    AllegraEra -> no
    MaryEra -> no
    AlonzoEra -> no
    BabbageEra -> no
    ConwayEra -> no

instance ToCardanoEra ShelleyEraOnly where
  toCardanoEra = \case
    ShelleyEraOnlyShelley -> ShelleyEra

instance Convert ShelleyEraOnly CardanoEra where
  convert = toCardanoEra

instance Convert ShelleyEraOnly ShelleyBasedEra where
  convert = \case
    ShelleyEraOnlyShelley -> ShelleyBasedEraShelley

type ShelleyEraOnlyConstraints era =
  ( C.HashAlgorithm (L.HASH (L.EraCrypto (ShelleyLedgerEra era)))
  , C.Signable (L.VRF (L.EraCrypto (ShelleyLedgerEra era))) L.Seed
  , Consensus.PraosProtocolSupportsNode (ConsensusProtocol era)
  , Consensus.ShelleyBlock (ConsensusProtocol era) (ShelleyLedgerEra era) ~ ConsensusBlockForEra era
  , Consensus.ShelleyCompatible (ConsensusProtocol era) (ShelleyLedgerEra era)
  , L.ADDRHASH (Consensus.PraosProtocolSupportsNodeCrypto (ConsensusProtocol era)) ~ Blake2b.Blake2b_224
  , L.Crypto (L.EraCrypto (ShelleyLedgerEra era))
  , L.Era (ShelleyLedgerEra era)
  , L.EraCrypto (ShelleyLedgerEra era) ~ L.StandardCrypto
  , L.EraPParams (ShelleyLedgerEra era)
  , L.EraTx (ShelleyLedgerEra era)
  , L.EraTxBody (ShelleyLedgerEra era)
  , L.EraTxOut (ShelleyLedgerEra era)
  , L.ExactEra L.ShelleyEra (ShelleyLedgerEra era)
  , L.ExactEra L.ShelleyEra (ShelleyLedgerEra era)
  , L.HashAnnotated (L.TxBody (ShelleyLedgerEra era)) L.EraIndependentTxBody L.StandardCrypto
  , L.ProtVerAtMost (ShelleyLedgerEra era) 2
  , L.ProtVerAtMost (ShelleyLedgerEra era) 6
  , L.ProtVerAtMost (ShelleyLedgerEra era) 8
  , L.ShelleyEraTxBody (ShelleyLedgerEra era)
  , L.ShelleyEraTxCert (ShelleyLedgerEra era)
  , L.TxCert (ShelleyLedgerEra era) ~ L.ShelleyTxCert (ShelleyLedgerEra era)
  , L.Value (ShelleyLedgerEra era) ~ L.Coin
  , FromCBOR (Consensus.ChainDepState (ConsensusProtocol era))
  , FromCBOR (DebugLedgerState era)
  , IsCardanoEra era
  , IsShelleyBasedEra era
  , ToJSON (DebugLedgerState era)
  , Typeable era
  )

shelleyEraOnlyConstraints
  :: ()
  => ShelleyEraOnly era
  -> (ShelleyEraOnlyConstraints era => a)
  -> a
shelleyEraOnlyConstraints = \case
  ShelleyEraOnlyShelley -> id

{-# DEPRECATED shelleyEraOnlyToShelleyBasedEra "Use 'convert' instead." #-}
shelleyEraOnlyToShelleyBasedEra :: ShelleyEraOnly era -> ShelleyBasedEra era
shelleyEraOnlyToShelleyBasedEra = convert
