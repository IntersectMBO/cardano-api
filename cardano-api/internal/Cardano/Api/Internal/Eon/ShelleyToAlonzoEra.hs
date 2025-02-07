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

module Cardano.Api.Internal.Eon.ShelleyToAlonzoEra
  ( ShelleyToAlonzoEra (..)
  , shelleyToAlonzoEraConstraints
  , shelleyToAlonzoEraToShelleyBasedEra
  , ShelleyToAlonzoEraConstraints
  )
where

import           Cardano.Api.Internal.Eon.Convert
import           Cardano.Api.Internal.Eon.ShelleyBasedEra
import           Cardano.Api.Internal.Eras.Core
import           Cardano.Api.Internal.Modes
import           Cardano.Api.Internal.Query.Types

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

data ShelleyToAlonzoEra era where
  ShelleyToAlonzoEraShelley :: ShelleyToAlonzoEra ShelleyEra
  ShelleyToAlonzoEraAllegra :: ShelleyToAlonzoEra AllegraEra
  ShelleyToAlonzoEraMary :: ShelleyToAlonzoEra MaryEra
  ShelleyToAlonzoEraAlonzo :: ShelleyToAlonzoEra AlonzoEra

deriving instance Show (ShelleyToAlonzoEra era)

deriving instance Eq (ShelleyToAlonzoEra era)

instance Eon ShelleyToAlonzoEra where
  inEonForEra no yes = \case
    ByronEra -> no
    ShelleyEra -> yes ShelleyToAlonzoEraShelley
    AllegraEra -> yes ShelleyToAlonzoEraAllegra
    MaryEra -> yes ShelleyToAlonzoEraMary
    AlonzoEra -> yes ShelleyToAlonzoEraAlonzo
    BabbageEra -> no
    ConwayEra -> no

instance ToCardanoEra ShelleyToAlonzoEra where
  toCardanoEra = \case
    ShelleyToAlonzoEraShelley -> ShelleyEra
    ShelleyToAlonzoEraAllegra -> AllegraEra
    ShelleyToAlonzoEraMary -> MaryEra
    ShelleyToAlonzoEraAlonzo -> AlonzoEra

instance Convert ShelleyToAlonzoEra CardanoEra where
  convert = toCardanoEra

instance Convert ShelleyToAlonzoEra ShelleyBasedEra where
  convert = \case
    ShelleyToAlonzoEraShelley -> ShelleyBasedEraShelley
    ShelleyToAlonzoEraAllegra -> ShelleyBasedEraAllegra
    ShelleyToAlonzoEraMary -> ShelleyBasedEraMary
    ShelleyToAlonzoEraAlonzo -> ShelleyBasedEraAlonzo

type ShelleyToAlonzoEraConstraints era =
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
  , L.HashAnnotated (L.TxBody (ShelleyLedgerEra era)) L.EraIndependentTxBody L.StandardCrypto
  , L.ProtVerAtMost (ShelleyLedgerEra era) 6
  , L.ProtVerAtMost (ShelleyLedgerEra era) 8
  , L.ShelleyEraTxBody (ShelleyLedgerEra era)
  , L.ShelleyEraTxCert (ShelleyLedgerEra era)
  , L.TxCert (ShelleyLedgerEra era) ~ L.ShelleyTxCert (ShelleyLedgerEra era)
  , FromCBOR (Consensus.ChainDepState (ConsensusProtocol era))
  , FromCBOR (DebugLedgerState era)
  , IsCardanoEra era
  , IsShelleyBasedEra era
  , ToJSON (DebugLedgerState era)
  , Typeable era
  )

shelleyToAlonzoEraConstraints
  :: ()
  => ShelleyToAlonzoEra era
  -> (ShelleyToAlonzoEraConstraints era => a)
  -> a
shelleyToAlonzoEraConstraints = \case
  ShelleyToAlonzoEraShelley -> id
  ShelleyToAlonzoEraAllegra -> id
  ShelleyToAlonzoEraMary -> id
  ShelleyToAlonzoEraAlonzo -> id

shelleyToAlonzoEraToShelleyBasedEra :: ShelleyToAlonzoEra era -> ShelleyBasedEra era
shelleyToAlonzoEraToShelleyBasedEra = convert
