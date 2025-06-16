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

module Cardano.Api.Era.Internal.Eon.ShelleyToAlonzoEra
  ( ShelleyToAlonzoEra (..)
  , shelleyToAlonzoEraConstraints
  , shelleyToAlonzoEraToShelleyBasedEra
  , ShelleyToAlonzoEraConstraints
  )
where

import Cardano.Api.Consensus.Internal.Mode
import Cardano.Api.Era.Internal.Core
import Cardano.Api.Era.Internal.Eon.Convert
import Cardano.Api.Era.Internal.Eon.ShelleyBasedEra
import Cardano.Api.Query.Internal.Type.DebugLedgerState

import Cardano.Binary
import Cardano.Crypto.Hash.Blake2b qualified as Blake2b
import Cardano.Crypto.Hash.Class qualified as C
import Cardano.Crypto.VRF qualified as C
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.BaseTypes qualified as L
import Cardano.Ledger.Core qualified as L
import Cardano.Ledger.Shelley.TxCert qualified as L
import Cardano.Protocol.Crypto qualified as L
import Ouroboros.Consensus.Protocol.Abstract qualified as Consensus
import Ouroboros.Consensus.Protocol.Praos.Common qualified as Consensus
import Ouroboros.Consensus.Shelley.Ledger qualified as Consensus

import Data.Aeson
import Data.Type.Equality
import Data.Typeable (Typeable)

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
  ( C.HashAlgorithm L.HASH
  , C.Signable (L.VRF L.StandardCrypto) L.Seed
  , Consensus.PraosProtocolSupportsNode (ConsensusProtocol era)
  , Consensus.ShelleyBlock (ConsensusProtocol era) (ShelleyLedgerEra era) ~ ConsensusBlockForEra era
  , Consensus.ShelleyCompatible (ConsensusProtocol era) (ShelleyLedgerEra era)
  , L.ADDRHASH ~ Blake2b.Blake2b_224
  , L.Era (ShelleyLedgerEra era)
  , L.EraPParams (ShelleyLedgerEra era)
  , L.EraTx (ShelleyLedgerEra era)
  , L.EraTxBody (ShelleyLedgerEra era)
  , L.EraTxOut (ShelleyLedgerEra era)
  , L.HashAnnotated (L.TxBody (ShelleyLedgerEra era)) L.EraIndependentTxBody
  , L.ProtVerAtMost (ShelleyLedgerEra era) 6
  , L.ProtVerAtMost (ShelleyLedgerEra era) 8
  , L.ShelleyEraTxBody (ShelleyLedgerEra era)
  , L.ShelleyEraTxCert (ShelleyLedgerEra era)
  , L.TxCert (ShelleyLedgerEra era) ~ L.ShelleyTxCert (ShelleyLedgerEra era)
  , FromCBOR (Consensus.ChainDepState (ConsensusProtocol era))
  , FromCBOR (DebugLedgerState era)
  , IsCardanoEra era
  , IsShelleyBasedEra era
  , ToJSON (Consensus.ChainDepState (ConsensusProtocol era))
  , ToJSON (DebugLedgerState era)
  , Typeable era
  , (era == ByronEra) ~ False
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
