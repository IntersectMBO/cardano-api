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

module Cardano.Api.Internal.Eon.ShelleyToAllegraEra
  ( ShelleyToAllegraEra (..)
  , shelleyToAllegraEraConstraints
  , shelleyToAllegraEraToShelleyBasedEra
  , ShelleyToAllegraEraConstraints
  )
where

import Cardano.Api.Internal.Eon.Convert
import Cardano.Api.Internal.Eon.ShelleyBasedEra
import Cardano.Api.Internal.Eras.Core
import Cardano.Api.Internal.Modes
import Cardano.Api.Internal.Query.Types

import Cardano.Binary
import Cardano.Crypto.Hash.Blake2b qualified as Blake2b
import Cardano.Crypto.Hash.Class qualified as C
import Cardano.Crypto.VRF qualified as C
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.BaseTypes qualified as L
import Cardano.Ledger.Coin qualified as L
import Cardano.Ledger.Core qualified as L
import Cardano.Ledger.Shelley.TxCert qualified as L
import Cardano.Ledger.State qualified as L
import Cardano.Protocol.Crypto qualified as L
import Ouroboros.Consensus.Protocol.Abstract qualified as Consensus
import Ouroboros.Consensus.Protocol.Praos.Common qualified as Consensus
import Ouroboros.Consensus.Shelley.Ledger qualified as Consensus

import Data.Aeson
import Data.Typeable (Typeable)

data ShelleyToAllegraEra era where
  ShelleyToAllegraEraShelley :: ShelleyToAllegraEra ShelleyEra
  ShelleyToAllegraEraAllegra :: ShelleyToAllegraEra AllegraEra

deriving instance Show (ShelleyToAllegraEra era)

deriving instance Eq (ShelleyToAllegraEra era)

instance Eon ShelleyToAllegraEra where
  inEonForEra no yes = \case
    ByronEra -> no
    ShelleyEra -> yes ShelleyToAllegraEraShelley
    AllegraEra -> yes ShelleyToAllegraEraAllegra
    MaryEra -> no
    AlonzoEra -> no
    BabbageEra -> no
    ConwayEra -> no

instance ToCardanoEra ShelleyToAllegraEra where
  toCardanoEra = \case
    ShelleyToAllegraEraShelley -> ShelleyEra
    ShelleyToAllegraEraAllegra -> AllegraEra

instance Convert ShelleyToAllegraEra CardanoEra where
  convert = toCardanoEra

instance Convert ShelleyToAllegraEra ShelleyBasedEra where
  convert = \case
    ShelleyToAllegraEraShelley -> ShelleyBasedEraShelley
    ShelleyToAllegraEraAllegra -> ShelleyBasedEraAllegra

type ShelleyToAllegraEraConstraints era =
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
  , L.EraUTxO (ShelleyLedgerEra era)
  , L.HashAnnotated (L.TxBody (ShelleyLedgerEra era)) L.EraIndependentTxBody
  , L.ProtVerAtMost (ShelleyLedgerEra era) 4
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
  , ToJSON (Consensus.ChainDepState (ConsensusProtocol era))
  , ToJSON (DebugLedgerState era)
  , Typeable era
  )

shelleyToAllegraEraConstraints
  :: ()
  => ShelleyToAllegraEra era
  -> (ShelleyToAllegraEraConstraints era => a)
  -> a
shelleyToAllegraEraConstraints = \case
  ShelleyToAllegraEraShelley -> id
  ShelleyToAllegraEraAllegra -> id

{-# DEPRECATED shelleyToAllegraEraToShelleyBasedEra "Use 'convert' instead." #-}
shelleyToAllegraEraToShelleyBasedEra :: ShelleyToAllegraEra era -> ShelleyBasedEra era
shelleyToAllegraEraToShelleyBasedEra = convert
