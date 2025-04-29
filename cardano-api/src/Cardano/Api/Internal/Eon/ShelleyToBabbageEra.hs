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

module Cardano.Api.Internal.Eon.ShelleyToBabbageEra
  ( ShelleyToBabbageEra (..)
  , shelleyToBabbageEraConstraints
  , shelleyToBabbageEraToShelleyBasedEra
  , ShelleyToBabbageEraConstraints
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
import Cardano.Ledger.Core qualified as L
import Cardano.Ledger.Shelley.TxCert qualified as L
import Cardano.Protocol.Crypto qualified as L
import Ouroboros.Consensus.Protocol.Abstract qualified as Consensus
import Ouroboros.Consensus.Protocol.Praos.Common qualified as Consensus
import Ouroboros.Consensus.Shelley.Ledger qualified as Consensus

import Data.Aeson
import Data.Type.Equality
import Data.Typeable (Typeable)

data ShelleyToBabbageEra era where
  ShelleyToBabbageEraShelley :: ShelleyToBabbageEra ShelleyEra
  ShelleyToBabbageEraAllegra :: ShelleyToBabbageEra AllegraEra
  ShelleyToBabbageEraMary :: ShelleyToBabbageEra MaryEra
  ShelleyToBabbageEraAlonzo :: ShelleyToBabbageEra AlonzoEra
  ShelleyToBabbageEraBabbage :: ShelleyToBabbageEra BabbageEra

deriving instance Show (ShelleyToBabbageEra era)

deriving instance Eq (ShelleyToBabbageEra era)

deriving instance Ord (ShelleyToBabbageEra era)

instance Eon ShelleyToBabbageEra where
  inEonForEra no yes = \case
    ByronEra -> no
    ShelleyEra -> yes ShelleyToBabbageEraShelley
    AllegraEra -> yes ShelleyToBabbageEraAllegra
    MaryEra -> yes ShelleyToBabbageEraMary
    AlonzoEra -> yes ShelleyToBabbageEraAlonzo
    BabbageEra -> yes ShelleyToBabbageEraBabbage
    ConwayEra -> no

instance ToCardanoEra ShelleyToBabbageEra where
  toCardanoEra = \case
    ShelleyToBabbageEraShelley -> ShelleyEra
    ShelleyToBabbageEraAllegra -> AllegraEra
    ShelleyToBabbageEraMary -> MaryEra
    ShelleyToBabbageEraAlonzo -> AlonzoEra
    ShelleyToBabbageEraBabbage -> BabbageEra

instance Convert ShelleyToBabbageEra CardanoEra where
  convert = toCardanoEra

instance Convert ShelleyToBabbageEra ShelleyBasedEra where
  convert = \case
    ShelleyToBabbageEraShelley -> ShelleyBasedEraShelley
    ShelleyToBabbageEraAllegra -> ShelleyBasedEraAllegra
    ShelleyToBabbageEraMary -> ShelleyBasedEraMary
    ShelleyToBabbageEraAlonzo -> ShelleyBasedEraAlonzo
    ShelleyToBabbageEraBabbage -> ShelleyBasedEraBabbage

type ShelleyToBabbageEraConstraints era =
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

shelleyToBabbageEraConstraints
  :: ()
  => ShelleyToBabbageEra era
  -> (ShelleyToBabbageEraConstraints era => a)
  -> a
shelleyToBabbageEraConstraints = \case
  ShelleyToBabbageEraShelley -> id
  ShelleyToBabbageEraAllegra -> id
  ShelleyToBabbageEraMary -> id
  ShelleyToBabbageEraAlonzo -> id
  ShelleyToBabbageEraBabbage -> id

{-# DEPRECATED shelleyToBabbageEraToShelleyBasedEra "Use 'convert' instead." #-}
shelleyToBabbageEraToShelleyBasedEra :: ShelleyToBabbageEra era -> ShelleyBasedEra era
shelleyToBabbageEraToShelleyBasedEra = convert
