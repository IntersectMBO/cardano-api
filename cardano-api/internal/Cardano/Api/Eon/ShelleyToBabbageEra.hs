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

module Cardano.Api.Eon.ShelleyToBabbageEra
  ( ShelleyToBabbageEra (..)
  , shelleyToBabbageEraConstraints
  , shelleyToBabbageEraToShelleyBasedEra
  , ShelleyToBabbageEraConstraints
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
