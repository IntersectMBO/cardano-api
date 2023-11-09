{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Api.Eon.ShelleyToBabbageEra
  ( ShelleyToBabbageEra(..)
  , shelleyToBabbageEraConstraints
  , shelleyToBabbageEraToCardanoEra
  , shelleyToBabbageEraToShelleyBasedEra

  , ShelleyToBabbageEraConstraints
  ) where

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
    ByronEra    -> no
    ShelleyEra  -> yes ShelleyToBabbageEraShelley
    AllegraEra  -> yes ShelleyToBabbageEraAllegra
    MaryEra     -> yes ShelleyToBabbageEraMary
    AlonzoEra   -> yes ShelleyToBabbageEraAlonzo
    BabbageEra  -> yes ShelleyToBabbageEraBabbage
    ConwayEra   -> no

instance ToCardanoEra ShelleyToBabbageEra where
  toCardanoEra = \case
    ShelleyToBabbageEraShelley  -> ShelleyEra
    ShelleyToBabbageEraAllegra  -> AllegraEra
    ShelleyToBabbageEraMary     -> MaryEra
    ShelleyToBabbageEraAlonzo   -> AlonzoEra
    ShelleyToBabbageEraBabbage  -> BabbageEra

type ShelleyToBabbageEraConstraints era =
  ( C.HashAlgorithm (L.HASH (L.EraCrypto (LedgerEra era)))
  , C.Signable (L.VRF (L.EraCrypto (LedgerEra era))) L.Seed
  , Consensus.PraosProtocolSupportsNode (ConsensusProtocol era)
  , Consensus.ShelleyBlock (ConsensusProtocol era) (LedgerEra era) ~ ConsensusBlockForEra era
  , Consensus.ShelleyCompatible (ConsensusProtocol era) (LedgerEra era)
  , L.ADDRHASH (Consensus.PraosProtocolSupportsNodeCrypto (ConsensusProtocol era)) ~ Blake2b.Blake2b_224
  , L.Crypto (L.EraCrypto (LedgerEra era))
  , L.Era (LedgerEra era)
  , L.EraCrypto (LedgerEra era) ~ L.StandardCrypto
  , L.EraPParams (LedgerEra era)
  , L.EraTx (LedgerEra era)
  , L.EraTxBody (LedgerEra era)
  , L.EraTxOut (LedgerEra era)
  , L.HashAnnotated (L.TxBody (LedgerEra era)) L.EraIndependentTxBody L.StandardCrypto
  , L.ProtVerAtMost (LedgerEra era) 8
  , L.ShelleyEraTxBody (LedgerEra era)
  , L.ShelleyEraTxCert (LedgerEra era)
  , L.TxCert (LedgerEra era) ~ L.ShelleyTxCert (LedgerEra era)

  , FromCBOR (Consensus.ChainDepState (ConsensusProtocol era))
  , FromCBOR (DebugLedgerState era)
  , IsCardanoEra era
  , IsShelleyBasedEra era
  , ToJSON (DebugLedgerState era)
  , Typeable era
  )

shelleyToBabbageEraConstraints :: ()
  => ShelleyToBabbageEra era
  -> (ShelleyToBabbageEraConstraints era => a)
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
