{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Api.Eon.MaryEraOnwards
  ( MaryEraOnwards(..)
  , maryEraOnwardsConstraints
  , maryEraOnwardsToCardanoEra
  , maryEraOnwardsToShelleyBasedEra

  , MaryEraOnwardsConstraints
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
import qualified Cardano.Ledger.Mary.Value as L
import qualified Cardano.Ledger.SafeHash as L
import qualified Cardano.Ledger.UTxO as L
import qualified Ouroboros.Consensus.Protocol.Abstract as Consensus
import qualified Ouroboros.Consensus.Protocol.Praos.Common as Consensus
import qualified Ouroboros.Consensus.Shelley.Ledger as Consensus

import           Data.Aeson
import           Data.Typeable (Typeable)

data MaryEraOnwards era where
  MaryEraOnwardsMary  :: MaryEraOnwards MaryEra
  MaryEraOnwardsAlonzo  :: MaryEraOnwards AlonzoEra
  MaryEraOnwardsBabbage :: MaryEraOnwards BabbageEra
  MaryEraOnwardsConway  :: MaryEraOnwards ConwayEra

deriving instance Show (MaryEraOnwards era)
deriving instance Eq (MaryEraOnwards era)

instance Eon MaryEraOnwards where
  inEonForEra no yes = \case
    ByronEra    -> no
    ShelleyEra  -> no
    AllegraEra  -> no
    MaryEra     -> yes MaryEraOnwardsMary
    AlonzoEra   -> yes MaryEraOnwardsAlonzo
    BabbageEra  -> yes MaryEraOnwardsBabbage
    ConwayEra   -> yes MaryEraOnwardsConway

instance ToCardanoEra MaryEraOnwards where
  toCardanoEra = \case
    MaryEraOnwardsMary    -> MaryEra
    MaryEraOnwardsAlonzo  -> AlonzoEra
    MaryEraOnwardsBabbage -> BabbageEra
    MaryEraOnwardsConway  -> ConwayEra

type MaryEraOnwardsConstraints era =
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
  , L.EraUTxO (ShelleyLedgerEra era)
  , L.HashAnnotated (L.TxBody (ShelleyLedgerEra era)) L.EraIndependentTxBody L.StandardCrypto
  , L.MaryEraTxBody (ShelleyLedgerEra era)
  , L.ShelleyEraTxCert (ShelleyLedgerEra era)
  , L.Value (ShelleyLedgerEra era) ~ L.MaryValue L.StandardCrypto

  , FromCBOR (Consensus.ChainDepState (ConsensusProtocol era))
  , FromCBOR (DebugLedgerState era)
  , IsCardanoEra era
  , IsShelleyBasedEra era
  , ToJSON (DebugLedgerState era)
  , Typeable era
  )

maryEraOnwardsConstraints :: ()
  => MaryEraOnwards era
  -> (MaryEraOnwardsConstraints era => a)
  -> a
maryEraOnwardsConstraints = \case
  MaryEraOnwardsMary    -> id
  MaryEraOnwardsAlonzo  -> id
  MaryEraOnwardsBabbage -> id
  MaryEraOnwardsConway  -> id

maryEraOnwardsToCardanoEra :: MaryEraOnwards era -> CardanoEra era
maryEraOnwardsToCardanoEra = shelleyBasedToCardanoEra . maryEraOnwardsToShelleyBasedEra

maryEraOnwardsToShelleyBasedEra :: MaryEraOnwards era -> ShelleyBasedEra era
maryEraOnwardsToShelleyBasedEra = \case
  MaryEraOnwardsMary    -> ShelleyBasedEraMary
  MaryEraOnwardsAlonzo  -> ShelleyBasedEraAlonzo
  MaryEraOnwardsBabbage -> ShelleyBasedEraBabbage
  MaryEraOnwardsConway  -> ShelleyBasedEraConway
