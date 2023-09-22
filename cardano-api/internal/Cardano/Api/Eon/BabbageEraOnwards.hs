{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Api.Eon.BabbageEraOnwards
  ( BabbageEraOnwards(..)
  , AnyBabbageEraOnwards(..)
  , babbageEraOnwardsConstraints
  , babbageEraOnwardsToCardanoEra
  , babbageEraOnwardsToShelleyBasedEra

  , BabbageEraOnwardsConstraints
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
import qualified Ouroboros.Consensus.Protocol.Abstract as Consensus
import qualified Ouroboros.Consensus.Protocol.Praos.Common as Consensus
import qualified Ouroboros.Consensus.Shelley.Ledger as Consensus

import           Data.Aeson
import           Data.Typeable (Typeable)

data BabbageEraOnwards era where
  BabbageEraOnwardsBabbage :: BabbageEraOnwards BabbageEra
  BabbageEraOnwardsConway  :: BabbageEraOnwards ConwayEra

deriving instance Show (BabbageEraOnwards era)
deriving instance Eq (BabbageEraOnwards era)

instance Eon BabbageEraOnwards where
  inEonForEra no yes = \case
    ByronEra    -> no
    ShelleyEra  -> no
    AllegraEra  -> no
    MaryEra     -> no
    AlonzoEra   -> no
    BabbageEra  -> yes BabbageEraOnwardsBabbage
    ConwayEra   -> yes BabbageEraOnwardsConway

instance ToCardanoEra BabbageEraOnwards where
  toCardanoEra = \case
    BabbageEraOnwardsBabbage -> BabbageEra
    BabbageEraOnwardsConway  -> ConwayEra

data AnyBabbageEraOnwards where
  AnyBabbageEraOnwards :: BabbageEraOnwards era -> AnyBabbageEraOnwards

deriving instance Show AnyBabbageEraOnwards

type BabbageEraOnwardsConstraints era =
  ( C.HashAlgorithm (L.HASH (L.EraCrypto (ShelleyLedgerEra era)))
  , C.Signable (L.VRF (L.EraCrypto (ShelleyLedgerEra era))) L.Seed
  , Consensus.PraosProtocolSupportsNode (ConsensusProtocol era)
  , Consensus.ShelleyCompatible (ConsensusProtocol era) (ShelleyLedgerEra era)
  , L.ADDRHASH (Consensus.PraosProtocolSupportsNodeCrypto (ConsensusProtocol era)) ~ Blake2b.Blake2b_224
  , L.BabbageEraPParams (ShelleyLedgerEra era)
  , L.BabbageEraTxBody (ShelleyLedgerEra era)
  , L.BabbageEraTxOut (ShelleyLedgerEra era)
  , L.Crypto (L.EraCrypto (ShelleyLedgerEra era))
  , L.Era (ShelleyLedgerEra era)
  , L.EraCrypto (ShelleyLedgerEra era) ~ L.StandardCrypto
  , L.EraPParams (ShelleyLedgerEra era)
  , L.EraTx (ShelleyLedgerEra era)
  , L.EraTxBody (ShelleyLedgerEra era)
  , L.HashAnnotated (L.TxBody (ShelleyLedgerEra era)) L.EraIndependentTxBody L.StandardCrypto
  , L.ShelleyEraTxBody (ShelleyLedgerEra era)
  , L.ShelleyEraTxCert (ShelleyLedgerEra era)

  , FromCBOR (Consensus.ChainDepState (ConsensusProtocol era))
  , FromCBOR (DebugLedgerState era)
  , IsCardanoEra era
  , IsShelleyBasedEra era
  , ToJSON (DebugLedgerState era)
  , Typeable era
  )

babbageEraOnwardsConstraints :: ()
  => BabbageEraOnwards era
  -> (BabbageEraOnwardsConstraints era => a)
  -> a
babbageEraOnwardsConstraints = \case
  BabbageEraOnwardsBabbage -> id
  BabbageEraOnwardsConway  -> id

babbageEraOnwardsToCardanoEra :: BabbageEraOnwards era -> CardanoEra era
babbageEraOnwardsToCardanoEra = shelleyBasedToCardanoEra . babbageEraOnwardsToShelleyBasedEra

babbageEraOnwardsToShelleyBasedEra :: BabbageEraOnwards era -> ShelleyBasedEra era
babbageEraOnwardsToShelleyBasedEra = \case
  BabbageEraOnwardsBabbage -> ShelleyBasedEraBabbage
  BabbageEraOnwardsConway  -> ShelleyBasedEraConway
