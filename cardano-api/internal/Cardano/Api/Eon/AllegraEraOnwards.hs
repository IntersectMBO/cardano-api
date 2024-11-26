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

module Cardano.Api.Eon.AllegraEraOnwards
  ( AllegraEraOnwards (..)
  , allegraEraOnwardsConstraints
  , allegraEraOnwardsToShelleyBasedEra
  , AllegraEraOnwardsConstraints
  , IsAllegraBasedEra (..)
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
import qualified Ouroboros.Consensus.Protocol.Abstract as Consensus
import qualified Ouroboros.Consensus.Protocol.Praos.Common as Consensus
import qualified Ouroboros.Consensus.Shelley.Ledger as Consensus

import           Data.Aeson
import           Data.Typeable (Typeable)

data AllegraEraOnwards era where
  AllegraEraOnwardsAllegra :: AllegraEraOnwards AllegraEra
  AllegraEraOnwardsMary :: AllegraEraOnwards MaryEra
  AllegraEraOnwardsAlonzo :: AllegraEraOnwards AlonzoEra
  AllegraEraOnwardsBabbage :: AllegraEraOnwards BabbageEra
  AllegraEraOnwardsConway :: AllegraEraOnwards ConwayEra

deriving instance Show (AllegraEraOnwards era)

deriving instance Eq (AllegraEraOnwards era)

instance Eon AllegraEraOnwards where
  inEonForEra no yes = \case
    ByronEra -> no
    ShelleyEra -> no
    AllegraEra -> yes AllegraEraOnwardsAllegra
    MaryEra -> yes AllegraEraOnwardsMary
    AlonzoEra -> yes AllegraEraOnwardsAlonzo
    BabbageEra -> yes AllegraEraOnwardsBabbage
    ConwayEra -> yes AllegraEraOnwardsConway

instance ToCardanoEra AllegraEraOnwards where
  toCardanoEra = \case
    AllegraEraOnwardsAllegra -> AllegraEra
    AllegraEraOnwardsMary -> MaryEra
    AllegraEraOnwardsAlonzo -> AlonzoEra
    AllegraEraOnwardsBabbage -> BabbageEra
    AllegraEraOnwardsConway -> ConwayEra

instance Convert AllegraEraOnwards CardanoEra where
  convert = toCardanoEra

instance Convert AllegraEraOnwards ShelleyBasedEra where
  convert = \case
    AllegraEraOnwardsAllegra -> ShelleyBasedEraAllegra
    AllegraEraOnwardsMary -> ShelleyBasedEraMary
    AllegraEraOnwardsAlonzo -> ShelleyBasedEraAlonzo
    AllegraEraOnwardsBabbage -> ShelleyBasedEraBabbage
    AllegraEraOnwardsConway -> ShelleyBasedEraConway

type AllegraEraOnwardsConstraints era =
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
  , L.AllegraEraTxBody (ShelleyLedgerEra era)
  , L.ShelleyEraTxCert (ShelleyLedgerEra era)
  , FromCBOR (Consensus.ChainDepState (ConsensusProtocol era))
  , FromCBOR (DebugLedgerState era)
  , IsCardanoEra era
  , IsShelleyBasedEra era
  , ToJSON (DebugLedgerState era)
  , Typeable era
  )

allegraEraOnwardsConstraints
  :: ()
  => AllegraEraOnwards era
  -> (AllegraEraOnwardsConstraints era => a)
  -> a
allegraEraOnwardsConstraints = \case
  AllegraEraOnwardsAllegra -> id
  AllegraEraOnwardsMary -> id
  AllegraEraOnwardsAlonzo -> id
  AllegraEraOnwardsBabbage -> id
  AllegraEraOnwardsConway -> id

{-# DEPRECATED allegraEraOnwardsToShelleyBasedEra "Use 'convert' instead." #-}
allegraEraOnwardsToShelleyBasedEra :: AllegraEraOnwards era -> ShelleyBasedEra era
allegraEraOnwardsToShelleyBasedEra = convert

class IsShelleyBasedEra era => IsAllegraBasedEra era where
  allegraBasedEra :: AllegraEraOnwards era

instance IsAllegraBasedEra AllegraEra where
  allegraBasedEra = AllegraEraOnwardsAllegra

instance IsAllegraBasedEra MaryEra where
  allegraBasedEra = AllegraEraOnwardsMary

instance IsAllegraBasedEra AlonzoEra where
  allegraBasedEra = AllegraEraOnwardsAlonzo

instance IsAllegraBasedEra BabbageEra where
  allegraBasedEra = AllegraEraOnwardsBabbage

instance IsAllegraBasedEra ConwayEra where
  allegraBasedEra = AllegraEraOnwardsConway
