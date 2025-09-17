{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Api.Experimental.Tx.Internal.Certificate
  ( Certificate (..)
  , convertToOldApiCertificate
  , convertToNewCertificate
  )
where

import Cardano.Api.Certificate.Internal qualified as Api
import Cardano.Api.Era.Internal.Eon.ConwayEraOnwards
import Cardano.Api.Era.Internal.Eon.ShelleyBasedEra
import Cardano.Api.Era.Internal.Eon.ShelleyToBabbageEra qualified as Api
import Cardano.Api.Experimental.Era
import Cardano.Api.HasTypeProxy
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Serialise.Cbor
import Cardano.Api.Serialise.TextEnvelope.Internal

import Cardano.Binary qualified as CBOR

import Data.Typeable

data Certificate era where
  Certificate :: L.EraTxCert era => L.TxCert era -> Certificate era

deriving instance Show (Certificate era)

deriving instance Eq (Certificate era)

deriving instance Ord (Certificate era)

instance
  ( Typeable ledgerera
  , IsShelleyBasedEra era
  , ShelleyLedgerEra era ~ ledgerera
  )
  => HasTextEnvelope (Certificate ledgerera)
  where
  textEnvelopeType _ = "Certificate"

instance Typeable era => HasTypeProxy (Certificate era) where
  data AsType (Certificate era) = AsCertificate
  proxyToAsType _ = AsCertificate

instance
  ( Typeable ledgerera
  , IsShelleyBasedEra era
  , ShelleyLedgerEra era ~ ledgerera
  )
  => SerialiseAsCBOR (Certificate ledgerera)
  where
  serialiseToCBOR (Certificate cert) =
    CBOR.serialize' cert
  deserialiseFromCBOR _ bs =
    shelleyBasedEraConstraints (shelleyBasedEra @era) $ Certificate <$> CBOR.decodeFull' bs

convertToOldApiCertificate :: Era era -> Certificate (LedgerEra era) -> Api.Certificate era
convertToOldApiCertificate ConwayEra (Certificate cert) =
  Api.ConwayCertificate ConwayEraOnwardsConway cert

convertToNewCertificate :: Era era -> Api.Certificate era -> Certificate (ShelleyLedgerEra era)
convertToNewCertificate ConwayEra (Api.ConwayCertificate _ cert) = Certificate cert
convertToNewCertificate ConwayEra (Api.ShelleyRelatedCertificate sToBab _) =
  case sToBab :: Api.ShelleyToBabbageEra ConwayEra of {}
