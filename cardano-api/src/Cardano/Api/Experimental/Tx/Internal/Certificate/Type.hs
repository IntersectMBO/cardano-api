{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Api.Experimental.Tx.Internal.Certificate.Type
  ( Certificate (..)
  , AsType (AsCertificate)
  )
where

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
  , L.EraTxCert ledgerera
  )
  => HasTextEnvelope (Certificate ledgerera)
  where
  textEnvelopeType _ = "Certificate"

instance Typeable era => HasTypeProxy (Certificate era) where
  data AsType (Certificate era) = AsCertificate
  proxyToAsType _ = AsCertificate

instance
  ( Typeable ledgerera
  , L.EraTxCert ledgerera
  )
  => SerialiseAsCBOR (Certificate ledgerera)
  where
  serialiseToCBOR (Certificate cert) =
    CBOR.serialize' cert
  deserialiseFromCBOR _ bs =
    Certificate <$> CBOR.decodeFull' bs
