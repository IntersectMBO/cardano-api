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
  )
where

import Cardano.Api.Certificate.Internal qualified as Api
import Cardano.Api.Era.Internal.Core (DijkstraEra)
import Cardano.Api.Era.Internal.Eon.Convert
import Cardano.Api.Era.Internal.Eon.ConwayEraOnwards
import Cardano.Api.Era.Internal.Eon.ShelleyBasedEra
import Cardano.Api.Era.Internal.Eon.ShelleyToBabbageEra qualified as Api
import Cardano.Api.Experimental.Era
import Cardano.Api.Experimental.Plutus.Internal.Script qualified as Exp
import Cardano.Api.Experimental.Plutus.Internal.ScriptWitness qualified as Exp
import Cardano.Api.Experimental.Simple.Script qualified as Exp
import Cardano.Api.Experimental.Tx.Internal.AnyWitness
import Cardano.Api.HasTypeProxy
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Plutus.Internal.Script
import Cardano.Api.Plutus.Internal.Script qualified as Api
import Cardano.Api.Serialise.Cbor
import Cardano.Api.Serialise.TextEnvelope.Internal
import Cardano.Api.Tx.Internal.Body (TxCertificates (..))
import Cardano.Api.Tx.Internal.Body qualified as Api

import Cardano.Binary qualified as CBOR
import Cardano.Ledger.Allegra.Scripts qualified as L
import Cardano.Ledger.Plutus.Language qualified as L
import Cardano.Ledger.Plutus.Language qualified as Plutus

import Data.Typeable
import GHC.IsList

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

getAnchorDataFromCertificate
  :: Era era
  -> Certificate (ShelleyLedgerEra era)
  -> Either AnchorDataFromCertificateError (Maybe Ledger.Anchor)
getAnchorDataFromCertificate ConwayEra (Certificate c) =
  case c of
    Ledger.RegTxCert _ -> return Nothing
    Ledger.UnRegTxCert _ -> return Nothing
    Ledger.RegDepositTxCert _ _ -> return Nothing
    Ledger.UnRegDepositTxCert _ _ -> return Nothing
    Ledger.RegDepositDelegTxCert{} -> return Nothing
    Ledger.DelegTxCert{} -> return Nothing
    Ledger.RegPoolTxCert poolParams -> strictMaybe (return Nothing) anchorDataFromPoolMetadata $ Ledger.ppMetadata poolParams
    Ledger.RetirePoolTxCert _ _ -> return Nothing
    Ledger.RegDRepTxCert _ _ mAnchor -> return $ Ledger.strictMaybeToMaybe mAnchor
    Ledger.UnRegDRepTxCert _ _ -> return Nothing
    Ledger.UpdateDRepTxCert _ mAnchor -> return $ Ledger.strictMaybeToMaybe mAnchor
    Ledger.AuthCommitteeHotKeyTxCert _ _ -> return Nothing
    Ledger.ResignCommitteeColdTxCert _ mAnchor -> return $ Ledger.strictMaybeToMaybe mAnchor
 where
  anchorDataFromPoolMetadata
    :: MonadError AnchorDataFromCertificateError m
    => Ledger.PoolMetadata
    -> m (Maybe Ledger.Anchor)
  anchorDataFromPoolMetadata (Ledger.PoolMetadata{Ledger.pmUrl = url, Ledger.pmHash = hashBytes}) = do
    hash <-
      maybe (throwError $ InvalidPoolMetadataHashError url hashBytes) return $
        Ledger.hashFromBytes hashBytes
    return $
      Just
        ( Ledger.Anchor
            { Ledger.anchorUrl = url
            , Ledger.anchorDataHash = Ledger.unsafeMakeSafeHash hash
            }
        )
