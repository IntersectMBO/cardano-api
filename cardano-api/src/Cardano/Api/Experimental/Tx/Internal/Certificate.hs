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
  , mkTxCertificates
  , convertToOldApiCertificate
  , convertToNewCertificate
  )
where

import Cardano.Api.Address qualified as Api
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

convertToOldApiCertificate :: Era era -> Certificate (LedgerEra era) -> Api.Certificate era
convertToOldApiCertificate e@ConwayEra (Certificate cert) =
  obtainConwayConstraints e $ Api.ConwayCertificate (convert e) cert
convertToOldApiCertificate DijkstraEra _ = error "Dijkstra era not supported yet"

convertToNewCertificate :: Era era -> Api.Certificate era -> Certificate (LedgerEra era)
convertToNewCertificate era (Api.ConwayCertificate _ cert) =
  case era of
    ConwayEra -> Certificate cert
    DijkstraEra -> error "convertToNewCertificate: DijkstraEra not supported"
convertToNewCertificate era (Api.ShelleyRelatedCertificate sToBab _) =
  case era of
    ConwayEra -> case sToBab :: Api.ShelleyToBabbageEra ConwayEra of {}
    DijkstraEra -> case sToBab :: Api.ShelleyToBabbageEra DijkstraEra of {}

mkTxCertificates
  :: forall era
   . IsEra era
  => [(Certificate (LedgerEra era), AnyWitness (LedgerEra era))]
  -> Api.TxCertificates Api.BuildTx era
mkTxCertificates [] = TxCertificatesNone
mkTxCertificates certs =
  TxCertificates (convert useEra) $ fromList $ map (getStakeCred useEra) certs

getStakeCred
  :: Era era
  -> (Certificate (LedgerEra era), AnyWitness (LedgerEra era))
  -> ( Api.Certificate era
     , Api.BuildTxWith
         Api.BuildTx
         (Maybe (Api.StakeCredential, Api.Witness Api.WitCtxStake era))
     )
getStakeCred e@ConwayEra (Certificate cert, witness) = do
  let oldApiCert = obtainConwayConstraints e $ Api.ConwayCertificate (convert e) cert
      mStakeCred = Api.selectStakeCredentialWitness oldApiCert
      wit =
        case witness of
          AnyKeyWitnessPlaceholder -> Api.KeyWitness Api.KeyWitnessForStakeAddr
          AnySimpleScriptWitness ss ->
            Api.ScriptWitness Api.ScriptWitnessForStakeAddr $
              obtainCommonConstraints e $
                newToOldSimpleScriptWitness e ss
          AnyPlutusScriptWitness psw ->
            Api.ScriptWitness Api.ScriptWitnessForStakeAddr $
              newToOldPlutusCertificateScriptWitness e psw
  (oldApiCert, pure $ (,wit) <$> mStakeCred)
getStakeCred DijkstraEra _ = error "Dijkstra era not supported yet"

newToOldSimpleScriptWitness
  :: L.AllegraEraScript (LedgerEra era)
  => Era era -> Exp.SimpleScriptOrReferenceInput (LedgerEra era) -> Api.ScriptWitness Api.WitCtxStake era
newToOldSimpleScriptWitness era simple =
  case simple of
    Exp.SScript (Exp.SimpleScript script) ->
      Api.SimpleScriptWitness
        (sbeToSimpleScriptLanguageInEra $ convert era)
        (Api.SScript $ fromAllegraTimelock script)
    Exp.SReferenceScript inp ->
      Api.SimpleScriptWitness
        (sbeToSimpleScriptLanguageInEra $ convert era)
        (Api.SReferenceScript inp)

newToOldPlutusCertificateScriptWitness
  :: Era era
  -> Exp.PlutusScriptWitness lang purpose (LedgerEra era)
  -> Api.ScriptWitness Api.WitCtxStake era
newToOldPlutusCertificateScriptWitness ConwayEra (Exp.PlutusScriptWitness Plutus.SPlutusV1 scriptOrRef _ redeemer execUnits) =
  Api.PlutusScriptWitness
    Api.PlutusScriptV1InConway
    Api.PlutusScriptV1
    (newToOldPlutusScriptOrReferenceInput ConwayEra scriptOrRef)
    Api.NoScriptDatumForStake
    redeemer
    execUnits
newToOldPlutusCertificateScriptWitness ConwayEra (Exp.PlutusScriptWitness Plutus.SPlutusV2 scriptOrRef _ redeemer execUnits) =
  Api.PlutusScriptWitness
    Api.PlutusScriptV2InConway
    Api.PlutusScriptV2
    (newToOldPlutusScriptOrReferenceInput ConwayEra scriptOrRef)
    Api.NoScriptDatumForStake
    redeemer
    execUnits
newToOldPlutusCertificateScriptWitness ConwayEra (Exp.PlutusScriptWitness Plutus.SPlutusV3 scriptOrRef _ redeemer execUnits) =
  Api.PlutusScriptWitness
    Api.PlutusScriptV3InConway
    Api.PlutusScriptV3
    (newToOldPlutusScriptOrReferenceInput ConwayEra scriptOrRef)
    Api.NoScriptDatumForStake
    redeemer
    execUnits
newToOldPlutusCertificateScriptWitness ConwayEra (Exp.PlutusScriptWitness Plutus.SPlutusV4 _scriptOrRef _ _redeemer _execUnits) =
  error "dijkstra"
newToOldPlutusCertificateScriptWitness DijkstraEra (Exp.PlutusScriptWitness Plutus.SPlutusV1 scriptOrRef _ redeemer execUnits) =
  Api.PlutusScriptWitness
    Api.PlutusScriptV1InDijkstra
    Api.PlutusScriptV1
    (newToOldPlutusScriptOrReferenceInput DijkstraEra scriptOrRef)
    Api.NoScriptDatumForStake
    redeemer
    execUnits
newToOldPlutusCertificateScriptWitness DijkstraEra (Exp.PlutusScriptWitness Plutus.SPlutusV2 scriptOrRef _ redeemer execUnits) =
  Api.PlutusScriptWitness
    Api.PlutusScriptV2InDijkstra
    Api.PlutusScriptV2
    (newToOldPlutusScriptOrReferenceInput DijkstraEra scriptOrRef)
    Api.NoScriptDatumForStake
    redeemer
    execUnits
newToOldPlutusCertificateScriptWitness DijkstraEra (Exp.PlutusScriptWitness Plutus.SPlutusV3 scriptOrRef _ redeemer execUnits) =
  Api.PlutusScriptWitness
    Api.PlutusScriptV3InDijkstra
    Api.PlutusScriptV3
    (newToOldPlutusScriptOrReferenceInput DijkstraEra scriptOrRef)
    Api.NoScriptDatumForStake
    redeemer
    execUnits
newToOldPlutusCertificateScriptWitness DijkstraEra (Exp.PlutusScriptWitness Plutus.SPlutusV4 _scriptOrRef _ _redeemer _execUnits) =
  error "dijkstra"

newToOldPlutusScriptOrReferenceInput
  :: Era era
  -> Exp.PlutusScriptOrReferenceInput lang (LedgerEra era)
  -> Api.PlutusScriptOrReferenceInput oldlang
newToOldPlutusScriptOrReferenceInput _ (Exp.PReferenceScript txin) = Api.PReferenceScript txin
newToOldPlutusScriptOrReferenceInput _ (Exp.PScript (Exp.PlutusScriptInEra plutusRunnable)) =
  let oldScript = L.unPlutusBinary . L.plutusBinary $ L.plutusFromRunnable plutusRunnable
   in Api.PScript $ Api.PlutusScriptSerialised oldScript
