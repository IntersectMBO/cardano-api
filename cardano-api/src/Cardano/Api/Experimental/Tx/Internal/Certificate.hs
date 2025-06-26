{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

module Cardano.Api.Experimental.Tx.Internal.Certificate
  ( Certificate (..)
  , mkTxCertificates
  , convertToOldApiCertificate
  , convertToNewCertificate
  )
where

import Cardano.Api.Address qualified as Api
import Cardano.Api.Certificate.Internal qualified as Api
import Cardano.Api.Era.Internal.Eon.Convert
import Cardano.Api.Era.Internal.Eon.ConwayEraOnwards
import Cardano.Api.Era.Internal.Eon.ShelleyToBabbageEra qualified as Api
import Cardano.Api.Experimental.Era
import Cardano.Api.Experimental.Plutus.Internal.Script qualified as Exp
import Cardano.Api.Experimental.Plutus.Internal.ScriptWitness qualified as Exp
import Cardano.Api.Experimental.Simple.Script qualified as Exp
import Cardano.Api.Experimental.Tx.Internal.AnyWitness
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Plutus.Internal.Script
import Cardano.Api.Plutus.Internal.Script qualified as Api
import Cardano.Api.Tx.Internal.Body (TxCertificates (..))
import Cardano.Api.Tx.Internal.Body qualified as Api

import Cardano.Ledger.Allegra.Scripts qualified as L
import Cardano.Ledger.Plutus.Language qualified as L
import Cardano.Ledger.Plutus.Language qualified as Plutus

import GHC.IsList

data Certificate era where
  Certificate :: L.EraTxCert era => L.TxCert era -> Certificate era

deriving instance Show (Certificate era)

deriving instance Eq (Certificate era)

deriving instance Ord (Certificate era)

convertToOldApiCertificate :: Era era -> Certificate (LedgerEra era) -> Api.Certificate era
convertToOldApiCertificate ConwayEra (Certificate cert) =
  Api.ConwayCertificate ConwayEraOnwardsConway cert

convertToNewCertificate :: Era era -> Api.Certificate era -> Certificate (LedgerEra era)
convertToNewCertificate ConwayEra (Api.ConwayCertificate _ cert) = Certificate cert
convertToNewCertificate ConwayEra (Api.ShelleyRelatedCertificate sToBab _) =
  case sToBab :: Api.ShelleyToBabbageEra ConwayEra of {}

mkTxCertificates
  :: forall era
   . IsEra era
  => [(Certificate (LedgerEra era), AnyWitness (LedgerEra era))]
  -> Api.TxCertificates Api.BuildTx era
mkTxCertificates [] = TxCertificatesNone
mkTxCertificates certs =
  TxCertificates (convert useEra) $ fromList $ map (getStakeCred useEra) certs
 where
  getStakeCred
    :: Era era
    -> (Certificate (LedgerEra era), AnyWitness (LedgerEra era))
    -> ( Api.Certificate era
       , Api.BuildTxWith
           Api.BuildTx
           (Maybe (Api.StakeCredential, Api.Witness Api.WitCtxStake era))
       )
  getStakeCred era (Certificate cert, witness) =
    case era of
      ConwayEra -> do
        let oldApiCert = Api.ConwayCertificate (convert era) cert
            mStakeCred = Api.selectStakeCredentialWitness oldApiCert
            wit =
              case witness of
                AnyKeyWitnessPlaceholder -> Api.KeyWitness Api.KeyWitnessForStakeAddr
                AnySimpleScriptWitness ss ->
                  Api.ScriptWitness Api.ScriptWitnessForStakeAddr $ newToOldSimpleScriptWitness era ss
                AnyPlutusScriptWitness psw ->
                  Api.ScriptWitness Api.ScriptWitnessForStakeAddr $
                    newToOldPlutusCertificateScriptWitness ConwayEra psw
        (oldApiCert, pure $ (,wit) <$> mStakeCred)

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

newToOldPlutusScriptOrReferenceInput
  :: Era era
  -> Exp.PlutusScriptOrReferenceInput lang (LedgerEra era)
  -> Api.PlutusScriptOrReferenceInput oldlang
newToOldPlutusScriptOrReferenceInput ConwayEra (Exp.PReferenceScript txin) = Api.PReferenceScript txin
newToOldPlutusScriptOrReferenceInput ConwayEra (Exp.PScript (Exp.PlutusScriptInEra plutusRunnable)) =
  let oldScript = L.unPlutusBinary . L.plutusBinary $ L.plutusFromRunnable plutusRunnable
   in Api.PScript $ Api.PlutusScriptSerialised oldScript
