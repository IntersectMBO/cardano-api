{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Cardano.Api.Internal.Experimental.TxBody.Certificate
  ( Certificate (..)
  , mkTxCertificates
  , convertToOldApiCertificate
  , convertToNewCertificate
  )
where

import Cardano.Api.Internal.Address qualified as Api
import Cardano.Api.Internal.Certificate qualified as Api
import Cardano.Api.Internal.Eon.ConwayEraOnwards qualified as Api
import Cardano.Api.Internal.Eon.ShelleyToBabbageEra qualified as Api
import Cardano.Api.Internal.Experimental.Eras
import Cardano.Api.Internal.Experimental.Plutus.Script
import Cardano.Api.Internal.Experimental.Plutus.ScriptWitness
import Cardano.Api.Internal.Experimental.Simple.Script
import Cardano.Api.Internal.Experimental.Witness.AnyWitness
import Cardano.Api.Internal.Script qualified as Api
import Cardano.Api.Internal.Tx.Body qualified as Api
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Shelley
  ( Convert (..)
  , TxCertificates (..)
  , fromAllegraTimelock
  , sbeToSimpleScriptLanguageInEra
  )

import Cardano.Ledger.Allegra.Scripts qualified as L
import Cardano.Ledger.Plutus.Language qualified as L
import Cardano.Ledger.Plutus.Language qualified as Plutus

import GHC.IsList

data Certificate era where
  Certificate :: L.TxCert era -> Certificate era

convertToOldApiCertificate :: Era era -> Certificate (LedgerEra era) -> Api.Certificate era
convertToOldApiCertificate ConwayEra (Certificate cert) =
  Api.ConwayCertificate Api.ConwayEraOnwardsConway cert

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
  getStakeCred ConwayEra (Certificate cert, AnyKeyWitnessPlaceholder) =
    (Api.ConwayCertificate (convert ConwayEra) cert, Api.BuildTxWith Nothing)
  getStakeCred ConwayEra (Certificate cert, AnySimpleScriptWitness ss) =
    let oldApiCert = Api.ConwayCertificate (convert ConwayEra) cert
        mStakeCred = Api.selectStakeCredentialWitness oldApiCert
        wit = Api.ScriptWitness Api.ScriptWitnessForStakeAddr $ newToOldSimpleScriptWitness ConwayEra ss
     in ( oldApiCert
        , pure $ (,wit) <$> mStakeCred
        )
  getStakeCred ConwayEra (Certificate cert, AnyPlutusScriptWitness psw) =
    let oldApiCert = Api.ConwayCertificate (convert ConwayEra) cert
        mStakeCred = Api.selectStakeCredentialWitness oldApiCert
        wit =
          Api.ScriptWitness Api.ScriptWitnessForStakeAddr $
            newToOldPlutusCertificateScriptWitness ConwayEra psw
     in ( oldApiCert
        , pure $ (,wit) <$> mStakeCred
        )

newToOldSimpleScriptWitness
  :: L.AllegraEraScript (LedgerEra era)
  => Era era -> SimpleScriptOrReferenceInput (LedgerEra era) -> Api.ScriptWitness Api.WitCtxStake era
newToOldSimpleScriptWitness era simple =
  case simple of
    SScript (SimpleScript script) ->
      Api.SimpleScriptWitness
        (sbeToSimpleScriptLanguageInEra $ convert era)
        (Api.SScript $ fromAllegraTimelock script)
    SReferenceScript inp ->
      Api.SimpleScriptWitness
        (sbeToSimpleScriptLanguageInEra $ convert era)
        (Api.SReferenceScript inp)

newToOldPlutusCertificateScriptWitness
  :: Era era -> PlutusScriptWitness lang purpose (LedgerEra era) -> Api.ScriptWitness Api.WitCtxStake era
newToOldPlutusCertificateScriptWitness ConwayEra (PlutusScriptWitness Plutus.SPlutusV1 scriptOrRef _ redeemer execUnits) =
  Api.PlutusScriptWitness
    Api.PlutusScriptV1InConway
    Api.PlutusScriptV1
    (newToOldPlutusScriptOrReferenceInput ConwayEra scriptOrRef)
    Api.NoScriptDatumForStake
    redeemer
    execUnits
newToOldPlutusCertificateScriptWitness ConwayEra (PlutusScriptWitness Plutus.SPlutusV2 scriptOrRef _ redeemer execUnits) =
  Api.PlutusScriptWitness
    Api.PlutusScriptV2InConway
    Api.PlutusScriptV2
    (newToOldPlutusScriptOrReferenceInput ConwayEra scriptOrRef)
    Api.NoScriptDatumForStake
    redeemer
    execUnits
newToOldPlutusCertificateScriptWitness ConwayEra (PlutusScriptWitness Plutus.SPlutusV3 scriptOrRef _ redeemer execUnits) =
  Api.PlutusScriptWitness
    Api.PlutusScriptV3InConway
    Api.PlutusScriptV3
    (newToOldPlutusScriptOrReferenceInput ConwayEra scriptOrRef)
    Api.NoScriptDatumForStake
    redeemer
    execUnits

newToOldPlutusScriptOrReferenceInput
  :: Era era
  -> PlutusScriptOrReferenceInput lang (LedgerEra era)
  -> Api.PlutusScriptOrReferenceInput oldlang
newToOldPlutusScriptOrReferenceInput ConwayEra (PReferenceScript txin) = Api.PReferenceScript txin
newToOldPlutusScriptOrReferenceInput ConwayEra (PScript (PlutusScriptInEra plutusRunnable)) =
  let oldScript = L.unPlutusBinary . L.plutusBinary $ L.plutusFromRunnable plutusRunnable
   in Api.PScript $ Api.PlutusScriptSerialised oldScript
