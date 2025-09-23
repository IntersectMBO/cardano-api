{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Cardano.Api.Experimental.Tx.Internal.Compatible
  ( mkTxCertificates
  )
where

import Cardano.Api.Address qualified as Api
import Cardano.Api.Certificate.Internal qualified as Api
import Cardano.Api.Era.Internal.Eon.Convert
import Cardano.Api.Era.Internal.Eon.ShelleyBasedEra
import Cardano.Api.Experimental.Era
import Cardano.Api.Experimental.Plutus.Internal.Script qualified as Exp
import Cardano.Api.Experimental.Plutus.Internal.ScriptWitness qualified as Exp
import Cardano.Api.Experimental.Simple.Script qualified as Exp
import Cardano.Api.Experimental.Tx.Internal.AnyWitness
import Cardano.Api.Experimental.Tx.Internal.Certificate qualified as Exp
import Cardano.Api.Ledger.Internal.Reexport qualified as L
import Cardano.Api.Plutus.Internal.Script (fromAllegraTimelock, sbeToSimpleScriptLanguageInEra)
import Cardano.Api.Plutus.Internal.Script qualified as Api
import Cardano.Api.Tx.Internal.Body (TxCertificates (..))
import Cardano.Api.Tx.Internal.Body qualified as Api

import Cardano.Ledger.Allegra.Scripts qualified as L
import Cardano.Ledger.Alonzo.Scripts qualified as L
import Cardano.Ledger.Plutus.Language qualified as L
import Cardano.Ledger.Plutus.Language qualified as Plutus

import GHC.Exts (IsList (..))

mkTxCertificates
  :: forall era
   . IsEra era
  => [(Exp.Certificate (ShelleyLedgerEra era), AnyWitness (LedgerEra era))]
  -> Api.TxCertificates Api.BuildTx era
mkTxCertificates [] = TxCertificatesNone
mkTxCertificates certs =
  TxCertificates (convert useEra) $ fromList $ map (getStakeCred useEra) certs
 where
  getStakeCred
    :: Era era
    -> (Exp.Certificate (ShelleyLedgerEra era), AnyWitness (LedgerEra era))
    -> ( Exp.Certificate (ShelleyLedgerEra era)
       , Api.BuildTxWith
           Api.BuildTx
           (Maybe (Api.StakeCredential, Api.Witness Api.WitCtxStake era))
       )
  getStakeCred era (cert, witness) =
    case era of
      ConwayEra -> do
        let Exp.Certificate c = cert
            mStakeCred = Api.getTxCertWitness (convert era) c
            wit =
              case witness of
                AnyKeyWitnessPlaceholder -> Api.KeyWitness Api.KeyWitnessForStakeAddr
                AnySimpleScriptWitness ss ->
                  Api.ScriptWitness Api.ScriptWitnessForStakeAddr $ newToOldSimpleScriptWitness era ss
                AnyPlutusScriptWitness psw ->
                  Api.ScriptWitness Api.ScriptWitnessForStakeAddr $
                    newToOldPlutusCertificateScriptWitness ConwayEra psw
        (cert, pure $ (,wit) <$> mStakeCred)
      DijkstraEra -> do
        let Exp.Certificate c = cert
            mStakeCred = Api.getTxCertWitness (convert era) c
            wit =
              case witness of
                AnyKeyWitnessPlaceholder -> Api.KeyWitness Api.KeyWitnessForStakeAddr
                AnySimpleScriptWitness ss ->
                  Api.ScriptWitness Api.ScriptWitnessForStakeAddr $ newToOldSimpleScriptWitness era ss
                AnyPlutusScriptWitness psw ->
                  Api.ScriptWitness Api.ScriptWitnessForStakeAddr $
                    newToOldPlutusCertificateScriptWitness DijkstraEra psw
        (cert, pure $ (,wit) <$> mStakeCred)

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
    (newToOldPlutusScriptOrReferenceInput scriptOrRef)
    Api.NoScriptDatumForStake
    redeemer
    execUnits
newToOldPlutusCertificateScriptWitness ConwayEra (Exp.PlutusScriptWitness Plutus.SPlutusV2 scriptOrRef _ redeemer execUnits) =
  Api.PlutusScriptWitness
    Api.PlutusScriptV2InConway
    Api.PlutusScriptV2
    (newToOldPlutusScriptOrReferenceInput scriptOrRef)
    Api.NoScriptDatumForStake
    redeemer
    execUnits
newToOldPlutusCertificateScriptWitness ConwayEra (Exp.PlutusScriptWitness Plutus.SPlutusV3 scriptOrRef _ redeemer execUnits) =
  Api.PlutusScriptWitness
    Api.PlutusScriptV3InConway
    Api.PlutusScriptV3
    (newToOldPlutusScriptOrReferenceInput scriptOrRef)
    Api.NoScriptDatumForStake
    redeemer
    execUnits
newToOldPlutusCertificateScriptWitness ConwayEra (Exp.PlutusScriptWitness Plutus.SPlutusV4 _ _ _ _) =
  error "newToOldPlutusCertificateScriptWitness: PlutusV4 script not possible in Conway era"
newToOldPlutusCertificateScriptWitness DijkstraEra (Exp.PlutusScriptWitness Plutus.SPlutusV1 scriptOrRef _ redeemer execUnits) =
  Api.PlutusScriptWitness
    Api.PlutusScriptV1InDijkstra
    Api.PlutusScriptV1
    (newToOldPlutusScriptOrReferenceInput scriptOrRef)
    Api.NoScriptDatumForStake
    redeemer
    execUnits
newToOldPlutusCertificateScriptWitness DijkstraEra (Exp.PlutusScriptWitness Plutus.SPlutusV2 scriptOrRef _ redeemer execUnits) =
  Api.PlutusScriptWitness
    Api.PlutusScriptV2InDijkstra
    Api.PlutusScriptV2
    (newToOldPlutusScriptOrReferenceInput scriptOrRef)
    Api.NoScriptDatumForStake
    redeemer
    execUnits
newToOldPlutusCertificateScriptWitness DijkstraEra (Exp.PlutusScriptWitness Plutus.SPlutusV3 scriptOrRef _ redeemer execUnits) =
  Api.PlutusScriptWitness
    Api.PlutusScriptV3InDijkstra
    Api.PlutusScriptV3
    (newToOldPlutusScriptOrReferenceInput scriptOrRef)
    Api.NoScriptDatumForStake
    redeemer
    execUnits
newToOldPlutusCertificateScriptWitness DijkstraEra (Exp.PlutusScriptWitness Plutus.SPlutusV4 scriptOrRef _ redeemer execUnits) =
  Api.PlutusScriptWitness
    Api.PlutusScriptV4InDijkstra
    Api.PlutusScriptV4
    (newToOldPlutusScriptOrReferenceInput scriptOrRef)
    Api.NoScriptDatumForStake
    redeemer
    execUnits

newToOldPlutusScriptOrReferenceInput
  :: Exp.PlutusScriptOrReferenceInput lang (LedgerEra era)
  -> Api.PlutusScriptOrReferenceInput oldlang
newToOldPlutusScriptOrReferenceInput (Exp.PReferenceScript txin) = Api.PReferenceScript txin
newToOldPlutusScriptOrReferenceInput (Exp.PScript (Exp.PlutusScriptInEra plutusRunnable)) =
  let oldScript = L.unPlutusBinary . L.plutusBinary $ L.plutusFromRunnable plutusRunnable
   in Api.PScript $ Api.PlutusScriptSerialised oldScript
