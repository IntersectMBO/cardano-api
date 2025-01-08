{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Api.Experimental.Tx
  ( -- * Creating transactions using the new API

    -- |
    -- Both the old and the new API can be used to create transactions, and
    -- it is possible to transform a transaction created in one format to the other
    -- since they have the same representation underneath. But we will be moving
    -- towards using the new API and deprecating the old way, since the latter is
    -- simpler, closer to the ledger, and easier to maintain.
    --
    -- In both the new and the old API, in order to construct a transaction,
    -- we need to construct a 'TxBodyContent', and we will need at least a
    -- witness (for example, a 'ShelleyWitnessSigningKey'), to sign the transaction.
    -- This hasn't changed.
    --
    -- To see how to create a transaction using the old API, see the documentation
    -- of the "Cardano.Api.Tx.Body" module.
    --
    -- In the following examples, we are using the following qualified modules:
    --
    -- @
    -- import qualified Cardano.Api as Api                -- the general `cardano-api` exports (including the old API)
    -- import qualified Cardano.Api.Script as Script      -- types related to scripts (Plutus and native)
    -- import qualified Cardano.Api.Ledger as Ledger      -- cardano-ledger re-exports
    -- import qualified Cardano.Api.Experimental as Exp   -- the experimental API
    -- @
    --
    -- You can find a compilable version of these examples in "Test.Cardano.Api.Experimental".

    -- ** Creating a 'TxBodyContent'

    -- |
    -- Independently of whether we use the Experimental or the traditoinal API, we need to create a 'TxBodyContent'.
    --
    -- You can see how to do this in the documentation of the "Cardano.Api.Tx.Body" module.

    -- ** Balancing a transaction

    -- |
    -- If we have a UTxO with exactly 12 ADA, we could just construct the transaction like in the
    -- previous section directly, and it would be a valid transaction, but:
    --
    --   * We are likely wasting ADA
    --   * We may not have exactly one UTxO of 12 ADA
    --   * Our transaciton may not be this simple
    --
    -- For these reasons, it is recommended that we balance the transaction before proceeding with
    -- signing and submitting.
    --
    -- You can see how to balance a transaction in the documentation of the "Cardano.Api.Fees" module.

    -- ** Creating a 'ShelleyWitnessSigningKey'

    -- |
    -- To sign the transaction, we need a witness. For example, a 'ShelleyWitnessSigningKey'.
    --
    -- You can see how to create a 'ShelleyWitnessSigningKey' in the documentation of the "Cardano.Api.Tx.Sign" module.

    -- ** Creating a transaction using the new API

    -- |
    -- Now, let's see how we can create a transaction using the new API. First, we create an 'UnsignedTx' using the 'makeUnsignedTx'
    -- function and the 'Era' and 'TxBodyContent' that we defined earlier:
    --
    -- @
    -- let (Right unsignedTx) = Exp.makeUnsignedTx era txBodyContent
    -- @
    --
    -- Then we use the key witness to witness the current unsigned transaction using the 'makeKeyWitness' function:
    --
    -- @
    -- let transactionWitness = Exp.makeKeyWitness era unsignedTx (Api.WitnessPaymentKey signingKey)
    -- @
    --
    -- Finally, we sign the transaction using the 'signTx' function:
    --
    -- @
    -- let newApiSignedTx :: Ledger.Tx (Exp.LedgerEra Exp.ConwayEra) = Exp.signTx era [] [transactionWitness] unsignedTx
    -- @
    --
    -- Where the empty list is for the bootstrap witnesses, which, in this case, we don't have any.
    --
    -- And that is it. We have a signed transaction.

    -- ** Converting a transaction from the new API to the old API

    -- |
    -- If we have a transaction created using the new API, we can convert it to the old API very easily by
    -- just wrapping it using the 'ShelleyTx' constructor:
    --
    -- @
    -- let oldStyleTx :: Api.Tx Api.ConwayEra = ShelleyTx sbe newApiSignedTx
    -- @

    -- ** Inspecting transactions

    -- |
    -- When using a 'Tx' created using the experimental API, you can extract the 'TxBody' and
    -- 'TxWits' using the lenses 'txBody' and 'txWits' respectively, from "Cardano.Api.Ledger".

    -- * Contents
    UnsignedTx (..)
  , UnsignedTxError (..)
  , makeUnsignedTx
  , makeKeyWitness
  , signTx
  , convertTxBodyToUnsignedTx
  , hashTxBody
  )
where

import           Cardano.Api.Eon.Convert
import           Cardano.Api.Eon.ShelleyBasedEra
import           Cardano.Api.Eras.Core (ToCardanoEra (toCardanoEra), forEraInEon)
import           Cardano.Api.Experimental.Eras
import           Cardano.Api.Feature
import           Cardano.Api.Pretty (docToString, pretty)
import           Cardano.Api.ReexposeLedger (StrictMaybe (..), maybeToStrictMaybe)
import qualified Cardano.Api.ReexposeLedger as L
import           Cardano.Api.Tx.Body
import           Cardano.Api.Tx.Sign

import qualified Cardano.Ledger.Alonzo.TxBody as L
import qualified Cardano.Ledger.Api as L
import qualified Cardano.Ledger.Babbage as Ledger
import qualified Cardano.Ledger.Conway as Ledger
import qualified Cardano.Ledger.Conway.TxBody as L
import qualified Cardano.Ledger.Core as Ledger
import           Cardano.Ledger.Hashes
import qualified Cardano.Ledger.Keys as L
import qualified Cardano.Ledger.SafeHash as L

import qualified Data.Set as Set
import           GHC.Exts (IsList (..))
import           GHC.Stack
import           Lens.Micro

-- | A transaction that can contain everything
-- except key witnesses
newtype UnsignedTx era
  = UnsignedTx (Ledger.Tx (LedgerEra era))

instance IsEra era => Show (UnsignedTx era) where
  showsPrec p (UnsignedTx tx) = case useEra @era of
    BabbageEra -> showsPrec p (tx :: Ledger.Tx Ledger.Babbage)
    ConwayEra -> showsPrec p (tx :: Ledger.Tx Ledger.Conway)

newtype UnsignedTxError
  = UnsignedTxError TxBodyError

makeUnsignedTx
  :: Era era
  -> TxBodyContent BuildTx era
  -> Either TxBodyError (UnsignedTx era)
makeUnsignedTx era bc = obtainCommonConstraints era $ do
  let sbe = convert era

  -- cardano-api types
  let apiTxOuts = txOuts bc
      apiScriptWitnesses = collectTxBodyScriptWitnesses sbe bc
      apiScriptValidity = txScriptValidity bc
      apiMintValue = txMintValue bc
      apiProtocolParameters = txProtocolParams bc
      apiCollateralTxIns = txInsCollateral bc
      apiReferenceInputs = txInsReference bc
      apiExtraKeyWitnesses = txExtraKeyWits bc
      apiReturnCollateral = txReturnCollateral bc
      apiTotalCollateral = txTotalCollateral bc

      -- Ledger types
      txins = convTxIns $ txIns bc
      collTxIns = convCollateralTxIns apiCollateralTxIns
      refTxIns = convReferenceInputs apiReferenceInputs
      outs = convTxOuts sbe apiTxOuts
      fee = convTransactionFee sbe $ txFee bc
      withdrawals = convWithdrawals $ txWithdrawals bc
      returnCollateral = convReturnCollateral sbe apiReturnCollateral
      totalCollateral = convTotalCollateral apiTotalCollateral
      certs = convCertificates sbe $ txCertificates bc
      txAuxData = toAuxiliaryData sbe (txMetadata bc) (txAuxScripts bc)
      scripts = convScripts apiScriptWitnesses
      languages = convLanguages apiScriptWitnesses
      sData = convScriptData sbe apiTxOuts apiScriptWitnesses
      (datums, redeemers) = case sData of
        TxBodyScriptData _ ds rs -> (ds, rs)
        TxBodyNoScriptData -> (mempty, L.Redeemers mempty)

  let setMint = convMintValue apiMintValue
      setReqSignerHashes = convExtraKeyWitnesses apiExtraKeyWitnesses
      ledgerTxBody =
        L.mkBasicTxBody
          & L.inputsTxBodyL .~ txins
          & L.collateralInputsTxBodyL .~ collTxIns
          & L.referenceInputsTxBodyL .~ refTxIns
          & L.outputsTxBodyL .~ outs
          & L.totalCollateralTxBodyL .~ totalCollateral
          & L.collateralReturnTxBodyL .~ returnCollateral
          & L.feeTxBodyL .~ fee
          & L.vldtTxBodyL . L.invalidBeforeL .~ convValidityLowerBound (txValidityLowerBound bc)
          & L.vldtTxBodyL . L.invalidHereAfterL .~ convValidityUpperBound sbe (txValidityUpperBound bc)
          & L.reqSignerHashesTxBodyL .~ setReqSignerHashes
          & L.scriptIntegrityHashTxBodyL .~ getScriptIntegrityHash apiProtocolParameters languages sData
          & L.withdrawalsTxBodyL .~ withdrawals
          & L.certsTxBodyL .~ certs
          & L.mintTxBodyL .~ setMint
          & L.auxDataHashTxBodyL .~ maybe SNothing (SJust . Ledger.hashTxAuxData) txAuxData

      scriptWitnesses =
        L.mkBasicTxWits
          & L.scriptTxWitsL
            .~ fromList
              [ (L.hashScript sw, sw)
              | sw <- scripts
              ]
          & L.datsTxWitsL .~ datums
          & L.rdmrsTxWitsL .~ redeemers

  eraSpecificTxBody <- eraSpecificLedgerTxBody era ledgerTxBody bc

  return . UnsignedTx $
    L.mkBasicTx eraSpecificTxBody
      & L.witsTxL .~ scriptWitnesses
      & L.auxDataTxL .~ maybeToStrictMaybe (toAuxiliaryData sbe (txMetadata bc) (txAuxScripts bc))
      & L.isValidTxL .~ txScriptValidityToIsValid apiScriptValidity

eraSpecificLedgerTxBody
  :: Era era
  -> Ledger.TxBody (LedgerEra era)
  -> TxBodyContent BuildTx era
  -> Either TxBodyError (Ledger.TxBody (LedgerEra era))
eraSpecificLedgerTxBody BabbageEra ledgerbody bc = do
  let sbe = convert BabbageEra

  setUpdateProposal <- convTxUpdateProposal sbe (txUpdateProposal bc)

  return $ ledgerbody & L.updateTxBodyL .~ setUpdateProposal
eraSpecificLedgerTxBody ConwayEra ledgerbody bc =
  let propProcedures = txProposalProcedures bc
      voteProcedures = txVotingProcedures bc
      treasuryDonation = txTreasuryDonation bc
      currentTresuryValue = txCurrentTreasuryValue bc
   in return $
        ledgerbody
          & L.proposalProceduresTxBodyL
            .~ convProposalProcedures (maybe TxProposalProceduresNone unFeatured propProcedures)
          & L.votingProceduresTxBodyL
            .~ convVotingProcedures (maybe TxVotingProceduresNone unFeatured voteProcedures)
          & L.treasuryDonationTxBodyL .~ maybe (L.Coin 0) unFeatured treasuryDonation
          & L.currentTreasuryValueTxBodyL
            .~ L.maybeToStrictMaybe (unFeatured =<< currentTresuryValue)

hashTxBody
  :: L.HashAnnotated (Ledger.TxBody era) EraIndependentTxBody L.StandardCrypto
  => L.TxBody era -> L.Hash L.StandardCrypto EraIndependentTxBody
hashTxBody = L.extractHash @L.StandardCrypto . L.hashAnnotated

makeKeyWitness
  :: Era era
  -> UnsignedTx era
  -> ShelleyWitnessSigningKey
  -> L.WitVKey L.Witness L.StandardCrypto
makeKeyWitness era (UnsignedTx unsignedTx) wsk =
  obtainCommonConstraints era $
    let txbody = unsignedTx ^. L.bodyTxL
        txhash :: L.Hash L.StandardCrypto EraIndependentTxBody
        txhash = obtainCommonConstraints era $ hashTxBody txbody
        sk = toShelleySigningKey wsk
        vk = getShelleyKeyWitnessVerificationKey sk
        signature = makeShelleySignature txhash sk
     in L.WitVKey vk signature

signTx
  :: Era era
  -> [L.BootstrapWitness L.StandardCrypto]
  -> [L.WitVKey L.Witness L.StandardCrypto]
  -> UnsignedTx era
  -> Ledger.Tx (LedgerEra era)
signTx era bootstrapWits shelleyKeyWits (UnsignedTx unsigned) =
  obtainCommonConstraints era $
    let currentScriptWitnesses = unsigned ^. L.witsTxL
        keyWits =
          obtainCommonConstraints era $
            L.mkBasicTxWits
              & L.addrTxWitsL
                .~ Set.fromList shelleyKeyWits
              & L.bootAddrTxWitsL
                .~ Set.fromList bootstrapWits
        signedTx = unsigned & L.witsTxL .~ (keyWits <> currentScriptWitnesses)
     in signedTx

-- Compatibility related. Will be removed once the old api has been deprecated and deleted.

convertTxBodyToUnsignedTx
  :: HasCallStack => ShelleyBasedEra era -> TxBody era -> UnsignedTx era
convertTxBodyToUnsignedTx sbe txbody =
  forEraInEon
    (toCardanoEra sbe)
    (error $ "convertTxBodyToUnsignedTx: Error - unsupported era " <> docToString (pretty sbe))
    ( \w -> do
        let ShelleyTx _ unsignedLedgerTx = makeSignedTransaction [] txbody
        UnsignedTx $ obtainCommonConstraints w unsignedLedgerTx
    )
