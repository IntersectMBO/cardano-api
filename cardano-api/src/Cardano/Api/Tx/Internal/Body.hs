{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Api.Tx.Internal.Body
  ( -- * Contents

    -- ** Transaction bodies
    TxBody (.., TxBody)
  , createTransactionBody
  , createAndValidateTransactionBody
  , TxBodyContent (..)

    -- ** Byron only
  , makeByronTransactionBody

    -- *** Transaction body builders
  , defaultTxBodyContent
  , defaultTxFee
  , defaultTxValidityUpperBound
  , setTxIns
  , modTxIns
  , addTxIn
  , addTxIns
  , setTxInsCollateral
  , modTxInsCollateral
  , addTxInsCollateral
  , addTxInCollateral
  , setTxInsReference
  , modTxInsReference
  , addTxInsReference
  , addTxInReference
  , setTxOuts
  , modTxOuts
  , addTxOut
  , addTxOuts
  , setTxTotalCollateral
  , modTxTotalCollateral
  , setTxReturnCollateral
  , modTxReturnCollateral
  , setTxFee
  , modTxFee
  , setTxValidityLowerBound
  , modTxValidityLowerBound
  , setTxValidityUpperBound
  , modTxValidityUpperBound
  , setTxMetadata
  , modTxMetadata
  , setTxAuxScripts
  , modTxAuxScripts
  , setTxExtraKeyWits
  , modTxExtraKeyWits
  , addTxExtraKeyWits
  , setTxProtocolParams
  , setTxWithdrawals
  , modTxWithdrawals
  , setTxCertificates
  , modTxCertificates
  , setTxUpdateProposal
  , modTxUpdateProposal
  , setTxProposalProcedures
  , setTxVotingProcedures
  , setTxMintValue
  , modTxMintValue
  , addTxMintValue
  , subtractTxMintValue
  , setTxScriptValidity
  , modTxScriptValidity
  , setTxCurrentTreasuryValue
  , setTxTreasuryDonation
  , TxBodyError (..)
  , TxOutputError (..)
  , TxBodyScriptData (..)
  , TxScriptValidity (..)
  , ScriptValidity (..)
  , scriptValidityToIsValid
  , isValidToScriptValidity
  , txScriptValidityToIsValid
  , txScriptValidityToScriptValidity

    -- ** Transaction Ids
  , TxId (..)
  , parseTxId
  , getTxId
  , getTxIdByron
  , getTxIdShelley

    -- ** Transaction inputs
  , TxIn (..)
  , parseTxIn
  , TxIns
  , indexTxIns
  , TxIx (..)
  , parseTxIx
  , genesisUTxOPseudoTxIn
  , getReferenceInputsSizeForTxIds

    -- ** Transaction outputs
  , CtxTx
  , CtxUTxO
  , TxOut (..)
  , TxOutValue (..)
  , TxOutDatum (TxOutDatumNone, TxOutDatumHash, TxOutSupplementalDatum, TxOutDatumInline)
  , toCtxUTxOTxOut
  , fromCtxUTxOTxOut
  , lovelaceToTxOutValue
  , prettyRenderTxOut
  , txOutValueToLovelace
  , txOutValueToValue
  , TxOutInAnyEra (..)
  , txOutInAnyEra

    -- ** Other transaction body types
  , TxInsCollateral (..)
  , TxInsReference (..)
  , TxInsReferenceDatums
  , getReferenceInputDatumMap
  , TxReturnCollateral (..)
  , TxTotalCollateral (..)
  , TxFee (..)
  , TxValidityLowerBound (..)
  , TxValidityUpperBound (..)
  , TxMetadataInEra (..)
  , TxAuxScripts (..)
  , TxExtraKeyWitnesses (..)
  , TxWithdrawals (..)
  , indexTxWithdrawals
  , TxCertificates (..)
  , mkTxCertificates
  , indexTxCertificates
  , TxUpdateProposal (..)
  , TxMintValue (..)
  , mkTxMintValue
  , txMintValueToValue
  , indexTxMintValue
  , TxVotingProcedures (..)
  , mkTxVotingProcedures
  , indexTxVotingProcedures
  , TxProposalProcedures (..)
  , mkTxProposalProcedures
  , indexTxProposalProcedures
  , indexWitnessedTxProposalProcedures
  , convProposalProcedures

    -- *** Building vs viewing transactions
  , BuildTxWith (..)
  , BuildTx
  , ViewTx
  , buildTxWithToMaybe

    -- ** Inspecting 'ScriptWitness'es
  , AnyScriptWitness (..)
  , ScriptWitnessIndex (..)
  , renderScriptWitnessIndex
  , collectTxBodyScriptWitnesses
  , collectTxBodyScriptWitnessRequirements
  , toScriptIndex

    -- ** Conversion to inline data
  , scriptDataToInlineDatum

    -- ** Internal conversion functions & types
  , convCertificates
  , convCollateralTxIns
  , convExtraKeyWitnesses
  , convLanguages
  , convMintValue
  , convPParamsToScriptIntegrityHash
  , convReferenceInputs
  , convReturnCollateral
  , convScripts
  , convScriptData
  , convTotalCollateral
  , convTransactionFee
  , convTxIns
  , convTxOuts
  , convTxUpdateProposal
  , convValidityLowerBound
  , convValidityUpperBound
  , convVotingProcedures
  , convWithdrawals
  , getScriptIntegrityHash
  , mkCommonTxBody
  , toAuxiliaryData
  , toByronTxId
  , toShelleyTxId
  , toShelleyTxIn
  , toShelleyTxOut
  , toShelleyTxOutAny
  , fromShelleyTxId
  , fromShelleyTxIn
  , fromShelleyTxOut
  , fromByronTxIn
  , fromLedgerTxOuts
  , renderTxIn

    -- ** Misc helpers
  , calculateExecutionUnitsLovelace

    -- ** Data family instances
  , AsType (AsTxId, AsTxBody, AsByronTxBody, AsShelleyTxBody, AsMaryTxBody)
  , getTxBodyContent
  -- Temp
  , validateTxIns
  , guardShelleyTxInsOverflow
  , validateTxOuts
  , validateMetadata
  , validateTxInsCollateral
  , validateProtocolParameters
  )
where

import Cardano.Api.Address
import Cardano.Api.Byron.Internal.Key
import Cardano.Api.Certificate.Internal
import Cardano.Api.Era.Internal.Case
import Cardano.Api.Era.Internal.Core
import Cardano.Api.Era.Internal.Eon.AllegraEraOnwards
import Cardano.Api.Era.Internal.Eon.AlonzoEraOnwards
import Cardano.Api.Era.Internal.Eon.BabbageEraOnwards
import Cardano.Api.Era.Internal.Eon.Convert
import Cardano.Api.Era.Internal.Eon.ConwayEraOnwards
import Cardano.Api.Era.Internal.Eon.MaryEraOnwards
import Cardano.Api.Era.Internal.Eon.ShelleyBasedEra
import Cardano.Api.Era.Internal.Eon.ShelleyToBabbageEra
import Cardano.Api.Era.Internal.Feature
import Cardano.Api.Error
import Cardano.Api.Experimental.Plutus.Internal.IndexedPlutusScriptWitness
  ( Witnessable (..)
  , WitnessableItem (..)
  , obtainAlonzoScriptPurposeConstraints
  )
import Cardano.Api.Experimental.Plutus.Internal.Shim.LegacyScripts
import Cardano.Api.Experimental.Tx.Internal.TxScriptWitnessRequirements
import Cardano.Api.Governance.Internal.Action.ProposalProcedure
import Cardano.Api.Governance.Internal.Action.VotingProcedure
import Cardano.Api.Internal.Utils
import Cardano.Api.Key.Internal
import Cardano.Api.Ledger.Internal.Reexport qualified as Ledger
import Cardano.Api.Network.Internal.NetworkId
import Cardano.Api.Plutus.Internal.Script
import Cardano.Api.Plutus.Internal.ScriptData
import Cardano.Api.Pretty
import Cardano.Api.ProtocolParameters
import Cardano.Api.Serialise.Json
import Cardano.Api.Tx.Internal.Body.Lens qualified as A
import Cardano.Api.Tx.Internal.BuildTxWith
import Cardano.Api.Tx.Internal.Output
import Cardano.Api.Tx.Internal.Sign
import Cardano.Api.Tx.Internal.TxIn
import Cardano.Api.Tx.Internal.TxMetadata
import Cardano.Api.Value.Internal

import Cardano.Chain.Common qualified as Byron
import Cardano.Chain.UTxO qualified as Byron
import Cardano.Crypto.Hash.Class qualified as Crypto
import Cardano.Crypto.Hashing qualified as Byron
import Cardano.Ledger.Allegra.Core qualified as L
import Cardano.Ledger.Alonzo.Core qualified as L
import Cardano.Ledger.Alonzo.Scripts qualified as Alonzo
import Cardano.Ledger.Alonzo.Tx qualified as Alonzo (hashScriptIntegrity)
import Cardano.Ledger.Alonzo.TxWits qualified as Alonzo
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.Babbage.UTxO qualified as L
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Binary (Annotated (..))
import Cardano.Ledger.Binary qualified as CBOR
import Cardano.Ledger.Coin qualified as L
import Cardano.Ledger.Conway.Core qualified as L
import Cardano.Ledger.Core ()
import Cardano.Ledger.Core qualified as Ledger
import Cardano.Ledger.Credential qualified as Shelley
import Cardano.Ledger.Hashes qualified as SafeHash
import Cardano.Ledger.Keys qualified as Shelley
import Cardano.Ledger.Mary.Value as L (MaryValue (..), MultiAsset)
import Cardano.Ledger.Plutus.Language qualified as Plutus
import Cardano.Ledger.Shelley.API qualified as Ledger
import Cardano.Ledger.Shelley.Genesis qualified as Shelley
import Cardano.Ledger.Shelley.TxCert qualified as Shelley
import Cardano.Ledger.TxIn qualified as L
import Cardano.Ledger.Val as L (isZero)
import Cardano.Slotting.Slot (SlotNo (..))
import Ouroboros.Consensus.Shelley.Eras qualified as E
  ( AllegraEra
  , AlonzoEra
  , BabbageEra
  , ConwayEra
  , MaryEra
  , ShelleyEra
  )

import Control.Monad
import Data.Aeson (object, (.=))
import Data.Aeson qualified as Aeson
import Data.Bifunctor (Bifunctor (..))
import Data.ByteString (ByteString)
import Data.Foldable (for_)
import Data.Function (on)
import Data.Functor (($>))
import Data.List (sortBy)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Ordered.Strict (OMap)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.MonoTraversable (omap)
import Data.Monoid
import Data.OSet.Strict (OSet)
import Data.OSet.Strict qualified as OSet
import Data.Sequence.Strict qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Lazy qualified as LText
import Data.Text.Lazy.Builder qualified as LText
import Data.Type.Equality
import Data.Typeable
import Data.Word (Word16, Word32, Word64)
import Formatting.Buildable (Buildable)
import Formatting.Buildable qualified as Build
import GHC.Exts (IsList (..))
import GHC.Stack
import Lens.Micro hiding (ix)
import Lens.Micro.Extras (view)

-- ----------------------------------------------------------------------------
-- Transaction input values (era-dependent)
--

type TxIns build era = [(TxIn, BuildTxWith build (Witness WitCtxTxIn era))]

-- | Index transaction inputs ordered by TxIn
-- Please note that the result can contain also 'KeyWitness'es.
-- See section 4.1 of https://github.com/intersectmbo/cardano-ledger/releases/latest/download/alonzo-ledger.pdf
indexTxIns
  :: TxIns BuildTx era
  -> [(ScriptWitnessIndex, TxIn, Witness WitCtxTxIn era)]
indexTxIns txins =
  [ (ScriptWitnessIndexTxIn ix, txIn, witness)
  | (ix, (txIn, BuildTxWith witness)) <- zip [0 ..] $ orderTxIns txins
  ]
 where
  -- This relies on the TxId Ord instance being consistent with the
  -- Ledger.TxId Ord instance via the toShelleyTxId conversion
  -- This is checked by prop_ord_distributive_TxId
  orderTxIns :: [(TxIn, v)] -> [(TxIn, v)]
  orderTxIns = sortBy (compare `on` fst)

data TxInsCollateral era where
  TxInsCollateralNone
    :: TxInsCollateral era
  TxInsCollateral
    :: AlonzoEraOnwards era
    -> [TxIn] -- Only key witnesses, no scripts.
    -> TxInsCollateral era

deriving instance Eq (TxInsCollateral era)

deriving instance Show (TxInsCollateral era)

data TxReturnCollateral ctx era where
  TxReturnCollateralNone
    :: TxReturnCollateral ctx era
  TxReturnCollateral
    :: BabbageEraOnwards era
    -> TxOut ctx era
    -> TxReturnCollateral ctx era

deriving instance Eq (TxReturnCollateral ctx era)

deriving instance Show (TxReturnCollateral ctx era)

data TxTotalCollateral era where
  TxTotalCollateralNone
    :: TxTotalCollateral era
  TxTotalCollateral
    :: BabbageEraOnwards era
    -> L.Coin
    -> TxTotalCollateral era

deriving instance Eq (TxTotalCollateral era)

deriving instance Show (TxTotalCollateral era)

-- | Transaction reference inputs. Those are not spent and do not require any witnessing to be included in a valid
-- transaction. Any output can be a reference input. Reference inputs only affect the information that is passed to
-- scripts by being included in @TxInfo@.
--
-- The third parameter of the 'TxInsReference' constructor stores actual resolved datums. When any outputs of referenced
-- 'TxIns' store only datum hash, the only way to use them in the plutus script is to provide a resolved datum here.
-- Note that inserting a datum with hash not present in the reference inputs will result in an error on transaction
-- submission.
--
-- See also: https://github.com/intersectmbo/cardano-ledger/releases/latest/download/babbage-ledger.pdf, chapter 3.
data TxInsReference build era where
  TxInsReferenceNone :: TxInsReference build era
  TxInsReference
    :: BabbageEraOnwards era
    -> [TxIn]
    -- ^ A list of reference inputs
    -> TxInsReferenceDatums build
    -- ^ A set of public key inputs resolved datums, whose hashes are referenced in UTXO of reference inputs.
    -> TxInsReference build era

deriving instance Eq (TxInsReference build era)

deriving instance Show (TxInsReference build era)

-- | The public key inputs' resolved datums, referenced by hash in the transaction reference inputs.
type TxInsReferenceDatums build = BuildTxWith build (Set HashableScriptData)

getReferenceInputDatumMap
  :: TxInsReferenceDatums build
  -> Map (Hash ScriptData) HashableScriptData
getReferenceInputDatumMap = \case
  ViewTx -> mempty
  BuildTxWith datumSet -> fromList $ map (\h -> (hashScriptDataBytes h, h)) $ toList datumSet

-- ----------------------------------------------------------------------------
-- Transaction fees
--

data TxFee era where
  TxFeeExplicit :: ShelleyBasedEra era -> L.Coin -> TxFee era

deriving instance Eq (TxFee era)

deriving instance Show (TxFee era)

defaultTxFee :: ShelleyBasedEra era -> TxFee era
defaultTxFee w = TxFeeExplicit w mempty

-- ----------------------------------------------------------------------------
-- Transaction validity range
--

-- | This was formerly known as the TTL.
data TxValidityUpperBound era where
  TxValidityUpperBound
    :: ShelleyBasedEra era
    -> Maybe SlotNo
    -> TxValidityUpperBound era

deriving instance Eq (TxValidityUpperBound era)

deriving instance Show (TxValidityUpperBound era)

defaultTxValidityUpperBound
  :: ()
  => ShelleyBasedEra era
  -> TxValidityUpperBound era
defaultTxValidityUpperBound sbe = TxValidityUpperBound sbe Nothing

data TxValidityLowerBound era where
  TxValidityNoLowerBound
    :: TxValidityLowerBound era
  TxValidityLowerBound
    :: AllegraEraOnwards era
    -> SlotNo
    -> TxValidityLowerBound era

deriving instance Eq (TxValidityLowerBound era)

deriving instance Show (TxValidityLowerBound era)

-- ----------------------------------------------------------------------------
-- Transaction metadata (era-dependent)
--

data TxMetadataInEra era where
  TxMetadataNone
    :: TxMetadataInEra era
  TxMetadataInEra
    :: ShelleyBasedEra era
    -> TxMetadata
    -> TxMetadataInEra era

deriving instance Eq (TxMetadataInEra era)

deriving instance Show (TxMetadataInEra era)

-- ----------------------------------------------------------------------------
-- Auxiliary scripts (era-dependent)
--

data TxAuxScripts era where
  TxAuxScriptsNone
    :: TxAuxScripts era
  TxAuxScripts
    :: AllegraEraOnwards era
    -> [ScriptInEra era]
    -> TxAuxScripts era

deriving instance Eq (TxAuxScripts era)

deriving instance Show (TxAuxScripts era)

-- ----------------------------------------------------------------------------
-- Optionally required signatures (era-dependent)
--

data TxExtraKeyWitnesses era where
  TxExtraKeyWitnessesNone
    :: TxExtraKeyWitnesses era
  TxExtraKeyWitnesses
    :: AlonzoEraOnwards era
    -> [Hash PaymentKey]
    -> TxExtraKeyWitnesses era

deriving instance Eq (TxExtraKeyWitnesses era)

deriving instance Show (TxExtraKeyWitnesses era)

-- ----------------------------------------------------------------------------
-- Withdrawals within transactions (era-dependent)
--

data TxWithdrawals build era where
  TxWithdrawalsNone
    :: TxWithdrawals build era
  TxWithdrawals
    :: ShelleyBasedEra era
    -> [(StakeAddress, L.Coin, BuildTxWith build (Witness WitCtxStake era))]
    -> TxWithdrawals build era

deriving instance Eq (TxWithdrawals build era)

deriving instance Show (TxWithdrawals build era)

-- | Index the withdrawals with witnesses in the order of stake addresses.
-- See section 4.1 of https://github.com/intersectmbo/cardano-ledger/releases/latest/download/alonzo-ledger.pdf
indexTxWithdrawals
  :: TxWithdrawals BuildTx era
  -> [(ScriptWitnessIndex, StakeAddress, L.Coin, Witness WitCtxStake era)]
indexTxWithdrawals TxWithdrawalsNone = []
indexTxWithdrawals (TxWithdrawals _ withdrawals) =
  [ (ScriptWitnessIndexWithdrawal ix, addr, coin, witness)
  | (ix, (addr, coin, BuildTxWith witness)) <- zip [0 ..] (orderStakeAddrs withdrawals)
  ]
 where
  -- This relies on the StakeAddress Ord instance being consistent with the
  -- Shelley.RewardAcnt Ord instance via the toShelleyStakeAddr conversion
  -- This is checked by prop_ord_distributive_StakeAddress
  orderStakeAddrs :: [(StakeAddress, x, v)] -> [(StakeAddress, x, v)]
  orderStakeAddrs = sortBy (compare `on` (\(k, _, _) -> k))

-- ----------------------------------------------------------------------------
-- Certificates within transactions (era-dependent)
--

data TxCertificates build era where
  -- | No certificates
  TxCertificatesNone
    :: TxCertificates build era
  -- | Represents certificates present in transaction. Prefer using 'mkTxCertificates' to constructing
  -- this type with a constructor
  TxCertificates
    :: ShelleyBasedEra era
    -> OMap
         (Certificate era)
         ( BuildTxWith
             build
             (Maybe (StakeCredential, Witness WitCtxStake era))
         )
    -> TxCertificates build era

deriving instance Eq (TxCertificates build era)

deriving instance Show (TxCertificates build era)

-- | Create 'TxCertificates'. Note that 'Certificate era' will be deduplicated. Only Certificates with a
-- stake credential will be in the result.
--
-- Note that, when building a transaction in Conway era, a witness is not required for staking credential
-- registration, but this is only the case during the transitional period of Conway era and only for staking
-- credential registration certificates without a deposit. Future eras will require a witness for
-- registration certificates, because the one without a deposit will be removed.
mkTxCertificates
  :: Applicative (BuildTxWith build)
  => ShelleyBasedEra era
  -> [(Certificate era, Maybe (ScriptWitness WitCtxStake era))]
  -> TxCertificates build era
mkTxCertificates _ [] = TxCertificatesNone
mkTxCertificates sbe certs = TxCertificates sbe . fromList $ map getStakeCred certs
 where
  getStakeCred (cert, mWit) = do
    let wit =
          maybe
            (KeyWitness KeyWitnessForStakeAddr)
            (ScriptWitness ScriptWitnessForStakeAddr)
            mWit
    ( cert
      , pure $
          (,wit) <$> selectStakeCredentialWitness cert
      )

-- | Index certificates with witnesses by the order they appear in the list (in the transaction).
-- See section 4.1 of https://github.com/intersectmbo/cardano-ledger/releases/latest/download/alonzo-ledger.pdf
indexTxCertificates
  :: TxCertificates BuildTx era
  -> [(ScriptWitnessIndex, Certificate era, StakeCredential, Witness WitCtxStake era)]
indexTxCertificates TxCertificatesNone = []
indexTxCertificates (TxCertificates _ certsWits) =
  [ (ScriptWitnessIndexCertificate ix, cert, stakeCred, witness)
  | (ix, (cert, BuildTxWith (Just (stakeCred, witness)))) <- zip [0 ..] $ toList certsWits
  ]

data TxUpdateProposal era where
  TxUpdateProposalNone :: TxUpdateProposal era
  TxUpdateProposal :: ShelleyToBabbageEra era -> UpdateProposal -> TxUpdateProposal era

deriving instance Eq (TxUpdateProposal era)

deriving instance Show (TxUpdateProposal era)

-- ----------------------------------------------------------------------------
-- Value minting within transactions (era-dependent)
--

data TxMintValue build era where
  TxMintNone :: TxMintValue build era
  TxMintValue
    :: MaryEraOnwards era
    -> Map
         PolicyId
         ( PolicyAssets
         , BuildTxWith build (ScriptWitness WitCtxMint era)
         )
    -> TxMintValue build era

deriving instance Eq (TxMintValue build era)

deriving instance Show (TxMintValue build era)

instance Semigroup (TxMintValue build era) where
  TxMintNone <> b = b
  a <> TxMintNone = a
  TxMintValue w a <> TxMintValue _ b = TxMintValue w $ Map.unionWith mergeElements a b
   where
    mergeElements (a1, w1) (a2, _w2) = (a1 <> a2, w1)

instance Monoid (TxMintValue build era) where
  mempty = TxMintNone
  mappend = (<>)

-- | A helper function for building 'TxMintValue' with present witnesses. Only the first witness
-- in the argument will be used for each policy id.
mkTxMintValue
  :: MaryEraOnwards era
  -> [(PolicyId, PolicyAssets, BuildTxWith build (ScriptWitness WitCtxMint era))]
  -> TxMintValue build era
mkTxMintValue _ [] = TxMintNone
mkTxMintValue w vs =
  mconcat $
    [ TxMintValue w (fromList [(policyId, (assets, bWit))])
    | (policyId, assets, bWit) <- vs
    ]

-- | Convert 'TxMintValue' to a more handy 'Value'.
txMintValueToValue :: TxMintValue build era -> Value
txMintValueToValue TxMintNone = mempty
txMintValueToValue (TxMintValue _ policiesWithAssets) =
  mconcat
    [ policyAssetsToValue policyId assets
    | (policyId, (assets, _witness)) <- toList policiesWithAssets
    ]

-- | Index the assets with witnesses in the order of policy ids.
-- See section 4.1 of https://github.com/intersectmbo/cardano-ledger/releases/latest/download/alonzo-ledger.pdf
indexTxMintValue
  :: TxMintValue build era
  -> [ ( ScriptWitnessIndex
       , PolicyId
       , PolicyAssets
       , BuildTxWith build (ScriptWitness WitCtxMint era)
       )
     ]
indexTxMintValue TxMintNone = []
indexTxMintValue (TxMintValue _ policiesWithAssets) =
  [ (ScriptWitnessIndexMint ix, policyId, assets, witness)
  | (ix, (policyId, (assets, witness))) <- zip [0 ..] $ toList policiesWithAssets
  ]

-- ----------------------------------------------------------------------------
-- Votes within transactions (era-dependent)
--

data TxVotingProcedures build era where
  TxVotingProceduresNone :: TxVotingProcedures build era
  TxVotingProcedures
    :: L.VotingProcedures (ShelleyLedgerEra era)
    -> BuildTxWith
         build
         (Map Ledger.Voter (ScriptWitness WitCtxStake era))
    -> TxVotingProcedures build era

deriving instance Eq (TxVotingProcedures build era)

deriving instance Show (TxVotingProcedures build era)

-- | Create voting procedures from map of voting procedures and optional witnesses.
-- Validates the function argument, to make sure the list of votes is legal.
-- See 'mergeVotingProcedures' for validation rules.
mkTxVotingProcedures
  :: Applicative (BuildTxWith build)
  => [(VotingProcedures era, Maybe (ScriptWitness WitCtxStake era))]
  -> Either (VotesMergingConflict era) (TxVotingProcedures build era)
mkTxVotingProcedures votingProcedures = do
  VotingProcedures procedure <-
    foldM f emptyVotingProcedures votingProcedures
  pure $ TxVotingProcedures procedure (pure votingScriptWitnessMap)
 where
  votingScriptWitnessMap =
    foldl
      (\acc next -> acc `Map.union` uncurry votingScriptWitnessSingleton next)
      Map.empty
      votingProcedures
  f acc (procedure, _witness) = mergeVotingProcedures acc procedure

  votingScriptWitnessSingleton
    :: VotingProcedures era
    -> Maybe (ScriptWitness WitCtxStake era)
    -> Map L.Voter (ScriptWitness WitCtxStake era)
  votingScriptWitnessSingleton _ Nothing = Map.empty
  votingScriptWitnessSingleton votingProcedures' (Just scriptWitness) = do
    let voter = fromJust $ getVotingScriptCredentials votingProcedures'
    Map.singleton voter scriptWitness

  getVotingScriptCredentials
    :: VotingProcedures era
    -> Maybe L.Voter
  getVotingScriptCredentials (VotingProcedures (L.VotingProcedures m)) =
    listToMaybe $ Map.keys m

-- | Index voting procedures by the order of the votes ('Ord').
indexTxVotingProcedures
  :: TxVotingProcedures BuildTx era
  -> [ ( ScriptWitnessIndex
       , L.Voter
       , ScriptWitness WitCtxStake era
       )
     ]
indexTxVotingProcedures TxVotingProceduresNone = []
indexTxVotingProcedures (TxVotingProcedures vProcedures (BuildTxWith sWitMap)) =
  [ (ScriptWitnessIndexVoting $ fromIntegral index, vote, scriptWitness)
  | let allVoteMap = L.unVotingProcedures vProcedures
  , (vote, scriptWitness) <- toList sWitMap
  , index <- maybeToList $ Map.lookupIndex vote allVoteMap
  ]

-- ----------------------------------------------------------------------------
-- Proposals within transactions (era-dependent)
--
-- A proposal procedure houses a governance action that is required to be voted into acceptance when submitted.
data TxProposalProcedures build era where
  -- | No proposals in transaction..
  TxProposalProceduresNone :: TxProposalProcedures build era
  -- | Represents proposal procedures present in transaction.
  TxProposalProcedures
    :: Ledger.EraPParams (ShelleyLedgerEra era)
    => OMap
         (L.ProposalProcedure (ShelleyLedgerEra era))
         ( BuildTxWith
             build
             (Maybe (ScriptWitness WitCtxStake era))
         )
    -> TxProposalProcedures build era

deriving instance Eq (TxProposalProcedures build era)

deriving instance Show (TxProposalProcedures build era)

-- | A smart constructor for 'TxProposalProcedures'. It makes sure that the value produced is consistent - the
-- witnessed proposals are also present in the first constructor parameter.
mkTxProposalProcedures
  :: forall era build
   . Applicative (BuildTxWith build)
  => IsShelleyBasedEra era
  => [(L.ProposalProcedure (ShelleyLedgerEra era), Maybe (ScriptWitness WitCtxStake era))]
  -> TxProposalProcedures build era
mkTxProposalProcedures proposals = do
  shelleyBasedEraConstraints (shelleyBasedEra @era) $
    TxProposalProcedures $
      fromList $
        map (second pure) proposals

-- | Index proposal procedures by their order ('Ord').
-- | and filter out the ones that do not have a witness.
indexTxProposalProcedures
  :: TxProposalProcedures BuildTx era
  -> [(ScriptWitnessIndex, L.ProposalProcedure (ShelleyLedgerEra era), ScriptWitness WitCtxStake era)]
indexTxProposalProcedures proposals =
  [ (ix, proposal, scriptWitness)
  | (proposal, Just (ix, scriptWitness)) <- indexWitnessedTxProposalProcedures proposals
  ]

-- | Index proposal procedures by their order ('Ord').
indexWitnessedTxProposalProcedures
  :: TxProposalProcedures BuildTx era
  -> [ ( L.ProposalProcedure (ShelleyLedgerEra era)
       , Maybe (ScriptWitnessIndex, ScriptWitness WitCtxStake era)
       )
     ]
indexWitnessedTxProposalProcedures TxProposalProceduresNone = []
indexWitnessedTxProposalProcedures (TxProposalProcedures proposals) = do
  let allProposalsList = zip [0 ..] $ toList proposals
  [ (proposal, fmap (ScriptWitnessIndexProposing ix,) mScriptWitness)
    | (ix, (proposal, BuildTxWith mScriptWitness)) <- allProposalsList
    ]

-- ----------------------------------------------------------------------------
-- Transaction body content
--

-- If you extend this type, consider updating:
-- - the 'makeShelleyTransactionBody' function of the relevant era below, and
-- - the @friendly*@ family of functions in cardano-cli.
data TxBodyContent build era
  = TxBodyContent
  { txIns :: TxIns build era
  , txInsCollateral :: TxInsCollateral era
  , txInsReference :: TxInsReference build era
  , txOuts :: [TxOut CtxTx era]
  , txTotalCollateral :: TxTotalCollateral era
  , txReturnCollateral :: TxReturnCollateral CtxTx era
  , txFee :: TxFee era
  , txValidityLowerBound :: TxValidityLowerBound era
  , txValidityUpperBound :: TxValidityUpperBound era
  , txMetadata :: TxMetadataInEra era
  , txAuxScripts :: TxAuxScripts era
  , txExtraKeyWits :: TxExtraKeyWitnesses era
  , txProtocolParams :: BuildTxWith build (Maybe (LedgerProtocolParameters era))
  , txWithdrawals :: TxWithdrawals build era
  , txCertificates :: TxCertificates build era
  , txUpdateProposal :: TxUpdateProposal era
  , txMintValue :: TxMintValue build era
  , txScriptValidity :: TxScriptValidity era
  , txProposalProcedures :: Maybe (Featured ConwayEraOnwards era (TxProposalProcedures build era))
  , txVotingProcedures :: Maybe (Featured ConwayEraOnwards era (TxVotingProcedures build era))
  , txCurrentTreasuryValue :: Maybe (Featured ConwayEraOnwards era (Maybe L.Coin))
  -- ^ Current treasury value
  , txTreasuryDonation :: Maybe (Featured ConwayEraOnwards era L.Coin)
  -- ^ Treasury donation to perform
  }
  deriving (Eq, Show)

defaultTxBodyContent
  :: ()
  => ShelleyBasedEra era
  -> TxBodyContent BuildTx era
defaultTxBodyContent era =
  TxBodyContent
    { txIns = []
    , txInsCollateral = TxInsCollateralNone
    , txInsReference = TxInsReferenceNone
    , txOuts = []
    , txTotalCollateral = TxTotalCollateralNone
    , txReturnCollateral = TxReturnCollateralNone
    , txFee = defaultTxFee era
    , txValidityLowerBound = TxValidityNoLowerBound
    , txValidityUpperBound = defaultTxValidityUpperBound era
    , txMetadata = TxMetadataNone
    , txAuxScripts = TxAuxScriptsNone
    , txExtraKeyWits = TxExtraKeyWitnessesNone
    , txProtocolParams = BuildTxWith Nothing
    , txWithdrawals = TxWithdrawalsNone
    , txCertificates = TxCertificatesNone
    , txUpdateProposal = TxUpdateProposalNone
    , txMintValue = TxMintNone
    , txScriptValidity = TxScriptValidityNone
    , txProposalProcedures = Nothing
    , txVotingProcedures = Nothing
    , txCurrentTreasuryValue = Nothing
    , txTreasuryDonation = Nothing
    }

setTxIns :: TxIns build era -> TxBodyContent build era -> TxBodyContent build era
setTxIns v txBodyContent = txBodyContent{txIns = v}

modTxIns
  :: (TxIns build era -> TxIns build era) -> TxBodyContent build era -> TxBodyContent build era
modTxIns f txBodyContent = txBodyContent{txIns = f (txIns txBodyContent)}

addTxIn
  :: (TxIn, BuildTxWith build (Witness WitCtxTxIn era))
  -> TxBodyContent build era
  -> TxBodyContent build era
addTxIn txIn = modTxIns (txIn :)

addTxIns :: TxIns build era -> TxBodyContent build era -> TxBodyContent build era
addTxIns txIns = modTxIns (<> txIns)

setTxInsCollateral :: TxInsCollateral era -> TxBodyContent build era -> TxBodyContent build era
setTxInsCollateral v txBodyContent = txBodyContent{txInsCollateral = v}

modTxInsCollateral
  :: (TxInsCollateral era -> TxInsCollateral era) -> TxBodyContent build era -> TxBodyContent build era
modTxInsCollateral f txBodyContent = txBodyContent{txInsCollateral = f (txInsCollateral txBodyContent)}

addTxInsCollateral
  :: IsAlonzoBasedEra era => [TxIn] -> TxBodyContent build era -> TxBodyContent build era
addTxInsCollateral txInsCollateral =
  modTxInsCollateral
    ( \case
        TxInsCollateralNone -> TxInsCollateral alonzoBasedEra txInsCollateral
        TxInsCollateral era xs -> TxInsCollateral era (xs <> txInsCollateral)
    )

addTxInCollateral
  :: IsAlonzoBasedEra era => TxIn -> TxBodyContent build era -> TxBodyContent build era
addTxInCollateral txInCollateral = addTxInsCollateral [txInCollateral]

setTxInsReference :: TxInsReference build era -> TxBodyContent build era -> TxBodyContent build era
setTxInsReference v txBodyContent = txBodyContent{txInsReference = v}

modTxInsReference
  :: (TxInsReference build era -> TxInsReference build era)
  -> TxBodyContent build era
  -> TxBodyContent build era
modTxInsReference f txBodyContent = txBodyContent{txInsReference = f (txInsReference txBodyContent)}

addTxInsReference
  :: Applicative (BuildTxWith build)
  => IsBabbageBasedEra era
  => [TxIn]
  -> Set HashableScriptData
  -> TxBodyContent build era
  -> TxBodyContent build era
addTxInsReference txInsReference scriptData =
  modTxInsReference $
    \case
      TxInsReferenceNone -> TxInsReference babbageBasedEra txInsReference (pure scriptData)
      TxInsReference era xs bScriptData' -> TxInsReference era (xs <> txInsReference) ((<> scriptData) <$> bScriptData')

addTxInReference
  :: Applicative (BuildTxWith build)
  => IsBabbageBasedEra era
  => TxIn
  -> Maybe HashableScriptData
  -> TxBodyContent build era
  -> TxBodyContent build era
addTxInReference txInReference mDatum = addTxInsReference [txInReference] . fromList $ maybeToList mDatum

setTxOuts :: [TxOut CtxTx era] -> TxBodyContent build era -> TxBodyContent build era
setTxOuts v txBodyContent = txBodyContent{txOuts = v}

modTxOuts
  :: ([TxOut CtxTx era] -> [TxOut CtxTx era]) -> TxBodyContent build era -> TxBodyContent build era
modTxOuts f txBodyContent = txBodyContent{txOuts = f (txOuts txBodyContent)}

addTxOut :: TxOut CtxTx era -> TxBodyContent build era -> TxBodyContent build era
addTxOut txOut = modTxOuts (txOut :)

addTxOuts :: [TxOut CtxTx era] -> TxBodyContent build era -> TxBodyContent build era
addTxOuts txOuts = modTxOuts (<> txOuts)

setTxTotalCollateral :: TxTotalCollateral era -> TxBodyContent build era -> TxBodyContent build era
setTxTotalCollateral v txBodyContent = txBodyContent{txTotalCollateral = v}

modTxTotalCollateral
  :: (TxTotalCollateral era -> TxTotalCollateral era)
  -> TxBodyContent build era
  -> TxBodyContent build era
modTxTotalCollateral f txBodyContent = txBodyContent{txTotalCollateral = f (txTotalCollateral txBodyContent)}

setTxReturnCollateral
  :: TxReturnCollateral CtxTx era -> TxBodyContent build era -> TxBodyContent build era
setTxReturnCollateral v txBodyContent = txBodyContent{txReturnCollateral = v}

modTxReturnCollateral
  :: (TxReturnCollateral CtxTx era -> TxReturnCollateral CtxTx era)
  -> TxBodyContent build era
  -> TxBodyContent build era
modTxReturnCollateral f txBodyContent = txBodyContent{txReturnCollateral = f (txReturnCollateral txBodyContent)}

setTxFee :: TxFee era -> TxBodyContent build era -> TxBodyContent build era
setTxFee v txBodyContent = txBodyContent{txFee = v}

modTxFee
  :: (TxFee era -> TxFee era)
  -> TxBodyContent build era
  -> TxBodyContent build era
modTxFee f txBodyContent = txBodyContent{txFee = f (txFee txBodyContent)}

setTxValidityLowerBound
  :: TxValidityLowerBound era -> TxBodyContent build era -> TxBodyContent build era
setTxValidityLowerBound v txBodyContent = txBodyContent{txValidityLowerBound = v}

modTxValidityLowerBound
  :: (TxValidityLowerBound era -> TxValidityLowerBound era)
  -> TxBodyContent build era
  -> TxBodyContent build era
modTxValidityLowerBound f txBodyContent = txBodyContent{txValidityLowerBound = f (txValidityLowerBound txBodyContent)}

setTxValidityUpperBound
  :: TxValidityUpperBound era -> TxBodyContent build era -> TxBodyContent build era
setTxValidityUpperBound v txBodyContent = txBodyContent{txValidityUpperBound = v}

modTxValidityUpperBound
  :: (TxValidityUpperBound era -> TxValidityUpperBound era)
  -> TxBodyContent build era
  -> TxBodyContent build era
modTxValidityUpperBound f txBodyContent = txBodyContent{txValidityUpperBound = f (txValidityUpperBound txBodyContent)}

setTxMetadata :: TxMetadataInEra era -> TxBodyContent build era -> TxBodyContent build era
setTxMetadata v txBodyContent = txBodyContent{txMetadata = v}

modTxMetadata
  :: (TxMetadataInEra era -> TxMetadataInEra era)
  -> TxBodyContent build era
  -> TxBodyContent build era
modTxMetadata f txBodyContent = txBodyContent{txMetadata = f (txMetadata txBodyContent)}

setTxAuxScripts :: TxAuxScripts era -> TxBodyContent build era -> TxBodyContent build era
setTxAuxScripts v txBodyContent = txBodyContent{txAuxScripts = v}

modTxAuxScripts
  :: (TxAuxScripts era -> TxAuxScripts era)
  -> TxBodyContent build era
  -> TxBodyContent build era
modTxAuxScripts f txBodyContent = txBodyContent{txAuxScripts = f (txAuxScripts txBodyContent)}

setTxExtraKeyWits :: TxExtraKeyWitnesses era -> TxBodyContent build era -> TxBodyContent build era
setTxExtraKeyWits v txBodyContent = txBodyContent{txExtraKeyWits = v}

modTxExtraKeyWits
  :: (TxExtraKeyWitnesses era -> TxExtraKeyWitnesses era)
  -> TxBodyContent build era
  -> TxBodyContent build era
modTxExtraKeyWits f txBodyContent = txBodyContent{txExtraKeyWits = f (txExtraKeyWits txBodyContent)}

addTxExtraKeyWits
  :: IsAlonzoBasedEra era => [Hash PaymentKey] -> TxBodyContent build era -> TxBodyContent build era
addTxExtraKeyWits vks =
  modTxExtraKeyWits
    ( \case
        TxExtraKeyWitnessesNone ->
          TxExtraKeyWitnesses alonzoBasedEra vks
        TxExtraKeyWitnesses era vks' ->
          TxExtraKeyWitnesses era (vks' <> vks)
    )

setTxProtocolParams
  :: BuildTxWith build (Maybe (LedgerProtocolParameters era))
  -> TxBodyContent build era
  -> TxBodyContent build era
setTxProtocolParams v txBodyContent = txBodyContent{txProtocolParams = v}

setTxWithdrawals :: TxWithdrawals build era -> TxBodyContent build era -> TxBodyContent build era
setTxWithdrawals v txBodyContent = txBodyContent{txWithdrawals = v}

modTxWithdrawals
  :: (TxWithdrawals build era -> TxWithdrawals build era)
  -> TxBodyContent build era
  -> TxBodyContent build era
modTxWithdrawals f txBodyContent = txBodyContent{txWithdrawals = f (txWithdrawals txBodyContent)}

setTxCertificates :: TxCertificates build era -> TxBodyContent build era -> TxBodyContent build era
setTxCertificates v txBodyContent = txBodyContent{txCertificates = v}

modTxCertificates
  :: (TxCertificates build era -> TxCertificates build era)
  -> TxBodyContent build era
  -> TxBodyContent build era
modTxCertificates f txBodyContent = txBodyContent{txCertificates = f (txCertificates txBodyContent)}

setTxUpdateProposal :: TxUpdateProposal era -> TxBodyContent build era -> TxBodyContent build era
setTxUpdateProposal v txBodyContent = txBodyContent{txUpdateProposal = v}

modTxUpdateProposal
  :: (TxUpdateProposal era -> TxUpdateProposal era)
  -> TxBodyContent build era
  -> TxBodyContent build era
modTxUpdateProposal f txBodyContent = txBodyContent{txUpdateProposal = f (txUpdateProposal txBodyContent)}

setTxMintValue :: TxMintValue build era -> TxBodyContent build era -> TxBodyContent build era
setTxMintValue v txBodyContent = txBodyContent{txMintValue = v}

modTxMintValue
  :: (TxMintValue build era -> TxMintValue build era)
  -> TxBodyContent build era
  -> TxBodyContent build era
modTxMintValue f tx = tx{txMintValue = f (txMintValue tx)}

addTxMintValue
  :: IsMaryBasedEra era
  => Map PolicyId (PolicyAssets, BuildTxWith build (ScriptWitness WitCtxMint era))
  -> TxBodyContent build era
  -> TxBodyContent build era
addTxMintValue assets =
  modTxMintValue
    ( \case
        TxMintNone -> TxMintValue maryBasedEra assets
        TxMintValue era t -> TxMintValue era $ Map.unionWith (\(v1, w1) (v2, _w2) -> (v1 <> v2, w1)) assets t -- w1 == w2
    )

-- | Adds the negation of the provided assets and quantities to the txMintValue field of the `TxBodyContent`.
subtractTxMintValue
  :: IsMaryBasedEra era
  => Map PolicyId (PolicyAssets, BuildTxWith build (ScriptWitness WitCtxMint era))
  -> TxBodyContent build era
  -> TxBodyContent build era
subtractTxMintValue assets = addTxMintValue $ first (omap negate) <$> assets

setTxScriptValidity :: TxScriptValidity era -> TxBodyContent build era -> TxBodyContent build era
setTxScriptValidity v txBodyContent = txBodyContent{txScriptValidity = v}

modTxScriptValidity
  :: (TxScriptValidity era -> TxScriptValidity era) -> TxBodyContent build era -> TxBodyContent build era
modTxScriptValidity f txBodyContent = txBodyContent{txScriptValidity = f (txScriptValidity txBodyContent)}

setTxProposalProcedures
  :: Maybe (Featured ConwayEraOnwards era (TxProposalProcedures build era))
  -> TxBodyContent build era
  -> TxBodyContent build era
setTxProposalProcedures v txBodyContent = txBodyContent{txProposalProcedures = v}

setTxVotingProcedures
  :: Maybe (Featured ConwayEraOnwards era (TxVotingProcedures build era))
  -> TxBodyContent build era
  -> TxBodyContent build era
setTxVotingProcedures v txBodyContent = txBodyContent{txVotingProcedures = v}

setTxCurrentTreasuryValue
  :: Maybe (Featured ConwayEraOnwards era (Maybe L.Coin))
  -> TxBodyContent build era
  -> TxBodyContent build era
setTxCurrentTreasuryValue v txBodyContent = txBodyContent{txCurrentTreasuryValue = v}

setTxTreasuryDonation
  :: Maybe (Featured ConwayEraOnwards era L.Coin) -> TxBodyContent build era -> TxBodyContent build era
setTxTreasuryDonation v txBodyContent = txBodyContent{txTreasuryDonation = v}

getTxIdByron :: Byron.ATxAux ByteString -> TxId
getTxIdByron (Byron.ATxAux{Byron.aTaTx = txbody}) =
  TxId
    . fromMaybe impossible
    . Crypto.hashFromBytesShort
    . Byron.abstractHashToShort
    . Byron.hashDecoded
    $ txbody
 where
  impossible =
    error "getTxIdByron: byron and shelley hash sizes do not match"

-- | Calculate the transaction identifier for a 'TxBody'.
getTxId :: TxBody era -> TxId
getTxId (ShelleyTxBody sbe tx _ _ _ _) =
  shelleyBasedEraConstraints sbe $ getTxIdShelley sbe tx

getTxIdShelley
  :: Ledger.EraTxBody (ShelleyLedgerEra era)
  => ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> TxId
getTxIdShelley _ tx =
  TxId
    . Crypto.castHash
    . (\(Ledger.TxId txhash) -> SafeHash.extractHash txhash)
    $ Ledger.txIdTxBody tx

-- ----------------------------------------------------------------------------
-- Constructing transaction bodies
--

data TxBodyError
  = TxBodyPlutusScriptDecodeError CBOR.DecoderError
  | TxBodyEmptyTxIns
  | TxBodyEmptyTxInsCollateral
  | TxBodyEmptyTxOuts
  | TxBodyOutputError !TxOutputError
  | TxBodyMetadataError ![(Word64, TxMetadataRangeError)]
  | TxBodyInIxOverflow !TxIn
  | TxBodyMissingProtocolParams
  | TxBodyProtocolParamsConversionError !ProtocolParametersConversionError
  deriving (Eq, Show)

renderBuildable :: Buildable a => a -> Text
renderBuildable e = LText.toStrict . LText.toLazyText $ Build.build e

instance Error TxBodyError where
  prettyError = \case
    TxBodyPlutusScriptDecodeError err ->
      "Error decoding Plutus script: " <> pretty (renderBuildable err)
    TxBodyEmptyTxIns ->
      "Transaction body has no inputs"
    TxBodyEmptyTxInsCollateral ->
      "Transaction body has no collateral inputs, but uses Plutus scripts"
    TxBodyEmptyTxOuts ->
      "Transaction body has no outputs"
    TxBodyOutputError err -> prettyError err
    TxBodyMetadataError [(k, err)] ->
      "Error in metadata entry " <> pretty k <> ": " <> prettyError err
    TxBodyMetadataError errs ->
      mconcat
        [ "Error in metadata entries: "
        , mconcat $
            List.intersperse
              "; "
              [ pretty k <> ": " <> prettyError err
              | (k, err) <- errs
              ]
        ]
    TxBodyMissingProtocolParams ->
      "Transaction uses Plutus scripts but does not provide the protocol "
        <> "parameters to hash"
    TxBodyInIxOverflow txin ->
      "Transaction input index is too big, "
        <> "acceptable value is up to 2^32-1, "
        <> "in input "
        <> pretty txin
    TxBodyProtocolParamsConversionError ppces ->
      "Errors in protocol parameters conversion: " <> prettyError ppces

createTransactionBody
  :: forall era
   . HasCallStack
  => ShelleyBasedEra era
  -> TxBodyContent BuildTx era
  -> Either TxBodyError (TxBody era)
createTransactionBody sbe bc =
  shelleyBasedEraConstraints sbe $ do
    (sData, mScriptIntegrityHash, scripts) <-
      caseShelleyToMaryOrAlonzoEraOnwards
        ( \eon -> do
            let scripts =
                  catMaybes
                    [ toShelleyScript <$> getScriptWitnessScript scriptwitness
                    | (_, AnyScriptWitness scriptwitness) <-
                        collectTxBodyScriptWitnesses (convert eon) bc
                    ]
            return (TxBodyNoScriptData, SNothing, scripts)
        )
        ( \aeon -> do
            TxScriptWitnessRequirements languages scripts dats redeemers <-
              collectTxBodyScriptWitnessRequirements aeon bc

            let pparams = txProtocolParams bc
                sData = TxBodyScriptData aeon dats redeemers
                mScriptIntegrityHash = getScriptIntegrityHash pparams languages sData
            return
              ( sData
              , mScriptIntegrityHash
              , scripts
              )
        )
        sbe
    let era = toCardanoEra sbe

        apiScriptValidity = txScriptValidity bc
        apiMintValue = txMintValue bc
        apiCollateralTxIns = txInsCollateral bc
        apiReferenceInputs = txInsReference bc
        apiExtraKeyWitnesses = txExtraKeyWits bc
        apiReturnCollateral = txReturnCollateral bc
        apiTotalCollateral = txTotalCollateral bc

        -- Ledger types
        collTxIns = convCollateralTxIns apiCollateralTxIns
        refTxIns = convReferenceInputs apiReferenceInputs
        returnCollateral = convReturnCollateral sbe apiReturnCollateral
        totalCollateral = convTotalCollateral apiTotalCollateral
        certs = convCertificates sbe $ txCertificates bc
        txAuxData = toAuxiliaryData sbe (txMetadata bc) (txAuxScripts bc)

        proposalProcedures = convProposalProcedures $ maybe TxProposalProceduresNone unFeatured (txProposalProcedures bc)
        votingProcedures = convVotingProcedures $ maybe TxVotingProceduresNone unFeatured (txVotingProcedures bc)
        currentTreasuryValue = Ledger.maybeToStrictMaybe $ unFeatured =<< txCurrentTreasuryValue bc
        treasuryDonation = maybe 0 unFeatured $ txTreasuryDonation bc

    setUpdateProposal <- monoidForEraInEonA era $ \w ->
      Endo . (A.updateTxBodyL w .~) <$> convTxUpdateProposal sbe (txUpdateProposal bc)

    setInvalidBefore <- monoidForEraInEonA era $ \w ->
      pure $ Endo $ A.invalidBeforeTxBodyL w .~ convValidityLowerBound (txValidityLowerBound bc)

    setMint <- monoidForEraInEonA era $ \w ->
      pure $ Endo $ A.mintTxBodyL w .~ convMintValue apiMintValue

    setScriptIntegrityHash <- monoidForEraInEonA era $ \w ->
      pure $
        Endo $
          A.scriptIntegrityHashTxBodyL w .~ mScriptIntegrityHash

    setCollateralInputs <- monoidForEraInEonA era $ \w ->
      pure $ Endo $ A.collateralInputsTxBodyL w .~ collTxIns

    setReqSignerHashes <- monoidForEraInEonA era $ \w ->
      pure $ Endo $ A.reqSignerHashesTxBodyL w .~ convExtraKeyWitnesses apiExtraKeyWitnesses

    setReferenceInputs <- monoidForEraInEonA era $ \w ->
      pure $ Endo $ A.referenceInputsTxBodyL w .~ refTxIns

    setCollateralReturn <- monoidForEraInEonA era $ \w ->
      pure $ Endo $ A.collateralReturnTxBodyL w .~ returnCollateral

    setTotalCollateral <- monoidForEraInEonA era $ \w ->
      pure $ Endo $ A.totalCollateralTxBodyL w .~ totalCollateral

    setProposalProcedures <- monoidForEraInEonA era $ \w ->
      pure $ Endo $ A.proposalProceduresTxBodyL w .~ proposalProcedures

    setVotingProcedures <- monoidForEraInEonA era $ \w ->
      pure $ Endo $ A.votingProceduresTxBodyL w .~ votingProcedures

    setCurrentTreasuryValue <- monoidForEraInEonA era $ \w ->
      pure $ Endo $ A.currentTreasuryValueTxBodyL w .~ currentTreasuryValue

    setTreasuryDonation <- monoidForEraInEonA era $ \w ->
      pure $ Endo $ A.treasuryDonationTxBodyL w .~ treasuryDonation

    let ledgerTxBody =
          mkCommonTxBody sbe (txIns bc) (txOuts bc) (txFee bc) (txWithdrawals bc) txAuxData
            & A.certsTxBodyL sbe
              .~ certs
            & A.invalidHereAfterTxBodyL sbe
              .~ convValidityUpperBound sbe (txValidityUpperBound bc)
            & appEndo
              ( mconcat
                  [ setUpdateProposal
                  , setInvalidBefore
                  , setMint
                  , setScriptIntegrityHash
                  , setCollateralInputs
                  , setReqSignerHashes
                  , setReferenceInputs
                  , setCollateralReturn
                  , setTotalCollateral
                  , setProposalProcedures
                  , setVotingProcedures
                  , setCurrentTreasuryValue
                  , setTreasuryDonation
                  ]
              )

    -- TODO: NetworkId for hardware wallets. We don't always want this
    -- & L.networkIdTxBodyL .~ ...

    pure $ ShelleyTxBody sbe (ledgerTxBody ^. A.txBodyL) scripts sData txAuxData apiScriptValidity

getScriptIntegrityHash
  :: ()
  => BuildTxWith BuildTx (Maybe (LedgerProtocolParameters era))
  -> Set Plutus.Language
  -> TxBodyScriptData era
  -> StrictMaybe L.ScriptIntegrityHash
getScriptIntegrityHash apiProtocolParameters languages = \case
  TxBodyNoScriptData -> SNothing
  TxBodyScriptData w datums redeemers ->
    convPParamsToScriptIntegrityHash w apiProtocolParameters redeemers datums languages

validateTxBodyContent
  :: ShelleyBasedEra era
  -> TxBodyContent BuildTx era
  -> Either TxBodyError ()
validateTxBodyContent
  sbe
  txBodContent@TxBodyContent
    { txIns
    , txInsCollateral
    , txOuts
    , txProtocolParams
    , txMetadata
    } =
    let witnesses = collectTxBodyScriptWitnesses sbe txBodContent
        languages =
          fromList
            [ toAlonzoLanguage (AnyPlutusScriptVersion v)
            | (_, AnyScriptWitness (PlutusScriptWitness _ v _ _ _ _)) <- witnesses
            ]
            :: Set Plutus.Language
     in case sbe of
          ShelleyBasedEraShelley -> do
            validateTxIns txIns
            guardShelleyTxInsOverflow (map fst txIns)
            first TxBodyOutputError $
              validateTxOuts sbe txOuts
            validateMetadata txMetadata
          ShelleyBasedEraAllegra -> do
            validateTxIns txIns
            guardShelleyTxInsOverflow (map fst txIns)
            first TxBodyOutputError $
              validateTxOuts sbe txOuts
            validateMetadata txMetadata
          ShelleyBasedEraMary -> do
            validateTxIns txIns
            guardShelleyTxInsOverflow (map fst txIns)
            first TxBodyOutputError $
              validateTxOuts sbe txOuts
            validateMetadata txMetadata
          ShelleyBasedEraAlonzo -> do
            validateTxIns txIns
            guardShelleyTxInsOverflow (map fst txIns)
            first TxBodyOutputError $
              validateTxOuts sbe txOuts
            validateMetadata txMetadata
            validateTxInsCollateral txInsCollateral languages
            validateProtocolParameters txProtocolParams languages
          ShelleyBasedEraBabbage -> do
            validateTxIns txIns
            guardShelleyTxInsOverflow (map fst txIns)
            first TxBodyOutputError $
              validateTxOuts sbe txOuts
            validateMetadata txMetadata
            validateTxInsCollateral txInsCollateral languages
            validateProtocolParameters txProtocolParams languages
          ShelleyBasedEraConway -> do
            validateTxIns txIns
            first TxBodyOutputError $
              validateTxOuts sbe txOuts
            validateMetadata txMetadata
            validateTxInsCollateral txInsCollateral languages
            validateProtocolParameters txProtocolParams languages

validateMetadata :: TxMetadataInEra era -> Either TxBodyError ()
validateMetadata txMetadata =
  case txMetadata of
    TxMetadataNone -> return ()
    TxMetadataInEra _ m -> first TxBodyMetadataError (validateTxMetadata m)

validateProtocolParameters
  :: BuildTxWith BuildTx (Maybe (LedgerProtocolParameters era))
  -> Set Plutus.Language
  -> Either TxBodyError ()
validateProtocolParameters txProtocolParams languages =
  case txProtocolParams of
    BuildTxWith Nothing
      | not (Set.null languages) ->
          Left TxBodyMissingProtocolParams
    _ -> return () -- TODO alonzo: validate protocol params for the Alonzo era.
    --             All the necessary params must be provided.

validateTxIns :: [(TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era))] -> Either TxBodyError ()
validateTxIns txIns =
  sequence_
    [ inputIndexDoesNotExceedMax txIns
    , txBodyContentHasTxIns txIns
    ]

validateTxInsCollateral
  :: TxInsCollateral era -> Set Plutus.Language -> Either TxBodyError ()
validateTxInsCollateral txInsCollateral languages =
  case txInsCollateral of
    TxInsCollateralNone ->
      unless (Set.null languages) (Left TxBodyEmptyTxInsCollateral)
    TxInsCollateral _ collateralTxIns ->
      guardShelleyTxInsOverflow collateralTxIns

inputIndexDoesNotExceedMax :: [(TxIn, a)] -> Either TxBodyError ()
inputIndexDoesNotExceedMax txIns =
  for_ txIns $ \(txin@(TxIn _ (TxIx txix)), _) ->
    guard (fromIntegral txix <= maxShelleyTxInIx) ?! TxBodyInIxOverflow txin

txBodyContentHasTxIns :: TxIns BuildTx era -> Either TxBodyError ()
txBodyContentHasTxIns txIns = guard (not (null txIns)) ?! TxBodyEmptyTxIns

maxShelleyTxInIx :: Word
maxShelleyTxInIx = fromIntegral $ maxBound @Word16

{-# DEPRECATED createAndValidateTransactionBody "Use createTransactionBody instead" #-}
createAndValidateTransactionBody
  :: ()
  => ShelleyBasedEra era
  -> TxBodyContent BuildTx era
  -> Either TxBodyError (TxBody era)
createAndValidateTransactionBody = makeShelleyTransactionBody

{-# DEPRECATED TxBody "Use getTxBodyContent $ getTxBody instead" #-}
pattern TxBody :: TxBodyContent ViewTx era -> TxBody era
pattern TxBody txbodycontent <- (getTxBodyContent -> txbodycontent)

{-# COMPLETE TxBody #-}

getTxBodyContent :: TxBody era -> TxBodyContent ViewTx era
getTxBodyContent = \case
  ShelleyTxBody sbe body _scripts scriptdata mAux scriptValidity ->
    fromLedgerTxBody sbe scriptValidity body scriptdata mAux

fromLedgerTxBody
  :: ShelleyBasedEra era
  -> TxScriptValidity era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> TxBodyScriptData era
  -> Maybe (L.TxAuxData (ShelleyLedgerEra era))
  -> TxBodyContent ViewTx era
fromLedgerTxBody sbe scriptValidity body scriptdata mAux =
  TxBodyContent
    { txIns = fromLedgerTxIns sbe body
    , txInsCollateral = fromLedgerTxInsCollateral sbe body
    , txInsReference = fromLedgerTxInsReference sbe body
    , txOuts = fromLedgerTxOuts sbe body scriptdata
    , txTotalCollateral = fromLedgerTxTotalCollateral sbe body
    , txReturnCollateral = fromLedgerTxReturnCollateral sbe body
    , txFee = fromLedgerTxFee sbe body
    , txValidityLowerBound = fromLedgerTxValidityLowerBound sbe (A.LedgerTxBody body)
    , txValidityUpperBound = fromLedgerTxValidityUpperBound sbe (A.LedgerTxBody body)
    , txWithdrawals = fromLedgerTxWithdrawals sbe body
    , txCertificates = fromLedgerTxCertificates sbe body
    , txUpdateProposal = maybeFromLedgerTxUpdateProposal sbe body
    , txMintValue = fromLedgerTxMintValue sbe body
    , txExtraKeyWits = fromLedgerTxExtraKeyWitnesses sbe body
    , txProtocolParams = ViewTx
    , txMetadata
    , txAuxScripts
    , txScriptValidity = scriptValidity
    , txProposalProcedures = fromLedgerProposalProcedures sbe body
    , txVotingProcedures = fromLedgerVotingProcedures sbe body
    , txCurrentTreasuryValue = fromLedgerCurrentTreasuryValue sbe body
    , txTreasuryDonation = fromLedgerTreasuryDonation sbe body
    }
 where
  (txMetadata, txAuxScripts) = fromLedgerTxAuxiliaryData sbe mAux

fromLedgerProposalProcedures
  :: ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> Maybe (Featured ConwayEraOnwards era (TxProposalProcedures ViewTx era))
fromLedgerProposalProcedures sbe body =
  forShelleyBasedEraInEonMaybe sbe $ \w ->
    conwayEraOnwardsConstraints w $
      Featured w $
        mkTxProposalProcedures
          (fmap (,Nothing) . toList $ body ^. L.proposalProceduresTxBodyL)

fromLedgerVotingProcedures
  :: ()
  => ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> Maybe (Featured ConwayEraOnwards era (TxVotingProcedures ViewTx era))
fromLedgerVotingProcedures sbe body =
  forShelleyBasedEraInEonMaybe sbe $ \w ->
    conwayEraOnwardsConstraints w $
      Featured w $
        TxVotingProcedures
          (body ^. L.votingProceduresTxBodyL)
          ViewTx

fromLedgerCurrentTreasuryValue
  :: ()
  => ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> Maybe (Featured ConwayEraOnwards era (Maybe Coin))
fromLedgerCurrentTreasuryValue sbe body = forEraInEonMaybe (toCardanoEra sbe) $ \ceo ->
  conwayEraOnwardsConstraints ceo $
    Featured ceo . Ledger.strictMaybeToMaybe $
      body ^. L.currentTreasuryValueTxBodyL

fromLedgerTreasuryDonation
  :: ()
  => ShelleyBasedEra era
  -> L.TxBody (ShelleyLedgerEra era)
  -> Maybe (Featured ConwayEraOnwards era Coin)
fromLedgerTreasuryDonation sbe body =
  forShelleyBasedEraInEonMaybe sbe $ \w ->
    conwayEraOnwardsConstraints w $
      Featured w (body ^. L.treasuryDonationTxBodyL)

fromLedgerTxIns
  :: forall era
   . ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> [(TxIn, BuildTxWith ViewTx (Witness WitCtxTxIn era))]
fromLedgerTxIns sbe body =
  [ (fromShelleyTxIn input, ViewTx)
  | input <- toList (inputs_ sbe body)
  ]
 where
  inputs_
    :: ShelleyBasedEra era
    -> Ledger.TxBody (ShelleyLedgerEra era)
    -> Set Ledger.TxIn
  inputs_ ShelleyBasedEraShelley = view L.inputsTxBodyL
  inputs_ ShelleyBasedEraAllegra = view L.inputsTxBodyL
  inputs_ ShelleyBasedEraMary = view L.inputsTxBodyL
  inputs_ ShelleyBasedEraAlonzo = view L.inputsTxBodyL
  inputs_ ShelleyBasedEraBabbage = view L.inputsTxBodyL
  inputs_ ShelleyBasedEraConway = view L.inputsTxBodyL

fromLedgerTxInsCollateral
  :: forall era
   . ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> TxInsCollateral era
fromLedgerTxInsCollateral sbe body =
  caseShelleyToMaryOrAlonzoEraOnwards
    (const TxInsCollateralNone)
    (\w -> TxInsCollateral w $ map fromShelleyTxIn $ toList $ body ^. L.collateralInputsTxBodyL)
    sbe

fromLedgerTxInsReference
  :: ShelleyBasedEra era -> Ledger.TxBody (ShelleyLedgerEra era) -> TxInsReference ViewTx era
fromLedgerTxInsReference sbe txBody =
  caseShelleyToAlonzoOrBabbageEraOnwards
    (const TxInsReferenceNone)
    (\w -> TxInsReference w (map fromShelleyTxIn . toList $ txBody ^. L.referenceInputsTxBodyL) ViewTx)
    sbe

fromLedgerTxTotalCollateral
  :: ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> TxTotalCollateral era
fromLedgerTxTotalCollateral sbe txbody =
  caseShelleyToAlonzoOrBabbageEraOnwards
    (const TxTotalCollateralNone)
    ( \w ->
        case txbody ^. L.totalCollateralTxBodyL of
          SNothing -> TxTotalCollateralNone
          SJust totColl -> TxTotalCollateral w totColl
    )
    sbe

fromLedgerTxReturnCollateral
  :: ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> TxReturnCollateral CtxTx era
fromLedgerTxReturnCollateral sbe txbody =
  caseShelleyToAlonzoOrBabbageEraOnwards
    (const TxReturnCollateralNone)
    ( \w ->
        case txbody ^. L.collateralReturnTxBodyL of
          SNothing -> TxReturnCollateralNone
          SJust collReturnOut -> TxReturnCollateral w $ fromShelleyTxOut sbe collReturnOut
    )
    sbe

fromLedgerTxFee
  :: ShelleyBasedEra era -> Ledger.TxBody (ShelleyLedgerEra era) -> TxFee era
fromLedgerTxFee sbe body =
  shelleyBasedEraConstraints sbe $
    TxFeeExplicit sbe $
      body ^. L.feeTxBodyL

fromLedgerTxValidityLowerBound
  :: ShelleyBasedEra era
  -> A.LedgerTxBody era
  -> TxValidityLowerBound era
fromLedgerTxValidityLowerBound sbe body =
  caseShelleyEraOnlyOrAllegraEraOnwards
    (const TxValidityNoLowerBound)
    ( \w ->
        let mInvalidBefore = body ^. A.invalidBeforeTxBodyL w
         in case mInvalidBefore of
              Nothing -> TxValidityNoLowerBound
              Just s -> TxValidityLowerBound w s
    )
    sbe

fromLedgerTxValidityUpperBound
  :: ShelleyBasedEra era
  -> A.LedgerTxBody era
  -> TxValidityUpperBound era
fromLedgerTxValidityUpperBound sbe body =
  TxValidityUpperBound sbe $ body ^. A.invalidHereAfterTxBodyL sbe

fromLedgerAuxiliaryData
  :: ShelleyBasedEra era
  -> L.TxAuxData (ShelleyLedgerEra era)
  -> (Map Word64 TxMetadataValue, [ScriptInEra era])
fromLedgerAuxiliaryData ShelleyBasedEraShelley (L.ShelleyTxAuxData metadata) =
  (fromShelleyMetadata metadata, [])
fromLedgerAuxiliaryData ShelleyBasedEraAllegra (L.AllegraTxAuxData ms ss) =
  ( fromShelleyMetadata ms
  , fromShelleyBasedScript ShelleyBasedEraAllegra <$> toList ss
  )
fromLedgerAuxiliaryData ShelleyBasedEraMary (L.AllegraTxAuxData ms ss) =
  ( fromShelleyMetadata ms
  , fromShelleyBasedScript ShelleyBasedEraMary <$> toList ss
  )
fromLedgerAuxiliaryData ShelleyBasedEraAlonzo txAuxData =
  ( fromShelleyMetadata (L.atadMetadata txAuxData)
  , fromShelleyBasedScript ShelleyBasedEraAlonzo
      <$> toList (L.getAlonzoTxAuxDataScripts txAuxData)
  )
fromLedgerAuxiliaryData ShelleyBasedEraBabbage txAuxData =
  ( fromShelleyMetadata (L.atadMetadata txAuxData)
  , fromShelleyBasedScript ShelleyBasedEraBabbage
      <$> toList (L.getAlonzoTxAuxDataScripts txAuxData)
  )
fromLedgerAuxiliaryData ShelleyBasedEraConway txAuxData =
  ( fromShelleyMetadata (L.atadMetadata txAuxData)
  , fromShelleyBasedScript ShelleyBasedEraConway
      <$> toList (L.getAlonzoTxAuxDataScripts txAuxData)
  )

fromLedgerTxAuxiliaryData
  :: ShelleyBasedEra era
  -> Maybe (L.TxAuxData (ShelleyLedgerEra era))
  -> (TxMetadataInEra era, TxAuxScripts era)
fromLedgerTxAuxiliaryData _ Nothing = (TxMetadataNone, TxAuxScriptsNone)
fromLedgerTxAuxiliaryData sbe (Just auxData) =
  (metadata, auxdata)
 where
  metadata = if null ms then TxMetadataNone else TxMetadataInEra sbe $ TxMetadata ms

  auxdata =
    caseShelleyEraOnlyOrAllegraEraOnwards
      (const TxAuxScriptsNone)
      ( \w ->
          case ss of
            [] -> TxAuxScriptsNone
            _ -> TxAuxScripts w ss
      )
      sbe

  (ms, ss) = fromLedgerAuxiliaryData sbe auxData

fromLedgerTxExtraKeyWitnesses
  :: ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> TxExtraKeyWitnesses era
fromLedgerTxExtraKeyWitnesses sbe body =
  caseShelleyToMaryOrAlonzoEraOnwards
    (const TxExtraKeyWitnessesNone)
    ( \w ->
        let keyhashes = body ^. L.reqSignerHashesTxBodyL
         in if Set.null keyhashes
              then TxExtraKeyWitnessesNone
              else
                TxExtraKeyWitnesses
                  w
                  [ PaymentKeyHash (Shelley.coerceKeyRole keyhash)
                  | keyhash <- toList $ body ^. L.reqSignerHashesTxBodyL
                  ]
    )
    sbe

fromLedgerTxWithdrawals
  :: ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> TxWithdrawals ViewTx era
fromLedgerTxWithdrawals sbe body =
  shelleyBasedEraConstraints sbe $
    let withdrawals = body ^. L.withdrawalsTxBodyL
     in if null (L.unWithdrawals withdrawals)
          then TxWithdrawalsNone
          else TxWithdrawals sbe $ fromShelleyWithdrawal withdrawals

fromLedgerTxCertificates
  :: ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> TxCertificates ViewTx era
fromLedgerTxCertificates sbe body =
  shelleyBasedEraConstraints sbe $
    let certificates = body ^. L.certsTxBodyL
     in if null certificates
          then TxCertificatesNone
          else
            TxCertificates sbe . fromList $ map ((,ViewTx) . fromShelleyCertificate sbe) $ toList certificates

maybeFromLedgerTxUpdateProposal
  :: ()
  => ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> TxUpdateProposal era
maybeFromLedgerTxUpdateProposal sbe body =
  caseShelleyToBabbageOrConwayEraOnwards
    ( \w ->
        case body ^. L.updateTxBodyL of
          SNothing -> TxUpdateProposalNone
          SJust p -> TxUpdateProposal w (fromLedgerUpdate sbe p)
    )
    (const TxUpdateProposalNone)
    sbe

fromLedgerTxMintValue
  :: ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> TxMintValue ViewTx era
fromLedgerTxMintValue sbe body = forEraInEon (toCardanoEra sbe) TxMintNone $ \w ->
  maryEraOnwardsConstraints w $ do
    let multiAsset = body ^. L.mintTxBodyL
    if L.isZero $ MaryValue (Ledger.Coin 0) multiAsset
      then TxMintNone
      else
        TxMintValue w $ (,ViewTx) <$> multiAssetToPolicyAssets multiAsset

makeByronTransactionBody
  :: ()
  => TxIns BuildTx ByronEra
  -> [TxOut CtxTx ByronEra]
  -> Either TxBodyError (Annotated Byron.Tx ByteString)
makeByronTransactionBody txIns txOuts = do
  ins' <- NonEmpty.nonEmpty (map fst txIns) ?! TxBodyEmptyTxIns
  for_ ins' $ \txin@(TxIn _ (TxIx txix)) ->
    guard (fromIntegral txix <= maxByronTxInIx) ?! TxBodyInIxOverflow txin
  let ins'' = fmap toByronTxIn ins'

  outs' <- NonEmpty.nonEmpty txOuts ?! TxBodyEmptyTxOuts
  outs'' <-
    traverse
      (first TxBodyOutputError . toByronTxOut)
      outs'
  return $
    CBOR.reAnnotate CBOR.byronProtVer $
      Annotated
        (Byron.UnsafeTx ins'' outs'' (Byron.mkAttributes ()))
        ()
 where
  maxByronTxInIx :: Word
  maxByronTxInIx = fromIntegral (maxBound :: Word32)

convTxIns :: TxIns BuildTx era -> Set L.TxIn
convTxIns txIns = fromList (map (toShelleyTxIn . fst) txIns)

convCollateralTxIns :: TxInsCollateral era -> Set Ledger.TxIn
convCollateralTxIns txInsCollateral =
  case txInsCollateral of
    TxInsCollateralNone -> Set.empty
    TxInsCollateral _ txins -> fromList (map toShelleyTxIn txins)

convReturnCollateral
  :: ShelleyBasedEra era
  -> TxReturnCollateral ctx era
  -> StrictMaybe (Ledger.TxOut (ShelleyLedgerEra era))
convReturnCollateral sbe txReturnCollateral =
  case txReturnCollateral of
    TxReturnCollateralNone -> SNothing
    TxReturnCollateral _ colTxOut -> SJust $ toShelleyTxOutAny sbe colTxOut

convTotalCollateral :: TxTotalCollateral era -> StrictMaybe Ledger.Coin
convTotalCollateral txTotalCollateral =
  case txTotalCollateral of
    TxTotalCollateralNone -> SNothing
    TxTotalCollateral _ totCollLovelace -> SJust totCollLovelace

convCertificates
  :: ShelleyBasedEra era
  -> TxCertificates build era
  -> Seq.StrictSeq (Shelley.TxCert (ShelleyLedgerEra era))
convCertificates _ = \case
  TxCertificatesNone -> Seq.empty
  TxCertificates _ cs -> fromList . map (toShelleyCertificate . fst) $ toList cs

convWithdrawals :: TxWithdrawals build era -> L.Withdrawals
convWithdrawals txWithdrawals =
  case txWithdrawals of
    TxWithdrawalsNone -> L.Withdrawals Map.empty
    TxWithdrawals _ ws -> toShelleyWithdrawal ws

convTransactionFee :: ShelleyBasedEra era -> TxFee era -> Ledger.Coin
convTransactionFee _ (TxFeeExplicit _ fee) = fee

convValidityLowerBound
  :: ()
  => TxValidityLowerBound era
  -> Maybe SlotNo
convValidityLowerBound = \case
  TxValidityNoLowerBound -> Nothing
  TxValidityLowerBound _ s -> Just s

convValidityUpperBound
  :: ()
  => ShelleyBasedEra era
  -> TxValidityUpperBound era
  -> Maybe SlotNo
convValidityUpperBound _ = \case
  TxValidityUpperBound _ ms -> ms

-- | Convert transaction update proposal into ledger update proposal
convTxUpdateProposal
  :: ()
  => ShelleyBasedEra era
  -> TxUpdateProposal era
  -> Either TxBodyError (StrictMaybe (Ledger.Update (ShelleyLedgerEra era)))
  -- ^ 'Left' when there's protocol params conversion error, 'Right' otherwise, 'Right SNothing' means that
  -- there's no update proposal
convTxUpdateProposal sbe = \case
  TxUpdateProposalNone -> Right SNothing
  TxUpdateProposal _ p -> bimap TxBodyProtocolParamsConversionError pure $ toLedgerUpdate sbe p

convMintValue :: TxMintValue build era -> MultiAsset
convMintValue txMintValue = do
  let L.MaryValue _coin multiAsset = toMaryValue $ txMintValueToValue txMintValue
  multiAsset

convExtraKeyWitnesses
  :: TxExtraKeyWitnesses era -> Set (Shelley.KeyHash Shelley.Witness)
convExtraKeyWitnesses txExtraKeyWits =
  case txExtraKeyWits of
    TxExtraKeyWitnessesNone -> Set.empty
    TxExtraKeyWitnesses _ khs ->
      fromList
        [ Shelley.asWitness kh
        | PaymentKeyHash kh <- khs
        ]

convScripts
  :: ShelleyLedgerEra era ~ ledgerera
  => [(ScriptWitnessIndex, AnyScriptWitness era)]
  -> [Ledger.Script ledgerera]
convScripts scriptWitnesses =
  catMaybes
    [ toShelleyScript <$> getScriptWitnessScript scriptwitness
    | (_, AnyScriptWitness scriptwitness) <- scriptWitnesses
    ]

-- ScriptData collectively refers to datums and/or redeemers
convScriptData
  :: ()
  => ShelleyBasedEra era
  -> [TxOut CtxTx era]
  -> [(ScriptWitnessIndex, AnyScriptWitness era)]
  -> TxBodyScriptData era
convScriptData sbe txOuts scriptWitnesses =
  caseShelleyToMaryOrAlonzoEraOnwards
    (const TxBodyNoScriptData)
    ( \w ->
        let redeemers =
              Alonzo.Redeemers $
                fromList
                  [ (i, (toAlonzoData d, toAlonzoExUnits e))
                  | ( idx
                      , AnyScriptWitness
                          (PlutusScriptWitness _ _ _ _ d e)
                      ) <-
                      scriptWitnesses
                  , Just i <- [fromScriptWitnessIndex w idx]
                  ]

            datums =
              Alonzo.TxDats $
                fromList
                  [ (L.hashData d', d')
                  | d <- scriptdata
                  , let d' = toAlonzoData d
                  ]

            scriptdata :: [HashableScriptData]
            scriptdata =
              [d | TxOut _ _ (TxOutSupplementalDatum _ d) _ <- txOuts]
                ++ [ d
                   | ( _
                       , AnyScriptWitness
                           ( PlutusScriptWitness
                               _
                               _
                               _
                               (ScriptDatumForTxIn (Just d))
                               _
                               _
                             )
                       ) <-
                       scriptWitnesses
                   ]
         in TxBodyScriptData w datums redeemers
    )
    sbe

convPParamsToScriptIntegrityHash
  :: ()
  => AlonzoEraOnwards era
  -> BuildTxWith BuildTx (Maybe (LedgerProtocolParameters era))
  -> Alonzo.Redeemers (ShelleyLedgerEra era)
  -> Alonzo.TxDats (ShelleyLedgerEra era)
  -> Set Plutus.Language
  -> StrictMaybe L.ScriptIntegrityHash
convPParamsToScriptIntegrityHash w (BuildTxWith mTxProtocolParams) redeemers datums languages =
  alonzoEraOnwardsConstraints w $
    case mTxProtocolParams of
      Nothing -> SNothing
      Just (LedgerProtocolParameters pp) ->
        Alonzo.hashScriptIntegrity (Set.map (L.getLanguageView pp) languages) redeemers datums

convLanguages :: [(ScriptWitnessIndex, AnyScriptWitness era)] -> Set Plutus.Language
convLanguages witnesses =
  fromList
    [ toAlonzoLanguage (AnyPlutusScriptVersion v)
    | (_, AnyScriptWitness (PlutusScriptWitness _ v _ _ _ _)) <- witnesses
    ]

convReferenceInputs :: TxInsReference build era -> Set Ledger.TxIn
convReferenceInputs txInsReference =
  case txInsReference of
    TxInsReferenceNone -> mempty
    TxInsReference _ refTxins _ -> fromList $ map toShelleyTxIn refTxins

-- | Returns an OSet of proposals from 'TxProposalProcedures'.
convProposalProcedures
  :: TxProposalProcedures build era -> OSet (L.ProposalProcedure (ShelleyLedgerEra era))
convProposalProcedures TxProposalProceduresNone = OSet.empty
convProposalProcedures (TxProposalProcedures proposals) = fromList $ fst <$> toList proposals

convVotingProcedures :: TxVotingProcedures build era -> L.VotingProcedures (ShelleyLedgerEra era)
convVotingProcedures txVotingProcedures =
  case txVotingProcedures of
    TxVotingProceduresNone -> L.VotingProcedures Map.empty
    TxVotingProcedures vps _ -> vps

guardShelleyTxInsOverflow :: [TxIn] -> Either TxBodyError ()
guardShelleyTxInsOverflow txIns = do
  for_ txIns $ \txin@(TxIn _ (TxIx txix)) ->
    guard (txix <= maxShelleyTxInIx) ?! TxBodyInIxOverflow txin

-- | A helper function that constructs a TxBody with all of the fields that are common for
-- all eras
mkCommonTxBody
  :: ()
  => HasCallStack
  => ShelleyBasedEra era
  -> TxIns BuildTx era
  -> [TxOut ctx era]
  -> TxFee era
  -> TxWithdrawals build era
  -> Maybe (L.TxAuxData (ShelleyLedgerEra era))
  -> A.LedgerTxBody era
mkCommonTxBody sbe txIns txOuts txFee txWithdrawals txAuxData =
  shelleyBasedEraConstraints sbe $
    A.LedgerTxBody $
      L.mkBasicTxBody
        & L.inputsTxBodyL
          .~ convTxIns txIns
        & L.outputsTxBodyL
          .~ convTxOuts sbe txOuts
        & L.feeTxBodyL
          .~ convTransactionFee sbe txFee
        & L.withdrawalsTxBodyL
          .~ convWithdrawals txWithdrawals
        & L.auxDataHashTxBodyL
          .~ maybe SNothing (SJust . Ledger.hashTxAuxData) txAuxData

{-# DEPRECATED makeShelleyTransactionBody "Use 'createTransactionBody' instead." #-}
makeShelleyTransactionBody
  :: forall era
   . ()
  => ShelleyBasedEra era
  -> TxBodyContent BuildTx era
  -> Either TxBodyError (TxBody era)
makeShelleyTransactionBody
  sbe@ShelleyBasedEraShelley
  txbodycontent@TxBodyContent
    { txIns
    , txOuts
    , txFee
    , txValidityUpperBound
    , txMetadata
    , txWithdrawals
    , txCertificates
    , txUpdateProposal
    } = do
    let s2b = ShelleyToBabbageEraShelley
    validateTxBodyContent sbe txbodycontent
    update <- convTxUpdateProposal sbe txUpdateProposal
    let txbody =
          ( mkCommonTxBody sbe txIns txOuts txFee txWithdrawals txAuxData
              & A.certsTxBodyL sbe
                .~ convCertificates sbe txCertificates
              & A.updateTxBodyL s2b
                .~ update
              & A.invalidHereAfterTxBodyL sbe
                .~ convValidityUpperBound sbe txValidityUpperBound
          )
            ^. A.txBodyL
    return $
      ShelleyTxBody
        sbe
        txbody
        scripts_
        TxBodyNoScriptData
        txAuxData
        TxScriptValidityNone
   where
    scripts_ :: [Ledger.Script E.ShelleyEra]
    scripts_ =
      catMaybes
        [ toShelleyScript <$> getScriptWitnessScript scriptwitness
        | (_, AnyScriptWitness scriptwitness) <-
            collectTxBodyScriptWitnesses sbe txbodycontent
        ]

    txAuxData :: Maybe (L.TxAuxData E.ShelleyEra)
    txAuxData = toAuxiliaryData sbe txMetadata TxAuxScriptsNone
makeShelleyTransactionBody
  sbe@ShelleyBasedEraAllegra
  txbodycontent@TxBodyContent
    { txIns
    , txOuts
    , txFee
    , txValidityLowerBound
    , txValidityUpperBound
    , txMetadata
    , txAuxScripts
    , txWithdrawals
    , txCertificates
    , txUpdateProposal
    } = do
    let aOn = AllegraEraOnwardsAllegra
    let s2b = ShelleyToBabbageEraAllegra
    validateTxBodyContent sbe txbodycontent
    update <- convTxUpdateProposal sbe txUpdateProposal
    let txbody =
          ( mkCommonTxBody sbe txIns txOuts txFee txWithdrawals txAuxData
              & A.certsTxBodyL sbe
                .~ convCertificates sbe txCertificates
              & A.invalidBeforeTxBodyL aOn
                .~ convValidityLowerBound txValidityLowerBound
              & A.invalidHereAfterTxBodyL sbe
                .~ convValidityUpperBound sbe txValidityUpperBound
              & A.updateTxBodyL s2b
                .~ update
          )
            ^. A.txBodyL
    return $
      ShelleyTxBody
        sbe
        txbody
        scripts_
        TxBodyNoScriptData
        txAuxData
        TxScriptValidityNone
   where
    scripts_ :: [Ledger.Script E.AllegraEra]
    scripts_ =
      catMaybes
        [ toShelleyScript <$> getScriptWitnessScript scriptwitness
        | (_, AnyScriptWitness scriptwitness) <-
            collectTxBodyScriptWitnesses sbe txbodycontent
        ]

    txAuxData :: Maybe (L.TxAuxData E.AllegraEra)
    txAuxData = toAuxiliaryData sbe txMetadata txAuxScripts
makeShelleyTransactionBody
  sbe@ShelleyBasedEraMary
  txbodycontent@TxBodyContent
    { txIns
    , txOuts
    , txFee
    , txValidityLowerBound
    , txValidityUpperBound
    , txMetadata
    , txAuxScripts
    , txWithdrawals
    , txCertificates
    , txUpdateProposal
    , txMintValue
    } = do
    let aOn = AllegraEraOnwardsMary
    let s2b = ShelleyToBabbageEraMary
    let mOn = MaryEraOnwardsMary
    validateTxBodyContent sbe txbodycontent
    update <- convTxUpdateProposal sbe txUpdateProposal
    let txbody =
          ( mkCommonTxBody sbe txIns txOuts txFee txWithdrawals txAuxData
              & A.certsTxBodyL sbe
                .~ convCertificates sbe txCertificates
              & A.invalidBeforeTxBodyL aOn
                .~ convValidityLowerBound txValidityLowerBound
              & A.invalidHereAfterTxBodyL sbe
                .~ convValidityUpperBound sbe txValidityUpperBound
              & A.updateTxBodyL s2b
                .~ update
              & A.mintTxBodyL mOn
                .~ convMintValue txMintValue
          )
            ^. A.txBodyL
    return $
      ShelleyTxBody
        sbe
        txbody
        scripts
        TxBodyNoScriptData
        txAuxData
        TxScriptValidityNone
   where
    scripts :: [Ledger.Script E.MaryEra]
    scripts =
      List.nub $
        catMaybes
          [ toShelleyScript <$> getScriptWitnessScript scriptwitness
          | (_, AnyScriptWitness scriptwitness) <-
              collectTxBodyScriptWitnesses sbe txbodycontent
          ]

    txAuxData :: Maybe (L.TxAuxData E.MaryEra)
    txAuxData = toAuxiliaryData sbe txMetadata txAuxScripts
makeShelleyTransactionBody
  sbe@ShelleyBasedEraAlonzo
  txbodycontent@TxBodyContent
    { txIns
    , txInsCollateral
    , txOuts
    , txFee
    , txValidityLowerBound
    , txValidityUpperBound
    , txMetadata
    , txAuxScripts
    , txExtraKeyWits
    , txProtocolParams
    , txWithdrawals
    , txCertificates
    , txUpdateProposal
    , txMintValue
    , txScriptValidity
    } = do
    let aOn = AllegraEraOnwardsAlonzo
    let s2b = ShelleyToBabbageEraAlonzo
    let mOn = MaryEraOnwardsAlonzo
    validateTxBodyContent sbe txbodycontent
    update <- convTxUpdateProposal sbe txUpdateProposal
    let scriptIntegrityHash =
          convPParamsToScriptIntegrityHash AlonzoEraOnwardsAlonzo txProtocolParams redeemers datums languages
    let txbody =
          ( mkCommonTxBody sbe txIns txOuts txFee txWithdrawals txAuxData
              & A.collateralInputsTxBodyL azOn
                .~ convCollateralTxIns txInsCollateral
              & A.certsTxBodyL sbe
                .~ convCertificates sbe txCertificates
              & A.invalidBeforeTxBodyL aOn
                .~ convValidityLowerBound txValidityLowerBound
              & A.invalidHereAfterTxBodyL sbe
                .~ convValidityUpperBound sbe txValidityUpperBound
              & A.updateTxBodyL s2b
                .~ update
              & A.reqSignerHashesTxBodyL azOn
                .~ convExtraKeyWitnesses txExtraKeyWits
              & A.mintTxBodyL mOn
                .~ convMintValue txMintValue
              & A.scriptIntegrityHashTxBodyL azOn
                .~ scriptIntegrityHash
                -- TODO Alonzo: support optional network id in TxBodyContent
                -- & L.networkIdTxBodyL .~ SNothing
          )
            ^. A.txBodyL
    return $
      ShelleyTxBody
        sbe
        txbody
        scripts
        (TxBodyScriptData AlonzoEraOnwardsAlonzo datums redeemers)
        txAuxData
        txScriptValidity
   where
    azOn = AlonzoEraOnwardsAlonzo

    witnesses :: [(ScriptWitnessIndex, AnyScriptWitness AlonzoEra)]
    witnesses = collectTxBodyScriptWitnesses sbe txbodycontent

    scripts :: [Ledger.Script E.AlonzoEra]
    scripts =
      List.nub $
        catMaybes
          [ toShelleyScript <$> getScriptWitnessScript scriptwitness
          | (_, AnyScriptWitness scriptwitness) <- witnesses
          ]

    datums :: Alonzo.TxDats E.AlonzoEra
    datums =
      Alonzo.TxDats $
        fromList
          [ (L.hashData d, d)
          | d <- toAlonzoData <$> scriptdata
          ]

    scriptdata :: [HashableScriptData]
    scriptdata =
      [d | TxOut _ _ (TxOutSupplementalDatum _ d) _ <- txOuts]
        ++ [ d
           | ( _
               , AnyScriptWitness
                   ( PlutusScriptWitness
                       _
                       _
                       _
                       (ScriptDatumForTxIn (Just d))
                       _
                       _
                     )
               ) <-
               witnesses
           ]

    redeemers :: Alonzo.Redeemers E.AlonzoEra
    redeemers =
      Alonzo.Redeemers $
        fromList
          [ (i, (toAlonzoData d, toAlonzoExUnits e))
          | ( idx
              , AnyScriptWitness
                  (PlutusScriptWitness _ _ _ _ d e)
              ) <-
              witnesses
          , Just i <- [fromScriptWitnessIndex azOn idx]
          ]

    languages :: Set Plutus.Language
    languages =
      fromList
        [ toAlonzoLanguage (AnyPlutusScriptVersion v)
        | (_, AnyScriptWitness (PlutusScriptWitness _ v _ _ _ _)) <- witnesses
        ]

    txAuxData :: Maybe (L.TxAuxData E.AlonzoEra)
    txAuxData = toAuxiliaryData sbe txMetadata txAuxScripts
makeShelleyTransactionBody
  sbe@ShelleyBasedEraBabbage
  txbodycontent@TxBodyContent
    { txIns
    , txInsCollateral
    , txInsReference
    , txReturnCollateral
    , txTotalCollateral
    , txOuts
    , txFee
    , txValidityLowerBound
    , txValidityUpperBound
    , txMetadata
    , txAuxScripts
    , txExtraKeyWits
    , txProtocolParams
    , txWithdrawals
    , txCertificates
    , txUpdateProposal
    , txMintValue
    , txScriptValidity
    } = do
    let aOn = AllegraEraOnwardsBabbage
    let mOn = MaryEraOnwardsBabbage
    let bOn = BabbageEraOnwardsBabbage
    let s2b = ShelleyToBabbageEraBabbage
    validateTxBodyContent sbe txbodycontent
    update <- convTxUpdateProposal sbe txUpdateProposal
    let scriptIntegrityHash =
          convPParamsToScriptIntegrityHash AlonzoEraOnwardsBabbage txProtocolParams redeemers datums languages
    let txbody =
          ( mkCommonTxBody sbe txIns txOuts txFee txWithdrawals txAuxData
              & A.collateralInputsTxBodyL azOn
                .~ convCollateralTxIns txInsCollateral
              & A.referenceInputsTxBodyL bOn
                .~ convReferenceInputs txInsReference
              & A.collateralReturnTxBodyL bOn
                .~ convReturnCollateral sbe txReturnCollateral
              & A.totalCollateralTxBodyL bOn
                .~ convTotalCollateral txTotalCollateral
              & A.certsTxBodyL sbe
                .~ convCertificates sbe txCertificates
              & A.invalidBeforeTxBodyL aOn
                .~ convValidityLowerBound txValidityLowerBound
              & A.invalidHereAfterTxBodyL sbe
                .~ convValidityUpperBound sbe txValidityUpperBound
              & A.updateTxBodyL s2b
                .~ update
              & A.reqSignerHashesTxBodyL azOn
                .~ convExtraKeyWitnesses txExtraKeyWits
              & A.mintTxBodyL mOn
                .~ convMintValue txMintValue
              & A.scriptIntegrityHashTxBodyL azOn
                .~ scriptIntegrityHash
                -- TODO Babbage: support optional network id in TxBodyContent
                -- & L.networkIdTxBodyL .~ SNothing
          )
            ^. A.txBodyL
    return $
      ShelleyTxBody
        sbe
        txbody
        scripts
        ( TxBodyScriptData
            AlonzoEraOnwardsBabbage
            datums
            redeemers
        )
        txAuxData
        txScriptValidity
   where
    azOn = AlonzoEraOnwardsBabbage

    witnesses :: [(ScriptWitnessIndex, AnyScriptWitness BabbageEra)]
    witnesses = collectTxBodyScriptWitnesses sbe txbodycontent

    scripts :: [Ledger.Script E.BabbageEra]
    scripts =
      List.nub $
        catMaybes
          [ toShelleyScript <$> getScriptWitnessScript scriptwitness
          | (_, AnyScriptWitness scriptwitness) <- witnesses
          ]

    -- Note these do not include inline datums!
    datums :: Alonzo.TxDats E.BabbageEra
    datums =
      Alonzo.TxDats $
        fromList
          [ (L.hashData d', d')
          | d <- scriptdata
          , let d' = toAlonzoData d
          ]

    scriptdata :: [HashableScriptData]
    scriptdata =
      [d | TxOut _ _ (TxOutSupplementalDatum _ d) _ <- txOuts]
        ++ [ d
           | ( _
               , AnyScriptWitness
                   ( PlutusScriptWitness
                       _
                       _
                       _
                       (ScriptDatumForTxIn (Just d))
                       _
                       _
                     )
               ) <-
               witnesses
           ]

    redeemers :: Alonzo.Redeemers E.BabbageEra
    redeemers =
      Alonzo.Redeemers $
        fromList
          [ (i, (toAlonzoData d, toAlonzoExUnits e))
          | ( idx
              , AnyScriptWitness
                  (PlutusScriptWitness _ _ _ _ d e)
              ) <-
              witnesses
          , Just i <- [fromScriptWitnessIndex azOn idx]
          ]

    languages :: Set Plutus.Language
    languages =
      fromList $
        catMaybes
          [ getScriptLanguage sw
          | (_, AnyScriptWitness sw) <- witnesses
          ]

    getScriptLanguage :: ScriptWitness witctx era -> Maybe Plutus.Language
    getScriptLanguage (PlutusScriptWitness _ v _ _ _ _) =
      Just $ toAlonzoLanguage (AnyPlutusScriptVersion v)
    getScriptLanguage SimpleScriptWitness{} = Nothing

    txAuxData :: Maybe (L.TxAuxData E.BabbageEra)
    txAuxData = toAuxiliaryData sbe txMetadata txAuxScripts
makeShelleyTransactionBody
  sbe@ShelleyBasedEraConway
  txbodycontent@TxBodyContent
    { txIns
    , txInsCollateral
    , txInsReference
    , txReturnCollateral
    , txTotalCollateral
    , txOuts
    , txFee
    , txValidityLowerBound
    , txValidityUpperBound
    , txMetadata
    , txAuxScripts
    , txExtraKeyWits
    , txProtocolParams
    , txWithdrawals
    , txCertificates
    , txMintValue
    , txScriptValidity
    , txProposalProcedures
    , txVotingProcedures
    , txCurrentTreasuryValue
    , txTreasuryDonation
    } = do
    let aOn = AllegraEraOnwardsConway
    let cOn = ConwayEraOnwardsConway
    let mOn = MaryEraOnwardsConway
    let bOn = BabbageEraOnwardsConway
    validateTxBodyContent sbe txbodycontent
    let scriptIntegrityHash =
          convPParamsToScriptIntegrityHash AlonzoEraOnwardsConway txProtocolParams redeemers datums languages
    let txbody =
          ( mkCommonTxBody sbe txIns txOuts txFee txWithdrawals txAuxData
              & A.collateralInputsTxBodyL azOn
                .~ case txInsCollateral of
                  TxInsCollateralNone -> Set.empty
                  TxInsCollateral _ txins -> fromList (map toShelleyTxIn txins)
              & A.referenceInputsTxBodyL bOn
                .~ convReferenceInputs txInsReference
              & A.collateralReturnTxBodyL bOn
                .~ convReturnCollateral sbe txReturnCollateral
              & A.totalCollateralTxBodyL bOn
                .~ convTotalCollateral txTotalCollateral
              & A.certsTxBodyL sbe
                .~ convCertificates sbe txCertificates
              & A.invalidBeforeTxBodyL aOn
                .~ convValidityLowerBound txValidityLowerBound
              & A.invalidHereAfterTxBodyL sbe
                .~ convValidityUpperBound sbe txValidityUpperBound
              & A.reqSignerHashesTxBodyL azOn
                .~ convExtraKeyWitnesses txExtraKeyWits
              & A.mintTxBodyL mOn
                .~ convMintValue txMintValue
              & A.scriptIntegrityHashTxBodyL azOn
                .~ scriptIntegrityHash
              & A.votingProceduresTxBodyL cOn
                .~ convVotingProcedures (maybe TxVotingProceduresNone unFeatured txVotingProcedures)
              & A.proposalProceduresTxBodyL cOn
                .~ convProposalProcedures (maybe TxProposalProceduresNone unFeatured txProposalProcedures)
              & A.currentTreasuryValueTxBodyL cOn
                .~ Ledger.maybeToStrictMaybe (unFeatured =<< txCurrentTreasuryValue)
              & A.treasuryDonationTxBodyL cOn
                .~ maybe (L.Coin 0) unFeatured txTreasuryDonation
                -- TODO Conway: support optional network id in TxBodyContent
                -- & L.networkIdTxBodyL .~ SNothing
          )
            ^. A.txBodyL
    return $
      ShelleyTxBody
        sbe
        txbody
        scripts
        ( TxBodyScriptData
            AlonzoEraOnwardsConway
            datums
            redeemers
        )
        txAuxData
        txScriptValidity
   where
    azOn = AlonzoEraOnwardsConway

    witnesses :: [(ScriptWitnessIndex, AnyScriptWitness ConwayEra)]
    witnesses = collectTxBodyScriptWitnesses sbe txbodycontent

    scripts :: [Ledger.Script E.ConwayEra]
    scripts =
      catMaybes
        [ toShelleyScript <$> getScriptWitnessScript scriptwitness
        | (_, AnyScriptWitness scriptwitness) <- witnesses
        ]

    -- Note these do not include inline datums!
    datums :: Alonzo.TxDats E.ConwayEra
    datums =
      Alonzo.TxDats $
        fromList
          [ (L.hashData d, d)
          | d <- toAlonzoData <$> scriptdata
          ]

    scriptdata :: [HashableScriptData]
    scriptdata =
      [d | TxOut _ _ (TxOutSupplementalDatum _ d) _ <- txOuts]
        <> [ d
           | ( _
               , AnyScriptWitness
                   ( PlutusScriptWitness
                       _
                       _
                       _
                       (ScriptDatumForTxIn (Just d))
                       _
                       _
                     )
               ) <-
               witnesses
           ]

    redeemers :: Alonzo.Redeemers E.ConwayEra
    redeemers =
      Alonzo.Redeemers $
        fromList
          [ (i, (toAlonzoData d, toAlonzoExUnits e))
          | ( idx
              , AnyScriptWitness
                  (PlutusScriptWitness _ _ _ _ d e)
              ) <-
              witnesses
          , Just i <- [fromScriptWitnessIndex azOn idx]
          ]

    languages :: Set Plutus.Language
    languages =
      fromList $
        catMaybes
          [ getScriptLanguage sw
          | (_, AnyScriptWitness sw) <- witnesses
          ]

    getScriptLanguage :: ScriptWitness witctx era -> Maybe Plutus.Language
    getScriptLanguage (PlutusScriptWitness _ v _ _ _ _) =
      Just $ toAlonzoLanguage (AnyPlutusScriptVersion v)
    getScriptLanguage SimpleScriptWitness{} = Nothing

    txAuxData :: Maybe (L.TxAuxData E.ConwayEra)
    txAuxData = toAuxiliaryData sbe txMetadata txAuxScripts

-- ----------------------------------------------------------------------------
-- Script witnesses within the tx body
--

-- | A 'ScriptWitness' in any 'WitCtx'. This lets us handle heterogeneous
-- collections of script witnesses from multiple contexts.
data AnyScriptWitness era where
  AnyScriptWitness
    :: Typeable witctx
    => ScriptWitness witctx era
    -> AnyScriptWitness era

deriving instance Show (AnyScriptWitness era)

instance Eq (AnyScriptWitness era) where
  AnyScriptWitness sw1 == AnyScriptWitness sw2 =
    case eqsw sw1 sw2 of
      Just Refl -> sw1 == sw2
      Nothing -> False
   where
    eqsw
      :: (Typeable w1, Typeable w2)
      => ScriptWitness w1 era
      -> ScriptWitness w2 era
      -> Maybe (w1 :~: w2)
    eqsw _ _ = eqT

-- | Identify the location of a 'ScriptWitness' within the context of a
-- 'TxBody'. These are indexes of the objects within the transaction that
-- need or can use script witnesses: inputs, minted assets, withdrawals and
-- certificates. These are simple numeric indices, enumerated from zero.
-- Thus the indices are not stable if the transaction body is modified.
data ScriptWitnessIndex
  = -- | The n'th transaction input, in the order of the 'TxId's.
    ScriptWitnessIndexTxIn !Word32
  | -- | The n'th minting 'PolicyId', in the order of the 'PolicyId's.
    ScriptWitnessIndexMint !Word32
  | -- | The n'th certificate, in the list order of the certificates.
    ScriptWitnessIndexCertificate !Word32
  | -- | The n'th withdrawal, in the order of the 'StakeAddress's.
    ScriptWitnessIndexWithdrawal !Word32
  | -- | The n'th vote, in the order of the votes.
    ScriptWitnessIndexVoting !Word32
  | -- | The n'th proposal, in the order of the proposals.
    ScriptWitnessIndexProposing !Word32
  deriving (Eq, Ord, Show)

instance ToJSON ScriptWitnessIndex where
  toJSON = \case
    ScriptWitnessIndexTxIn n ->
      object
        [ "kind" .= Aeson.String "ScriptWitnessIndexTxIn"
        , "value" .= n
        ]
    ScriptWitnessIndexMint n ->
      object
        [ "kind" .= Aeson.String "ScriptWitnessIndexMint"
        , "value" .= n
        ]
    ScriptWitnessIndexCertificate n ->
      object
        [ "kind" .= Aeson.String "ScriptWitnessIndexCertificate"
        , "value" .= n
        ]
    ScriptWitnessIndexWithdrawal n ->
      object
        [ "kind" .= Aeson.String "ScriptWitnessIndexWithdrawal"
        , "value" .= n
        ]
    ScriptWitnessIndexVoting n ->
      object
        [ "kind" .= Aeson.String "ScriptWitnessIndexVoting"
        , "value" .= n
        ]
    ScriptWitnessIndexProposing n ->
      object
        [ "kind" .= Aeson.String "ScriptWitnessIndexProposing"
        , "value" .= n
        ]

renderScriptWitnessIndex :: ScriptWitnessIndex -> String
renderScriptWitnessIndex (ScriptWitnessIndexTxIn index) =
  "transaction input " <> show index <> " (in ascending order of the TxIds)"
renderScriptWitnessIndex (ScriptWitnessIndexMint index) =
  "policyId " <> show index <> " (in ascending order of the PolicyIds)"
renderScriptWitnessIndex (ScriptWitnessIndexCertificate index) =
  "certificate " <> show index <> " (in the list order of the certificates)"
renderScriptWitnessIndex (ScriptWitnessIndexWithdrawal index) =
  "withdrawal " <> show index <> " (in ascending order of the StakeAddresses)"
renderScriptWitnessIndex (ScriptWitnessIndexVoting index) =
  "vote " <> show index <> " (in ascending order of the votes)"
renderScriptWitnessIndex (ScriptWitnessIndexProposing index) =
  "proposal " <> show index <> " (in ascending order of the proposals)"

fromScriptWitnessIndex
  :: AlonzoEraOnwards era
  -> ScriptWitnessIndex
  -> Maybe (L.PlutusPurpose L.AsIx (ShelleyLedgerEra era))
fromScriptWitnessIndex aOnwards widx =
  case aOnwards of
    AlonzoEraOnwardsAlonzo -> fromScriptWitnessIndexAlonzo widx
    AlonzoEraOnwardsBabbage -> fromScriptWitnessIndexBabbage widx
    AlonzoEraOnwardsConway -> fromScriptWitnessIndexConway widx

fromScriptWitnessIndexAlonzo
  :: ScriptWitnessIndex -> Maybe (L.PlutusPurpose L.AsIx (ShelleyLedgerEra AlonzoEra))
fromScriptWitnessIndexAlonzo i =
  case i of
    ScriptWitnessIndexTxIn n -> Just $ L.AlonzoSpending (L.AsIx n)
    ScriptWitnessIndexMint n -> Just $ L.AlonzoMinting (L.AsIx n)
    ScriptWitnessIndexCertificate n -> Just $ L.AlonzoCertifying (L.AsIx n)
    ScriptWitnessIndexWithdrawal n -> Just $ L.AlonzoRewarding (L.AsIx n)
    _ -> Nothing

fromScriptWitnessIndexBabbage
  :: ScriptWitnessIndex -> Maybe (L.PlutusPurpose L.AsIx (ShelleyLedgerEra BabbageEra))
fromScriptWitnessIndexBabbage i =
  case i of
    ScriptWitnessIndexTxIn n -> Just $ L.AlonzoSpending (L.AsIx n)
    ScriptWitnessIndexMint n -> Just $ L.AlonzoMinting (L.AsIx n)
    ScriptWitnessIndexCertificate n -> Just $ L.AlonzoCertifying (L.AsIx n)
    ScriptWitnessIndexWithdrawal n -> Just $ L.AlonzoRewarding (L.AsIx n)
    _ -> Nothing

fromScriptWitnessIndexConway
  :: ScriptWitnessIndex -> Maybe (L.PlutusPurpose L.AsIx (ShelleyLedgerEra ConwayEra))
fromScriptWitnessIndexConway i =
  case i of
    ScriptWitnessIndexTxIn n -> Just $ L.ConwaySpending (L.AsIx n)
    ScriptWitnessIndexMint n -> Just $ L.ConwayMinting (L.AsIx n)
    ScriptWitnessIndexCertificate n -> Just $ L.ConwayCertifying (L.AsIx n)
    ScriptWitnessIndexWithdrawal n -> Just $ L.ConwayRewarding (L.AsIx n)
    ScriptWitnessIndexVoting n -> Just $ L.ConwayVoting (L.AsIx n)
    ScriptWitnessIndexProposing n -> Just $ L.ConwayProposing (L.AsIx n)

toScriptIndex
  :: AlonzoEraOnwards era
  -> L.PlutusPurpose L.AsIx (ShelleyLedgerEra era)
  -> ScriptWitnessIndex
toScriptIndex sbe scriptPurposeIndex =
  case sbe of
    AlonzoEraOnwardsAlonzo -> toScriptIndexAlonzo scriptPurposeIndex
    AlonzoEraOnwardsBabbage -> toScriptIndexAlonzo scriptPurposeIndex
    AlonzoEraOnwardsConway -> toScriptIndexConway scriptPurposeIndex

toScriptIndexAlonzo
  :: L.AlonzoPlutusPurpose L.AsIx (ShelleyLedgerEra era)
  -> ScriptWitnessIndex
toScriptIndexAlonzo scriptPurposeIndex =
  case scriptPurposeIndex of
    L.AlonzoSpending (L.AsIx i) -> ScriptWitnessIndexTxIn i
    L.AlonzoMinting (L.AsIx i) -> ScriptWitnessIndexMint i
    L.AlonzoCertifying (L.AsIx i) -> ScriptWitnessIndexCertificate i
    L.AlonzoRewarding (L.AsIx i) -> ScriptWitnessIndexWithdrawal i

toScriptIndexConway
  :: L.ConwayPlutusPurpose L.AsIx (ShelleyLedgerEra era)
  -> ScriptWitnessIndex
toScriptIndexConway scriptPurposeIndex =
  case scriptPurposeIndex of
    L.ConwaySpending (L.AsIx i) -> ScriptWitnessIndexTxIn i
    L.ConwayMinting (L.AsIx i) -> ScriptWitnessIndexMint i
    L.ConwayCertifying (L.AsIx i) -> ScriptWitnessIndexCertificate i
    L.ConwayRewarding (L.AsIx i) -> ScriptWitnessIndexWithdrawal i
    L.ConwayVoting (L.AsIx i) -> ScriptWitnessIndexVoting i
    L.ConwayProposing (L.AsIx i) -> ScriptWitnessIndexProposing i

collectTxBodyScriptWitnesses
  :: forall era
   . ShelleyBasedEra era
  -> TxBodyContent BuildTx era
  -> [(ScriptWitnessIndex, AnyScriptWitness era)]
collectTxBodyScriptWitnesses
  _
  TxBodyContent
    { txIns
    , txWithdrawals
    , txCertificates
    , txMintValue
    , txVotingProcedures
    , txProposalProcedures
    } =
    concat
      [ scriptWitnessesTxIns txIns
      , scriptWitnessesWithdrawals txWithdrawals
      , scriptWitnessesCertificates txCertificates
      , scriptWitnessesMinting txMintValue
      , scriptWitnessesVoting (maybe TxVotingProceduresNone unFeatured txVotingProcedures)
      , scriptWitnessesProposing (maybe TxProposalProceduresNone unFeatured txProposalProcedures)
      ]
   where
    scriptWitnessesTxIns
      :: [(TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era))]
      -> [(ScriptWitnessIndex, AnyScriptWitness era)]
    scriptWitnessesTxIns txIns' =
      List.nub
        [ (ix, AnyScriptWitness witness)
        | (ix, _, ScriptWitness _ witness) <- indexTxIns txIns'
        ]

    scriptWitnessesWithdrawals
      :: TxWithdrawals BuildTx era
      -> [(ScriptWitnessIndex, AnyScriptWitness era)]
    scriptWitnessesWithdrawals TxWithdrawalsNone = []
    scriptWitnessesWithdrawals txw =
      List.nub
        [ (ix, AnyScriptWitness witness)
        | (ix, _, _, ScriptWitness _ witness) <- indexTxWithdrawals txw
        ]

    scriptWitnessesCertificates
      :: TxCertificates BuildTx era
      -> [(ScriptWitnessIndex, AnyScriptWitness era)]
    scriptWitnessesCertificates TxCertificatesNone = []
    scriptWitnessesCertificates txc =
      List.nub
        [ (ix, AnyScriptWitness witness)
        | (ix, _, _, ScriptWitness _ witness) <- indexTxCertificates txc
        ]

    scriptWitnessesMinting
      :: TxMintValue BuildTx era
      -> [(ScriptWitnessIndex, AnyScriptWitness era)]
    scriptWitnessesMinting TxMintNone = []
    scriptWitnessesMinting txMintValue' =
      List.nub
        [ (ix, AnyScriptWitness witness)
        | (ix, _, _, BuildTxWith witness) <- indexTxMintValue txMintValue'
        ]

    scriptWitnessesVoting
      :: TxVotingProcedures BuildTx era
      -> [(ScriptWitnessIndex, AnyScriptWitness era)]
    scriptWitnessesVoting TxVotingProceduresNone = []
    scriptWitnessesVoting txv =
      List.nub
        [ (ix, AnyScriptWitness witness)
        | (ix, _, witness) <- indexTxVotingProcedures txv
        ]

    scriptWitnessesProposing
      :: TxProposalProcedures BuildTx era
      -> [(ScriptWitnessIndex, AnyScriptWitness era)]
    scriptWitnessesProposing TxProposalProceduresNone = []
    scriptWitnessesProposing txp =
      List.nub
        [ (ix, AnyScriptWitness witness)
        | (ix, _, witness) <- indexTxProposalProcedures txp
        ]

collectTxBodyScriptWitnessRequirements
  :: forall era
   . IsShelleyBasedEra era
  => AlonzoEraOnwards era
  -> TxBodyContent BuildTx era
  -> Either
       TxBodyError
       (TxScriptWitnessRequirements (ShelleyLedgerEra era))
collectTxBodyScriptWitnessRequirements
  aEon
  bc@TxBodyContent
    { txInsReference
    , txOuts
    } =
    obtainAlonzoScriptPurposeConstraints aEon $ do
      let sbe = shelleyBasedEra @era
          supplementaldatums =
            TxScriptWitnessRequirements
              mempty
              mempty
              (getDatums aEon txInsReference txOuts)
              mempty
      txInWits <-
        first TxBodyPlutusScriptDecodeError $
          legacyWitnessToScriptRequirements aEon $
            extractWitnessableTxIns aEon bc

      txWithdrawalWits <-
        first TxBodyPlutusScriptDecodeError $
          legacyWitnessToScriptRequirements aEon $
            extractWitnessableWithdrawals aEon bc

      txCertWits <-
        first TxBodyPlutusScriptDecodeError $
          legacyWitnessToScriptRequirements aEon $
            extractWitnessableCertificates aEon bc

      txMintWits <-
        first TxBodyPlutusScriptDecodeError $
          legacyWitnessToScriptRequirements aEon $
            extractWitnessableMints aEon bc

      txVotingWits <-
        caseShelleyToBabbageOrConwayEraOnwards
          ( \w ->
              shelleyToBabbageEraConstraints w $ Right $ TxScriptWitnessRequirements mempty mempty mempty mempty
          )
          ( \eon ->
              first TxBodyPlutusScriptDecodeError $
                legacyWitnessToScriptRequirements aEon $
                  extractWitnessableVotes eon bc
          )
          sbe
      txProposalWits <-
        caseShelleyToBabbageOrConwayEraOnwards
          (const $ Right $ TxScriptWitnessRequirements mempty mempty mempty mempty)
          ( \eon ->
              first TxBodyPlutusScriptDecodeError $
                legacyWitnessToScriptRequirements aEon $
                  extractWitnessableProposals eon bc
          )
          sbe

      return $
        obtainMonoidConstraint aEon $
          mconcat
            [ supplementaldatums
            , txInWits
            , txWithdrawalWits
            , txCertWits
            , txMintWits
            , txVotingWits
            , txProposalWits
            ]

-- | Extract datum:
-- 1. supplemental datums from transaction outputs
-- 2. datums from reference inputs
--
-- Note that this function does not check whose datum hashes are present in the reference inputs. This means if there
-- are redundant datums in 'TxInsReference', a submission of such transaction will fail.
getDatums
  :: AlonzoEraOnwards era
  -> TxInsReference BuildTx era
  -- ^ reference inputs
  -> [TxOut CtxTx era]
  -> L.TxDats (ShelleyLedgerEra era)
getDatums eon txInsRef txOutsFromTx = alonzoEraOnwardsConstraints eon $ do
  let refTxInsDats =
        [ d
        | TxInsReference _ _ (BuildTxWith datumSet) <- [txInsRef]
        , d <- toList datumSet
        ]
      -- use only supplemental datum
      txOutsDats = [d | TxOut _ _ (TxOutSupplementalDatum _ d) _ <- txOutsFromTx]
  L.TxDats $
    fromList $
      [ (L.hashData ledgerData, ledgerData)
      | d <- refTxInsDats <> txOutsDats
      , let ledgerData = toAlonzoData d
      ]

extractWitnessableTxIns
  :: AlonzoEraOnwards era
  -> TxBodyContent BuildTx era
  -> [(Witnessable TxInItem (ShelleyLedgerEra era), BuildTxWith BuildTx (Witness WitCtxTxIn era))]
extractWitnessableTxIns aeon TxBodyContent{txIns} =
  alonzoEraOnwardsConstraints aeon $
    List.nub [(WitTxIn txin, wit) | (txin, wit) <- txIns]

extractWitnessableWithdrawals
  :: AlonzoEraOnwards era
  -> TxBodyContent BuildTx era
  -> [(Witnessable WithdrawalItem (ShelleyLedgerEra era), BuildTxWith BuildTx (Witness WitCtxStake era))]
extractWitnessableWithdrawals aeon TxBodyContent{txWithdrawals} =
  alonzoEraOnwardsConstraints aeon $
    List.nub
      [ (WitWithdrawal addr withAmt, wit)
      | (addr, withAmt, wit) <- getWithdrawals txWithdrawals
      ]
 where
  getWithdrawals TxWithdrawalsNone = []
  getWithdrawals (TxWithdrawals _ txws) = txws

extractWitnessableCertificates
  :: AlonzoEraOnwards era
  -> TxBodyContent BuildTx era
  -> [(Witnessable CertItem (ShelleyLedgerEra era), BuildTxWith BuildTx (Witness WitCtxStake era))]
extractWitnessableCertificates aeon TxBodyContent{txCertificates} =
  alonzoEraOnwardsConstraints aeon $
    List.nub
      [ ( WitTxCert (certificateToTxCert cert) stakeCred
        , BuildTxWith wit
        )
      | (cert, BuildTxWith (Just (stakeCred, wit))) <- getCertificates txCertificates
      ]
 where
  getCertificates TxCertificatesNone = []
  getCertificates (TxCertificates _ txcs) = toList txcs

extractWitnessableMints
  :: AlonzoEraOnwards era
  -> TxBodyContent BuildTx era
  -> [(Witnessable MintItem (ShelleyLedgerEra era), BuildTxWith BuildTx (Witness WitCtxMint era))]
extractWitnessableMints aeon TxBodyContent{txMintValue} =
  alonzoEraOnwardsConstraints aeon $
    List.nub
      [ (WitMint policyId policyAssets, BuildTxWith $ ScriptWitness ScriptWitnessForMinting wit)
      | (policyId, (policyAssets, BuildTxWith wit)) <- getMints txMintValue
      ]
 where
  getMints TxMintNone = []
  getMints (TxMintValue _ txms) = toList txms

extractWitnessableVotes
  :: ConwayEraOnwards era
  -> TxBodyContent BuildTx era
  -> [(Witnessable VoterItem (ShelleyLedgerEra era), BuildTxWith BuildTx (Witness WitCtxStake era))]
extractWitnessableVotes e@ConwayEraOnwardsConway TxBodyContent{txVotingProcedures} =
  List.nub
    [ (WitVote vote, BuildTxWith wit)
    | (vote, wit) <- getVotes e $ maybe TxVotingProceduresNone unFeatured txVotingProcedures
    ]
 where
  getVotes
    :: ConwayEraOnwards era
    -> TxVotingProcedures BuildTx era
    -> [(L.Voter, Witness WitCtxStake era)]
  getVotes ConwayEraOnwardsConway TxVotingProceduresNone = []
  getVotes ConwayEraOnwardsConway (TxVotingProcedures allVotingProcedures (BuildTxWith scriptWitnessedVotes)) =
    [ (voter, wit)
    | (voter, _) <- toList $ L.unVotingProcedures allVotingProcedures
    , let wit = case Map.lookup voter scriptWitnessedVotes of
            Just sWit -> ScriptWitness ScriptWitnessForStakeAddr sWit
            Nothing -> KeyWitness KeyWitnessForStakeAddr
    ]

extractWitnessableProposals
  :: ConwayEraOnwards era
  -> TxBodyContent BuildTx era
  -> [(Witnessable ProposalItem (ShelleyLedgerEra era), BuildTxWith BuildTx (Witness WitCtxStake era))]
extractWitnessableProposals e@ConwayEraOnwardsConway TxBodyContent{txProposalProcedures} =
  List.nub
    [ (WitProposal prop, BuildTxWith wit)
    | (Proposal prop, wit) <-
        getProposals e $ maybe TxProposalProceduresNone unFeatured txProposalProcedures
    ]
 where
  getProposals
    :: ConwayEraOnwards era
    -> TxProposalProcedures BuildTx era
    -> [(Proposal era, Witness WitCtxStake era)]
  getProposals ConwayEraOnwardsConway TxProposalProceduresNone = []
  getProposals ConwayEraOnwardsConway (TxProposalProcedures txps) =
    [ (Proposal p, wit)
    | (p, BuildTxWith mScriptWit) <- toList txps
    , let wit = case mScriptWit of
            Just sWit -> ScriptWitness ScriptWitnessForStakeAddr sWit
            Nothing -> KeyWitness KeyWitnessForStakeAddr
    ]

toShelleyWithdrawal :: [(StakeAddress, L.Coin, a)] -> L.Withdrawals
toShelleyWithdrawal withdrawals =
  L.Withdrawals $
    fromList
      [ (toShelleyStakeAddr stakeAddr, value)
      | (stakeAddr, value, _) <- withdrawals
      ]

fromShelleyWithdrawal
  :: L.Withdrawals
  -> [(StakeAddress, L.Coin, BuildTxWith ViewTx (Witness WitCtxStake era))]
fromShelleyWithdrawal (L.Withdrawals withdrawals) =
  [ (fromShelleyStakeAddr stakeAddr, value, ViewTx)
  | (stakeAddr, value) <- Map.assocs withdrawals
  ]

-- | In the Allegra and Mary eras the auxiliary data consists of the tx metadata
-- and the axiliary scripts. In the Alonzo and later eras the auxiliary data consists of the tx metadata
-- and the axiliary scripts, and the axiliary script data.
toAuxiliaryData
  :: ShelleyBasedEra era
  -> TxMetadataInEra era
  -> TxAuxScripts era
  -> Maybe (L.TxAuxData (ShelleyLedgerEra era))
toAuxiliaryData sbe txMetadata txAuxScripts =
  let ms = case txMetadata of
        TxMetadataNone -> Map.empty
        TxMetadataInEra _ (TxMetadata ms') -> toShelleyMetadata ms'
      ss = case txAuxScripts of
        TxAuxScriptsNone -> []
        TxAuxScripts _ ss' -> map toShelleyScript ss'
   in case sbe of
        ShelleyBasedEraShelley ->
          guard (not (Map.null ms)) $> L.ShelleyTxAuxData ms
        ShelleyBasedEraAllegra ->
          guard (not (Map.null ms && null ss)) $> L.AllegraTxAuxData ms (fromList ss)
        ShelleyBasedEraMary ->
          guard (not (Map.null ms && null ss)) $> L.AllegraTxAuxData ms (fromList ss)
        ShelleyBasedEraAlonzo ->
          guard (not (Map.null ms && null ss)) $> L.mkAlonzoTxAuxData ms ss
        ShelleyBasedEraBabbage ->
          guard (not (Map.null ms && null ss)) $> L.mkAlonzoTxAuxData ms ss
        ShelleyBasedEraConway ->
          guard (not (Map.null ms && null ss)) $> L.mkAlonzoTxAuxData ms ss

-- ----------------------------------------------------------------------------
-- Other utilities helpful with making transaction bodies
--

-- | Compute the 'TxIn' of the initial UTxO pseudo-transaction corresponding
-- to the given address in the genesis initial funds.
--
-- The Shelley initial UTxO is constructed from the 'sgInitialFunds' which
-- is not a full UTxO but just a map from addresses to coin values.
--
-- This gets turned into a UTxO by making a pseudo-transaction for each address,
-- with the 0th output being the coin value. So to spend from the initial UTxO
-- we need this same 'TxIn' to use as an input to the spending transaction.
genesisUTxOPseudoTxIn :: NetworkId -> Hash GenesisUTxOKey -> TxIn
genesisUTxOPseudoTxIn nw (GenesisUTxOKeyHash kh) =
  -- TODO: should handle Byron UTxO case too.
  fromShelleyTxIn (Shelley.initialFundsPseudoTxIn addr)
 where
  addr :: L.Addr
  addr =
    L.Addr
      (toShelleyNetwork nw)
      (Shelley.KeyHashObj kh)
      Shelley.StakeRefNull

-- | Calculate the reference inputs size in bytes for provided set of transaction IDs and UTXOs.
getReferenceInputsSizeForTxIds
  :: ShelleyLedgerEra era ~ ledgerera
  => BabbageEraOnwards era
  -> Ledger.UTxO ledgerera
  -> Set TxIn
  -> Int
getReferenceInputsSizeForTxIds beo utxo txIds = babbageEraOnwardsConstraints beo $ do
  let refScripts = L.getReferenceScriptsNonDistinct utxo (Set.map toShelleyTxIn txIds)
  getSum $ foldMap (Sum . SafeHash.originalBytesSize . snd) refScripts

calculateExecutionUnitsLovelace :: Ledger.Prices -> ExecutionUnits -> Maybe L.Coin
calculateExecutionUnitsLovelace prices eUnits =
  return $ Alonzo.txscriptfee prices (toAlonzoExUnits eUnits)
