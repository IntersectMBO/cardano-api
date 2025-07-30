{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
-- The Shelley ledger uses promoted data kinds which we have to use, but we do
-- not export any from this API. We also use them unticked as nature intended.
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

-- | Creating complete, signed transactions.
module Cardano.Api.Tx.Internal.Sign
  ( Tx (.., Tx)
  , Byron.ATxAux (..)
  , getTxBody
  , getByronTxBody
  , getTxWitnesses
  , getTxWitnessesByron
  , ScriptValidity (..)

    -- *** Signing in one go
  , ShelleySigningKey (..)
  , toShelleySigningKey
  , signByronTransaction
  , signShelleyTransaction

    -- *** Incremental signing and separate witnesses
  , makeSignedByronTransaction
  , makeSignedTransaction
  , makeSignedTransaction'
  , KeyWitness (..)
  , makeByronKeyWitness
  , ShelleyWitnessSigningKey (..)
  , makeShelleyKeyWitness
  , makeShelleyKeyWitness'
  , WitnessNetworkIdOrByronAddress (..)
  , makeShelleyBootstrapWitness
  , makeShelleyBasedBootstrapWitness
  , makeShelleySignature
  , getShelleyKeyWitnessVerificationKey
  , getTxBodyAndWitnesses

    -- ** Data family instances
  , AsType
    ( AsTx
    , AsMaryTx
    , AsAllegraTx
    , AsAlonzoTx
    , AsKeyWitness
    , AsByronWitness
    , AsShelleyWitness
    , AsTxId
    , AsTxBody
    , AsByronTxBody
    , AsShelleyTxBody
    , AsMaryTxBody
    )
  , TxBody (..)
  , TxScriptValidity (..)
  , scriptValidityToIsValid
  , isValidToScriptValidity
  , txScriptValidityToIsValid
  , txScriptValidityToScriptValidity
  , TxBodyScriptData (..)
  , selectTxDatums
  )
where

import Cardano.Api.Address
import Cardano.Api.Byron.Internal.Key
import Cardano.Api.Certificate.Internal
import Cardano.Api.Era
import Cardano.Api.HasTypeProxy
import Cardano.Api.Key.Internal
import Cardano.Api.Key.Internal.Class
import Cardano.Api.Network.Internal.NetworkId
import Cardano.Api.Serialise.Cbor
import Cardano.Api.Serialise.TextEnvelope.Internal

import Cardano.Chain.Common qualified as Byron
import Cardano.Chain.UTxO qualified as Byron
import Cardano.Crypto.DSIGN.Class qualified as Crypto
import Cardano.Crypto.Hash qualified as Hash
import Cardano.Crypto.Hashing qualified as Byron
import Cardano.Crypto.ProtocolMagic qualified as Byron
import Cardano.Crypto.Signing qualified as Byron
import Cardano.Crypto.Util qualified as Crypto
import Cardano.Crypto.Wallet qualified as Crypto.HD
import Cardano.Ledger.Alonzo.Core qualified as L
import Cardano.Ledger.Alonzo.TxWits qualified as Alonzo
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.BaseTypes (maybeToStrictMaybe, strictMaybeToMaybe)
import Cardano.Ledger.Binary (Annotated (..))
import Cardano.Ledger.Binary qualified as CBOR
import Cardano.Ledger.Binary.Plain qualified as Plain
import Cardano.Ledger.Core qualified as Ledger
import Cardano.Ledger.Keys qualified as Shelley

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Data.Type.Equality (TestEquality (..), (:~:) (Refl))
import GHC.Exts (IsList (..))
import Lens.Micro

-- ----------------------------------------------------------------------------
-- Signed transactions
--

data Tx era where
  ShelleyTx
    :: ShelleyBasedEra era
    -> L.Tx (ShelleyLedgerEra era)
    -> Tx era

-- | This pattern will be deprecated in the future. We advise against introducing new usage of it.
pattern Tx :: TxBody era -> [KeyWitness era] -> Tx era
pattern Tx txbody ws <- (getTxBodyAndWitnesses -> (txbody, ws))
  where
    Tx txbody ws = makeSignedTransaction ws txbody

instance Show (InAnyCardanoEra Tx) where
  show (InAnyCardanoEra _ tx) = show tx

instance Eq (InAnyCardanoEra Tx) where
  (==) (InAnyCardanoEra eraA txA) (InAnyCardanoEra eraB txB) =
    case testEquality eraA eraB of
      Nothing -> False
      Just Refl -> txA == txB

instance Show (InAnyShelleyBasedEra Tx) where
  show (InAnyShelleyBasedEra _ tx) = show tx

instance Eq (InAnyShelleyBasedEra Tx) where
  (==) (InAnyShelleyBasedEra eraA txA) (InAnyShelleyBasedEra eraB txB) =
    case testEquality eraA eraB of
      Nothing -> False
      Just Refl -> txA == txB

-- The GADT in the ShelleyTx case requires a custom instance
instance Eq (Tx era) where
  (==)
    (ShelleyTx sbe txA)
    (ShelleyTx _ txB) =
      shelleyBasedEraConstraints sbe $ txA == txB

-- The GADT in the ShelleyTx case requires a custom instance
instance Show (Tx era) where
  showsPrec p (ShelleyTx ShelleyBasedEraShelley tx) =
    showParen (p >= 11) $
      showString "ShelleyTx ShelleyBasedEraShelley "
        . showsPrec 11 tx
  showsPrec p (ShelleyTx ShelleyBasedEraAllegra tx) =
    showParen (p >= 11) $
      showString "ShelleyTx ShelleyBasedEraAllegra "
        . showsPrec 11 tx
  showsPrec p (ShelleyTx ShelleyBasedEraMary tx) =
    showParen (p >= 11) $
      showString "ShelleyTx ShelleyBasedEraMary "
        . showsPrec 11 tx
  showsPrec p (ShelleyTx ShelleyBasedEraAlonzo tx) =
    showParen (p >= 11) $
      showString "ShelleyTx ShelleyBasedEraAlonzo "
        . showsPrec 11 tx
  showsPrec p (ShelleyTx ShelleyBasedEraBabbage tx) =
    showParen (p >= 11) $
      showString "ShelleyTx ShelleyBasedEraBabbage "
        . showsPrec 11 tx
  showsPrec p (ShelleyTx ShelleyBasedEraConway tx) =
    showParen (p >= 11) $
      showString "ShelleyTx ShelleyBasedEraConway "
        . showsPrec 11 tx
  showsPrec p (ShelleyTx ShelleyBasedEraDijkstra tx) =
    showParen (p >= 11) $
      showString "ShelleyTx ShelleyBasedEraDijkstra "
        . showsPrec 11 tx

instance HasTypeProxy era => HasTypeProxy (Tx era) where
  data AsType (Tx era) = AsTx (AsType era)
  proxyToAsType _ = AsTx (proxyToAsType (Proxy :: Proxy era))

{-# DEPRECATED AsMaryTx "Use 'AsTx AsMaryEra' instead." #-}
pattern AsMaryTx :: AsType (Tx MaryEra)
pattern AsMaryTx = AsTx AsMaryEra

{-# COMPLETE AsMaryTx #-}

{-# DEPRECATED AsAllegraTx "Use 'AsTx AsAllegraEra' instead." #-}
pattern AsAllegraTx :: AsType (Tx AllegraEra)
pattern AsAllegraTx = AsTx AsAllegraEra

{-# COMPLETE AsAllegraTx #-}

{-# DEPRECATED AsAlonzoTx "Use 'AsTx AsAlonzoEra' instead." #-}
pattern AsAlonzoTx :: AsType (Tx AlonzoEra)
pattern AsAlonzoTx = AsTx AsAlonzoEra

{-# COMPLETE AsAlonzoTx #-}

instance IsShelleyBasedEra era => SerialiseAsCBOR (Tx era) where
  serialiseToCBOR (ShelleyTx sbe tx) =
    shelleyBasedEraConstraints sbe $ serialiseShelleyBasedTx tx

  deserialiseFromCBOR _ bs =
    shelleyBasedEraConstraints (shelleyBasedEra :: ShelleyBasedEra era) $
      deserialiseShelleyBasedTx (ShelleyTx shelleyBasedEra) bs

-- | The serialisation format for the different Shelley-based eras are not the
-- same, but they can be handled generally with one overloaded implementation.
serialiseShelleyBasedTx
  :: forall ledgerera
   . L.EraTx ledgerera
  => L.Tx ledgerera
  -> ByteString
serialiseShelleyBasedTx = Plain.serialize'

deserialiseShelleyBasedTx
  :: forall ledgerera tx'
   . L.EraTx ledgerera
  => (L.Tx ledgerera -> tx')
  -> ByteString
  -> Either CBOR.DecoderError tx'
deserialiseShelleyBasedTx mkTx bs =
  mkTx
    <$> CBOR.decodeFullAnnotator
      (L.eraProtVerLow @ledgerera)
      "Shelley Tx"
      CBOR.decCBOR
      (LBS.fromStrict bs)

-- NB: This is called in getTxBodyAndWitnesses which is fine as
-- getTxBodyAndWitnesses is only called in the context of a
-- shelley based era anyways. ByronTx will eventually be removed.
getTxBody :: Tx era -> TxBody era
getTxBody (ShelleyTx sbe tx) =
  caseShelleyToMaryOrAlonzoEraOnwards
    ( const $
        let txBody = tx ^. L.bodyTxL
            txAuxData = tx ^. L.auxDataTxL
            scriptWits = tx ^. L.witsTxL . L.scriptTxWitsL
         in ShelleyTxBody
              sbe
              txBody
              (Map.elems scriptWits)
              TxBodyNoScriptData
              (strictMaybeToMaybe txAuxData)
              TxScriptValidityNone
    )
    ( \w ->
        let txBody = tx ^. L.bodyTxL
            txAuxData = tx ^. L.auxDataTxL
            scriptWits = tx ^. L.witsTxL . L.scriptTxWitsL
            datsWits = tx ^. L.witsTxL . L.datsTxWitsL
            redeemerWits = tx ^. L.witsTxL . L.rdmrsTxWitsL
            isValid = tx ^. L.isValidTxL
         in ShelleyTxBody
              sbe
              txBody
              (Map.elems scriptWits)
              (TxBodyScriptData w datsWits redeemerWits)
              (strictMaybeToMaybe txAuxData)
              (TxScriptValidity w (isValidToScriptValidity isValid))
    )
    sbe

instance IsShelleyBasedEra era => HasTextEnvelope (Tx era) where
  textEnvelopeType _ =
    case shelleyBasedEra :: ShelleyBasedEra era of
      ShelleyBasedEraShelley -> "TxSignedShelley"
      ShelleyBasedEraAllegra -> "Tx AllegraEra"
      ShelleyBasedEraMary -> "Tx MaryEra"
      ShelleyBasedEraAlonzo -> "Tx AlonzoEra"
      ShelleyBasedEraBabbage -> "Tx BabbageEra"
      ShelleyBasedEraConway -> "Tx ConwayEra"
      ShelleyBasedEraDijkstra -> "Tx DijkstraEra"

-- ----------------------------------------------------------------------------
-- Transaction bodies
--
-- TODO: We can use Ledger.Tx era here however we would need to rename TxBody
-- as technically it is not strictly a transaction body.
data TxBody era where
  ShelleyTxBody
    :: ShelleyBasedEra era
    -> Ledger.TxBody (ShelleyLedgerEra era)
    -- We include the scripts along with the tx body, rather than the
    -- witnesses set, since they need to be known when building the body.
    -> [Ledger.Script (ShelleyLedgerEra era)]
    -- The info for each use of each script: the script input data, both
    -- the UTxO input data (called the "datum") and the supplied input
    -- data (called the "redeemer") and the execution units.
    -> TxBodyScriptData era
    -- The 'L.TxAuxData' consists of one or several things,
    -- depending on era:
    -- + transaction metadata  (in Shelley and later)
    -- + auxiliary scripts     (in Allegra and later)
    -- Note that there is no auxiliary script data as such, because the
    -- extra script data has to be passed to scripts and hence is needed
    -- for validation. It is thus part of the witness data, not the
    -- auxiliary data.
    -> Maybe (L.TxAuxData (ShelleyLedgerEra era))
    -> TxScriptValidity era
    -- ^ Mark script as expected to pass or fail validation
    -> TxBody era

-- The 'ShelleyBasedEra' GADT tells us what era we are in.
-- The 'ShelleyLedgerEra' type family maps that to the era type from the
-- ledger lib. The 'Ledger.TxBody' type family maps that to a specific
-- tx body type, which is different for each Shelley-based era.

-- The GADT in the ShelleyTxBody case requires a custom instance
instance Eq (TxBody era) where
  (==)
    (ShelleyTxBody sbe txbodyA txscriptsA redeemersA txmetadataA scriptValidityA)
    (ShelleyTxBody _ txbodyB txscriptsB redeemersB txmetadataB scriptValidityB) =
      caseShelleyToMaryOrAlonzoEraOnwards
        ( const $
            txbodyA == txbodyB
              && txscriptsA == txscriptsB
              && txmetadataA == txmetadataB
        )
        ( const $
            txbodyA == txbodyB
              && txscriptsA == txscriptsB
              && redeemersA == redeemersB
              && txmetadataA == txmetadataB
              && scriptValidityA == scriptValidityB
        )
        sbe

-- The GADT in the ShelleyTxBody case requires a custom instance
instance Show (TxBody era) where
  showsPrec
    p
    ( ShelleyTxBody
        ShelleyBasedEraShelley
        txbody
        txscripts
        redeemers
        txmetadata
        scriptValidity
      ) =
      showParen
        (p >= 11)
        ( showString "ShelleyTxBody ShelleyBasedEraShelley "
            . showsPrec 11 txbody
            . showChar ' '
            . showsPrec 11 txscripts
            . showChar ' '
            . showsPrec 11 redeemers
            . showChar ' '
            . showsPrec 11 txmetadata
            . showChar ' '
            . showsPrec 11 scriptValidity
        )
  showsPrec
    p
    ( ShelleyTxBody
        ShelleyBasedEraAllegra
        txbody
        txscripts
        redeemers
        txmetadata
        scriptValidity
      ) =
      showParen
        (p >= 11)
        ( showString "ShelleyTxBody ShelleyBasedEraAllegra "
            . showsPrec 11 txbody
            . showChar ' '
            . showsPrec 11 txscripts
            . showChar ' '
            . showsPrec 11 redeemers
            . showChar ' '
            . showsPrec 11 txmetadata
            . showChar ' '
            . showsPrec 11 scriptValidity
        )
  showsPrec
    p
    ( ShelleyTxBody
        ShelleyBasedEraMary
        txbody
        txscripts
        redeemers
        txmetadata
        scriptValidity
      ) =
      showParen
        (p >= 11)
        ( showString "ShelleyTxBody ShelleyBasedEraMary "
            . showsPrec 11 txbody
            . showChar ' '
            . showsPrec 11 txscripts
            . showChar ' '
            . showsPrec 11 redeemers
            . showChar ' '
            . showsPrec 11 txmetadata
            . showChar ' '
            . showsPrec 11 scriptValidity
        )
  showsPrec
    p
    ( ShelleyTxBody
        ShelleyBasedEraAlonzo
        txbody
        txscripts
        redeemers
        txmetadata
        scriptValidity
      ) =
      showParen
        (p >= 11)
        ( showString "ShelleyTxBody ShelleyBasedEraAlonzo "
            . showsPrec 11 txbody
            . showChar ' '
            . showsPrec 11 txscripts
            . showChar ' '
            . showsPrec 11 redeemers
            . showChar ' '
            . showsPrec 11 txmetadata
            . showChar ' '
            . showsPrec 11 scriptValidity
        )
  showsPrec
    p
    ( ShelleyTxBody
        ShelleyBasedEraBabbage
        txbody
        txscripts
        redeemers
        txmetadata
        scriptValidity
      ) =
      showParen
        (p >= 11)
        ( showString "ShelleyTxBody ShelleyBasedEraBabbage "
            . showsPrec 11 txbody
            . showChar ' '
            . showsPrec 11 txscripts
            . showChar ' '
            . showsPrec 11 redeemers
            . showChar ' '
            . showsPrec 11 txmetadata
            . showChar ' '
            . showsPrec 11 scriptValidity
        )
  showsPrec
    p
    ( ShelleyTxBody
        ShelleyBasedEraConway
        txbody
        txscripts
        redeemers
        txmetadata
        scriptValidity
      ) =
      showParen
        (p >= 11)
        ( showString "ShelleyTxBody ShelleyBasedEraConway "
            . showsPrec 11 txbody
            . showChar ' '
            . showsPrec 11 txscripts
            . showChar ' '
            . showsPrec 11 redeemers
            . showChar ' '
            . showsPrec 11 txmetadata
            . showChar ' '
            . showsPrec 11 scriptValidity
        )
  showsPrec
    p
    ( ShelleyTxBody
        ShelleyBasedEraDijkstra
        txbody
        txscripts
        redeemers
        txmetadata
        scriptValidity
      ) =
      showParen
        (p >= 11)
        ( showString "ShelleyTxBody ShelleyBasedEraDijkstra "
            . showsPrec 11 txbody
            . showChar ' '
            . showsPrec 11 txscripts
            . showChar ' '
            . showsPrec 11 redeemers
            . showChar ' '
            . showsPrec 11 txmetadata
            . showChar ' '
            . showsPrec 11 scriptValidity
        )

instance HasTypeProxy era => HasTypeProxy (TxBody era) where
  data AsType (TxBody era) = AsTxBody (AsType era)
  proxyToAsType _ = AsTxBody (proxyToAsType (Proxy :: Proxy era))

{-# DEPRECATED AsByronTxBody "Use 'AsTxBody AsByronEra' instead." #-}
pattern AsByronTxBody :: AsType (TxBody ByronEra)
pattern AsByronTxBody = AsTxBody AsByronEra

{-# COMPLETE AsByronTxBody #-}

{-# DEPRECATED AsShelleyTxBody "Use 'AsTxBody AsShelleyEra' instead." #-}
pattern AsShelleyTxBody :: AsType (TxBody ShelleyEra)
pattern AsShelleyTxBody = AsTxBody AsShelleyEra

{-# COMPLETE AsShelleyTxBody #-}

{-# DEPRECATED AsMaryTxBody "Use 'AsTxBody AsMaryEra' instead." #-}
pattern AsMaryTxBody :: AsType (TxBody MaryEra)
pattern AsMaryTxBody = AsTxBody AsMaryEra

{-# COMPLETE AsMaryTxBody #-}

instance IsShelleyBasedEra era => SerialiseAsCBOR (TxBody era) where
  serialiseToCBOR body = serialiseToCBOR $ signShelleyTransaction shelleyBasedEra body mempty

  deserialiseFromCBOR _ bs =
    fst . getTxBodyAndWitnesses
      <$> shelleyBasedEraConstraints
        (shelleyBasedEra :: ShelleyBasedEra era)
        (deserialiseShelleyBasedTx (ShelleyTx shelleyBasedEra) bs)

instance IsShelleyBasedEra era => HasTextEnvelope (TxBody era) where
  textEnvelopeType _ =
    case shelleyBasedEra :: ShelleyBasedEra era of
      ShelleyBasedEraShelley -> "TxUnsignedShelley"
      ShelleyBasedEraAllegra -> "TxBodyAllegra"
      ShelleyBasedEraMary -> "TxBodyMary"
      ShelleyBasedEraAlonzo -> "TxBodyAlonzo"
      ShelleyBasedEraBabbage -> "TxBodyBabbage"
      ShelleyBasedEraConway -> "TxBodyConway"
      ShelleyBasedEraDijkstra -> "TxBodyDijkstra"

data TxBodyScriptData era where
  TxBodyNoScriptData :: TxBodyScriptData era
  TxBodyScriptData
    :: AlonzoEraOnwardsConstraints era
    => AlonzoEraOnwards era
    -> Alonzo.TxDats (ShelleyLedgerEra era)
    -> Alonzo.Redeemers (ShelleyLedgerEra era)
    -> TxBodyScriptData era

deriving instance Eq (TxBodyScriptData era)

deriving instance Show (TxBodyScriptData era)

selectTxDatums
  :: TxBodyScriptData era
  -> Map L.DataHash (L.Data (ShelleyLedgerEra era))
selectTxDatums TxBodyNoScriptData = Map.empty
selectTxDatums (TxBodyScriptData _ (Alonzo.TxDats datums) _) = datums

-- | Indicates whether a script is expected to fail or pass validation.
data ScriptValidity
  = -- | Script is expected to fail validation.
    -- Transactions marked as such can include scripts that fail validation.
    -- Such transactions may be submitted to the chain, in which case the
    -- collateral will be taken upon on chain script validation failure.
    ScriptInvalid
  | -- | Script is expected to pass validation.
    -- Transactions marked as such cannot include scripts that fail validation.
    ScriptValid
  deriving (Eq, Show)

instance CBOR.EncCBOR ScriptValidity where
  encCBOR = CBOR.encCBOR . scriptValidityToIsValid

instance CBOR.DecCBOR ScriptValidity where
  decCBOR = isValidToScriptValidity <$> CBOR.decCBOR

scriptValidityToIsValid :: ScriptValidity -> L.IsValid
scriptValidityToIsValid ScriptInvalid = L.IsValid False
scriptValidityToIsValid ScriptValid = L.IsValid True

isValidToScriptValidity :: L.IsValid -> ScriptValidity
isValidToScriptValidity (L.IsValid False) = ScriptInvalid
isValidToScriptValidity (L.IsValid True) = ScriptValid

-- | A representation of whether the era supports tx script validity.
--
-- The Alonzo and subsequent eras support script validity.
data TxScriptValidity era where
  TxScriptValidityNone
    :: TxScriptValidity era
  -- | Tx script validity is supported in transactions in the 'Alonzo' era onwards.
  TxScriptValidity
    :: AlonzoEraOnwards era
    -> ScriptValidity
    -> TxScriptValidity era

deriving instance Eq (TxScriptValidity era)

deriving instance Show (TxScriptValidity era)

txScriptValidityToScriptValidity :: TxScriptValidity era -> ScriptValidity
txScriptValidityToScriptValidity TxScriptValidityNone = ScriptValid
txScriptValidityToScriptValidity (TxScriptValidity _ scriptValidity) = scriptValidity

txScriptValidityToIsValid :: TxScriptValidity era -> L.IsValid
txScriptValidityToIsValid = scriptValidityToIsValid . txScriptValidityToScriptValidity

data KeyWitness era where
  ByronKeyWitness
    :: Byron.TxInWitness
    -> KeyWitness ByronEra
  ShelleyBootstrapWitness
    :: ShelleyBasedEra era
    -> Shelley.BootstrapWitness
    -> KeyWitness era
  ShelleyKeyWitness
    :: ShelleyBasedEra era
    -> L.WitVKey Shelley.Witness
    -> KeyWitness era

-- The GADT in the Shelley cases requires a custom instance
instance Eq (KeyWitness era) where
  (==)
    (ByronKeyWitness wA)
    (ByronKeyWitness wB) = wA == wB
  (==)
    (ShelleyBootstrapWitness _ wA)
    (ShelleyBootstrapWitness _ wB) =
      wA == wB
  (==)
    (ShelleyKeyWitness _ wA)
    (ShelleyKeyWitness _ wB) =
      wA == wB
  (==) _ _ = False

-- The GADT in the ShelleyTx case requires a custom instance
-- TODO: once we start providing custom patterns we should do the show in terms
-- of those. It'll be less verbose too!
instance Show (KeyWitness era) where
  showsPrec p (ByronKeyWitness tx) =
    showParen (p >= 11) $
      showString "ByronKeyWitness "
        . showsPrec 11 tx
  showsPrec p (ShelleyBootstrapWitness ShelleyBasedEraShelley tx) =
    showParen (p >= 11) $
      showString "ShelleyBootstrapWitness ShelleyBasedEraShelley "
        . showsPrec 11 tx
  showsPrec p (ShelleyBootstrapWitness ShelleyBasedEraAllegra tx) =
    showParen (p >= 11) $
      showString "ShelleyBootstrapWitness ShelleyBasedEraAllegra "
        . showsPrec 11 tx
  showsPrec p (ShelleyBootstrapWitness ShelleyBasedEraMary tx) =
    showParen (p >= 11) $
      showString "ShelleyBootstrapWitness ShelleyBasedEraMary "
        . showsPrec 11 tx
  showsPrec p (ShelleyBootstrapWitness ShelleyBasedEraAlonzo tx) =
    showParen (p >= 11) $
      showString "ShelleyBootstrapWitness ShelleyBasedEraAlonzo "
        . showsPrec 11 tx
  showsPrec p (ShelleyBootstrapWitness ShelleyBasedEraBabbage tx) =
    showParen (p >= 11) $
      showString "ShelleyBootstrapWitness ShelleyBasedEraBabbage "
        . showsPrec 11 tx
  showsPrec p (ShelleyBootstrapWitness ShelleyBasedEraConway tx) =
    showParen (p >= 11) $
      showString "ShelleyBootstrapWitness ShelleyBasedEraConway "
        . showsPrec 11 tx
  showsPrec p (ShelleyBootstrapWitness ShelleyBasedEraDijkstra tx) =
    showParen (p >= 11) $
      showString "ShelleyBootstrapWitness ShelleyBasedEraDijkstra "
        . showsPrec 11 tx
  showsPrec p (ShelleyKeyWitness ShelleyBasedEraShelley tx) =
    showParen (p >= 11) $
      showString "ShelleyKeyWitness ShelleyBasedEraShelley "
        . showsPrec 11 tx
  showsPrec p (ShelleyKeyWitness ShelleyBasedEraAllegra tx) =
    showParen (p >= 11) $
      showString "ShelleyKeyWitness ShelleyBasedEraAllegra "
        . showsPrec 11 tx
  showsPrec p (ShelleyKeyWitness ShelleyBasedEraMary tx) =
    showParen (p >= 11) $
      showString "ShelleyKeyWitness ShelleyBasedEraMary "
        . showsPrec 11 tx
  showsPrec p (ShelleyKeyWitness ShelleyBasedEraAlonzo tx) =
    showParen (p >= 11) $
      showString "ShelleyKeyWitness ShelleyBasedEraAlonzo "
        . showsPrec 11 tx
  showsPrec p (ShelleyKeyWitness ShelleyBasedEraBabbage tx) =
    showParen (p >= 11) $
      showString "ShelleyKeyWitness ShelleyBasedEraBabbage "
        . showsPrec 11 tx
  showsPrec p (ShelleyKeyWitness ShelleyBasedEraConway tx) =
    showParen (p >= 11) $
      showString "ShelleyKeyWitness ShelleyBasedEraConway "
        . showsPrec 11 tx
  showsPrec p (ShelleyKeyWitness ShelleyBasedEraDijkstra tx) =
    showParen (p >= 11) $
      showString "ShelleyKeyWitness ShelleyBasedEraDijkstra "
        . showsPrec 11 tx

instance HasTypeProxy era => HasTypeProxy (KeyWitness era) where
  data AsType (KeyWitness era) = AsKeyWitness (AsType era)
  proxyToAsType _ = AsKeyWitness (proxyToAsType (Proxy :: Proxy era))

{-# DEPRECATED AsByronWitness "Use AsKeyWitness AsByronEra instead" #-}
pattern AsByronWitness :: AsType (KeyWitness ByronEra)
pattern AsByronWitness = AsKeyWitness AsByronEra

{-# COMPLETE AsByronWitness #-}

{-# DEPRECATED AsShelleyWitness "Use AsKeyWitness AsShelleyEra instead" #-}
pattern AsShelleyWitness :: AsType (KeyWitness ShelleyEra)
pattern AsShelleyWitness = AsKeyWitness AsShelleyEra

{-# COMPLETE AsShelleyWitness #-}

-- This custom instance differs from cardano-ledger
-- because we want to be able to tell the difference between
-- on disk witnesses for the cli's 'assemble' command.
instance IsCardanoEra era => SerialiseAsCBOR (KeyWitness era) where
  serialiseToCBOR (ByronKeyWitness wit) =
    Plain.serialize' wit
  serialiseToCBOR (ShelleyKeyWitness sbe wit) =
    CBOR.serialize' (eraProtVerLow sbe) $
      encodeShelleyBasedKeyWitness wit
  serialiseToCBOR (ShelleyBootstrapWitness sbe wit) =
    CBOR.serialize' (eraProtVerLow sbe) $
      encodeShelleyBasedBootstrapWitness wit

  deserialiseFromCBOR _ bs =
    case cardanoEra :: CardanoEra era of
      ByronEra ->
        ByronKeyWitness <$> Plain.decodeFull' bs
      -- Use the same derialisation impl, but at different types:
      ShelleyEra -> decodeShelleyBasedWitness ShelleyBasedEraShelley bs
      AllegraEra -> decodeShelleyBasedWitness ShelleyBasedEraAllegra bs
      MaryEra -> decodeShelleyBasedWitness ShelleyBasedEraMary bs
      AlonzoEra -> decodeShelleyBasedWitness ShelleyBasedEraAlonzo bs
      BabbageEra -> decodeShelleyBasedWitness ShelleyBasedEraBabbage bs
      ConwayEra -> decodeShelleyBasedWitness ShelleyBasedEraConway bs
      DijkstraEra -> decodeShelleyBasedWitness ShelleyBasedEraDijkstra bs

encodeShelleyBasedKeyWitness :: CBOR.EncCBOR w => w -> CBOR.Encoding
encodeShelleyBasedKeyWitness wit =
  CBOR.encodeListLen 2 <> CBOR.encodeWord 0 <> CBOR.encCBOR wit

encodeShelleyBasedBootstrapWitness :: CBOR.EncCBOR w => w -> CBOR.Encoding
encodeShelleyBasedBootstrapWitness wit =
  CBOR.encodeListLen 2 <> CBOR.encodeWord 1 <> CBOR.encCBOR wit

decodeShelleyBasedWitness
  :: forall era
   . L.Era (ShelleyLedgerEra era)
  => ShelleyBasedEra era
  -> ByteString
  -> Either CBOR.DecoderError (KeyWitness era)
decodeShelleyBasedWitness sbe =
  CBOR.decodeFullAnnotator
    (L.eraProtVerLow @(ShelleyLedgerEra era))
    "Shelley Witness"
    decode
    . LBS.fromStrict
 where
  decode :: CBOR.Decoder s (CBOR.Annotator (KeyWitness era))
  decode = do
    CBOR.decodeListLenOf 2
    t <- CBOR.decodeWord
    case t of
      0 -> fmap (fmap (ShelleyKeyWitness sbe)) CBOR.decCBOR
      1 -> fmap (fmap (ShelleyBootstrapWitness sbe)) CBOR.decCBOR
      _ ->
        CBOR.cborError $
          CBOR.DecoderErrorUnknownTag
            "Shelley Witness"
            (fromIntegral t)

instance IsCardanoEra era => HasTextEnvelope (KeyWitness era) where
  textEnvelopeType _ =
    case cardanoEra :: CardanoEra era of
      ByronEra -> "TxWitnessByron"
      ShelleyEra -> "TxWitness ShelleyEra"
      AllegraEra -> "TxWitness AllegraEra"
      MaryEra -> "TxWitness MaryEra"
      AlonzoEra -> "TxWitness AlonzoEra"
      BabbageEra -> "TxWitness BabbageEra"
      ConwayEra -> "TxWitness ConwayEra"
      DijkstraEra -> "TxWitness DijkstraEra"

getTxBodyAndWitnesses :: Tx era -> (TxBody era, [KeyWitness era])
getTxBodyAndWitnesses tx = (getTxBody tx, getTxWitnesses tx)

{-# COMPLETE Tx #-}

data ShelleyWitnessSigningKey
  = WitnessPaymentKey (SigningKey PaymentKey)
  | WitnessPaymentExtendedKey (SigningKey PaymentExtendedKey)
  | WitnessStakeKey (SigningKey StakeKey)
  | WitnessStakeExtendedKey (SigningKey StakeExtendedKey)
  | WitnessStakePoolKey (SigningKey StakePoolKey)
  | WitnessStakePoolExtendedKey (SigningKey StakePoolExtendedKey)
  | WitnessGenesisKey (SigningKey GenesisKey)
  | WitnessGenesisExtendedKey (SigningKey GenesisExtendedKey)
  | WitnessGenesisDelegateKey (SigningKey GenesisDelegateKey)
  | WitnessGenesisDelegateExtendedKey
      (SigningKey GenesisDelegateExtendedKey)
  | WitnessGenesisUTxOKey (SigningKey GenesisUTxOKey)
  | WitnessCommitteeColdKey (SigningKey CommitteeColdKey)
  | WitnessCommitteeColdExtendedKey (SigningKey CommitteeColdExtendedKey)
  | WitnessCommitteeHotKey (SigningKey CommitteeHotKey)
  | WitnessCommitteeHotExtendedKey (SigningKey CommitteeHotExtendedKey)
  | WitnessDRepKey (SigningKey DRepKey)
  | WitnessDRepExtendedKey (SigningKey DRepExtendedKey)

-- | We support making key witnesses with both normal and extended signing keys.
data ShelleySigningKey
  = -- | A normal ed25519 signing key
    ShelleyNormalSigningKey (Crypto.SignKeyDSIGN Shelley.DSIGN)
  | -- | An extended ed25519 signing key
    ShelleyExtendedSigningKey Crypto.HD.XPrv

makeShelleySignature
  :: Crypto.SignableRepresentation tosign
  => tosign
  -> ShelleySigningKey
  -> (Crypto.SignedDSIGN Shelley.DSIGN) tosign
makeShelleySignature tosign (ShelleyNormalSigningKey sk) =
  Crypto.signedDSIGN () tosign sk
makeShelleySignature tosign (ShelleyExtendedSigningKey sk) =
  fromXSignature $
    Crypto.HD.sign
      BS.empty -- passphrase for (unused) in-memory encryption
      sk
      (Crypto.getSignableRepresentation tosign)
 where
  fromXSignature
    :: Crypto.HD.XSignature
    -> (Crypto.SignedDSIGN Shelley.DSIGN) b
  fromXSignature =
    Crypto.SignedDSIGN
      . fromMaybe impossible
      . Crypto.rawDeserialiseSigDSIGN
      . Crypto.HD.unXSignature

  impossible =
    error "makeShelleyKeyWitnessSignature: byron and shelley signature sizes do not match"

makeSignedTransaction'
  :: ()
  => CardanoEra era
  -> [KeyWitness era]
  -> TxBody era
  -> Tx era
makeSignedTransaction' _ = makeSignedTransaction

makeSignedByronTransaction
  :: [KeyWitness era] -> Annotated Byron.Tx ByteString -> Byron.ATxAux ByteString
makeSignedByronTransaction witnesses txbody =
  Byron.annotateTxAux $
    Byron.mkTxAux
      (unAnnotated txbody)
      (fromList [w | ByronKeyWitness w <- witnesses])

-- order of signing keys must match txins
signByronTransaction
  :: NetworkId
  -> Annotated Byron.Tx ByteString
  -> [SigningKey ByronKey]
  -> Byron.ATxAux ByteString
signByronTransaction nw txbody sks =
  makeSignedByronTransaction witnesses txbody
 where
  witnesses = map (makeByronKeyWitness nw txbody) sks

-- signing keys is a set
signShelleyTransaction
  :: ()
  => ShelleyBasedEra era
  -> TxBody era
  -> [ShelleyWitnessSigningKey]
  -> Tx era
signShelleyTransaction sbe txbody sks =
  makeSignedTransaction witnesses txbody
 where
  witnesses = map (makeShelleyKeyWitness sbe txbody) sks

getByronTxBody :: Byron.ATxAux ByteString -> Annotated Byron.Tx ByteString
getByronTxBody (Byron.ATxAux{Byron.aTaTx = txbody}) = txbody

getTxWitnessesByron :: Byron.ATxAux ByteString -> [KeyWitness ByronEra]
getTxWitnessesByron (Byron.ATxAux{Byron.aTaWitness = witnesses}) =
  map ByronKeyWitness
    . toList
    . unAnnotated
    $ witnesses

getTxWitnesses :: forall era. Tx era -> [KeyWitness era]
getTxWitnesses (ShelleyTx sbe tx') =
  caseShelleyToMaryOrAlonzoEraOnwards
    (const (getShelleyTxWitnesses tx'))
    (const (getAlonzoTxWitnesses tx'))
    sbe
 where
  getShelleyTxWitnesses
    :: forall ledgerera
     . L.EraTx ledgerera
    => L.Tx ledgerera
    -> [KeyWitness era]
  getShelleyTxWitnesses tx =
    map (ShelleyBootstrapWitness sbe) (Set.elems (tx ^. L.witsTxL . L.bootAddrTxWitsL))
      ++ map (ShelleyKeyWitness sbe) (Set.elems (tx ^. L.witsTxL . L.addrTxWitsL))

  getAlonzoTxWitnesses
    :: forall ledgerera
     . L.EraTx ledgerera
    => L.Tx ledgerera
    -> [KeyWitness era]
  getAlonzoTxWitnesses = getShelleyTxWitnesses

makeSignedTransaction
  :: forall era
   . [KeyWitness era]
  -> TxBody era
  -> Tx era
makeSignedTransaction
  witnesses
  ( ShelleyTxBody
      sbe
      txbody
      txscripts
      txscriptdata
      txmetadata
      scriptValidity
    ) =
    case sbe of
      ShelleyBasedEraShelley -> shelleySignedTransaction
      ShelleyBasedEraAllegra -> shelleySignedTransaction
      ShelleyBasedEraMary -> shelleySignedTransaction
      ShelleyBasedEraAlonzo -> alonzoSignedTransaction
      ShelleyBasedEraBabbage -> alonzoSignedTransaction
      ShelleyBasedEraConway -> alonzoSignedTransaction
      ShelleyBasedEraDijkstra -> alonzoSignedTransaction
   where
    txCommon
      :: forall ledgerera
       . ShelleyLedgerEra era ~ ledgerera
      => L.EraTx ledgerera
      => L.Tx ledgerera
    txCommon =
      L.mkBasicTx txbody
        & L.witsTxL
          .~ ( L.mkBasicTxWits
                 & L.addrTxWitsL .~ fromList [w | ShelleyKeyWitness _ w <- witnesses]
                 & L.scriptTxWitsL
                   .~ fromList
                     [ (Ledger.hashScript @ledgerera sw, sw)
                     | sw <- txscripts
                     ]
                 & L.bootAddrTxWitsL
                   .~ fromList [w | ShelleyBootstrapWitness _ w <- witnesses]
             )
        & L.auxDataTxL .~ maybeToStrictMaybe txmetadata

    shelleySignedTransaction
      :: forall ledgerera
       . ShelleyLedgerEra era ~ ledgerera
      => Ledger.EraTx ledgerera
      => Tx era
    shelleySignedTransaction = ShelleyTx sbe txCommon

    alonzoSignedTransaction
      :: forall ledgerera
       . ShelleyLedgerEra era ~ ledgerera
      => L.AlonzoEraTx ledgerera
      => Tx era
    alonzoSignedTransaction =
      ShelleyTx
        sbe
        ( txCommon
            & L.witsTxL . L.datsTxWitsL .~ datums
            & L.witsTxL . L.rdmrsTxWitsL .~ redeemers
            & L.isValidTxL .~ txScriptValidityToIsValid scriptValidity
        )
     where
      (datums, redeemers) =
        case txscriptdata of
          TxBodyScriptData _ ds rs -> (ds, rs)
          TxBodyNoScriptData -> (mempty, L.Redeemers mempty)

makeByronKeyWitness
  :: forall key
   . IsByronKey key
  => NetworkId
  -> Annotated Byron.Tx ByteString
  -> SigningKey key
  -> KeyWitness ByronEra
makeByronKeyWitness nw txbody =
  let txhash :: Byron.Hash Byron.Tx
      txhash = Byron.hashDecoded txbody

      pm :: Byron.ProtocolMagicId
      pm = toByronProtocolMagicId nw
   in -- To allow sharing of the txhash computation across many signatures we
      -- define and share the txhash outside the lambda for the signing key:
      case byronKeyFormat :: ByronKeyFormat key of
        ByronLegacyKeyFormat ->
          \(ByronSigningKeyLegacy sk) -> witness sk pm txhash
        ByronModernKeyFormat ->
          \(ByronSigningKey sk) -> witness sk pm txhash
 where
  witness
    :: Byron.SigningKey
    -> Byron.ProtocolMagicId
    -> Byron.Hash Byron.Tx
    -> KeyWitness ByronEra
  witness sk pm txHash =
    ByronKeyWitness $
      Byron.VKWitness
        (Byron.toVerification sk)
        (Byron.sign pm Byron.SignTx sk (Byron.TxSigData txHash))

-- | Either a network ID or a Byron address to be used in constructing a
-- Shelley bootstrap witness.
data WitnessNetworkIdOrByronAddress
  = -- | Network ID.
    --
    -- If this value is used in the construction of a Shelley bootstrap witness,
    -- the result will not consist of a derivation path. If that is required,
    -- specify a 'WitnessByronAddress' value instead.
    WitnessNetworkId !NetworkId
  | -- | Byron address.
    --
    -- If this value is used in the construction of a Shelley bootstrap witness,
    -- both the network ID and derivation path will be extracted from the
    -- address and used in the construction of the witness.
    WitnessByronAddress !(Address ByronAddr)

makeShelleyBootstrapWitness
  :: forall era
   . ()
  => ShelleyBasedEra era
  -> WitnessNetworkIdOrByronAddress
  -> TxBody era
  -> SigningKey ByronKey
  -> KeyWitness era
makeShelleyBootstrapWitness sbe nwOrAddr txBody sk =
  case txBody of
    ShelleyTxBody _ txbody _ _ _ _ -> makeShelleyBasedBootstrapWitness sbe nwOrAddr txbody sk

makeShelleyBasedBootstrapWitness
  :: forall era
   . ()
  => ShelleyBasedEra era
  -> WitnessNetworkIdOrByronAddress
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> SigningKey ByronKey
  -> KeyWitness era
makeShelleyBasedBootstrapWitness sbe nwOrAddr txbody (ByronSigningKey sk) =
  ShelleyBootstrapWitness sbe $
    -- Byron era witnesses were weird. This reveals all that weirdness.
    Shelley.BootstrapWitness
      { Shelley.bwKey = vk
      , Shelley.bwSignature = signature
      , Shelley.bwChainCode = chainCode
      , Shelley.bwAttributes = attributes
      }
 where
  -- Starting with the easy bits: we /can/ convert the Byron verification key
  -- to a the pair of a Shelley verification key plus the chain code.
  --
  (vk, chainCode) = Shelley.unpackByronVKey (Byron.toVerification sk)

  -- Now the hairy bits.
  --
  -- Byron era signing keys were all /extended/ ed25519 keys. We have to
  -- produce a signature using this extended signing key directly. They
  -- /cannot/ be converted to a plain (non-extended) signing keys. Since we
  -- now support extended signing keys for the Shelley too, we are able to
  -- reuse that here.
  --
  signature
    :: (Crypto.SignedDSIGN Shelley.DSIGN)
         (Hash.Hash Ledger.HASH Ledger.EraIndependentTxBody)
  signature =
    makeShelleySignature
      txhash
      -- Make the signature with the extended key directly:
      (ShelleyExtendedSigningKey (Byron.unSigningKey sk))

  txhash :: Hash.Hash Ledger.HASH Ledger.EraIndependentTxBody
  txhash = shelleyBasedEraConstraints sbe $ Ledger.extractHash (Ledger.hashAnnotated txbody)
  -- TODO: use Shelley.eraIndTxBodyHash txbody once that function has a
  -- suitably general type.

  -- And finally we need to provide the extra suffix bytes necessary to
  -- reconstruct the mini-Merkel tree that is a Byron address. The suffix
  -- bytes are the serialised address attributes.
  attributes =
    Plain.serialize' $
      Byron.mkAttributes
        Byron.AddrAttributes
          { Byron.aaVKDerivationPath = derivationPath
          , Byron.aaNetworkMagic = networkMagic
          }

  -- The 'WitnessNetworkIdOrByronAddress' value converted to an 'Either'.
  eitherNwOrAddr :: Either NetworkId (Address ByronAddr)
  eitherNwOrAddr =
    case nwOrAddr of
      WitnessNetworkId nw -> Left nw
      WitnessByronAddress addr -> Right addr

  unByronAddr :: Address ByronAddr -> Byron.Address
  unByronAddr (ByronAddress addr) = addr

  unAddrAttrs :: Address ByronAddr -> Byron.AddrAttributes
  unAddrAttrs = Byron.attrData . Byron.addrAttributes . unByronAddr

  derivationPath :: Maybe Byron.HDAddressPayload
  derivationPath =
    either
      (const Nothing)
      (Byron.aaVKDerivationPath . unAddrAttrs)
      eitherNwOrAddr

  networkMagic :: Byron.NetworkMagic
  networkMagic =
    either
      toByronNetworkMagic
      (Byron.aaNetworkMagic . unAddrAttrs)
      eitherNwOrAddr

makeShelleyKeyWitness
  :: forall era
   . ()
  => ShelleyBasedEra era
  -> TxBody era
  -> ShelleyWitnessSigningKey
  -> KeyWitness era
makeShelleyKeyWitness sbe (ShelleyTxBody _ txBody _ _ _ _) =
  makeShelleyKeyWitness' sbe txBody

makeShelleyKeyWitness'
  :: forall era
   . ()
  => ShelleyBasedEra era
  -> L.TxBody (ShelleyLedgerEra era)
  -> ShelleyWitnessSigningKey
  -> KeyWitness era
makeShelleyKeyWitness' sbe txBody wsk =
  shelleyBasedEraConstraints sbe $ do
    let txhash :: Hash.Hash Ledger.HASH Ledger.EraIndependentTxBody
        txhash = Ledger.extractHash (Ledger.hashAnnotated txBody)
        -- To allow sharing of the txhash computation across many signatures we
        -- define and share the txhash outside the lambda for the signing key:
        sk = toShelleySigningKey wsk
        vk = getShelleyKeyWitnessVerificationKey sk
        signature = makeShelleySignature txhash sk
    ShelleyKeyWitness sbe $
      L.WitVKey vk signature

toShelleySigningKey :: ShelleyWitnessSigningKey -> ShelleySigningKey
toShelleySigningKey key = case key of
  WitnessPaymentKey (PaymentSigningKey sk) -> ShelleyNormalSigningKey sk
  WitnessStakeKey (StakeSigningKey sk) -> ShelleyNormalSigningKey sk
  WitnessStakePoolKey (StakePoolSigningKey sk) -> ShelleyNormalSigningKey sk
  WitnessGenesisKey (GenesisSigningKey sk) -> ShelleyNormalSigningKey sk
  WitnessGenesisUTxOKey (GenesisUTxOSigningKey sk) -> ShelleyNormalSigningKey sk
  WitnessGenesisDelegateKey (GenesisDelegateSigningKey sk) -> ShelleyNormalSigningKey sk
  WitnessCommitteeColdKey (CommitteeColdSigningKey sk) -> ShelleyNormalSigningKey sk
  WitnessCommitteeHotKey (CommitteeHotSigningKey sk) -> ShelleyNormalSigningKey sk
  WitnessDRepKey (DRepSigningKey sk) -> ShelleyNormalSigningKey sk
  -- The cases for extended keys
  WitnessPaymentExtendedKey (PaymentExtendedSigningKey sk) -> ShelleyExtendedSigningKey sk
  WitnessStakeExtendedKey (StakeExtendedSigningKey sk) -> ShelleyExtendedSigningKey sk
  WitnessStakePoolExtendedKey (StakePoolExtendedSigningKey sk) -> ShelleyExtendedSigningKey sk
  WitnessGenesisExtendedKey (GenesisExtendedSigningKey sk) -> ShelleyExtendedSigningKey sk
  WitnessGenesisDelegateExtendedKey (GenesisDelegateExtendedSigningKey sk) -> ShelleyExtendedSigningKey sk
  WitnessCommitteeColdExtendedKey (CommitteeColdExtendedSigningKey sk) -> ShelleyExtendedSigningKey sk
  WitnessCommitteeHotExtendedKey (CommitteeHotExtendedSigningKey sk) -> ShelleyExtendedSigningKey sk
  WitnessDRepExtendedKey (DRepExtendedSigningKey sk) -> ShelleyExtendedSigningKey sk

getShelleyKeyWitnessVerificationKey
  :: ShelleySigningKey
  -> Shelley.VKey Shelley.Witness
getShelleyKeyWitnessVerificationKey (ShelleyNormalSigningKey sk) =
  ( Shelley.asWitness
      :: Shelley.VKey Shelley.Payment
      -> Shelley.VKey Shelley.Witness
  )
    . (\(PaymentVerificationKey vk) -> vk)
    . getVerificationKey
    . PaymentSigningKey
    $ sk
getShelleyKeyWitnessVerificationKey (ShelleyExtendedSigningKey sk) =
  ( Shelley.asWitness
      :: Shelley.VKey Shelley.Payment
      -> Shelley.VKey Shelley.Witness
  )
    . (\(PaymentVerificationKey vk) -> vk)
    . ( castVerificationKey
          :: VerificationKey PaymentExtendedKey
          -> VerificationKey PaymentKey
      )
    . getVerificationKey
    . PaymentExtendedSigningKey
    $ sk
