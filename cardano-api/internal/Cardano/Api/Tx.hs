{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

-- The Shelley ledger uses promoted data kinds which we have to use, but we do
-- not export any from this API. We also use them unticked as nature intended.
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

{- HLINT ignore "Avoid lambda using `infix`" -}

-- | Complete, signed transactions
--
module Cardano.Api.Tx (

    -- * Signing transactions
    -- | Creating transaction witnesses one by one, or all in one go.
    Tx(.., Tx),
    getTxBody,
    getTxWitnesses,
    ScriptValidity(..),

    -- ** Signing in one go
    ShelleySigningKey(..),
    toShelleySigningKey,
    signByronTransaction,
    signShelleyTransaction,

    -- ** Incremental signing and separate witnesses
    makeSignedTransaction,
    KeyWitness(..),
    makeByronKeyWitness,
    ShelleyWitnessSigningKey(..),
    makeShelleyKeyWitness,
    WitnessNetworkIdOrByronAddress (..),
    makeShelleyBootstrapWitness,
    makeShelleySignature,
    getShelleyKeyWitnessVerificationKey,
    getTxBodyAndWitnesses,

    -- * Data family instances
    AsType(AsTx, AsByronTx, AsShelleyTx, AsMaryTx, AsAllegraTx, AsAlonzoTx,
           AsKeyWitness, AsByronWitness, AsShelleyWitness),
  ) where

import           Cardano.Api.Address
import           Cardano.Api.Certificate
import           Cardano.Api.Eon.ShelleyBasedEra
import           Cardano.Api.Eras
import           Cardano.Api.Eras.Constraints
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.Keys.Byron
import           Cardano.Api.Keys.Class
import           Cardano.Api.Keys.Shelley
import           Cardano.Api.NetworkId
import           Cardano.Api.SerialiseCBOR
import           Cardano.Api.SerialiseTextEnvelope
import           Cardano.Api.TxBody

import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Chain.UTxO as Byron
import qualified Cardano.Crypto.DSIGN.Class as Crypto
import qualified Cardano.Crypto.Hashing as Byron
import qualified Cardano.Crypto.ProtocolMagic as Byron
import qualified Cardano.Crypto.Signing as Byron
import qualified Cardano.Crypto.Util as Crypto
import qualified Cardano.Crypto.Wallet as Crypto.HD
import qualified Cardano.Ledger.Alonzo.Core as L
import qualified Cardano.Ledger.Api as L
import           Cardano.Ledger.BaseTypes (maybeToStrictMaybe, strictMaybeToMaybe)
import           Cardano.Ledger.Binary (Annotated (..))
import qualified Cardano.Ledger.Binary as CBOR
import qualified Cardano.Ledger.Binary.Plain as Plain
import qualified Cardano.Ledger.Core as Ledger
import           Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Keys as Shelley
import qualified Cardano.Ledger.Keys.Bootstrap as Shelley
import qualified Cardano.Ledger.SafeHash as Ledger

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Data.Set as Set
import           Data.Type.Equality (TestEquality (..), (:~:) (Refl))
import qualified Data.Vector as Vector
import           Lens.Micro

-- ----------------------------------------------------------------------------
-- Signed transactions
--

data Tx era where

     ByronTx
       :: Byron.ATxAux ByteString
       -> Tx ByronEra

     ShelleyTx
       :: ShelleyBasedEra era
       -> L.Tx (ShelleyLedgerEra era)
       -> Tx era


instance Show (InAnyCardanoEra Tx) where
    show (InAnyCardanoEra _ tx) = show tx

instance Eq (InAnyCardanoEra Tx) where
    (==) (InAnyCardanoEra eraA txA) (InAnyCardanoEra eraB txB) =
      case testEquality eraA eraB of
        Nothing -> False
        Just Refl -> txA == txB

-- The GADT in the ShelleyTx case requires a custom instance
instance Eq (Tx era) where
    (==) (ByronTx txA)
         (ByronTx txB) = txA == txB

    (==) (ShelleyTx sbe txA)
         (ShelleyTx _   txB) =
      shelleyBasedEraConstraints sbe $ txA == txB

    (==) ByronTx{} (ShelleyTx sbe _) = case sbe of {}
    (==) (ShelleyTx sbe _) ByronTx{} = case sbe of {}

-- The GADT in the ShelleyTx case requires a custom instance
instance Show (Tx era) where
    showsPrec p (ByronTx tx) =
      showParen (p >= 11) $
        showString "ByronTx "
      . showsPrec 11 tx

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

instance HasTypeProxy era => HasTypeProxy (Tx era) where
    data AsType (Tx era) = AsTx (AsType era)
    proxyToAsType _ = AsTx (proxyToAsType (Proxy :: Proxy era))

{-# DEPRECATED AsByronTx "Use AsTx AsByronEra instead." #-}
pattern AsByronTx :: AsType (Tx ByronEra)
pattern AsByronTx = AsTx AsByronEra
{-# COMPLETE AsByronTx #-}

{-# DEPRECATED AsShelleyTx "Use AsTx AsShelleyEra instead." #-}
pattern AsShelleyTx :: AsType (Tx ShelleyEra)
pattern AsShelleyTx = AsTx AsShelleyEra
{-# COMPLETE AsShelleyTx #-}

pattern AsMaryTx :: AsType (Tx MaryEra)
pattern AsMaryTx = AsTx AsMaryEra
{-# COMPLETE AsMaryTx #-}

pattern AsAllegraTx :: AsType (Tx AllegraEra)
pattern AsAllegraTx = AsTx AsAllegraEra
{-# COMPLETE AsAllegraTx #-}

pattern AsAlonzoTx :: AsType (Tx AlonzoEra)
pattern AsAlonzoTx = AsTx AsAlonzoEra
{-# COMPLETE AsAlonzoTx #-}

instance IsCardanoEra era => SerialiseAsCBOR (Tx era) where
    serialiseToCBOR (ByronTx tx) = CBOR.recoverBytes tx

    serialiseToCBOR (ShelleyTx sbe tx) =
      shelleyBasedEraConstraints sbe $ serialiseShelleyBasedTx tx

    deserialiseFromCBOR _ bs =
      case cardanoEra :: CardanoEra era of
        ByronEra ->
          ByronTx <$>
            CBOR.decodeFullAnnotatedBytes
              CBOR.byronProtVer "Byron Tx" CBOR.decCBOR (LBS.fromStrict bs)

        -- Use the same derialisation impl, but at different types:
        ShelleyEra -> deserialiseShelleyBasedTx
                        (ShelleyTx ShelleyBasedEraShelley) bs
        AllegraEra -> deserialiseShelleyBasedTx
                        (ShelleyTx ShelleyBasedEraAllegra) bs
        MaryEra    -> deserialiseShelleyBasedTx
                        (ShelleyTx ShelleyBasedEraMary) bs
        AlonzoEra  -> deserialiseShelleyBasedTx
                        (ShelleyTx ShelleyBasedEraAlonzo) bs
        BabbageEra -> deserialiseShelleyBasedTx
                        (ShelleyTx ShelleyBasedEraBabbage) bs
        ConwayEra  -> deserialiseShelleyBasedTx
                        (ShelleyTx ShelleyBasedEraConway) bs

-- | The serialisation format for the different Shelley-based eras are not the
-- same, but they can be handled generally with one overloaded implementation.
--
serialiseShelleyBasedTx :: forall ledgerera .
                           L.EraTx ledgerera
                        => L.Tx ledgerera
                        -> ByteString
serialiseShelleyBasedTx = Plain.serialize'

deserialiseShelleyBasedTx :: forall ledgerera tx' .
                             L.EraTx ledgerera
                          => (L.Tx ledgerera -> tx')
                          -> ByteString
                          -> Either CBOR.DecoderError tx'
deserialiseShelleyBasedTx mkTx bs =
    mkTx <$> CBOR.decodeFullAnnotator
               (L.eraProtVerLow @ledgerera) "Shelley Tx" CBOR.decCBOR (LBS.fromStrict bs)


instance IsCardanoEra era => HasTextEnvelope (Tx era) where
    textEnvelopeType _ =
      case cardanoEra :: CardanoEra era of
        ByronEra   -> "TxSignedByron"
        ShelleyEra -> "TxSignedShelley"
        AllegraEra -> "Tx AllegraEra"
        MaryEra    -> "Tx MaryEra"
        AlonzoEra  -> "Tx AlonzoEra"
        BabbageEra -> "Tx BabbageEra"
        ConwayEra  -> "Tx ConwayEra"

data KeyWitness era where

     ByronKeyWitness
       :: Byron.TxInWitness
       -> KeyWitness ByronEra

     ShelleyBootstrapWitness
       :: ShelleyBasedEra era
       -> Shelley.BootstrapWitness StandardCrypto
       -> KeyWitness era

     ShelleyKeyWitness
       :: ShelleyBasedEra era
       -> L.WitVKey Shelley.Witness StandardCrypto
       -> KeyWitness era


-- The GADT in the Shelley cases requires a custom instance
instance Eq (KeyWitness era) where
    (==) (ByronKeyWitness wA)
         (ByronKeyWitness wB) = wA == wB

    (==) (ShelleyBootstrapWitness sbe wA)
         (ShelleyBootstrapWitness _   wB) =
      shelleyBasedEraConstraints sbe $ wA == wB

    (==) (ShelleyKeyWitness sbe wA)
         (ShelleyKeyWitness _   wB) =
      shelleyBasedEraConstraints sbe $ wA == wB

    (==) _ _ = False

-- The GADT in the ShelleyTx case requires a custom instance
--TODO: once we start providing custom patterns we should do the show in terms
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

instance HasTypeProxy era => HasTypeProxy (KeyWitness era) where
    data AsType (KeyWitness era) = AsKeyWitness (AsType era)
    proxyToAsType _ = AsKeyWitness (proxyToAsType (Proxy :: Proxy era))

pattern AsByronWitness :: AsType (KeyWitness ByronEra)
pattern AsByronWitness   = AsKeyWitness AsByronEra
{-# COMPLETE AsByronWitness #-}

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
        MaryEra    -> decodeShelleyBasedWitness ShelleyBasedEraMary    bs
        AlonzoEra  -> decodeShelleyBasedWitness ShelleyBasedEraAlonzo  bs
        BabbageEra -> decodeShelleyBasedWitness ShelleyBasedEraBabbage bs
        ConwayEra  -> decodeShelleyBasedWitness ShelleyBasedEraConway bs


encodeShelleyBasedKeyWitness :: CBOR.EncCBOR w => w -> CBOR.Encoding
encodeShelleyBasedKeyWitness wit =
    CBOR.encodeListLen 2 <> CBOR.encodeWord 0 <> CBOR.encCBOR wit

encodeShelleyBasedBootstrapWitness :: CBOR.EncCBOR w => w -> CBOR.Encoding
encodeShelleyBasedBootstrapWitness wit =
    CBOR.encodeListLen 2 <> CBOR.encodeWord 1 <> CBOR.encCBOR wit

decodeShelleyBasedWitness :: forall era.
                             L.Era (ShelleyLedgerEra era)
                          => ShelleyBasedEra era
                          -> ByteString
                          -> Either CBOR.DecoderError (KeyWitness era)
decodeShelleyBasedWitness sbe =
    CBOR.decodeFullAnnotator (L.eraProtVerLow @(ShelleyLedgerEra era))
      "Shelley Witness" decode
    . LBS.fromStrict
  where
    decode :: CBOR.Decoder s (CBOR.Annotator (KeyWitness era))
    decode =  do
      CBOR.decodeListLenOf 2
      t <- CBOR.decodeWord
      case t of
        0 -> fmap (fmap (ShelleyKeyWitness sbe)) CBOR.decCBOR
        1 -> fmap (fmap (ShelleyBootstrapWitness sbe)) CBOR.decCBOR
        _ -> CBOR.cborError $ CBOR.DecoderErrorUnknownTag
                                "Shelley Witness" (fromIntegral t)


instance IsCardanoEra era => HasTextEnvelope (KeyWitness era) where
    textEnvelopeType _ =
      case cardanoEra :: CardanoEra era of
        ByronEra   -> "TxWitnessByron"
        ShelleyEra -> "TxWitness ShelleyEra"
        AllegraEra -> "TxWitness AllegraEra"
        MaryEra    -> "TxWitness MaryEra"
        AlonzoEra  -> "TxWitness AlonzoEra"
        BabbageEra -> "TxWitness BabbageEra"
        ConwayEra  -> "TxWitness ConwayEra"

pattern Tx :: TxBody era -> [KeyWitness era] -> Tx era
pattern Tx txbody ws <- (getTxBodyAndWitnesses -> (txbody, ws))
  where
    Tx txbody ws = makeSignedTransaction ws txbody
{-# COMPLETE Tx #-}

getTxBodyAndWitnesses :: Tx era -> (TxBody era, [KeyWitness era])
getTxBodyAndWitnesses tx = (getTxBody tx, getTxWitnesses tx)

getTxBody :: forall era. Tx era -> TxBody era
getTxBody (ByronTx Byron.ATxAux { Byron.aTaTx = txbody }) =
    ByronTxBody txbody

getTxBody (ShelleyTx sbe tx) =
  caseShelleyToMaryOrAlonzoEraOnwards
    ( const $
        let txBody     = tx ^. L.bodyTxL
            txAuxData  = tx ^. L.auxDataTxL
            scriptWits = tx ^. L.witsTxL . L.scriptTxWitsL
        in ShelleyTxBody sbe txBody
            (Map.elems scriptWits)
            TxBodyNoScriptData
            (strictMaybeToMaybe txAuxData)
            TxScriptValidityNone
    )
    (\w ->
      let txBody       = tx ^. L.bodyTxL
          txAuxData    = tx ^. L.auxDataTxL
          scriptWits   = tx ^. L.witsTxL . L.scriptTxWitsL
          datsWits     = tx ^. L.witsTxL . L.datsTxWitsL
          redeemerWits = tx ^. L.witsTxL . L.rdmrsTxWitsL
          isValid      = tx ^. L.isValidTxL
      in ShelleyTxBody sbe txBody
          (Map.elems scriptWits)
          (TxBodyScriptData w datsWits redeemerWits)
          (strictMaybeToMaybe txAuxData)
          (TxScriptValidity w (isValidToScriptValidity isValid))
    )
    sbe


getTxWitnesses :: forall era. Tx era -> [KeyWitness era]
getTxWitnesses (ByronTx Byron.ATxAux { Byron.aTaWitness = witnesses }) =
    map ByronKeyWitness
  . Vector.toList
  . unAnnotated
  $ witnesses

getTxWitnesses (ShelleyTx sbe tx') =
  caseShelleyToMaryOrAlonzoEraOnwards
    (const (getShelleyTxWitnesses tx'))
    (const (getAlonzoTxWitnesses  tx'))
    sbe
  where
    getShelleyTxWitnesses :: forall ledgerera.
                             L.EraTx ledgerera
                          => L.EraCrypto ledgerera ~ StandardCrypto
                          => L.Tx ledgerera
                          -> [KeyWitness era]
    getShelleyTxWitnesses tx =
        map (ShelleyBootstrapWitness sbe) (Set.elems (tx ^. L.witsTxL . L.bootAddrTxWitsL))
     ++ map (ShelleyKeyWitness       sbe) (Set.elems (tx ^. L.witsTxL . L.addrTxWitsL))

    getAlonzoTxWitnesses :: forall ledgerera.
                            L.EraCrypto ledgerera ~ StandardCrypto
                         => L.EraTx ledgerera
                         => L.Tx ledgerera
                         -> [KeyWitness era]
    getAlonzoTxWitnesses = getShelleyTxWitnesses

makeSignedTransaction :: forall era.
     [KeyWitness era]
  -> TxBody era
  -> Tx era
makeSignedTransaction witnesses (ByronTxBody txbody) =
    ByronTx
  . Byron.annotateTxAux
  $ Byron.mkTxAux
      (unAnnotated txbody)
      (Vector.fromList [ w | ByronKeyWitness w <- witnesses ])

makeSignedTransaction witnesses (ShelleyTxBody sbe txbody
                                               txscripts
                                               txscriptdata
                                               txmetadata
                                               scriptValidity
                                               ) =
    case sbe of
      ShelleyBasedEraShelley -> shelleySignedTransaction
      ShelleyBasedEraAllegra -> shelleySignedTransaction
      ShelleyBasedEraMary    -> shelleySignedTransaction
      ShelleyBasedEraAlonzo  -> alonzoSignedTransaction
      ShelleyBasedEraBabbage -> alonzoSignedTransaction
      ShelleyBasedEraConway  -> alonzoSignedTransaction
  where
    txCommon
      :: forall ledgerera.
         ShelleyLedgerEra era ~ ledgerera
      => L.EraCrypto ledgerera ~ StandardCrypto
      => L.EraTx ledgerera
      => L.Tx ledgerera
    txCommon =
        L.mkBasicTx txbody
          & L.witsTxL .~
            (L.mkBasicTxWits
              & L.addrTxWitsL .~ Set.fromList [ w | ShelleyKeyWitness _ w <- witnesses ]
              & L.scriptTxWitsL .~
                Map.fromList [ (Ledger.hashScript @ledgerera sw, sw)
                             | sw <- txscripts ]
              & L.bootAddrTxWitsL .~
                Set.fromList [ w | ShelleyBootstrapWitness _ w <- witnesses ]
            )
          & L.auxDataTxL .~ maybeToStrictMaybe txmetadata

    shelleySignedTransaction
      :: forall ledgerera.
         ShelleyLedgerEra era ~ ledgerera
      => Ledger.EraCrypto ledgerera ~ StandardCrypto
      => Ledger.EraTx ledgerera
      => Tx era
    shelleySignedTransaction = ShelleyTx sbe txCommon

    alonzoSignedTransaction
      :: forall ledgerera.
         ShelleyLedgerEra era ~ ledgerera
      => Ledger.EraCrypto ledgerera ~ StandardCrypto
      => L.AlonzoEraTx ledgerera
      => Tx era
    alonzoSignedTransaction =
      ShelleyTx sbe
        (txCommon
         & L.witsTxL . L.datsTxWitsL .~ datums
         & L.witsTxL . L.rdmrsTxWitsL .~ redeemers
         & L.isValidTxL .~ txScriptValidityToIsValid scriptValidity)
      where
        (datums, redeemers) =
          case txscriptdata of
            TxBodyScriptData _ ds rs -> (ds, rs)
            TxBodyNoScriptData       -> (mempty, L.Redeemers mempty)

makeByronKeyWitness :: forall key.
                       IsByronKey key
                    => NetworkId
                    -> TxBody ByronEra
                    -> SigningKey key
                    -> KeyWitness ByronEra
makeByronKeyWitness _ (ShelleyTxBody sbe _ _ _ _ _) = case sbe of {}
makeByronKeyWitness nw (ByronTxBody txbody) =
    let txhash :: Byron.Hash Byron.Tx
        txhash = Byron.hashDecoded txbody

        pm :: Byron.ProtocolMagicId
        pm = toByronProtocolMagicId nw

        -- To allow sharing of the txhash computation across many signatures we
        -- define and share the txhash outside the lambda for the signing key:
     in case byronKeyFormat :: ByronKeyFormat key of
          ByronLegacyKeyFormat ->
            \(ByronSigningKeyLegacy sk) -> witness sk pm txhash
          ByronModernKeyFormat ->
            \(ByronSigningKey sk) -> witness sk pm txhash
 where
   witness :: Byron.SigningKey
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
  = WitnessNetworkId !NetworkId
  -- ^ Network ID.
  --
  -- If this value is used in the construction of a Shelley bootstrap witness,
  -- the result will not consist of a derivation path. If that is required,
  -- specify a 'WitnessByronAddress' value instead.
  | WitnessByronAddress !(Address ByronAddr)
  -- ^ Byron address.
  --
  -- If this value is used in the construction of a Shelley bootstrap witness,
  -- both the network ID and derivation path will be extracted from the
  -- address and used in the construction of the witness.

makeShelleyBootstrapWitness :: forall era.
                               IsShelleyBasedEra era
                            => WitnessNetworkIdOrByronAddress
                            -> TxBody era
                            -> SigningKey ByronKey
                            -> KeyWitness era
makeShelleyBootstrapWitness _ ByronTxBody{} _ =
    case shelleyBasedEra :: ShelleyBasedEra era of {}

makeShelleyBootstrapWitness nwOrAddr (ShelleyTxBody sbe txbody _ _ _ _) sk =
  shelleyBasedEraConstraints sbe $
    makeShelleyBasedBootstrapWitness sbe nwOrAddr txbody sk

makeShelleyBasedBootstrapWitness :: forall era.
                                    (Ledger.HashAnnotated
                                       (Ledger.TxBody (ShelleyLedgerEra era))
                                       Ledger.EraIndependentTxBody
                                       StandardCrypto)
                                 => ShelleyBasedEra era
                                 -> WitnessNetworkIdOrByronAddress
                                 -> Ledger.TxBody (ShelleyLedgerEra era)
                                 -> SigningKey ByronKey
                                 -> KeyWitness era
makeShelleyBasedBootstrapWitness sbe nwOrAddr txbody (ByronSigningKey sk) =
    ShelleyBootstrapWitness sbe $
      -- Byron era witnesses were weird. This reveals all that weirdness.
      Shelley.BootstrapWitness {
        Shelley.bwKey        = vk,
        Shelley.bwSig        = signature,
        Shelley.bwChainCode  = chainCode,
        Shelley.bwAttributes = attributes
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
    signature :: Shelley.SignedDSIGN StandardCrypto
                  (Shelley.Hash StandardCrypto Ledger.EraIndependentTxBody)
    signature = makeShelleySignature
                  txhash
                  -- Make the signature with the extended key directly:
                  (ShelleyExtendedSigningKey (Byron.unSigningKey sk))

    txhash :: Shelley.Hash StandardCrypto Ledger.EraIndependentTxBody
    txhash = Ledger.extractHash (Ledger.hashAnnotated txbody)
    --TODO: use Shelley.eraIndTxBodyHash txbody once that function has a
    -- suitably general type.

    -- And finally we need to provide the extra suffix bytes necessary to
    -- reconstruct the mini-Merkel tree that is a Byron address. The suffix
    -- bytes are the serialised address attributes.
    attributes =
      Plain.serialize' $
        Byron.mkAttributes Byron.AddrAttributes {
          Byron.aaVKDerivationPath = derivationPath,
          Byron.aaNetworkMagic     = networkMagic
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


data ShelleyWitnessSigningKey =
       WitnessPaymentKey         (SigningKey PaymentKey)
     | WitnessPaymentExtendedKey (SigningKey PaymentExtendedKey)
     | WitnessStakeKey           (SigningKey StakeKey)
     | WitnessStakeExtendedKey   (SigningKey StakeExtendedKey)
     | WitnessStakePoolKey       (SigningKey StakePoolKey)
     | WitnessGenesisKey         (SigningKey GenesisKey)
     | WitnessGenesisExtendedKey (SigningKey GenesisExtendedKey)
     | WitnessGenesisDelegateKey (SigningKey GenesisDelegateKey)
     | WitnessGenesisDelegateExtendedKey
                                 (SigningKey GenesisDelegateExtendedKey)
     | WitnessGenesisUTxOKey     (SigningKey GenesisUTxOKey)
     | WitnessCommitteeColdKey   (SigningKey CommitteeColdKey)


makeShelleyKeyWitness :: forall era
                      .  IsShelleyBasedEra era
                      => TxBody era
                      -> ShelleyWitnessSigningKey
                      -> KeyWitness era
makeShelleyKeyWitness (ShelleyTxBody sbe txbody _ _ _ _) =
  shelleyBasedEraConstraints sbe $ makeShelleyBasedKeyWitness txbody
  where
    makeShelleyBasedKeyWitness :: Ledger.HashAnnotated (Ledger.TxBody (ShelleyLedgerEra era)) Ledger.EraIndependentTxBody StandardCrypto
                               => Ledger.TxBody (ShelleyLedgerEra era)
                               -> ShelleyWitnessSigningKey
                               -> KeyWitness era
    makeShelleyBasedKeyWitness txbody' =

     let txhash :: Shelley.Hash StandardCrypto Ledger.EraIndependentTxBody
         txhash = Ledger.extractHash @StandardCrypto (Ledger.hashAnnotated txbody')

        -- To allow sharing of the txhash computation across many signatures we
        -- define and share the txhash outside the lambda for the signing key:
     in \wsk ->
        let sk        = toShelleySigningKey wsk
            vk        = getShelleyKeyWitnessVerificationKey sk
            signature = makeShelleySignature txhash sk
         in ShelleyKeyWitness sbe $
              L.WitVKey vk signature

makeShelleyKeyWitness ByronTxBody{} =
    case shelleyBasedEra :: ShelleyBasedEra era of {}


-- | We support making key witnesses with both normal and extended signing keys.
--
data ShelleySigningKey =
       -- | A normal ed25519 signing key
       ShelleyNormalSigningKey   (Shelley.SignKeyDSIGN StandardCrypto)

       -- | An extended ed25519 signing key
     | ShelleyExtendedSigningKey Crypto.HD.XPrv


toShelleySigningKey :: ShelleyWitnessSigningKey -> ShelleySigningKey
toShelleySigningKey key = case key of
  WitnessPaymentKey     (PaymentSigningKey     sk) -> ShelleyNormalSigningKey sk
  WitnessStakeKey       (StakeSigningKey       sk) -> ShelleyNormalSigningKey sk
  WitnessStakePoolKey   (StakePoolSigningKey   sk) -> ShelleyNormalSigningKey sk
  WitnessGenesisKey     (GenesisSigningKey     sk) -> ShelleyNormalSigningKey sk
  WitnessGenesisUTxOKey (GenesisUTxOSigningKey sk) -> ShelleyNormalSigningKey sk
  WitnessGenesisDelegateKey (GenesisDelegateSigningKey sk) ->
    ShelleyNormalSigningKey sk

  -- The cases for extended keys
  WitnessPaymentExtendedKey (PaymentExtendedSigningKey sk) ->
    ShelleyExtendedSigningKey sk

  WitnessStakeExtendedKey (StakeExtendedSigningKey sk) ->
    ShelleyExtendedSigningKey sk

  WitnessGenesisExtendedKey (GenesisExtendedSigningKey sk) ->
    ShelleyExtendedSigningKey sk

  WitnessGenesisDelegateExtendedKey (GenesisDelegateExtendedSigningKey sk) ->
    ShelleyExtendedSigningKey sk

  WitnessCommitteeColdKey (CommitteeColdSigningKey sk) ->
    ShelleyNormalSigningKey sk


getShelleyKeyWitnessVerificationKey
  :: ShelleySigningKey
  -> Shelley.VKey Shelley.Witness StandardCrypto
getShelleyKeyWitnessVerificationKey (ShelleyNormalSigningKey sk) =
      (Shelley.coerceKeyRole :: Shelley.VKey Shelley.Payment StandardCrypto
                             -> Shelley.VKey Shelley.Witness StandardCrypto)
    . (\(PaymentVerificationKey vk) -> vk)
    . getVerificationKey
    . PaymentSigningKey
    $ sk

getShelleyKeyWitnessVerificationKey (ShelleyExtendedSigningKey sk) =
      (Shelley.coerceKeyRole :: Shelley.VKey Shelley.Payment StandardCrypto
                             -> Shelley.VKey Shelley.Witness StandardCrypto)
    . (\(PaymentVerificationKey vk) -> vk)
    . (castVerificationKey :: VerificationKey PaymentExtendedKey
                           -> VerificationKey PaymentKey)
    . getVerificationKey
    . PaymentExtendedSigningKey
    $ sk


makeShelleySignature
  :: Crypto.SignableRepresentation tosign
  => tosign
  -> ShelleySigningKey
  -> Shelley.SignedDSIGN StandardCrypto tosign
makeShelleySignature tosign (ShelleyNormalSigningKey sk) =
    Crypto.signedDSIGN () tosign sk

makeShelleySignature tosign (ShelleyExtendedSigningKey sk) =
    fromXSignature $
      Crypto.HD.sign
        BS.empty  -- passphrase for (unused) in-memory encryption
        sk
        (Crypto.getSignableRepresentation tosign)
  where
    fromXSignature :: Crypto.HD.XSignature
                   -> Shelley.SignedDSIGN StandardCrypto b
    fromXSignature =
        Crypto.SignedDSIGN
      . fromMaybe impossible
      . Crypto.rawDeserialiseSigDSIGN
      . Crypto.HD.unXSignature

    impossible =
      error "makeShelleyKeyWitnessSignature: byron and shelley signature sizes do not match"


-- order of signing keys must match txins
signByronTransaction :: NetworkId
                     -> TxBody ByronEra
                     -> [SigningKey ByronKey]
                     -> Tx ByronEra
signByronTransaction nw txbody sks =
    makeSignedTransaction witnesses txbody
  where
    witnesses = map (makeByronKeyWitness nw txbody) sks

-- signing keys is a set
signShelleyTransaction :: IsShelleyBasedEra era
                       => TxBody era
                       -> [ShelleyWitnessSigningKey]
                       -> Tx era
signShelleyTransaction txbody sks =
    makeSignedTransaction witnesses txbody
  where
    witnesses = map (makeShelleyKeyWitness txbody) sks

