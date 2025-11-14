{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wasm.Api.Tx
  ( UnsignedTxObject (..)
  , SignedTxObject (..)
  , ProtocolParamsJSON (..)
  , newTxImpl
  , newExperimentalEraTxImpl
  , newConwayTxImpl
  , addTxInputImpl
  , addSimpleTxOutImpl
  , appendCertificateToTxImpl
  , estimateMinFeeImpl
  , setFeeImpl
  , signWithPaymentKeyImpl
  , alsoSignWithPaymentKeyImpl
  , toCborImpl
  )
where

import Cardano.Api (FromJSON)
import Cardano.Api qualified as Api
import Cardano.Api.Experimental (obtainCommonConstraints)
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Ledger qualified as Ledger
import Cardano.Api.Plutus qualified as Shelley
import Cardano.Api.Tx qualified as TxBody

import Cardano.Ledger.Api qualified as Ledger
import Cardano.Wasm.ExceptionHandling (justOrError, rightOrError, toMonadFail)

import Control.Monad.Catch (MonadThrow)
import Data.Aeson (ToJSON (toJSON), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GHC.Stack (HasCallStack)
import Lens.Micro ((%~), (&), (.~), (<>~))

-- * @UnsignedTx@ object

-- | An object representing a transaction that is being built and has not
-- been signed yet. It abstracts over the era of the transaction.
-- It is meant to be an opaque object in the JavaScript API.
data UnsignedTxObject
  = forall era. UnsignedTxObject
  { unsignedTxEra :: Exp.Era era
  , unsignedTx :: Exp.UnsignedTx era
  }

deriving instance Show UnsignedTxObject

instance ToJSON UnsignedTxObject where
  toJSON :: UnsignedTxObject -> Aeson.Value
  toJSON (UnsignedTxObject era utx) =
    obtainCommonConstraints era $
      Aeson.object
        [ "era" .= Exp.Some era
        , "tx" .= Text.decodeUtf8 (Api.serialiseToRawBytesHex utx)
        ]

instance FromJSON UnsignedTxObject where
  parseJSON :: HasCallStack => Aeson.Value -> Aeson.Parser UnsignedTxObject
  parseJSON = Aeson.withObject "UnsignedTxObject" $ \o -> do
    Exp.Some era <- o Aeson..: "era"
    tx :: Text.Text <- o Aeson..: "tx"
    obtainCommonConstraints era $ do
      UnsignedTxObject
        era
        <$> toMonadFail (rightOrError $ Api.deserialiseFromRawBytesHex $ Text.encodeUtf8 tx)

-- | Create a new unsigned transaction object for making a transaction in the current era.
newTxImpl :: UnsignedTxObject
newTxImpl = newConwayTxImpl

-- | Create a new unsigned transaction object for making a transaction in the current experimental era.
newExperimentalEraTxImpl :: MonadThrow m => m UnsignedTxObject
newExperimentalEraTxImpl =
  return $ UnsignedTxObject Exp.DijkstraEra (Exp.UnsignedTx (Ledger.mkBasicTx Ledger.mkBasicTxBody))

-- | Create a new unsigned transaction object for making a Conway era transaction.
newConwayTxImpl :: UnsignedTxObject
newConwayTxImpl = UnsignedTxObject Exp.ConwayEra (Exp.UnsignedTx (Ledger.mkBasicTx Ledger.mkBasicTxBody))

-- | Add a simple transaction input to an unsigned transaction object.
addTxInputImpl :: UnsignedTxObject -> Api.TxId -> Api.TxIx -> UnsignedTxObject
addTxInputImpl (UnsignedTxObject era (Exp.UnsignedTx tx)) txId txIx =
  obtainCommonConstraints era $
    let txIn = Api.TxIn txId txIx
        tx' = tx & Ledger.bodyTxL . Ledger.inputsTxBodyL %~ (<> Set.fromList [TxBody.toShelleyTxIn txIn])
     in UnsignedTxObject era $ Exp.UnsignedTx tx'

-- | Add a simple transaction output to an unsigned transaction object.
-- It takes a destination address and an amount in lovelaces.
addSimpleTxOutImpl
  :: (HasCallStack, MonadThrow m) => UnsignedTxObject -> String -> Ledger.Coin -> m UnsignedTxObject
addSimpleTxOutImpl (UnsignedTxObject era (Exp.UnsignedTx tx)) destAddr lovelaceAmount =
  obtainCommonConstraints era $ do
    destAddress <- deserialiseAddress era destAddr
    let sbe = Api.convert era
        txOut =
          Api.TxOut
            destAddress
            (Api.lovelaceToTxOutValue sbe lovelaceAmount)
            Api.TxOutDatumNone
            Shelley.ReferenceScriptNone
        shelleyTxOut = TxBody.toShelleyTxOutAny sbe txOut
        tx' = tx & Ledger.bodyTxL . Ledger.outputsTxBodyL %~ (<> StrictSeq.fromList [shelleyTxOut])
    return $ UnsignedTxObject era $ Exp.UnsignedTx tx'
 where
  deserialiseAddress
    :: (HasCallStack, MonadThrow m, Exp.EraCommonConstraints era)
    => Exp.Era era -> String -> m (Api.AddressInEra era)
  deserialiseAddress _eon destAddrStr =
    justOrError
      ("Couldn't deserialise destination address: " ++ show destAddrStr)
      $ Api.deserialiseAddress
        (Api.AsAddressInEra Api.asType)
        (Text.pack destAddrStr)

-- | Append a certificate (in CBOR hex string format) to an unsigned transaction object.
appendCertificateToTxImpl
  :: (HasCallStack, MonadThrow m) => UnsignedTxObject -> String -> m UnsignedTxObject
appendCertificateToTxImpl (UnsignedTxObject era (Exp.UnsignedTx tx)) certCbor = do
  Exp.Certificate cert <- deserialiseCertificate era certCbor
  let tx' = tx & Ledger.bodyTxL . Ledger.certsTxBodyL %~ (<> StrictSeq.fromList [cert])
  return $ UnsignedTxObject era $ Exp.UnsignedTx tx'
 where
  deserialiseCertificate
    :: (HasCallStack, MonadThrow m) => Exp.Era era -> String -> m (Exp.Certificate (Exp.LedgerEra era))
  deserialiseCertificate era' certCbor' =
    obtainCommonConstraints era' $
      rightOrError $
        Api.deserialiseFromCBOR Exp.AsCertificate (Text.encodeUtf8 $ Text.pack certCbor')

-- | Set the fee for an unsigned transaction object.
setFeeImpl :: UnsignedTxObject -> Ledger.Coin -> UnsignedTxObject
setFeeImpl (UnsignedTxObject era (Exp.UnsignedTx tx)) fee =
  obtainCommonConstraints era $
    let tx' = tx & Ledger.bodyTxL . Ledger.feeTxBodyL .~ fee
     in UnsignedTxObject era $ Exp.UnsignedTx tx'

-- | Sign an unsigned transaction using a payment key.
signWithPaymentKeyImpl
  :: UnsignedTxObject -> Api.SigningKey Api.PaymentKey -> SignedTxObject
signWithPaymentKeyImpl (UnsignedTxObject era fullUnsignedTx@(Exp.UnsignedTx tx)) signingKey =
  obtainCommonConstraints era $
    let witness = Exp.makeKeyWitness era fullUnsignedTx . Api.WitnessPaymentKey $ signingKey
        txWits =
          Ledger.mkBasicTxWits
            & Ledger.addrTxWitsL
              .~ Set.fromList [witness]
        txWithWits =
          obtainCommonConstraints
            era
            (tx & Ledger.witsTxL .~ txWits)
     in SignedTxObject
          era
          (Exp.SignedTx txWithWits)

newtype ProtocolParamsJSON = ProtocolParamsJSON String

-- | Estimate min fees for an unsigned transaction object.
estimateMinFeeImpl
  :: (HasCallStack, MonadThrow m)
  => UnsignedTxObject
  -- ^ The unsigned transaction object to estimate fees for.
  -> ProtocolParamsJSON
  -- ^ The JSON for the protocol parameters of the correct era and network.
  -> Int
  -- ^ The number of key witnesses still to be added to the transaction.
  -> Int
  -- ^ The number of Byron key witnesses still to be added to the transaction.
  -> Int
  -- ^ The total size in bytes of reference scripts
  -> m Ledger.Coin
estimateMinFeeImpl
  (UnsignedTxObject era (Exp.UnsignedTx ledgerTx))
  (ProtocolParamsJSON protocolParamsJson)
  numKeyWitnesses
  numByronKeyWitnesses
  totalRefScriptSize =
    obtainCommonConstraints era $ do
      protocolParams <- rightOrError $ Aeson.eitherDecodeStrictText (Text.pack protocolParamsJson)
      return $
        Ledger.estimateMinFeeTx
          protocolParams
          ledgerTx
          numKeyWitnesses
          numByronKeyWitnesses
          totalRefScriptSize

-- * @SignedTx@ object

-- | An object representing a signed transaction.
data SignedTxObject
  = forall era. SignedTxObject (Exp.Era era) (Exp.SignedTx era)

deriving instance Show SignedTxObject

instance ToJSON SignedTxObject where
  toJSON :: SignedTxObject -> Aeson.Value
  toJSON (SignedTxObject era tx) =
    obtainCommonConstraints era $
      Aeson.object
        [ "era" .= Exp.Some era
        , "tx" .= Text.decodeUtf8 (Api.serialiseToRawBytesHex tx)
        ]

instance FromJSON SignedTxObject where
  parseJSON :: HasCallStack => Aeson.Value -> Aeson.Parser SignedTxObject
  parseJSON = Aeson.withObject "SignedTxObject" $ \o -> do
    Exp.Some era <- o Aeson..: "era"
    tx :: Text.Text <- o Aeson..: "tx"
    obtainCommonConstraints era $ do
      decodedTx <- toMonadFail (rightOrError $ Api.deserialiseFromRawBytesHex $ Text.encodeUtf8 tx)
      return $
        SignedTxObject era decodedTx

-- | Add an extra signature to an already signed transaction using a payment key.
alsoSignWithPaymentKeyImpl
  :: SignedTxObject -> Api.SigningKey Api.PaymentKey -> SignedTxObject
alsoSignWithPaymentKeyImpl (SignedTxObject era (Exp.SignedTx tx)) signingKey =
  obtainCommonConstraints era $
    let witness = Exp.makeKeyWitness era (Exp.UnsignedTx tx) . Api.WitnessPaymentKey $ signingKey
        txWits =
          Ledger.mkBasicTxWits
            & Ledger.addrTxWitsL
              .~ Set.fromList [witness]
        txWithWits =
          obtainCommonConstraints
            era
            (tx & Ledger.witsTxL <>~ txWits)
     in SignedTxObject
          era
          (Exp.SignedTx txWithWits)

-- | Convert a signed transaction object to a base16 encoded string of its CBOR representation.
toCborImpl :: SignedTxObject -> String
toCborImpl (SignedTxObject era signedTx) =
  obtainCommonConstraints era $
    Text.unpack $
      Text.decodeUtf8 (Api.serialiseToRawBytesHex signedTx)
