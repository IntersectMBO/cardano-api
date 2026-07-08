{-# LANGUAGE CPP #-}

module Cardano.Wasi.Internal.Api.Tx
  ( newTx
  , newUpcomingEraTx
  , addTxInput
  , addSimpleTxOut
  , appendCertificateToTx
  , setFee
  , estimateMinFee
  , signWithPaymentKey
  , signWithStakeKey
  , alsoSignWithPaymentKey
  , alsoSignWithStakeKey
  , toCbor
  , getUnsignedTxId
  , getSignedTxId
  )
where

import Cardano.Api qualified as Api

import Cardano.Wasi.Internal.Conversion
  ( cstrToInt
  , fromCJSON
  , intToCStr
  , stringToSigningKey
  , toCJSON
  , txIdToString
  )
import Cardano.Wasm.Api.Tx

import Control.Monad (join)

import Foreign.C (CString)
import Foreign.C.String (newCString, peekCString)

-- * UnsignedTxObject

#if defined(wasm32_HOST_ARCH)

foreign export ccall "newTx"
  newTx :: IO UnsignedTxObjectJSON

foreign export ccall "newUpcomingEraTx"
  newUpcomingEraTx :: IO UnsignedTxObjectJSON

foreign export ccall "addTxInput"
  addTxInput :: UnsignedTxObjectJSON -> CString -> Int -> IO UnsignedTxObjectJSON

foreign export ccall "addSimpleTxOut"
  addSimpleTxOut :: UnsignedTxObjectJSON -> CString -> CString -> IO UnsignedTxObjectJSON

foreign export ccall "appendCertificateToTx"
  appendCertificateToTx :: UnsignedTxObjectJSON -> CString -> IO UnsignedTxObjectJSON

foreign export ccall "setFee"
  setFee :: UnsignedTxObjectJSON -> CString -> IO UnsignedTxObjectJSON

foreign export ccall "estimateMinFee"
  estimateMinFee :: UnsignedTxObjectJSON -> CString -> Int -> Int -> Int -> IO CString

foreign export ccall "signWithPaymentKey"
  signWithPaymentKey :: UnsignedTxObjectJSON -> CString -> IO SignedTxObjectJSON

foreign export ccall "signWithStakeKey"
  signWithStakeKey :: UnsignedTxObjectJSON -> CString -> IO SignedTxObjectJSON

foreign export ccall "getUnsignedTxId"
  getUnsignedTxId :: UnsignedTxObjectJSON -> IO CString

#endif

type UnsignedTxObjectJSON = CString

newTx :: IO UnsignedTxObjectJSON
newTx = toCJSON newTxImpl

newUpcomingEraTx :: IO UnsignedTxObjectJSON
newUpcomingEraTx = toCJSON =<< newUpcomingEraTxImpl

addTxInput :: UnsignedTxObjectJSON -> CString -> Int -> IO UnsignedTxObjectJSON
addTxInput unsignedTxObject txId txIx =
  toCJSON
    =<< ( addTxInputImpl
            <$> fromCJSON True "UnsignedTx" unsignedTxObject
            <*> txIdToString txId
            <*> pure (Api.TxIx (fromIntegral txIx))
        )

addSimpleTxOut :: UnsignedTxObjectJSON -> CString -> CString -> IO UnsignedTxObjectJSON
addSimpleTxOut unsignedTxObject destAddr lovelaceAmountStr =
  toCJSON
    =<< join
      ( addSimpleTxOutImpl
          <$> fromCJSON True "UnsignedTx" unsignedTxObject
          <*> peekCString destAddr
          <*> (Api.Coin <$> cstrToInt "Lovelace amount" lovelaceAmountStr)
      )

appendCertificateToTx :: UnsignedTxObjectJSON -> CString -> IO UnsignedTxObjectJSON
appendCertificateToTx unsignedTxObject certCborStr =
  toCJSON
    =<< join
      ( appendCertificateToTxImpl
          <$> fromCJSON True "UnsignedTx" unsignedTxObject
          <*> peekCString certCborStr
      )

setFee :: UnsignedTxObjectJSON -> CString -> IO UnsignedTxObjectJSON
setFee unsignedTxObject feeStr =
  toCJSON
    =<< ( setFeeImpl
            <$> fromCJSON True "UnsignedTx" unsignedTxObject
            <*> (Api.Coin <$> cstrToInt "fee" feeStr)
        )

estimateMinFee :: UnsignedTxObjectJSON -> CString -> Int -> Int -> Int -> IO CString
estimateMinFee ptrUnsignedTxObject pparams numExtraKeyWitnesses numExtraByronKeyWitnesses totalRefScriptSize = do
  (intToCStr . Api.unCoin)
    =<< join
      ( estimateMinFeeImpl
          <$> fromCJSON False "UnsignedTx" ptrUnsignedTxObject
          <*> (ProtocolParamsJSON <$> fromCJSON False "ProtocolParameters" pparams)
          <*> pure (fromIntegral numExtraKeyWitnesses)
          <*> pure (fromIntegral numExtraByronKeyWitnesses)
          <*> pure (fromIntegral totalRefScriptSize)
      )

signWithPaymentKey :: UnsignedTxObjectJSON -> CString -> IO SignedTxObjectJSON
signWithPaymentKey unsignedTxObject signingKeyBech32 =
  toCJSON
    =<< ( signWithPaymentKeyImpl
            <$> fromCJSON True "UnsignedTx" unsignedTxObject
            <*> stringToSigningKey signingKeyBech32
        )

signWithStakeKey :: UnsignedTxObjectJSON -> CString -> IO SignedTxObjectJSON
signWithStakeKey unsignedTxObject signingKeyBech32 =
  toCJSON
    =<< ( signWithStakeKeyImpl
            <$> fromCJSON True "UnsignedTx" unsignedTxObject
            <*> stringToSigningKey signingKeyBech32
        )

getUnsignedTxId :: UnsignedTxObjectJSON -> IO CString
getUnsignedTxId unsignedTxObject =
  newCString . getUnsignedTxIdImpl
    =<< fromCJSON False "UnsignedTx" unsignedTxObject

-- * SignedTxObject

#if defined(wasm32_HOST_ARCH)

foreign export ccall "alsoSignWithPaymentKey"
  alsoSignWithPaymentKey :: SignedTxObjectJSON -> CString -> IO SignedTxObjectJSON

foreign export ccall "alsoSignWithStakeKey"
  alsoSignWithStakeKey :: SignedTxObjectJSON -> CString -> IO SignedTxObjectJSON

foreign export ccall "toCbor"
  toCbor :: SignedTxObjectJSON -> IO CString

foreign export ccall "getSignedTxId"
  getSignedTxId :: SignedTxObjectJSON -> IO CString

#endif

type SignedTxObjectJSON = CString

alsoSignWithPaymentKey :: SignedTxObjectJSON -> CString -> IO SignedTxObjectJSON
alsoSignWithPaymentKey signedTxObject signingKeyBech32 =
  toCJSON
    =<< ( alsoSignWithPaymentKeyImpl
            <$> fromCJSON True "SignedTx" signedTxObject
            <*> stringToSigningKey signingKeyBech32
        )

alsoSignWithStakeKey :: SignedTxObjectJSON -> CString -> IO SignedTxObjectJSON
alsoSignWithStakeKey signedTxObject signingKeyBech32 =
  toCJSON
    =<< ( alsoSignWithStakeKeyImpl
            <$> fromCJSON True "SignedTx" signedTxObject
            <*> stringToSigningKey signingKeyBech32
        )

toCbor :: SignedTxObjectJSON -> IO CString
toCbor signedTxObject =
  newCString . toCborImpl
    =<< fromCJSON False "SignedTx" signedTxObject

getSignedTxId :: SignedTxObjectJSON -> IO CString
getSignedTxId signedTxObject =
  newCString . getSignedTxIdImpl
    =<< fromCJSON False "SignedTx" signedTxObject
