{-# LANGUAGE CPP #-}

module Cardano.Wasi.Internal.Api.Tx
  ( newTx
  , newExperimentalEraTx
  , newConwayTx
  , addTxInput
  , addSimpleTxOut
  , setFee
  , estimateMinFee
  , signWithPaymentKey
  , alsoSignWithPaymentKey
  , toCbor
  )
where

import Cardano.Api qualified as Api

import Cardano.Wasi.Internal.Conversion
  ( cstrToInt
  , fromCJSON
  , intToCStr
  , stringTosigningKey
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

foreign export ccall "newExperimentalEraTx"
  newExperimentalEraTx :: IO UnsignedTxObjectJSON

foreign export ccall "newConwayTx"
  newConwayTx :: IO UnsignedTxObjectJSON

foreign export ccall "addTxInput"
  addTxInput :: UnsignedTxObjectJSON -> CString -> Int -> IO UnsignedTxObjectJSON

foreign export ccall "addSimpleTxOut"
  addSimpleTxOut :: UnsignedTxObjectJSON -> CString -> CString -> IO UnsignedTxObjectJSON

foreign export ccall "setFee"
  setFee :: UnsignedTxObjectJSON -> CString -> IO UnsignedTxObjectJSON

foreign export ccall "estimateMinFee"
  estimateMinFee :: UnsignedTxObjectJSON -> CString -> Int -> Int -> Int -> IO CString

foreign export ccall "signWithPaymentKey"
  signWithPaymentKey :: UnsignedTxObjectJSON -> CString -> IO SignedTxObjectJSON

#endif

type UnsignedTxObjectJSON = CString

newTx :: IO UnsignedTxObjectJSON
newTx = toCJSON newTxImpl

newExperimentalEraTx :: IO UnsignedTxObjectJSON
newExperimentalEraTx = toCJSON =<< newExperimentalEraTxImpl

newConwayTx :: IO UnsignedTxObjectJSON
newConwayTx = toCJSON newConwayTxImpl

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

setFee :: UnsignedTxObjectJSON -> CString -> IO UnsignedTxObjectJSON
setFee unsignedTxObject feeStr =
  toCJSON
    =<< ( setFeeImpl
            <$> fromCJSON True "UnsignedTx" unsignedTxObject
            <*> (Api.Coin <$> cstrToInt "fee" feeStr)
        )

estimateMinFee :: UnsignedTxObjectJSON -> CString -> Int -> Int -> Int -> IO CString
estimateMinFee ptrUnsignedTxObject pparams numInputs numOutputs numShelleyKeyWitnesses = do
  (intToCStr . Api.unCoin)
    =<< join
      ( estimateMinFeeImpl
          <$> fromCJSON False "UnsignedTx" ptrUnsignedTxObject
          <*> (ProtocolParamsJSON <$> fromCJSON False "ProtocolParameters" pparams)
          <*> pure (fromIntegral numInputs)
          <*> pure (fromIntegral numOutputs)
          <*> pure (fromIntegral numShelleyKeyWitnesses)
      )

signWithPaymentKey :: UnsignedTxObjectJSON -> CString -> IO SignedTxObjectJSON
signWithPaymentKey unsignedTxObject signingKeyBech32 =
  toCJSON
    =<< ( signWithPaymentKeyImpl
            <$> fromCJSON True "UnsignedTx" unsignedTxObject
            <*> stringTosigningKey signingKeyBech32
        )

-- * SignedTxObject

#if defined(wasm32_HOST_ARCH)

foreign export ccall "alsoSignWithPaymentKey"
  alsoSignWithPaymentKey :: SignedTxObjectJSON -> CString -> IO SignedTxObjectJSON

foreign export ccall "toCbor"
  toCbor :: SignedTxObjectJSON -> IO CString

#endif

type SignedTxObjectJSON = CString

alsoSignWithPaymentKey :: SignedTxObjectJSON -> CString -> IO SignedTxObjectJSON
alsoSignWithPaymentKey signedTxObject signingKeyBech32 =
  toCJSON
    =<< ( alsoSignWithPaymentKeyImpl
            <$> fromCJSON True "SignedTx" signedTxObject
            <*> stringTosigningKey signingKeyBech32
        )

toCbor :: SignedTxObjectJSON -> IO CString
toCbor signedTxObject =
  newCString . toCborImpl
    =<< fromCJSON False "SignedTx" signedTxObject
