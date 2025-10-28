{-# LANGUAGE CPP #-}

module Cardano.Wasi.Internal.Api.Wallet
  ( generatePaymentWallet
  , restorePaymentWalletFromSigningKeyBech32
  , restoreTestnetPaymentWalletFromSigningKeyBech32
  , getAddressBech32
  , getBech32ForVerificationKey
  , getBech32ForSigningKey
  , getBase16ForVerificationKeyHash
  )
where

import Cardano.Wasi.Internal.Conversion (fromCJSON, toCJSON)
import Cardano.Wasm.Api.Wallet qualified as Wallet

import Foreign.C (CString)
import Foreign.C.String (newCString, peekCString)

-- * WalletObject

#if defined(wasm32_HOST_ARCH)

foreign export ccall "generatePaymentWallet"
  generatePaymentWallet :: IO WalletObjectJSON

foreign export ccall "restorePaymentWalletFromSigningKeyBech32"
  restorePaymentWalletFromSigningKeyBech32 :: CString -> IO WalletObjectJSON

foreign export ccall "restoreTestnetPaymentWalletFromSigningKeyBech32"
  restoreTestnetPaymentWalletFromSigningKeyBech32 :: Int -> CString -> IO WalletObjectJSON

foreign export ccall "getAddressBech32"
  getAddressBech32 :: WalletObjectJSON -> IO CString

foreign export ccall "getBech32ForVerificationKey"
  getBech32ForVerificationKey :: WalletObjectJSON -> IO CString

foreign export ccall "getBech32ForSigningKey"
  getBech32ForSigningKey :: WalletObjectJSON -> IO CString

foreign export ccall "getBase16ForVerificationKeyHash"
  getBase16ForVerificationKeyHash :: WalletObjectJSON -> IO CString

#endif

type WalletObjectJSON = CString

generatePaymentWallet :: IO WalletObjectJSON
generatePaymentWallet = toCJSON =<< Wallet.generatePaymentWalletImpl

restorePaymentWalletFromSigningKeyBech32 :: CString -> IO WalletObjectJSON
restorePaymentWalletFromSigningKeyBech32 signingKeyBech32CStr = do
  signingKeyBech32 <- peekCString signingKeyBech32CStr
  toCJSON =<< Wallet.restorePaymentWalletFromSigningKeyBech32Impl signingKeyBech32

restoreTestnetPaymentWalletFromSigningKeyBech32 :: Int -> CString -> IO WalletObjectJSON
restoreTestnetPaymentWalletFromSigningKeyBech32 networkMagic signingKeyBech32CStr =
  toCJSON
    =<< ( Wallet.restoreTestnetPaymentWalletFromSigningKeyBech32Impl
            (fromIntegral networkMagic)
            =<< peekCString signingKeyBech32CStr
        )

getAddressBech32 :: WalletObjectJSON -> IO CString
getAddressBech32 walletCStr =
  newCString . Wallet.getAddressBech32
    =<< fromCJSON False "WalletObject" walletCStr

getBech32ForVerificationKey :: WalletObjectJSON -> IO CString
getBech32ForVerificationKey walletCStr =
  newCString . Wallet.getBech32ForVerificationKeyImpl
    =<< fromCJSON False "WalletObject" walletCStr

getBech32ForSigningKey :: WalletObjectJSON -> IO CString
getBech32ForSigningKey walletCStr =
  newCString . Wallet.getBech32ForSigningKeyImpl
    =<< fromCJSON False "WalletObject" walletCStr

getBase16ForVerificationKeyHash :: WalletObjectJSON -> IO CString
getBase16ForVerificationKeyHash walletCStr =
  newCString . Wallet.getBase16ForVerificationKeyHashImpl
    =<< fromCJSON False "WalletObject" walletCStr
