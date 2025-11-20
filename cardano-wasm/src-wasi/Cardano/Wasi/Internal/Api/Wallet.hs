{-# LANGUAGE CPP #-}

module Cardano.Wasi.Internal.Api.Wallet
  ( generatePaymentWallet
  , generateStakeWallet
  , restorePaymentWalletFromSigningKeyBech32
  , restoreStakeWalletFromSigningKeyBech32
  , generateTestnetPaymentWallet
  , generateTestnetStakeWallet
  , restoreTestnetPaymentWalletFromSigningKeyBech32
  , restoreTestnetStakeWalletFromSigningKeyBech32
  , getAddressBech32
  , getBech32ForPaymentVerificationKey
  , getBech32ForPaymentSigningKey
  , getBech32ForStakeVerificationKey
  , getBech32ForStakeSigningKey
  , getBase16ForPaymentVerificationKeyHash
  , getBase16ForStakeVerificationKeyHash
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

foreign export ccall "generateStakeWallet"
  generateStakeWallet :: IO WalletObjectJSON

foreign export ccall "restorePaymentWalletFromSigningKeyBech32"
  restorePaymentWalletFromSigningKeyBech32 :: CString -> IO WalletObjectJSON

foreign export ccall "restoreStakeWalletFromSigningKeyBech32"
  restoreStakeWalletFromSigningKeyBech32 :: CString -> CString -> IO WalletObjectJSON

foreign export ccall "generateTestnetPaymentWallet"
  generateTestnetPaymentWallet :: Int -> IO WalletObjectJSON

foreign export ccall "generateTestnetStakeWallet"
  generateTestnetStakeWallet :: Int -> IO WalletObjectJSON

foreign export ccall "restoreTestnetPaymentWalletFromSigningKeyBech32"
  restoreTestnetPaymentWalletFromSigningKeyBech32 :: Int -> CString -> IO WalletObjectJSON

foreign export ccall "restoreTestnetStakeWalletFromSigningKeyBech32"
  restoreTestnetStakeWalletFromSigningKeyBech32 :: Int -> CString -> CString -> IO WalletObjectJSON

foreign export ccall "getAddressBech32"
  getAddressBech32 :: WalletObjectJSON -> IO CString

foreign export ccall "getBech32ForPaymentVerificationKey"
  getBech32ForPaymentVerificationKey :: WalletObjectJSON -> IO CString

foreign export ccall "getBech32ForPaymentSigningKey"
  getBech32ForPaymentSigningKey :: WalletObjectJSON -> IO CString

foreign export ccall "getBech32ForStakeVerificationKey"
  getBech32ForStakeVerificationKey :: WalletObjectJSON -> IO CString

foreign export ccall "getBech32ForStakeSigningKey"
  getBech32ForStakeSigningKey :: WalletObjectJSON -> IO CString

foreign export ccall "getBase16ForPaymentVerificationKeyHash"
  getBase16ForPaymentVerificationKeyHash :: WalletObjectJSON -> IO CString

foreign export ccall "getBase16ForStakeVerificationKeyHash"
  getBase16ForStakeVerificationKeyHash :: WalletObjectJSON -> IO CString

#endif

type WalletObjectJSON = CString

generatePaymentWallet :: IO WalletObjectJSON
generatePaymentWallet = toCJSON =<< Wallet.generatePaymentWalletImpl

generateStakeWallet :: IO WalletObjectJSON
generateStakeWallet = toCJSON =<< Wallet.generateStakeWalletImpl

restorePaymentWalletFromSigningKeyBech32 :: CString -> IO WalletObjectJSON
restorePaymentWalletFromSigningKeyBech32 signingKeyBech32CStr = do
  signingKeyBech32 <- peekCString signingKeyBech32CStr
  toCJSON =<< Wallet.restorePaymentWalletFromSigningKeyBech32Impl signingKeyBech32

restoreStakeWalletFromSigningKeyBech32 :: CString -> CString -> IO WalletObjectJSON
restoreStakeWalletFromSigningKeyBech32 paymentSigningKeyCStr stakeSigningKeyCStr = do
  paymentSigningKey <- peekCString paymentSigningKeyCStr
  stakeSigningKey <- peekCString stakeSigningKeyCStr
  toCJSON =<< Wallet.restoreStakeWalletFromSigningKeyBech32Impl paymentSigningKey stakeSigningKey

generateTestnetPaymentWallet :: Int -> IO WalletObjectJSON
generateTestnetPaymentWallet networkMagic =
  toCJSON =<< Wallet.generateTestnetPaymentWalletImpl (fromIntegral networkMagic)

generateTestnetStakeWallet :: Int -> IO WalletObjectJSON
generateTestnetStakeWallet networkMagic =
  toCJSON =<< Wallet.generateTestnetStakeWalletImpl (fromIntegral networkMagic)

restoreTestnetPaymentWalletFromSigningKeyBech32 :: Int -> CString -> IO WalletObjectJSON
restoreTestnetPaymentWalletFromSigningKeyBech32 networkMagic signingKeyBech32CStr =
  toCJSON
    =<< ( Wallet.restoreTestnetPaymentWalletFromSigningKeyBech32Impl
            (fromIntegral networkMagic)
            =<< peekCString signingKeyBech32CStr
        )

restoreTestnetStakeWalletFromSigningKeyBech32 :: Int -> CString -> CString -> IO WalletObjectJSON
restoreTestnetStakeWalletFromSigningKeyBech32 networkMagic paymentSigningKeyCStr stakeSigningKeyCStr = do
  paymentKey <- peekCString paymentSigningKeyCStr
  stakeKey <- peekCString stakeSigningKeyCStr
  toCJSON
    =<< Wallet.restoreTestnetStakeWalletFromSigningKeyBech32Impl
      (fromIntegral networkMagic)
      paymentKey
      stakeKey

getAddressBech32 :: WalletObjectJSON -> IO CString
getAddressBech32 walletCStr =
  newCString . Wallet.getAddressBech32Impl
    =<< fromCJSON False "WalletObject" walletCStr

getBech32ForPaymentVerificationKey :: WalletObjectJSON -> IO CString
getBech32ForPaymentVerificationKey walletCStr =
  newCString . Wallet.getBech32ForPaymentVerificationKeyImpl
    =<< fromCJSON False "WalletObject" walletCStr

getBech32ForPaymentSigningKey :: WalletObjectJSON -> IO CString
getBech32ForPaymentSigningKey walletCStr =
  newCString . Wallet.getBech32ForPaymentSigningKeyImpl
    =<< fromCJSON False "WalletObject" walletCStr

getBech32ForStakeVerificationKey :: WalletObjectJSON -> IO CString
getBech32ForStakeVerificationKey walletCStr =
  newCString . Wallet.getBech32ForStakeVerificationKeyImpl
    =<< fromCJSON False "WalletObject" walletCStr

getBech32ForStakeSigningKey :: WalletObjectJSON -> IO CString
getBech32ForStakeSigningKey walletCStr =
  newCString . Wallet.getBech32ForStakeSigningKeyImpl
    =<< fromCJSON False "WalletObject" walletCStr

getBase16ForPaymentVerificationKeyHash :: WalletObjectJSON -> IO CString
getBase16ForPaymentVerificationKeyHash walletCStr =
  newCString . Wallet.getBase16ForPaymentVerificationKeyHashImpl
    =<< fromCJSON False "WalletObject" walletCStr

getBase16ForStakeVerificationKeyHash :: WalletObjectJSON -> IO CString
getBase16ForStakeVerificationKeyHash walletCStr =
  newCString . Wallet.getBase16ForStakeVerificationKeyHashImpl
    =<< fromCJSON False "WalletObject" walletCStr
