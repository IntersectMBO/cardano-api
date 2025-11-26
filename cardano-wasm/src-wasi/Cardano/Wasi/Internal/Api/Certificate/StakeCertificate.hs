{-# LANGUAGE CPP #-}

module Cardano.Wasi.Internal.Api.Certificate.StakeCertificate
  ( makeStakeAddressStakeDelegationCertificate
  , makeStakeAddressStakeDelegationCertificateUpcomingEra
  , makeStakeAddressRegistrationCertificate
  , makeStakeAddressRegistrationCertificateUpcomingEra
  , makeStakeAddressUnregistrationCertificate
  , makeStakeAddressUnregistrationCertificateUpcomingEra
  )
where

import Cardano.Wasi.Internal.Conversion (cstrToInt)
import Cardano.Wasm.Api.Certificate.StakeCertificate qualified as Wasm

import Control.Monad (join)

import Foreign.C (CString)
import Foreign.C.String (newCString, peekCString)

#if defined(wasm32_HOST_ARCH)

foreign export ccall "makeStakeAddressStakeDelegationCertificate"
  makeStakeAddressStakeDelegationCertificate :: CString -> CString -> IO CString

foreign export ccall "makeStakeAddressStakeDelegationCertificateUpcomingEra"
  makeStakeAddressStakeDelegationCertificateUpcomingEra :: CString -> CString -> IO CString

foreign export ccall "makeStakeAddressRegistrationCertificate"
  makeStakeAddressRegistrationCertificate :: CString -> CString -> IO CString

foreign export ccall "makeStakeAddressRegistrationCertificateUpcomingEra"
  makeStakeAddressRegistrationCertificateUpcomingEra :: CString -> CString -> IO CString

foreign export ccall "makeStakeAddressUnregistrationCertificate"
  makeStakeAddressUnregistrationCertificate :: CString -> CString -> IO CString

foreign export ccall "makeStakeAddressUnregistrationCertificateUpcomingEra"
  makeStakeAddressUnregistrationCertificateUpcomingEra :: CString -> CString -> IO CString

#endif

-- | Make a certificate that delegates a stake address to a stake pool in the current era.
makeStakeAddressStakeDelegationCertificate :: CString -> CString -> IO CString
makeStakeAddressStakeDelegationCertificate stakeKeyHashCStr poolIdCStr =
  newCString
    =<< join
      ( Wasm.makeStakeAddressStakeDelegationCertificateImpl
          <$> peekCString stakeKeyHashCStr
          <*> peekCString poolIdCStr
      )

-- | Make a certificate that delegates a stake address to a stake pool in the upcoming era.
makeStakeAddressStakeDelegationCertificateUpcomingEra :: CString -> CString -> IO CString
makeStakeAddressStakeDelegationCertificateUpcomingEra stakeKeyHashCStr poolIdCStr =
  newCString
    =<< join
      ( Wasm.makeStakeAddressStakeDelegationCertificateUpcomingEraImpl
          <$> peekCString stakeKeyHashCStr
          <*> peekCString poolIdCStr
      )

-- | Make a stake address registration certificate in the current era.
makeStakeAddressRegistrationCertificate :: CString -> CString -> IO CString
makeStakeAddressRegistrationCertificate stakeKeyHashCStr depositCStr =
  newCString
    =<< join
      ( Wasm.makeStakeAddressRegistrationCertificateImpl
          <$> peekCString stakeKeyHashCStr
          <*> (toInteger <$> cstrToInt "deposit" depositCStr)
      )

-- | Make a stake address registration certificate in the upcoming era.
makeStakeAddressRegistrationCertificateUpcomingEra :: CString -> CString -> IO CString
makeStakeAddressRegistrationCertificateUpcomingEra stakeKeyHashCStr depositCStr =
  newCString
    =<< join
      ( Wasm.makeStakeAddressRegistrationCertificateUpcomingEraImpl
          <$> peekCString stakeKeyHashCStr
          <*> (toInteger <$> cstrToInt "deposit" depositCStr)
      )

-- | Make a stake address unregistration certificate in the current era.
makeStakeAddressUnregistrationCertificate :: CString -> CString -> IO CString
makeStakeAddressUnregistrationCertificate stakeKeyHashCStr depositCStr =
  newCString
    =<< join
      ( Wasm.makeStakeAddressUnregistrationCertificateImpl
          <$> peekCString stakeKeyHashCStr
          <*> (toInteger <$> cstrToInt "deposit" depositCStr)
      )

-- | Make a stake address unregistration certificate in the upcoming era.
makeStakeAddressUnregistrationCertificateUpcomingEra :: CString -> CString -> IO CString
makeStakeAddressUnregistrationCertificateUpcomingEra stakeKeyHashCStr depositCStr =
  newCString
    =<< join
      ( Wasm.makeStakeAddressUnregistrationCertificateUpcomingEraImpl
          <$> peekCString stakeKeyHashCStr
          <*> (toInteger <$> cstrToInt "deposit" depositCStr)
      )
