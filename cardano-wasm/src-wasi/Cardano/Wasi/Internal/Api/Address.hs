{-# LANGUAGE CPP #-}

module Cardano.Wasi.Internal.Api.Address
  ( inspectAddress
  )
where

import Cardano.Wasi.Internal.Conversion (toCJSON)
import Cardano.Wasm.Api.Address qualified as Address

import Foreign.C (CString)
import Foreign.C.String (peekCString)

-- * Address inspection

#if defined(wasm32_HOST_ARCH)

foreign export ccall "inspectAddress"
  inspectAddress :: CString -> IO AddressInfoJSON

#endif

type AddressInfoJSON = CString

inspectAddress :: CString -> IO AddressInfoJSON
inspectAddress address =
  toCJSON . Address.inspectAddressImpl
    =<< peekCString address
