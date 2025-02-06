{-# LANGUAGE CPP #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

module Cardano.Api.Internal.IO.Compat.Win32
  (
#ifndef UNIX
  module Cardano.Api.IO.Compat.Win32
#endif
  )
where

#ifndef UNIX
import Cardano.Api.IO.Compat.Win32
#endif

