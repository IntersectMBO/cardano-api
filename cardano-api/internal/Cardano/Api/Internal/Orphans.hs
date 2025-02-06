{-# LANGUAGE CPP #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

module Cardano.Api.Internal.Orphans
  (
#ifndef UNIX
  module Cardano.Api.Orphans
#endif
  )
where

#ifndef UNIX
import Cardano.Api.Orphans
#endif

