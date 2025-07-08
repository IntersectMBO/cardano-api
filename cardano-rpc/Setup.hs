#!/usr/bin/env cabal
{- cabal:
build-depends: base, Cabal, proto-lens-setup
-}

-- this is a cabal script because of https://github.com/haskell/haskell-language-server/issues/3735

import Data.ProtoLens.Setup

main = defaultMainGeneratingProtos "proto"
