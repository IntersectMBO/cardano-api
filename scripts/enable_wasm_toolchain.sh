#!/usr/bin/env bash


source $HOME/.ghc-wasm/env

# This should be able to build!
# cabal build --project-file=cabal-wasm.project cardano-api-wasm --keep-going --enable-split-sections

# Check if `happy` is already installed
if ! command -v happy &> /dev/null; then
    echo "Happy is not installed. Installing it now..."
    cabal install happy
else
    echo "Happy is already installed. Skipping installation."
fi

wasm32-wasi-cabal build --project-file=cabal-wasm.project cardano-api-wasm --keep-going

# Hints
# https://github.com/bradrn/brassica/blob/master/BUILDING.md
# https://github.com/haskell-wasm/pandoc/blob/76a6f03cb938539f33842cb17cfd29f67e44d9e8/cabal.project#L116
# https://finley.dev/blog/2024-08-24-ghc-wasm.html