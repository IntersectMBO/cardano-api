#!/usr/bin/env bash


source $HOME/.ghc-wasm/env

# export CONF_CC_OPTS_STAGE2="$CONF_CC_OPTS_STAGE2 -D_WASI_EMULATED_MMAN"
# export CONF_GCC_LINKER_OPTS_STAGE2="$CONF_GCC_LINKER_OPTS_STAGE2 -lwasi-emulated-mman"

# This should be able to build!
# cabal build --project-file=cabal-wasm.project cardano-api-wasm --keep-going

# Check if `happy` is already installed
if ! command -v happy &> /dev/null; then
    echo "Happy is not installed. Installing it now..."
    cabal install happy
else
    echo "Happy is already installed. Skipping installation."
fi

wasm32-wasi-cabal update  --project-file=cabal-wasm-ledger-only.project --keep-going
wasm32-wasi-cabal build  --project-file=cabal-wasm-ledger-only.project \
  cardano-api-wasm-ledger-only 


# Hints
# https://github.com/bradrn/brassica/blob/master/BUILDING.md
# https://github.com/haskell-wasm/pandoc/blob/76a6f03cb938539f33842cb17cfd29f67e44d9e8/cabal.project#L116
# https://finley.dev/blog/2024-08-24-ghc-wasm.html