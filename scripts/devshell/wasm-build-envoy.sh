#!/usr/bin/env bash

# This script should be executed in wasm devshell: nix develop -i '.#wasm'
# It rebuilds wasm, its bindings and copies everything into `cardano-wasm/grpc-example`
# TODO: move this to nix from here

set -euxo pipefail

wasm32-wasi-cabal build cardano-wasm
"$(wasm32-wasi-ghc --print-libdir)/post-link.mjs" -i "$(env -u CABAL_CONFIG wasm32-wasi-cabal list-bin exe:cardano-wasm | tail -n1)" -o cardano-wasm.js
cp -f "$(env -u CABAL_CONFIG wasm32-wasi-cabal list-bin exe:cardano-wasm | tail -n1)" cardano-wasm/grpc-example/
cp cardano-wasm.js cardano-wasm/grpc-example/
cp -rf cardano-wasm/lib-wrapper/* cardano-wasm/grpc-example/
(cd cardano-wasm/grpc-example/ ; envoy -c envoy-conf.yaml)

