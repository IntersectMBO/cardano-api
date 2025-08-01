#!/usr/bin/env bash

# This script should be executed in wasm devshell: nix develop -i '.#wasm'
# It rebuilds wasm, its bindings and copies everything into `cardano-wasm/example_name`
# TODO: move this to nix from here

set -euxo pipefail

example_name="${1:-wallet-example}"

wasm32-wasi-cabal build cardano-wasm
"$(wasm32-wasi-ghc --print-libdir)/post-link.mjs" -i "$(env -u CABAL_CONFIG wasm32-wasi-cabal list-bin exe:cardano-wasm | tail -n1)" -o cardano-wasm.js
cp -f "$(env -u CABAL_CONFIG wasm32-wasi-cabal list-bin exe:cardano-wasm | tail -n1)" "cardano-wasm/$example_name/"
cp cardano-wasm.js "cardano-wasm/$example_name/"
cp -rf cardano-wasm/lib-wrapper/* "cardano-wasm/$example_name/"
cp -f result/cardano_node_grpc_web_pb.js "cardano-wasm/$example_name/"
(cd "cardano-wasm/$example_name/" ; envoy -c envoy-conf.yaml)

