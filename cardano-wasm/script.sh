#!/bin/bash
wasm32-wasi-cabal update
wasm32-wasi-cabal build
wasm-dis "$(env -u CABAL_CONFIG wasm32-wasi-cabal list-bin exe:cardano-wasm | tail -n1)" | grep "export "
$(wasm32-wasi-ghc --print-libdir)/post-link.mjs -i "$(env -u CABAL_CONFIG wasm32-wasi-cabal list-bin exe:cardano-wasm | tail -n1)" -o cardano-wasm.js
scp "$(env -u CABAL_CONFIG wasm32-wasi-cabal list-bin exe:cardano-wasm | tail -n1)" palas@f.palas87.es:~/www/www.palas87.es/public_html/test/cardano-wasm.wasm
scp cardano-wasm.js palas@f.palas87.es:~/www/www.palas87.es/public_html/test/
rm -f cardano-wasm.js
