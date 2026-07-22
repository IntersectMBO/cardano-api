# cardano-wasm wallet demo

A browser wallet demo built on the [`cardano-wasm`](../) library. Everything runs
client-side in a static page: key generation, transaction building, fee estimation and
signing happen in the wasm engine; chain data (UTxOs, stake pools) and transaction
submission go through [Blockfrost](https://blockfrost.io) using a project id the user
types into the page (kept in memory only).

The demo built from `master` is published at:
**https://cardano-api.cardano.intersectmbo.org/cardano-wasm/demo/**

> ⚠️ Real cryptography on the selected network — use the **Preprod** or **Preview**
> testnets (there is a faucet link in the app).

## Building locally

The Elm toolchain comes from the repo's dev shell:

```bash
nix develop .#demo
```

The demo also needs the built cardano-wasm library (`cardano-wasm.wasm`,
`cardano-wasm.js`, `cardano-api.js`, `main.js`). Build it into `../lib-wrapper/` by
following [the cardano-wasm README](../README.md) (nix: `nix develop .#wasm`, then the
compile + `post-link.mjs` steps). Alternatively, for a quick start, download the
`cardano-wasm` artifact of a recent [Haskell CI (WASM) run](https://github.com/IntersectMBO/cardano-api/actions/workflows/haskell-wasm.yml)
and unpack it anywhere.

Then:

```bash
./build.sh --wasm-dir ../lib-wrapper --out dist
python3 -m http.server -d dist 8080     # any static server works
```

## Layout

```
src/        the Elm application
web/        page shell, CSS, and the JS glue that drives the cardano-wasm API
build.sh    compiles the Elm app and assembles a servable directory
```

The Elm code holds the state and UI; every Cardano operation crosses a port to
`web/ports.js`, which calls the cardano-wasm wrapper. Blockfrost is queried directly
from Elm over HTTPS.

## Development

- Source is `elm-format`-canonical; CI validates it (`elm-format --validate src/`).
- CI builds the demo in the *Haskell CI (WASM)* workflow and the GitHub Pages workflow
  publishes the result.
