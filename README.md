# cardano-api

`cardano-api` is the Haskell library for writing applications that interact with the Cardano blockchain.
It lets you build and sign transactions, manage keys and addresses, query a running node, and convert between the formats used on Cardano (CBOR, JSON, bech32).

It is the same layer that [`cardano-cli`](https://github.com/IntersectMBO/cardano-cli) and [`cardano-node`](https://github.com/IntersectMBO/cardano-node) are built on.
Under the hood it combines the [`ledger`](https://github.com/IntersectMBO/cardano-ledger), [`consensus`](https://github.com/IntersectMBO/ouroboros-consensus) and [`networking`](https://github.com/IntersectMBO/ouroboros-network) libraries, and hides most of their details behind a single module: `Cardano.Api`.

## What is in this repository

| Package | What it is |
| --- | --- |
| [`cardano-api`](cardano-api/) | The main library. Start here. |
| [`cardano-api-gen`](cardano-api-gen/) | Hedgehog generators for `cardano-api` types, useful for writing tests. |
| [`cardano-rpc`](cardano-rpc/) | gRPC client and server for talking to a node, implementing the [UTxO RPC](https://utxorpc.org/) spec. |
| [`cardano-wasm`](cardano-wasm/) | The API compiled to WebAssembly, with JavaScript/TypeScript bindings for browsers and Node.js. |

## Requirements

You can build with Nix (easiest: it provides everything) or with your own Haskell toolchain.

**With Nix:**

- [Nix](https://nixos.org/download/) with flakes enabled.
- Answer "yes" when Nix asks to accept the flake settings.
  That enables the IOG binary cache (`cache.iog.io`).
  Without it, you will compile GHC and every dependency from source.
- Works on `x86_64-linux`, `aarch64-linux` and `aarch64-darwin`.

**Without Nix:**

The Developer Portal's [Installing cardano-node](https://developers.cardano.org/docs/operators/node/installing-cardano-node/) guide covers this exact setup step by step.
Follow it up to the point where it starts building the node itself.
In short, you need:

- GHC 9.6, 9.10, 9.12 or 9.14, and Cabal 3.16 (for example via [GHCup](https://www.haskell.org/ghcup/)).
  Development mostly happens on GHC 9.12.
- Cardano's C libraries: `libsodium` (the IOG fork, with VRF support), `libsecp256k1` and `libblst`.
  Prebuilt packages are on the [iohk-nix releases page](https://github.com/input-output-hk/iohk-nix/releases/latest); [this GitHub action](https://github.com/input-output-hk/actions/tree/latest/base) shows how CI installs them.
- A few common system packages; on Ubuntu: `libsystemd-dev liblmdb-dev liburing-dev libsnappy-dev protobuf-compiler`.

## Quick start

```bash
git clone https://github.com/IntersectMBO/cardano-api
cd cardano-api
nix develop          # skip this line if you are not using Nix
cabal update         # needed at least once, see note below
cabal build all --enable-tests
```

Run the tests:

```bash
cabal test all --enable-tests --test-show-details=direct
```

Build notes:

- `cabal update` downloads the package lists of **two** repositories: Hackage and [CHaP](https://chap.intersectmbo.org/) (Cardano Haskell Packages, where the Cardano-specific dependencies live).
  CHaP is already configured in this repo's `cabal.project`.
- The project builds with `-Werror`, so every warning is an error.
  Stick to the GHC versions listed above.
- Other Nix shells: `nix develop .#ghc967` and `.#ghc914` (other compilers, `x86_64-linux` only), `.#profiling`, `.#wasm` (WebAssembly toolchain) and `.#demo` (Elm toolchain for the browser demo).
- To build the WebAssembly module, see [`cardano-wasm/README.md`](cardano-wasm/README.md).

## Using the library in your project

`cardano-api` is released on [CHaP](https://chap.intersectmbo.org/), not on Hackage, so your project needs CHaP configured.
A minimal project is three files:

`cabal.project` points at your package and registers CHaP:

```
packages: .

repository cardano-haskell-packages
  url: https://chap.intersectmbo.org/
  secure: True
  root-keys:
    3e0cce471cf09815f930210f7827266fd09045445d65923e6d0238a6cd15126f
    443abb7fb497a134c343faf52f0b659bd7999bc06b7f63fa76dc99d631f9bea1
    a86a1f6ce86c449c46666bda44268677abf29b5b2d2eb5ec7af903ec2f117a82
    bcec67e8e99cabfa7764d75ad9b158d72bfacf70ca1d0ec8bc6b4406d1bf8413
    c00aae8461a256275598500ea0e187588c35a5d5d7454fb57eac18d9edb86a56
    d4a35cd3121aa00d18544bb0ac01c3e1691d618f462c46129271bccf39f7e8ee
```

(Tip: also pin an `index-state` to make your builds reproducible; see the [CHaP README](https://github.com/IntersectMBO/cardano-haskell-packages).)

`example.cabal`:

```cabal
cabal-version: 3.0
name:          example
version:       0.1.0.0
build-type:    Simple

executable example
  main-is:          Main.hs
  default-language: Haskell2010
  build-depends:
    , base
    , cardano-api ^>=11.5
    , text
```

`Main.hs` creates a key and prints a fresh mainnet address.
No running node needed:

```haskell
module Main where

import Cardano.Api

import qualified Data.Text.IO as Text

main :: IO ()
main = do
  -- Make a new payment key.
  signingKey <- generateSigningKey AsPaymentKey
  -- Hash its verification (public) key.
  let keyHash = verificationKeyHash (getVerificationKey signingKey)
      -- Build a mainnet address that pays to that key, with no stake part.
      address = makeShelleyAddress Mainnet (PaymentCredentialByKey keyHash) NoStakeAddress
  Text.putStrLn (serialiseAddress address)
```

Run it with `cabal update && cabal run`.
It prints a mainnet address (it starts with `addr1`).
Expect the first build to take a while: it compiles the whole Cardano stack.
Without Nix, you also need the C libraries from [Requirements](#requirements).

Where to go next:

- The [`Cardano.Api.Tx` haddocks](https://cardano-api.cardano.intersectmbo.org/cardano-api/Cardano-Api-Tx.html) contain worked examples for building, balancing and signing transactions.
- The test suite doubles as a set of small examples: [`Test.Cardano.Api.Envelope`](cardano-api/test/cardano-api-test/Test/Cardano/Api/Envelope.hs) reads and writes key files, [`Test.Cardano.Api.Address`](cardano-api/test/cardano-api-test/Test/Cardano/Api/Address.hs) turns keys into addresses, and [`Test.Cardano.Api.Experimental`](cardano-api/test/cardano-api-test/Test/Cardano/Api/Experimental.hs) builds and balances transactions with protocol parameters.
- To talk to a live node from Haskell, look at [`Cardano.Api.Network.IPC`](https://cardano-api.cardano.intersectmbo.org/cardano-api/Cardano-Api-Network-IPC.html) (queries and transaction submission over the node's local socket).
- [`Cardano.Api.Experimental`](https://cardano-api.cardano.intersectmbo.org/cardano-api/Cardano-Api-Experimental.html) is a newer transaction-building API that will replace parts of the current one.
  It is usable, but still changing.

## Documentation

- [Haddock documentation](https://cardano-api.cardano.intersectmbo.org/): the full API reference, rebuilt from `master`.
- [Cardano Node Wiki](https://github.com/input-output-hk/cardano-node-wiki/wiki): development documentation.
- [Browser wallet demo](https://cardano-api.cardano.intersectmbo.org/cardano-wasm/demo/): built on [`cardano-wasm`](cardano-wasm/).
- [Cardano Developer Portal](https://developers.cardano.org/): if you are new to Cardano itself.

## Contributing

See the [Contributing guide](CONTRIBUTING.md) for how to contribute to this project.

## Core maintainers

* [Jordan Millar](https://github.com/Jimbo4350)
* [Mateusz Gałażyn](https://github.com/carbolymer)
* [Pablo Lamela](https://github.com/palas)

[![x86\_64-linux](https://img.shields.io/endpoint?url=https://ci.iog.io/job/IntersectMBO-cardano-api/master/x86_64-linux.required/shield&style=flat-square&label=x86_64-linux)](https://ci.iog.io/job/IntersectMBO-cardano-api/master/x86_64-linux.required)
[![x86\_64-darwin](https://img.shields.io/endpoint?url=https://ci.iog.io/job/IntersectMBO-cardano-api/master/x86_64-darwin.required/shield&style=flat-square&label=x86_64-darwin)](https://ci.iog.io/job/IntersectMBO-cardano-api/master/x86_64-darwin.required)
[![GHA Build](https://img.shields.io/github/actions/workflow/status/intersectmbo/cardano-api/haskell.yml?branch=master&label=GHA%20Build&style=flat-square)](https://github.com/IntersectMBO/cardano-api/actions/workflows/haskell.yml?query=branch%3Amaster)
[![Haddock](https://img.shields.io/github/actions/workflow/status/intersectmbo/cardano-api/github-page.yml?branch=master&label=Haddocks&style=flat-square)](https://github.com/IntersectMBO/cardano-api/actions/workflows/github-page.yml?query=branch%3Amaster)
