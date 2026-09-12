# Haskell quickstart

cardano-rpc ships its own client library, so this example needs no separate SDK and no protoc: `cardano-rpc`'s public [`Cardano.Rpc.Client`](https://cardano-api.cardano.intersectmbo.org/cardano-rpc/Cardano-Rpc-Client.html) module re-exports the [grapesy](https://hackage.haskell.org/package/grapesy) gRPC client together with the generated `Cardano.Rpc.Proto.Api.UtxoRpc.*` proto-lens bindings for every UTxO RPC v1beta message and service; see the [cardano-rpc haddocks](https://cardano-api.cardano.intersectmbo.org/) for the full API.
`cabal.project`, `cardano-rpc-quickstart.cabal` and `app/Main.hs` are already in this directory; no `cabal init` needed.

> [!NOTE]
> Unlike the other language examples, this one needs the repository checkout's own dev shell, not a standalone `nix-shell`: building against `cardano-rpc` needs GHC and `cabal` plus the Cardano-specific C libraries (the libsodium VRF fork, secp256k1, blst) that a plain nixpkgs `mkShell` cannot provide.

## Prerequisites

Start a local cluster as described in the main [Quickstart](../../README.md#quickstart).
`app/Main.hs` here connects to `localhost:50051`, the cluster's gRPC address.
The example itself builds anywhere once you copy these files; only `nix develop` below needs the repository checkout.

## Run it

From this directory, get GHC, `cabal` and the C libraries from the repository's flake (the `.` flake reference resolves to the repository root from anywhere inside the clone):

```bash
nix develop .#rpc-quickstart-haskell
```

Then, from this directory:

```bash
cabal run
```

Sample output (your values will differ):

```
Tip: slot 492 height 19 hash 0297a109252289074344a448e8dacfba2738c5e7131f765abb80996de346e83f
Protocol parameters: max_tx_size 16384 max_block_body_size 65536
```

