# Haskell quickstart

cardano-rpc ships its own client library, so this example needs no separate SDK and no protoc: `cardano-rpc`'s public [`Cardano.Rpc.Client`](https://cardano-api.cardano.intersectmbo.org/cardano-rpc/Cardano-Rpc-Client.html) module re-exports the [grapesy](https://hackage.haskell.org/package/grapesy) gRPC client, and the package separately exposes the generated `Cardano.Rpc.Proto.Api.UtxoRpc.*` proto-lens bindings for every UTxO RPC v1beta message and service; see the [cardano-rpc haddocks](https://cardano-api.cardano.intersectmbo.org/) for the full API.
`cabal.project`, `cardano-rpc-quickstart.cabal`, `app/Main.hs` and `app/SendLovelace.hs` are already in this directory; no `cabal init` needed.

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

Then, from this directory (this package now has two executables, so name the one to run):

```bash
cabal run cardano-rpc-quickstart
```

Sample output (your values will differ):

```
Tip: slot 492 height 19 hash 0297a109252289074344a448e8dacfba2738c5e7131f765abb80996de346e83f
Protocol parameters: max_tx_size 16384 max_block_body_size 65536
```

## Build and submit a transaction

[`app/SendLovelace.hs`](app/SendLovelace.hs) builds and submits a transaction with cardano-api's experimental transaction-building API ([`Cardano.Api.Experimental`](https://cardano-api.cardano.intersectmbo.org/cardano-api/Cardano-Api-Experimental.html)), using cardano-rpc for UTxO queries, protocol parameters, and submission.
It sends 5 ADA from the cluster's `utxo1` wallet to the `utxo2` address, then polls the recipient's UTxOs until the new output appears.

> [!IMPORTANT]
> This example sticks to the same public surface as [`app/Main.hs`](app/Main.hs): `Cardano.Rpc.Client` and the generated `Cardano.Rpc.Proto.Api.UtxoRpc.*` proto-lens bindings.
> cardano-rpc's own address-predicate and protocol-parameter conversion helpers live under `Cardano.Rpc.Server.Internal.*`, which is implementation detail, not a published API; `SendLovelace.hs` reimplements the handful of lines it needs (an exact-address predicate, a `BigInt` reader) directly, the same way the TypeScript and Python examples reimplement their own equivalents.
> It is demo scaffolding for a single plain ADA payment: it spends every UTxO found at the sender address, and it does not surface datums, inline datums, reference scripts, or script cost models.
> Do not lift it unchanged into a dApp that touches script-locked UTxOs.

The [Prerequisites](#prerequisites) and [Run it](#run-it) sections above already set up a running cluster and the dev shell.
From this directory:

```bash
cabal run send-lovelace
```

Sample output (your values will differ):

```
Sender address:    addr_test1vput634px2ka84c8ydy85mlddq9sdrz87n4lqpqvnx85cmch0wp00
Recipient address: addr_test1vpr27xl05ss66almdtyphucus4qf6ug0p6nggmd044682jsuxj86e
Spendable UTxOs:   1
Submitted tx: 099ea99019db961e26cb0791f43a2be329a924d86e7994135f1b90dfef7e64c5
Confirmed: 5000000 lovelace landed at addr_test1vpr27xl05ss66almdtyphucus4qf6ug0p6nggmd044682jsuxj86e (099ea99019db961e26cb0791f43a2be329a924d86e7994135f1b90dfef7e64c5#0)
```

> [!NOTE]
> The `utxo1` genesis signing key's TextEnvelope has type `GenesisUTxOSigningKey_ed25519`, not the more common `PaymentSigningKeyShelley_ed25519`.
> `readFileTextEnvelopeAnyOf [FromSomeType asType WitnessGenesisUTxOKey]` reads it straight into a `ShelleyWitnessSigningKey` that can sign the transaction, no separate key-type handling needed.

> [!NOTE]
> There are no scripts in this transaction, so the minimum fee is exactly `transactionSizeInBytes * minFeeCoefficient + minFeeConstant`, both of which `ReadParams` returns.
> `SendLovelace.hs` builds and signs the transaction, measures the encoded size, and repeats with the computed fee until it stops changing, rather than assembling a full `PParams` value just to call the ledger's fee estimator.

