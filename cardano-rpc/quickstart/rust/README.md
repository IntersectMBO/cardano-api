# Rust quickstart

The Rust path is the simplest one: the published [`utxorpc-spec`](https://crates.io/crates/utxorpc-spec) crate already ships tonic-generated v1beta bindings, so no proto files and no protoc are needed.
`Cargo.toml` and `src/main.rs` are already in this directory; no `cargo new` needed.

> [!IMPORTANT]
> Use `utxorpc-spec` directly, not the higher-level `utxorpc` SDK crate: that wrapper is hardwired to the older `utxorpc.v1alpha` services and cannot talk to cardano-rpc, which serves `v1beta` (v1beta packaging for the SDKs is tracked in utxorpc/spec#209).

## Prerequisites

Start a local cluster as described in the main [Quickstart](../../README.md#quickstart).
`Cargo.toml` and `src/main.rs` here connect to `localhost:50051`, the cluster's gRPC address.
The example itself builds anywhere once you copy these files; only the `nix develop` and `nix-shell` commands below need the repository checkout.

## Run it

From this directory, get `cargo`, `rustc` and `gcc` from the repository's flake (the `.` flake reference resolves to the repository root from anywhere inside the clone):

```bash
nix develop .#rpc-quickstart-rust  # recommended
```

or with `nix-shell` (using the `shell.nix` in this directory):

```bash
nix-shell
```

or fetch them ad hoc:

```bash
nix shell nixpkgs#cargo nixpkgs#rustc nixpkgs#gcc
```

Then, from this directory:

```bash
cargo run
```

Sample output (your values will differ):

```
Tip: slot 834 height 37 hash d76df679ffa93ca9d1224cff29b3479c7535a84ed3eb8c17ce1d34aaeb4e774d
Protocol parameters: max_tx_size 16384 max_block_body_size 65536
```

tonic can also connect straight to the Unix socket through a custom connector (`Endpoint::connect_with_connector` with a `tokio::net::UnixStream`), for a node started with `--enable-grpc` instead; the TCP form used here is the simpler one.

## Build and submit a transaction

[`src/bin/send-lovelace.rs`](src/bin/send-lovelace.rs) builds and submits a transaction with [`pallas-txbuilder`](https://crates.io/crates/pallas-txbuilder), using cardano-rpc for UTxO queries, protocol parameters, and submission.
It sends 5 ADA from the cluster's `utxo1` wallet to the `utxo2` address, then polls the recipient's UTxOs until the new output appears.
If the cluster is not reachable, the first call fails immediately with a connection error; if the submitted transaction never confirms, the script times out after about a minute with an explicit error.
`src/bin/send-lovelace.rs` and the extra dependencies it needs are already in this directory; `Cargo.toml` already lists them.

> [!IMPORTANT]
> `pallas-txbuilder`'s `build_conway_raw` performs no automatic fee or change balancing: the fee and every output must already be exact.
> This example builds the transaction once with a fee drafted with the protocol fee rates applied to an assumed size, then corrects the fee against the real encoded size of that draft and rebuilds with the corrected change output (the leftover value returned to the sender).
> It is demo scaffolding for plain ADA payments: it spends every UTxO it finds at the sender address instead of running proper coin selection (choosing only the inputs needed), and it does not handle native assets and does not surface datums, inline datums, reference scripts, or script cost models.
> The examples sign with the demo cluster's throwaway keys; do not point them at a node whose wallet keys hold real funds.
> Do not lift it unchanged into a dApp that touches script-locked UTxOs.

> [!NOTE]
> `pallas-txbuilder` only builds Conway-era transactions (the `BuildConway` trait and its `build_conway_raw` method); the crate's own docs say earlier-era builders are "intentionally not maintained".
> A cardano-testnet cluster starts in the Conway era already, so this is not a limitation here.

The [Prerequisites](#prerequisites) and [Run it](#run-it) sections above set up a running cluster and a shell with `cargo`, `rustc` and `gcc`.
From this directory:

```bash
cargo run --bin send-lovelace
```

Sample output (your values will differ):

```
Sender address:    addr_test1vpt60mfvlt0k5qvv0cwyde5txuxpfhy5y3n5400z5xl5ptcpxm907
Recipient address: addr_test1vzu86fzvx95chrj4gq9h6w73dvthjp4c08dqs65rvvgcfwgu9u9g9
Spendable UTxOs:   1
Submitted tx: bba0f4709d5078fbf8b2f3cf6f3821d5b4ec716f77b59663736df9be0552917f
Confirmed: 5000000 lovelace landed at addr_test1vzu86fzvx95chrj4gq9h6w73dvthjp4c08dqs65rvvgcfwgu9u9g9 (bba0f4709d5078fbf8b2f3cf6f3821d5b4ec716f77b59663736df9be0552917f#0)
```

`send-lovelace.rs` reads the `utxo1` genesis signing key straight from its TextEnvelope file and decodes the CBOR bytestring by hand: cardano-cli's normal (non-extended) ed25519 signing keys are a `0x58 0x20` CBOR header followed by the raw 32-byte seed, which is exactly what `pallas_crypto::key::ed25519::SecretKey` expects.
Use `pallas_crypto::key::ed25519::SecretKeyExtended` instead only for BIP32-ed25519 HD wallet keys, which this key type is not.
