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
nix develop .#rpc-quickstart-rust
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
