# cardano-rpc

## What's this

The `cardano-rpc` package provides the client and server Haskell modules for cardano-node's built-in gRPC interface ([gRPC](https://grpc.io) is a typed, binary RPC framework served over HTTP/2), implementing the [UTxO RPC](https://utxorpc.org/introduction) specification.
It is enabled with a configuration flag: there is no extra service to run, and it is built, tested, and released together with cardano-node.
Because UTxO RPC is a standard, the same client code also works against other servers that implement it, such as Dolos.
It serves live chain data: the tip, UTxO queries, protocol parameters, and transaction evaluation and submission.
It is not an indexer: there are no address-history or by-hash lookups; see [UTxO RPC v1beta spec coverage](#utxo-rpc-v1beta-spec-coverage) below for the full method status.

## Quickstart

The steps below start a local cluster and make first calls against it; the same configuration works against [any network](#using-your-own-node), from the local cluster used here to mainnet.

### Prerequisites

1. [Nix](https://nixos.org/download/) with flakes enabled; every command below fetches its tools through it.
   The first invocation downloads the toolchain: several minutes and a few gigabytes.
2. A checkout of this repository: every `nix develop`/`nix-shell` command below resolves against it, and the TypeScript example also loads its vendored proto files from it:

   ```bash
   git clone https://github.com/IntersectMBO/cardano-api
   cd cardano-api
   ```

Work through [Start a local cluster](#start-a-local-cluster) and [Make your first call](#make-your-first-call) below (the first calls are optional; each language guide stands on its own once the cluster is running), then follow whichever quickstart in [Language examples](#language-examples) matches your stack.

### Start a local cluster

The `cardano-testnet` command below bundles the matching `cardano-node` and `cardano-cli` binaries, so one command is enough:

```bash
nix run github:IntersectMBO/cardano-node#cardano-testnet -- \
  cardano --num-pool-nodes 1 --enable-grpc-http --grpc-listen-port-base 50051 --output-dir /tmp/demo-cluster
```

This starts a testnet with a single block-producing cardano-node and its gRPC server enabled over plain HTTP/2 without TLS (h2c), and keeps running in the foreground until you press Ctrl+C.
The cluster is ready once it logs `gRPC endpoint of node1: http://127.0.0.1:50051`, typically well under a minute once binaries are cached; open a second terminal for everything below.
The cluster comes with funded test wallets, created at startup under `/tmp/demo-cluster/utxo-keys/utxo1` to `utxo3` (`utxo.skey`, `utxo.vkey`, `utxo.addr`); the language examples' transaction flows spend from `utxo1`.

The gRPC endpoint is `localhost:50051`.
Without `--enable-grpc-http` (i.e. with `--enable-grpc`), you get a Unix socket at `/tmp/demo-cluster/socket/node1/rpc.sock` instead, next to cardano-node's IPC socket; see the [configuration reference](#configuration-reference) for the full set of transports.
Every example below talks to `localhost:50051`.

> [!TIP]
> To use your own binaries instead of the bundled ones, export `CARDANO_NODE` and `CARDANO_CLI` with their paths before running.

> [!NOTE]
> Flag names differ between the two CLIs: `cardano-testnet` takes `--enable-grpc`/`--enable-grpc-http`, `cardano-node` itself takes `--grpc-enable`.
> `cardano-testnet`'s two flags are mutually exclusive: pick the Unix socket or the HTTP/2 listener, not both.

> [!WARNING]
> The output directory must not exist from a previous run; genesis creation fails on leftovers (`Genesis output directory already exists`).
> Run `rm -rf /tmp/demo-cluster` first when retrying, and make sure no cardano-node processes from an earlier attempt are still alive.
>
> Unix socket paths are capped at 108 bytes on Linux, and `cardano-testnet` fails at startup (`pokeSockAddr: path is too long`) when the output directory is nested too deep.
> Keep `--output-dir` shallow, e.g. under `/tmp`.

### Make your first call

For node operators, these calls are a scriptable network equivalent of `cardano-cli query tip` and `query protocol-parameters`, useful for monitoring.

First enter a subshell that puts the tools on PATH (your prompt changes; run the commands below inside it).
`.#rpc-quickstart` bundles every tool the CLI examples and the Rust, TypeScript, Go, and Python language examples below need; the per-language shells (`.#rpc-quickstart-rust`, `.#rpc-quickstart-typescript`, `.#rpc-quickstart-go`, `.#rpc-quickstart-python`) are minimal alternatives if you only want one.
The Haskell example uses the repository's own dev shell instead (`.#rpc-quickstart-haskell`), since it needs the full haskell.nix toolchain rather than a plain nixpkgs shell:

```bash
nix develop .#rpc-quickstart  # recommended
```

or with `nix-shell` (using `cardano-rpc/quickstart/shell.nix`):

```bash
nix-shell cardano-rpc/quickstart/shell.nix
```

or fetch them ad hoc:

```bash
nix shell nixpkgs#buf nixpkgs#grpcurl
```

The server supports gRPC reflection, so it can describe its own services and methods; none of the commands below need local schema files (the `.proto` files describing the API).
List the services it exposes with grpcurl:

```bash
grpcurl -plaintext localhost:50051 list
```

Then inspect one of them:

```bash
grpcurl -plaintext localhost:50051 describe utxorpc.v1beta.query.QueryService
```

buf's equivalent is `buf curl --list-methods --protocol grpc --http2-prior-knowledge http://localhost:50051`.

Read the chain tip (the most recently adopted block) with [buf](https://buf.build/docs/installation):

```bash
buf curl \
  --protocol grpc --http2-prior-knowledge \
  http://localhost:50051/utxorpc.v1beta.sync.SyncService/ReadTip
```

It prints the tip as JSON: slot, hash, height, and timestamp.
Your values will differ per run, and the tip advances between calls.

With grpcurl instead:

```bash
grpcurl -plaintext localhost:50051 utxorpc.v1beta.sync.SyncService/ReadTip
```

Then read the protocol parameters (the chain's current fee, size, and cost limits):

```bash
buf curl \
  --protocol grpc --http2-prior-knowledge -d '{}' \
  http://localhost:50051/utxorpc.v1beta.query.QueryService/ReadParams
```

grpcurl and buf alone are enough to explore every implemented method; the language quickstarts below are only needed if you want to call the API from code.

### Using your own node

Already running a `cardano-node`?
Enable the gRPC server in its configuration file:

```json
{
  "EnableRpc": true,
  "RpcListenPort": 50051
}
```

`RpcListenAddress` defaults to `127.0.0.1`.
The configuration is read at startup, so enabling it requires a node restart.
The same grpcurl/buf commands above work against `<host>:50051`.
See the [configuration reference](#configuration-reference) for the Unix-socket and TLS transports, and [Security](#security) before exposing anything beyond localhost.

### Language examples

- **Rust**: first calls and a transaction example with pallas-txbuilder via the `utxorpc-spec` crate (the `utxorpc` wrapper crate is v1alpha-only, so the example uses the generated `utxorpc-spec` bindings). See [quickstart/rust/README.md](quickstart/rust/README.md).
- **TypeScript**: build and submit a transaction with MeshJS (published UTxO RPC providers are v1alpha-only, so the example ships its own small v1beta provider). See [quickstart/typescript/README.md](quickstart/typescript/README.md).
- **Go**: first calls via the `go-sdk` wrapper (its `cardano` package speaks v1beta directly and builds the h2c cleartext transport for you). See [quickstart/go/README.md](quickstart/go/README.md).
- **Python**: first calls and a transaction example with PyCardano via the `utxorpc-spec` package (the `utxorpc` wrapper package is v1alpha-only, so the example uses the generated `utxorpc-spec` bindings). See [quickstart/python/README.md](quickstart/python/README.md).
- **Haskell**: first calls and a transaction example with cardano-api's experimental transaction API via the `cardano-rpc` package's own client, `Cardano.Rpc.Client` (which re-exports the grapesy gRPC client, with the generated proto-lens bindings separately exposed as package modules, so no separate SDK is needed). See [quickstart/haskell/README.md](quickstart/haskell/README.md).

### Clean up

Stop the cluster with Ctrl+C in its terminal, then remove the cluster directory:

```bash
rm -rf /tmp/demo-cluster
```

### Configuration reference

The gRPC server is off by default.
Enable it with `--grpc-enable` or `EnableRpc: true` in the cardano-node configuration; a node socket path must also be configured.
Settings are read at node startup; changing them requires a restart.

Exactly one transport is active at a time:

1. Unix socket (default): `rpc.sock` next to the node socket, or `--grpc-socket-path` / `RpcSocketPath`.
2. HTTP/2 without TLS (h2c): `--grpc-listen-port` / `RpcListenPort`, optionally `--grpc-listen-address` / `RpcListenAddress` (default `127.0.0.1`).
3. HTTP/2 with TLS: add `--grpc-tls-certificate` and `--grpc-tls-private-key` (`RpcTlsCertificateFile`, `RpcTlsPrivateKeyFile`), optionally repeatable `--grpc-tls-chain-certificate` (`RpcTlsChainCertificateFiles`).

The three transports are mutually exclusive.
TLS additionally requires a listen port, and its certificate and key must be set together; see the [Security](#security) section below before exposing an endpoint beyond localhost.

Clients connect over TCP the same way as in the examples above: replace the Unix-socket connector or `unix://` target with the node's address and port.

## UTxO RPC spec coverage

Methods that are not implemented (⬜, ❌) still respond, with the `UNIMPLEMENTED` gRPC status.
The ❌ methods need a whole-chain index (transaction by hash, datum by hash) that the node does not maintain; use a dedicated chain indexing service for those.

### `v1beta` version
#### [QueryService](https://utxorpc.org/query/spec/)

| Method | Status |
|--------|--------|
| [ReadParams](https://utxorpc.org/query/spec/#readparamsrequest) | ✅ Supported |
| [ReadUtxos](https://utxorpc.org/query/spec/#readutxosrequest) | ✅ Supported |
| [SearchUtxos](https://utxorpc.org/query/spec/#searchutxosrequest) | ✅ Supported for exact-address predicates† |
| [ReadData](https://utxorpc.org/query/spec/#readdatarequest) | ❌ Not supported, needs a chain indexer |
| [ReadTx](https://utxorpc.org/query/spec/#queryservice) | ❌ Not supported, needs a chain indexer |
| [ReadGenesis](https://utxorpc.org/query/spec/#queryservice) | ✅ Supported |
| [ReadEraSummary](https://utxorpc.org/query/spec/#queryservice) | ✅ Supported |
| [ReadState](https://utxorpc.org/query/spec/#queryservice) | ⬜ Not supported |

† The predicate must be a single exact-address match, or `anyOf` over exact-address matches. Other predicate shapes (payment or delegation part only, asset only, `allOf`, `not`) are rejected with `INVALID_ARGUMENT`, because the node can only query the UTxO set by address.

#### [SubmitService](https://utxorpc.org/submit/spec/)

| Method | Status |
|--------|--------|
| [SubmitTx](https://utxorpc.org/submit/spec/#submittx) | ✅ Supported |
| [EvalTx](https://utxorpc.org/submit/spec/#evaltx) | ✅ Supported |
| [WaitForTx](https://utxorpc.org/submit/spec/#waitfortx) | ⬜ Not supported |
| [ReadMempool](https://utxorpc.org/submit/spec/#readmempool) | ✅ Supported |
| [WatchMempool](https://utxorpc.org/submit/spec/#watchmempool) | ✅ Supported |

#### [SyncService](https://utxorpc.org/sync/spec/)

| Method | Status |
|--------|--------|
| [FetchBlock](https://utxorpc.org/sync/spec/#fetchblockrequest) | ✅ Supported |
| [DumpHistory](https://utxorpc.org/sync/spec/#dumphistoryrequest) | ⬜ Not supported |
| [FollowTip](https://utxorpc.org/sync/spec/#followtiprequest) | ✅ Supported |
| [ReadTip](https://utxorpc.org/sync/spec/#readtiprequest) | ✅ Supported |

#### [WatchService](https://utxorpc.org/watch/spec/)

| Method | Status |
|--------|--------|
| [WatchTx](https://utxorpc.org/watch/spec/#watchservice) | ⬜ Not supported |

### `v1alpha` version

`v1alpha` endpoint is not provided by `cardano-rpc`.

## Other gRPC services

Besides the UTxO RPC spec above, `cardano-rpc` implements the standard [gRPC Server Reflection Protocol](https://github.com/grpc/grpc/blob/master/doc/server-reflection.md), so tools such as `grpcurl` can list and describe the server's services without needing local `.proto` files.

| Method | Status |
|--------|--------|
| [grpc.reflection.v1.ServerReflection/ServerReflectionInfo](https://github.com/grpc/grpc/blob/master/doc/server-reflection.md) | ✅ Supported |
| [grpc.reflection.v1alpha.ServerReflection/ServerReflectionInfo](https://github.com/grpc/grpc/blob/master/doc/server-reflection.md) | ✅ Supported |

## Building

You need the following dependencies installed on your system:

- `ghc` with version >= 9.6.6
- `cabal`
- [`snappy`](https://github.com/google/snappy) development files (`libsnappy-dev` in Ubuntu)
- [`protobuf`](https://developers.google.com/protocol-buffers/) compiler (`protobuf-compiler` in Ubuntu)

### Generating the Haskell code from proto definitions

You need to install `buf` and `proto-lens-protoc`.
1. Follow the `buf` installation guide at: https://buf.build/docs/cli/installation/
1. To install Haskell protobuf code compiler:
    ```bash
    cabal install proto-lens-protoc
    ```

1. Generate Haskell code using:
    ```bash
    ( cd cardano-rpc/ ; buf generate proto )
    ```
    This will output the generated Haskell code into `cardano-rpc/gen` directory.

### Building the Haskell code

To build the package use the following command:
```bash
cabal build cardano-rpc
```

## Security

The RPC server has no authentication or authorisation: every method is open to anyone who can reach the listener, including transaction submission and script evaluation.
Everything served is public chain data, so the concern is resource consumption and node exposure rather than confidentiality.

Defaults are conservative: the server is off unless `--grpc-enable` is given, it listens on a Unix socket by default, and `--grpc-listen-port` binds `127.0.0.1` unless another address is given.
The node warns at startup when RPC is enabled on a block-producing node.

TLS encrypts the connection and lets clients verify the node; it does not restrict who may call, since there is no client-certificate support.
A TLS listener on a public address is as open as a cleartext one.

For deployment, keep the listener on loopback or a trusted network segment.
Anywhere else, front it with a reverse proxy that terminates TLS and handles authentication and rate limiting.

The server writes TLS key-log material if `SSLKEYLOGFILE` is set in its environment.
