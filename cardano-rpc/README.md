# cardano-rpc

## What's this

The `cardano-rpc` package provides client and server haskell modules for gRPC interface of `cardano-node`.
It implements [UTxO RPC](https://utxorpc.org/introduction) protobuf communication protocol specification.

## UTxO RPC v1beta spec coverage

Methods marked ⬜ or ❌ are exposed by the server but respond with the `UNIMPLEMENTED` gRPC status.
Methods marked ❌ cannot be served by `cardano-node` at all: they need a whole-chain index (transaction by hash, datum by hash) that the node does not maintain, and supporting them would mean building an external chain indexer into the node.
Use a dedicated chain indexing service for those.

### [QueryService](https://utxorpc.org/query/spec/)

| Method | Status |
|--------|--------|
| [ReadParams](https://utxorpc.org/query/spec/#readparamsrequest) | ✅ Supported |
| [ReadUtxos](https://utxorpc.org/query/spec/#readutxosrequest) | ✅ Supported |
| [SearchUtxos](https://utxorpc.org/query/spec/#searchutxosrequest) | ✅ Supported |
| [ReadData](https://utxorpc.org/query/spec/#readdatarequest) | ❌ Not supported, needs a chain indexer |
| [ReadTx](https://utxorpc.org/query/spec/#queryservice) | ❌ Not supported, needs a chain indexer |
| [ReadGenesis](https://utxorpc.org/query/spec/#queryservice) | ✅ Supported |
| [ReadEraSummary](https://utxorpc.org/query/spec/#queryservice) | ✅ Supported |
| [ReadState](https://utxorpc.org/query/spec/#queryservice) | ⬜ Not supported |

### [SubmitService](https://utxorpc.org/submit/spec/)

| Method | Status |
|--------|--------|
| [SubmitTx](https://utxorpc.org/submit/spec/#submittx) | ✅ Supported |
| [EvalTx](https://utxorpc.org/submit/spec/#evaltx) | ✅ Supported |
| [WaitForTx](https://utxorpc.org/submit/spec/#waitfortx) | ⬜ Not supported |
| [ReadMempool](https://utxorpc.org/submit/spec/#readmempool) | ⬜ Not supported |
| [WatchMempool](https://utxorpc.org/submit/spec/#watchmempool) | ⬜ Not supported |

### [SyncService](https://utxorpc.org/sync/spec/)

| Method | Status |
|--------|--------|
| [FetchBlock](https://utxorpc.org/sync/spec/#fetchblockrequest) | ✅ Supported |
| [DumpHistory](https://utxorpc.org/sync/spec/#dumphistoryrequest) | ⬜ Not supported |
| [FollowTip](https://utxorpc.org/sync/spec/#followtiprequest) | ✅ Supported |
| [ReadTip](https://utxorpc.org/sync/spec/#readtiprequest) | ✅ Supported |

### [WatchService](https://utxorpc.org/watch/spec/)

| Method | Status |
|--------|--------|
| [WatchTx](https://utxorpc.org/watch/spec/#watchservice) | ⬜ Not supported |

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

### Building the haskell code

To build the package use the following command:
```bash
cabal build cardano-rpc
```

## Security

The RPC server has no authentication or authorisation: every method is open to anyone who can reach the listener, including transaction submission and script evaluation.
Everything served is public chain data, so the concern is resource consumption and node exposure rather than confidentiality.

Defaults are conservative: the server is off unless `--grpc-enable` is given, it listens on a unix socket by default, and `--grpc-listen-port` binds `127.0.0.1` unless another address is given.
The node warns at startup when RPC is enabled on a block-producing node.

TLS encrypts the connection and lets clients verify the node; it does not restrict who may call, since there is no client-certificate support.
A TLS listener on a public address is as open as a cleartext one.

For deployment, keep the listener on loopback or a trusted network segment.
Anywhere else, front it with a reverse proxy that terminates TLS and handles authentication and rate limiting, the pattern recommended in ADR-018.

The server writes TLS key-log material if `SSLKEYLOGFILE` is set in its environment.
