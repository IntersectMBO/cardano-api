# cardano-rpc

## What's this

The `cardano-rpc` package provides client and server haskell modules for gRPC interface of `cardano-node`.
It implements [UTxO RPC](https://utxorpc.org/introduction) protobuf communication protocol specification.

## UTxO RPC v1beta spec coverage

### [QueryService](https://utxorpc.org/query/spec/)

| Method | Status |
|--------|--------|
| [ReadParams](https://utxorpc.org/query/spec/#readparamsrequest) | ✅ Supported |
| [ReadUtxos](https://utxorpc.org/query/spec/#readutxosrequest) | ✅ Supported |
| [SearchUtxos](https://utxorpc.org/query/spec/#searchutxosrequest) | ✅ Supported |
| [ReadData](https://utxorpc.org/query/spec/#readdatarequest) | ⬜ Not supported |
| [ReadTx](https://utxorpc.org/query/spec/#queryservice) | ⬜ Not supported |
| [ReadGenesis](https://utxorpc.org/query/spec/#queryservice) | ⬜ Not supported |
| [ReadEraSummary](https://utxorpc.org/query/spec/#queryservice) | ⬜ Not supported |

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
| [FetchBlock](https://utxorpc.org/sync/spec/#fetchblockrequest) | 🚧 In progress (missing: `Block.body.tx`) |
| [DumpHistory](https://utxorpc.org/sync/spec/#dumphistoryrequest) | ⬜ Not supported |
| [FollowTip](https://utxorpc.org/sync/spec/#followtiprequest) | ⬜ Not supported |
| [ReadTip](https://utxorpc.org/sync/spec/#readtiprequest) | ⬜ Not supported |

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

