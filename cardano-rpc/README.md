# cardano-rpc

## What's this

The `cardano-rpc` package provides client and server haskell modules for gRPC interface of `cardano-node`.
It implements [UTxO RPC](https://utxorpc.org/introduction) protobuf communication protocol specification.

## Building

You need the following dependencies installed on your system:

- `ghc` with version >= 9.6.6 and < 9.12
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

