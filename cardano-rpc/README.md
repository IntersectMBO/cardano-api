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

Then do:
```bash
cabal build cardano-rpc
```

