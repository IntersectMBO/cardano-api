# Go quickstart

The published [`github.com/utxorpc/go-sdk`](https://github.com/utxorpc/go-sdk) module (tag `v0.1.0`) is a higher-level wrapper client for UTxO RPC.
Its `cardano` subpackage speaks `utxorpc.v1beta.*` directly: underneath, it uses the same generated [`go-codegen`](https://github.com/utxorpc/go-codegen) connect-go bindings (pinned at `v0.19.2`) as a plain generated client would, so no proto files and no protoc are needed either way.
`go.mod` and `main.go` are already in this directory; no `go mod init` needed.

> [!NOTE]
> `client.UtxorpcClient` exposes the underlying `Query`, `Sync`, `Submit`, and `Watch` connect-go clients for any method the `cardano` package has no dedicated helper for.
> Stored headers (set with `client.UtxorpcClient.SetHeader`, e.g. an API key) are injected automatically on every call, including through that field.

## Prerequisites

Start a local cluster as described in the main [Quickstart](../../README.md#quickstart).
`go.mod` and `main.go` here connect to `localhost:50051`, the cluster's gRPC address.
The example itself builds anywhere once you copy these files; only the `nix develop` and `nix-shell` commands below need the repository checkout.

## Run it

From this directory, get `go` from the repository's flake (the `.` flake reference resolves to the repository root from anywhere inside the clone):

```bash
nix develop .#rpc-quickstart-go
```

or with `nix-shell` (using the `shell.nix` in this directory):

```bash
nix-shell
```

or fetch it ad hoc:

```bash
nix shell nixpkgs#go
```

Then, from this directory:

```bash
go run .
```

Sample output (your values will differ):

```
Tip: slot 1210 height 70 hash 1ef64ae4effb955bd6ab214f214705f6cb49338ce5a98d9ba3e4eaf62a2add2d
Protocol parameters: max_tx_size 16384 max_block_body_size 65536
```

cardano-rpc serves plain gRPC over cleartext HTTP/2 (h2c), not the Connect protocol.
The `http://` scheme in `sdk.WithBaseUrl` is what tells the SDK to build a cleartext HTTP/2 client and speak gRPC framing over it; any other scheme (or none) makes it dial TLS instead, so pointing `rpcURL` at `localhost:50051` without a scheme would fail.
To connect to the Unix socket a node started with `--enable-grpc` exposes instead, pass `sdk.WithHttpClient` a custom `http.Client` whose transport dials `net.Dial("unix", socketPath)`; `sdk.WithDialTimeout` and `sdk.WithRequestTimeout` are then ignored, since a custom HTTP client bypasses the SDK's own dial and timeout handling.
