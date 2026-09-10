# TypeScript quickstart

Build and submit a transaction with [MeshJS](https://meshjs.dev), using cardano-rpc for UTxO queries, protocol parameters, and submission.
The example sends 5 ADA from the cluster's `utxo1` wallet to the `utxo2` address.
`package.json`, `cardano-rpc-provider.mjs` and `send-lovelace.mjs` are already in this directory.

> [!IMPORTANT]
> `@meshsdk/provider`'s off-the-shelf `U5CProvider` does not work against cardano-rpc: it is pinned to `@utxorpc/sdk` 0.6.x, which speaks the older `utxorpc.v1alpha` services, while cardano-rpc serves `v1beta` (v1beta packaging for the SDKs is tracked in utxorpc/spec#209).
> Until the SDKs move to v1beta, [`cardano-rpc-provider.mjs`](cardano-rpc-provider.mjs) talks v1beta directly by loading the proto files at runtime.
>
> The provider is demo scaffolding for plain ADA and native-asset payments: it maps oversized numeric values onto the `int` variant only, and it does not surface datums, inline datums, or reference scripts.
> Do not lift it unchanged into a dApp that touches script-locked UTxOs.

## Prerequisites

Start a local cluster as described in the main [Quickstart](../../README.md#quickstart).
It talks to the cluster's gRPC endpoint at `localhost:50051`.

## Run it

From this directory, get `node` from the repository's flake (the `.` flake reference resolves to the repository root from anywhere inside the clone):

```bash
nix develop .#rpc-quickstart-typescript
```

or with `nix-shell` (using the `shell.nix` in this directory):

```bash
nix-shell
```

or fetch it ad hoc:

```bash
nix shell nixpkgs#nodejs
```

Install the dependencies (pinned versions are in `package.json`):

```bash
npm install
```

[`cardano-rpc-provider.mjs`](cardano-rpc-provider.mjs) loads the proto files vendored in this repository, from `../../proto` relative to the current directory by default.
Run the command below from this directory, or set `CARDANO_RPC_PROTO` to point at a `cardano-rpc/proto` directory elsewhere:

```bash
node send-lovelace.mjs
```

Sample output (your values will differ):

```
Sender address:    addr_test1vp0cg0r2w9xczav4g0txn6suy9z0g24er7h25eqk639hwfgcmtj72
Recipient address: addr_test1vp0fsh3r9t3zmsfkv27qkwh66vudurnttpy80f8yjagxqyqz27px0
Spendable UTxOs:   1
Submitted tx: c8f9332364a81e687599a0b1c4599cd8bff0213c3a4c23a45a7d525ccc124018
Confirmed: 5000000 lovelace landed at addr_test1vp0fsh3r9t3zmsfkv27qkwh66vudurnttpy80f8yjagxqyqz27px0 (c8f9332364a81e687599a0b1c4599cd8bff0213c3a4c23a45a7d525ccc124018#0)
```

See [`send-lovelace.mjs`](send-lovelace.mjs) for the full script: building the transaction with `MeshTxBuilder`, then confirming by polling the recipient's UTxOs for the new output.

`@grpc/grpc-js` can also target the Unix socket directly with a `unix://` address, for a node started with `--enable-grpc` instead.
