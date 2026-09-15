# Python quickstart

The published [`utxorpc-spec`](https://pypi.org/project/utxorpc-spec/) package already ships grpcio-generated v1beta bindings, so no proto files and no protoc are needed.
The responses are plain generated protobuf messages; there is no PyCardano integration yet, so mapping them onto PyCardano's types is up to you.
`main.py` and `requirements.txt` are already in this directory; no project scaffolding needed.

> [!IMPORTANT]
> Use `utxorpc-spec` directly, not the higher-level `utxorpc` SDK package: that wrapper is hardwired to the older `utxorpc.v1alpha` services and cannot talk to cardano-rpc, which serves `v1beta` (v1beta packaging for the SDKs is tracked in utxorpc/spec#209).

## Prerequisites

Start a local cluster as described in the main [Quickstart](../../README.md#quickstart).
`main.py` here connects to `localhost:50051`, the cluster's gRPC address.
The example itself runs anywhere once you copy these files; only the `nix develop` and `nix-shell` commands below need the repository checkout.

## Run it

From the repository root, `cd cardano-rpc/quickstart/python`, then get `python3` from the repository's flake (the `.` flake reference resolves to the repository root from anywhere inside the clone):

```bash
nix develop .#rpc-quickstart-python
```

or with `nix-shell` (using the `shell.nix` in this directory):

```bash
nix-shell
```

or fetch it ad hoc:

```bash
nix shell nixpkgs#python3
```

> [!NOTE]
> Using `nix develop` or `nix-shell` above? They already handle this; skip this note.
> Otherwise, the ad hoc shell can fail at import time with `ImportError: libstdc++.so.6: cannot open shared object file`, because the `grpcio` wheel `pip install` fetches is a manylinux build that links against the host's `libstdc++`, which a plain nixpkgs shell does not put on the loader path.
> Use `nix shell nixpkgs#python3 nixpkgs#stdenv.cc.cc.lib` instead and export `LD_LIBRARY_PATH="$(nix eval --raw nixpkgs#stdenv.cc.cc.lib.outPath)/lib"` before installing.

Then, from this directory, create a virtual environment and install the pinned dependencies (Python 3.10 or newer; the nix shells satisfy this):

```bash
python3 -m venv venv
source venv/bin/activate
pip install -r requirements.txt
python main.py
```

(After activation, `python` points at the venv's interpreter, so plain `python main.py` is correct.)

Sample output (your values will differ):

```
Tip: slot 65 height 5 hash bc51dd8ca524466726cd0f4f50d5ae481eda1630bf7dba59c3cb10849547f432
Protocol parameters: max_tx_size 16384 max_block_body_size 65536
```

grpcio can also connect straight to the Unix socket with a `unix:` target (e.g. `grpc.insecure_channel("unix:///tmp/demo-cluster/socket/node1/rpc.sock")`), for a node started with `--enable-grpc` instead.
