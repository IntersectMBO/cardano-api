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

## Build and submit a transaction

[`send_lovelace.py`](send_lovelace.py) builds and submits a transaction with [PyCardano](https://pycardano.readthedocs.io/), using cardano-rpc for UTxO queries, protocol parameters, and submission.
It sends 5 ADA from the cluster's `utxo1` wallet to the `utxo2` address, then polls the recipient's UTxOs until the new output appears.
`send_lovelace.py` and the extra dependencies it needs are already in this directory; `requirements.txt` already lists them.

> [!IMPORTANT]
> PyCardano has no cardano-rpc integration, so `send_lovelace.py` defines `CardanoRpcChainContext`, a `pycardano.ChainContext` subclass that talks `v1beta` directly through `utxorpc-spec`, the same generated stubs [`main.py`](main.py) uses.
> It is demo scaffolding for plain ADA payments: it maps oversized numeric values onto the `int` branch of cardano-rpc's `BigInt` type only, and it does not surface datums, inline datums, reference scripts, or script cost models.
> Do not lift it unchanged into a dApp that touches script-locked UTxOs.

The [Prerequisites](#prerequisites) and [Run it](#run-it) sections above set up a running cluster and an activated virtual environment with the pinned dependencies installed.
From this directory, with the virtual environment still active:

```bash
python send_lovelace.py
```

Sample output (your values will differ):

```
Sender address:    addr_test1vzk7y8ppza3qzmqvsmfm6ysc5wxw6kraandep2p06g9yf2s20478e
Recipient address: addr_test1vrgrv8vwfpqu42l3qpxecmfykh82ygyfd8aa8dsqzlh5j0qff9sxr
Spendable UTxOs:   1
Submitted tx: d06a9bf3562671e4975f10abb91e06842bf2a124d11619d27279d0ba2df2671e
Confirmed: 5000000 lovelace landed at addr_test1vrgrv8vwfpqu42l3qpxecmfykh82ygyfd8aa8dsqzlh5j0qff9sxr (d06a9bf3562671e4975f10abb91e06842bf2a124d11619d27279d0ba2df2671e#0)
```

> [!NOTE]
> `requirements.txt` pins `cbor2==5.8.0` and `cbor2pure==5.8.0` alongside `pycardano==0.19.2`.
> PyCardano depends on `cbor2>=5.6.5` with no upper bound, but `cbor2` 6.x renamed APIs (e.g. `FrozenDict`) that PyCardano 0.19.2 still imports, so an unpinned install pulls in a `cbor2` that breaks `import pycardano` outright.
> If you bump `pycardano` in future, check whether a newer release has picked up the `cbor2` 6.x rename before dropping this pin.

`send_lovelace.py` loads the `utxo1` genesis signing key straight from its TextEnvelope file with `PaymentSigningKey.from_json()`.
The file's `type` field is `GenesisUTxOSigningKey_ed25519`, not `PaymentSigningKeyShelley_ed25519`, but `from_json()` only checks the type when called with `validate_type=True`, so the mismatch is harmless here.
