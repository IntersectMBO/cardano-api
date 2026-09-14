#!/usr/bin/env bash
set -euo pipefail

# Open grpcui against a cardano-rpc endpoint. The schema comes from the
# server's reflection service, so no local proto files are needed.
#
# Usage: grpcui.sh [ADDRESS]
#   ADDRESS is host:port (the default matches cardano-testnet started with
#   --enable-grpc-http --grpc-listen-port-base 50051), or a path to the
#   node's rpc.sock when it listens on a Unix socket (e.g. rpc.sock or ./rpc.sock).

endpoint="${1:-localhost:50051}"
flags=()

if [[ -S "$endpoint" ]]; then
  flags=(-unix)
elif [[ "$endpoint" == */* || "$endpoint" == *.sock ]]; then
  echo "No Unix socket at $endpoint" >&2
  exit 1
fi

exec grpcui "${flags[@]}" -plaintext "$endpoint"
