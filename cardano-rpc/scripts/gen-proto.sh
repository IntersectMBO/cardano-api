#!/usr/bin/env bash
#
# Regenerates the Haskell code for the proto definitions:
#
# - gen/: the definitions in proto/ (see buf.gen.yaml).
# - gen-wkt/: the google.protobuf well-known types that proto/ imports. Only
#   the wasm32 build uses these; other builds take them from
#   proto-lens-protobuf-types, which does not build for wasm32.
#
# buf, protoc and proto-lens-protoc must be on PATH (the nix dev shell has them).

set -euo pipefail

cd "$(dirname "${BASH_SOURCE[0]}")/.."

rm -rf gen gen-wkt

buf generate proto

mapfile -t well_known_types < <(grep -rhoE '^import "google/protobuf/[^"]+\.proto"' proto | cut -d'"' -f2 | sort -u)

mkdir gen-wkt
protoc \
  --proto_path="$(dirname "$(dirname "$(command -v protoc)")")/include" \
  --plugin=protoc-gen-haskell="$(command -v proto-lens-protoc)" \
  --haskell_out=gen-wkt \
  "${well_known_types[@]}"
