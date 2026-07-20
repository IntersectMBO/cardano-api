#!/usr/bin/env bash
# Assemble the static demo site.
#
#   ./build.sh --wasm-dir ../lib-wrapper [--out dist]
#
# --wasm-dir must contain the built cardano-wasm library (cardano-wasm.wasm,
# cardano-wasm.js, cardano-api.js, main.js). See README.md for how to build it.
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
WASM_DIR=
OUT="$HERE/dist"
while [ $# -gt 0 ]; do
  case "$1" in
    --wasm-dir)
      [ $# -ge 2 ] || { echo "error: --wasm-dir requires an argument" >&2; exit 2; }
      WASM_DIR="$(cd "$2" && pwd)"; shift 2
      ;;
    --out)
      [ $# -ge 2 ] || { echo "error: --out requires an argument" >&2; exit 2; }
      mkdir -p "$2"; OUT="$(cd "$2" && pwd)"; shift 2
      ;;
    *) echo "unknown argument: $1" >&2; exit 2 ;;
  esac
done
if [ -z "$WASM_DIR" ]; then
  echo "usage: ./build.sh --wasm-dir <dir> [--out <dir>]" >&2
  exit 2
fi
for f in cardano-wasm.wasm cardano-wasm.js cardano-api.js main.js; do
  if [ ! -f "$WASM_DIR/$f" ]; then
    echo "error: $WASM_DIR/$f not found — build the cardano-wasm library first (see README.md)" >&2
    exit 1
  fi
done
mkdir -p "$OUT"
(cd "$HERE" && elm make src/Main.elm --optimize --output="$OUT/elm.js")
cp "$HERE"/web/* "$OUT"/
cp "$WASM_DIR"/cardano-wasm.wasm "$WASM_DIR"/cardano-wasm.js "$WASM_DIR"/cardano-api.js "$WASM_DIR"/main.js "$OUT"/
echo "demo assembled in $OUT"
