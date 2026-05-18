#!/usr/bin/env bash
# Build libsodium, libsecp256k1, and libblst for wasm32-wasi and install them
# into a single prefix that cabal.project can point at via extra-lib-dirs /
# extra-include-dirs. Mirrors ./nix/{libsodium,secp256k1,blst}.nix but uses
# wasm32-wasi-clang directly (no Nix).
#
# Requirements on PATH: wasm32-wasi-clang (from wasi-sdk), autoreconf,
# automake, libtool, make, git, pkg-config.

set -euo pipefail

# Script must be run from the root of the project
PROJECT_DIR="$(pwd)"
[ -f "$PROJECT_DIR/cabal.project" ] || {
    echo "Error: cabal.project not found in $PROJECT_DIR. Run this script from the root of the project" >&2
    exit 1
}

missing=()
[ -n "${PREFIX:-}" ]   || missing+=(PREFIX)
[ -n "${SRC_ROOT:-}" ] || missing+=(SRC_ROOT)
if [ "${#missing[@]}" -gt 0 ]; then
    cat >&2 <<EOF
Error: required environment variable(s) not set: ${missing[*]}

Both values are interpreted relative to the cabal.project directory
($PROJECT_DIR) unless given as an absolute path.

  PREFIX    where the wasm libs/headers will be installed, e.g. wasm-libs.
            Will be created if missing. Point cabal.project's
            extra-lib-dirs / extra-include-dirs at \$PREFIX/lib and
            \$PREFIX/include.

  SRC_ROOT  where the libsodium / secp256k1 / blst source trees live (or
            will be cloned into as \$SRC_ROOT/{libsodium,secp256k1,blst}),
            e.g. wasm-deps.

Example:
  PREFIX=wasm-libs SRC_ROOT=wasm-libs-src $0
EOF
    exit 1
fi

resolve_rel() {
    case "$1" in
        /*) printf '%s\n' "$1" ;;
        *)  printf '%s\n' "$PROJECT_DIR/$1" ;;
    esac
}

ABS_PREFIX="$(resolve_rel "$PREFIX")"
ABS_SRC_ROOT="$(resolve_rel "$SRC_ROOT")"

# Toolchain check: wasm32-wasi-clang must be on PATH (drives both compile and
# link). The other wasm32-wasi-* tools come along with it via the wasi-sdk /
# ghc-wasm env.
toolchain_missing=()
for t in wasm32-wasi-clang wasm-ld; do
    command -v "$t" >/dev/null 2>&1 || toolchain_missing+=("$t")
done
if [ "${#toolchain_missing[@]}" -gt 0 ]; then
    cat >&2 <<EOF
Error: wasm toolchain not on PATH (missing: ${toolchain_missing[*]}).

The ghc-wasm / wasi-sdk environment doesn't appear to be active. Activate it
with:

  source ~/.ghc-wasm/env

then re-run this script.
EOF
    exit 1
fi

LIBSODIUM_REV="9511c982fb1d046470a8b42aa36556cdb7da15de"
LIBSODIUM_REPO="https://github.com/jedisct1/libsodium.git"
SECP256K1_REPO="https://github.com/bitcoin-core/secp256k1.git"
BLST_REPO="https://github.com/supranational/blst.git"

mkdir -p "$ABS_PREFIX/lib" "$ABS_PREFIX/include" "$ABS_PREFIX/lib/pkgconfig"

clone_if_missing() {
    local repo="$1" dir="$2" rev="${3:-}"
    if [ ! -d "$dir/.git" ]; then
        git clone "$repo" "$dir"
    fi
    if [ -n "$rev" ]; then
        git -C "$dir" fetch --tags origin "$rev" 2>/dev/null || true
        git -C "$dir" checkout "$rev"
    fi
}

build_libsodium() {
    local dir="$ABS_SRC_ROOT/libsodium"
    clone_if_missing "$LIBSODIUM_REPO" "$dir" "$LIBSODIUM_REV"
    cd "$dir"
    [ -x ./configure ] || ./autogen.sh -s
    ./configure --host=wasm32-wasi --prefix="$ABS_PREFIX"
    make -j"$(nproc)"
    make install
    wasm32-wasi-clang -shared -Wl,--whole-archive \
        "$ABS_PREFIX/lib/libsodium.a" \
        -o "$ABS_PREFIX/lib/libsodium.so"
}

build_secp256k1() {
    local dir="$ABS_SRC_ROOT/secp256k1"
    clone_if_missing "$SECP256K1_REPO" "$dir"
    cd "$dir"
    [ -x ./configure ] || ./autogen.sh
    ./configure \
        --host=wasm32-wasi \
        --enable-module-schnorrsig \
        --prefix="$ABS_PREFIX" \
        SECP_CFLAGS=-fPIC
    make -j"$(nproc)"
    make install
    wasm32-wasi-clang -shared -Wl,--whole-archive \
        "$ABS_PREFIX/lib/libsecp256k1.a" \
        -o "$ABS_PREFIX/lib/libsecp256k1.so"
}

build_blst() {
    local dir="$ABS_SRC_ROOT/blst"
    clone_if_missing "$BLST_REPO" "$dir"
    cd "$dir"
    CC=wasm32-wasi-clang ./build.sh

    cp libblst.a "$ABS_PREFIX/lib/"
    cp bindings/blst.h bindings/blst_aux.h "$ABS_PREFIX/include/"

    wasm32-wasi-clang -shared -Wl,--whole-archive \
        "$ABS_PREFIX/lib/libblst.a" \
        -o "$ABS_PREFIX/lib/libblst.so"

    local version
    version="$(git -C "$dir" describe --tags --always 2>/dev/null || echo 0.0.0)"
    cat > "$ABS_PREFIX/lib/pkgconfig/libblst.pc" <<EOF
prefix=$ABS_PREFIX
exec_prefix=\${prefix}
libdir=\${exec_prefix}/lib
includedir=\${prefix}/include

Name: libblst
Description: blst BLS12-381 signature library
URL: https://github.com/supranational/blst
Version: $version

Cflags: -I\${includedir}
Libs: -L\${libdir} -lblst
Libs.private:
EOF
}

verify_wasm() {
    local archive="$1"
    local tmp
    tmp="$(mktemp -d)"
    ( cd "$tmp" && ar x "$archive" && \
      first_obj="$(ls *.o 2>/dev/null | head -1)" && \
      [ -n "$first_obj" ] && file "$first_obj" | grep -q WebAssembly )
    local rc=$?
    rm -rf "$tmp"
    if [ "$rc" -ne 0 ]; then
        echo "ERROR: $archive does not appear to contain wasm objects" >&2
        exit 1
    fi
}

build_libsodium 2>&1 | sed -u $'s/^/\033[1;32mlibsodium\033[0m > /'
build_secp256k1 2>&1 | sed -u $'s/^/\033[1;36msecp256k1\033[0m > /'
build_blst      2>&1 | sed -u $'s/^/\033[1;33mblst\033[0m      > /'

verify_wasm "$ABS_PREFIX/lib/libsodium.a"
verify_wasm "$ABS_PREFIX/lib/libsecp256k1.a"
verify_wasm "$ABS_PREFIX/lib/libblst.a"

echo
echo "Done. Installed to: $ABS_PREFIX"

FRAGMENT="$PROJECT_DIR/wasm-libs-without-nix.cabal"
cat > "$FRAGMENT" <<EOF
shared: True

package cardano-crypto-class
  extra-lib-dirs: $ABS_PREFIX/lib
  extra-include-dirs: $ABS_PREFIX/include

package cardano-crypto-praos
  extra-lib-dirs: $ABS_PREFIX/lib
  extra-include-dirs: $ABS_PREFIX/include
EOF
echo "Wrote project fragment: $FRAGMENT"

CABAL_PROJECT="$PROJECT_DIR/cabal.project"

if grep -qF "wasm-libs-without-nix.cabal" "$CABAL_PROJECT"; then
    echo "cabal.project already imports wasm-libs-without-nix.cabal — nothing to do."
else
    read -r -p "Add 'if arch(wasm32) / import: wasm-libs-without-nix.cabal' block to cabal.project? [y/N] " answer || answer=""
    case "${answer,,}" in
        y|yes)
            cat >> "$CABAL_PROJECT" <<'EOF'

if arch(wasm32)
  import: wasm-libs-without-nix.cabal
EOF
            echo "Added to $CABAL_PROJECT"
            ;;
        *)
            cat <<'EOF'
Skipped. To enable, append to cabal.project:

if arch(wasm32)
  import: wasm-libs-without-nix.cabal
EOF
            ;;
    esac
fi
