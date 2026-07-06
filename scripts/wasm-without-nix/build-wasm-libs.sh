#!/usr/bin/env bash
# Build libsodium, libsecp256k1, and libblst for wasm32-wasi and install them
# into a single prefix that cabal.project can point at via extra-lib-dirs /
# extra-include-dirs. Mirrors ./nix/{libsodium,secp256k1,blst}.nix but uses
# wasm32-wasi-clang directly (no Nix).
#
# Requirements on PATH: wasm32-wasi-clang, wasm-ld, llvm-ar and llvm-ranlib
# (all provided by the wasi-sdk / ghc-wasm environment), plus autoreconf,
# automake, libtoolize, make, git, pkg-config, ar, file, mktemp and nproc.

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

Requirements on PATH: wasm32-wasi-clang, wasm-ld, llvm-ar and llvm-ranlib
(all provided by the wasi-sdk / ghc-wasm environment), plus autoreconf,
automake, libtoolize, make, git, pkg-config, ar, file, mktemp and nproc.

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

# Check for all the required tools up front so that failures are detected
# early with a clear message. libtoolize is checked instead of libtool
# because some distros (e.g. Debian) package libtool without a libtool
# executable, and the autogen.sh scripts only need libtoolize.
tools_missing=()
for t in autoreconf automake libtoolize make git pkg-config ar file mktemp nproc; do
    command -v "$t" >/dev/null 2>&1 || tools_missing+=("$t")
done
if [ "${#tools_missing[@]}" -gt 0 ]; then
    cat >&2 <<EOF
Error: required tool(s) not on PATH: ${tools_missing[*]}

Install them with your system's package manager and re-run this script.
EOF
    exit 1
fi

# Toolchain check: the wasm32-wasi-* / llvm-* tools drive the compilation,
# linking and archiving of the libraries. They all come with the wasi-sdk /
# ghc-wasm env.
toolchain_missing=()
for t in wasm32-wasi-clang wasm-ld llvm-ar llvm-ranlib; do
    command -v "$t" >/dev/null 2>&1 || toolchain_missing+=("$t")
done
if [ "${#toolchain_missing[@]}" -gt 0 ]; then
    cat >&2 <<EOF
Error: wasm toolchain not on PATH (missing: ${toolchain_missing[*]}).

The ghc-wasm / wasi-sdk environment doesn't appear to be active. Activate it
with:

  source ~/.ghc-wasm/env

then re-run this script. If it is not installed, see
https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/-/tree/master
for installation instructions.
EOF
    exit 1
fi

LIBSODIUM_REV="9511c982fb1d046470a8b42aa36556cdb7da15de"
LIBSODIUM_REPO="https://github.com/jedisct1/libsodium.git"
SECP256K1_REV="acf5c55ae6a94e5ca847e07def40427547876101"
SECP256K1_REPO="https://github.com/bitcoin-core/secp256k1.git"
BLST_REV="8c7db7fe8d2ce6e76dc398ebd4d475c0ec564355"
BLST_REPO="https://github.com/supranational/blst.git"

mkdir -p "$ABS_PREFIX/lib" "$ABS_PREFIX/include" "$ABS_PREFIX/lib/pkgconfig"
mkdir -p "$ABS_SRC_ROOT"

clone_if_missing() {
    local repo="$1" dir="$2" rev="$3"
    if [ ! -d "$dir/.git" ]; then
        git clone "$repo" "$dir"
    fi
    git -C "$dir" fetch --tags origin "$rev" 2>/dev/null || true
    git -C "$dir" checkout "$rev"
}

build_libsodium() {
    local dir="$ABS_SRC_ROOT/libsodium"
    clone_if_missing "$LIBSODIUM_REPO" "$dir" "$LIBSODIUM_REV"
    cd "$dir"
    [ -x ./configure ] || ./autogen.sh -s
    CC=wasm32-wasi-clang AR=llvm-ar RANLIB=llvm-ranlib \
        ./configure --host=wasm32-wasi --prefix="$ABS_PREFIX"
    make -j"$(nproc)"
    make install
    wasm32-wasi-clang -shared -Wl,--whole-archive \
        "$ABS_PREFIX/lib/libsodium.a" \
        -o "$ABS_PREFIX/lib/libsodium.so"
    cd -
}

build_secp256k1() {
    local dir="$ABS_SRC_ROOT/secp256k1"
    clone_if_missing "$SECP256K1_REPO" "$dir" "$SECP256K1_REV"
    cd "$dir"
    [ -x ./configure ] || ./autogen.sh
    CC=wasm32-wasi-clang AR=llvm-ar RANLIB=llvm-ranlib \
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
    cd -
}

build_blst() {
    local dir="$ABS_SRC_ROOT/blst"
    clone_if_missing "$BLST_REPO" "$dir" "$BLST_REV"
    cd "$dir"
    CC=wasm32-wasi-clang AR=llvm-ar RANLIB=llvm-ranlib ./build.sh

    cp libblst.a "$ABS_PREFIX/lib/"
    cp bindings/blst.h bindings/blst_aux.h "$ABS_PREFIX/include/"

    wasm32-wasi-clang -shared -Wl,--whole-archive \
        "$ABS_PREFIX/lib/libblst.a" \
        -o "$ABS_PREFIX/lib/libblst.so"

    local version
    version="$(git -C "$dir" describe --tags --always 2>/dev/null || echo 0.0.0)"
    # pkg-config cannot compare versions that have a leading 'v' or a
    # '-<n>-g<hash>' git-describe suffix, and cabal relies on that comparison
    # (e.g. cardano-crypto-class declares 'pkgconfig-depends: libblst >= 0.3.14').
    version="${version#v}"
    version="${version%%-*}"
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
    cd -
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
echo
echo "Some packages (e.g. cardano-crypto-class and cardano-crypto-praos) find"
echo "these libraries through pkg-config. Since $ABS_PREFIX/lib/pkgconfig is"
echo "not a standard pkg-config location, export it before building:"
echo
echo "  export PKG_CONFIG_PATH=$ABS_PREFIX/lib/pkgconfig"

FRAGMENT="$ABS_PREFIX/wasm-libs-without-nix.cabal"
cat > "$FRAGMENT" <<EOF
-- Generated by scripts/wasm-without-nix/build-wasm-libs.sh. The paths are
-- absolute because relative extra-lib-dirs are not resolved reliably for
-- non-local packages; re-run the script if this checkout or the prefix moves.
shared: True

package cardano-crypto-class
  extra-lib-dirs: $ABS_PREFIX/lib
  extra-include-dirs: $ABS_PREFIX/include

package cardano-crypto-praos
  extra-lib-dirs: $ABS_PREFIX/lib
  extra-include-dirs: $ABS_PREFIX/include
EOF
echo
echo "Wrote project fragment: $FRAGMENT"

CABAL_PROJECT="$PROJECT_DIR/cabal.project"

case "$FRAGMENT" in
    "$PROJECT_DIR"/*) IMPORT_PATH="${FRAGMENT#"$PROJECT_DIR"/}" ;;
    *)                IMPORT_PATH="$FRAGMENT" ;;
esac

if grep -qF "wasm-libs-without-nix.cabal" "$CABAL_PROJECT"; then
    echo "cabal.project already imports wasm-libs-without-nix.cabal — nothing to do."
else
    read -r -p "Add 'if arch(wasm32) / import: $IMPORT_PATH' block to cabal.project? [y/N] " answer || answer=""
    case "${answer,,}" in
        y|yes)
            cat >> "$CABAL_PROJECT" <<EOF

if arch(wasm32)
  import: $IMPORT_PATH
EOF
            echo "Added to $CABAL_PROJECT"
            ;;
        *)
            cat <<EOF
Skipped. To enable, append to cabal.project:

if arch(wasm32)
  import: $IMPORT_PATH
EOF
            ;;
    esac
fi
