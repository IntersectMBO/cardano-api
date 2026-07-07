#!/usr/bin/env bash
# Build libsodium, libsecp256k1, and libblst for wasm32-wasi and install them
# into a single prefix that cabal.project can point at via extra-lib-dirs /
# extra-include-dirs. Mirrors ./nix/{libsodium,secp256k1,blst}.nix but uses
# wasm32-wasi-clang directly (no Nix).
#
# Usage:
#   PREFIX=wasm-libs SRC_ROOT=wasm-libs-src ./scripts/wasm-without-nix/build-wasm-libs.sh [--yes] [--force]
#
#   --yes    add the import block to cabal.project without prompting
#   --force  rebuild the libraries even if they are already installed in PREFIX
#
# Requirements on PATH: wasm32-wasi-clang, wasm-ld, llvm-ar and llvm-ranlib
# (all provided by the wasi-sdk / ghc-wasm environment), plus autoreconf,
# automake, libtoolize, make, git, pkg-config, ar, file and mktemp.

set -Eeuo pipefail

# Script must be run from the root of the project
PROJECT_DIR="$(pwd)"
[ -f "$PROJECT_DIR/cabal.project" ] || {
    echo "Error: cabal.project not found in $PROJECT_DIR. Run this script from the root of the project" >&2
    exit 1
}

ASSUME_YES=""
FORCE=""
for arg in "$@"; do
    case "$arg" in
        --yes)   ASSUME_YES=1 ;;
        --force) FORCE=1 ;;
        *)
            echo "Error: unknown argument: $arg (supported: --yes, --force)" >&2
            exit 1
            ;;
    esac
done

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

Flags:
  --yes     add the import block to cabal.project without prompting
  --force   rebuild the libraries even if they are already installed in PREFIX

Requirements on PATH: wasm32-wasi-clang, wasm-ld, llvm-ar and llvm-ranlib
(all provided by the wasi-sdk / ghc-wasm environment), plus autoreconf,
automake, libtoolize, make, git, pkg-config, ar, file and mktemp.

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
for t in autoreconf automake libtoolize make git pkg-config ar file mktemp; do
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

# nproc is not available on macOS; getconf is POSIX.
JOBS="$(nproc 2>/dev/null || getconf _NPROCESSORS_ONLN 2>/dev/null || echo 4)"

# libsodium is pinned to the same revision as ./nix/libsodium.nix
LIBSODIUM_REV="9511c982fb1d046470a8b42aa36556cdb7da15de"
LIBSODIUM_REPO="https://github.com/jedisct1/libsodium.git"
# secp256k1 (v0.3.2) and blst (v0.3.14) are pinned to the same revisions as
# the Nix build.
SECP256K1_REV="acf5c55ae6a94e5ca847e07def40427547876101"
SECP256K1_REPO="https://github.com/bitcoin-core/secp256k1.git"
BLST_REV="8c7db7fe8d2ce6e76dc398ebd4d475c0ec564355"
BLST_REPO="https://github.com/supranational/blst.git"

echo "Configuration:"
echo "  PREFIX:     $ABS_PREFIX"
echo "  SRC_ROOT:   $ABS_SRC_ROOT"
echo "  jobs:       $JOBS"
echo "  libsodium:  $LIBSODIUM_REV"
echo "  secp256k1:  $SECP256K1_REV"
echo "  blst:       $BLST_REV"
echo "  toolchain:  $(command -v wasm32-wasi-clang) ($(wasm32-wasi-clang --version | head -1))"
echo

mkdir -p "$ABS_PREFIX/lib" "$ABS_PREFIX/include" "$ABS_PREFIX/lib/pkgconfig"
mkdir -p "$ABS_SRC_ROOT"

# Fetch only the pinned revision (shallow) instead of a full clone; GitHub
# allows fetching arbitrary commit hashes.
checkout_rev() {
    local repo="$1" dir="$2" rev="$3"
    if [ ! -d "$dir/.git" ]; then
        git init -q "$dir"
        git -C "$dir" remote add origin "$repo"
    fi
    if [ "$(git -C "$dir" rev-parse HEAD 2>/dev/null || true)" != "$rev" ]; then
        git -C "$dir" fetch --depth 1 origin "$rev"
        git -C "$dir" checkout -q "$rev"
    fi
}

build_libsodium() {
    if [ -z "$FORCE" ] && [ -f "$ABS_PREFIX/lib/libsodium.a" ] && [ -f "$ABS_PREFIX/lib/libsodium.so" ]; then
        echo "already installed in $ABS_PREFIX, skipping (use --force to rebuild)"
        return
    fi
    local dir="$ABS_SRC_ROOT/libsodium"
    checkout_rev "$LIBSODIUM_REPO" "$dir" "$LIBSODIUM_REV"
    cd "$dir"
    [ -x ./configure ] || ./autogen.sh -s
    CC=wasm32-wasi-clang AR=llvm-ar RANLIB=llvm-ranlib \
        ./configure --host=wasm32-wasi --prefix="$ABS_PREFIX"
    make -j"$JOBS"
    make install
    wasm32-wasi-clang -shared -Wl,--whole-archive \
        "$ABS_PREFIX/lib/libsodium.a" \
        -o "$ABS_PREFIX/lib/libsodium.so"
    cd -
}

build_secp256k1() {
    if [ -z "$FORCE" ] && [ -f "$ABS_PREFIX/lib/libsecp256k1.a" ] && [ -f "$ABS_PREFIX/lib/libsecp256k1.so" ]; then
        echo "already installed in $ABS_PREFIX, skipping (use --force to rebuild)"
        return
    fi
    local dir="$ABS_SRC_ROOT/secp256k1"
    checkout_rev "$SECP256K1_REPO" "$dir" "$SECP256K1_REV"
    cd "$dir"
    [ -x ./configure ] || ./autogen.sh
    CC=wasm32-wasi-clang AR=llvm-ar RANLIB=llvm-ranlib \
        ./configure \
        --host=wasm32-wasi \
        --enable-module-schnorrsig \
        --prefix="$ABS_PREFIX" \
        SECP_CFLAGS=-fPIC
    make -j"$JOBS"
    make install
    wasm32-wasi-clang -shared -Wl,--whole-archive \
        "$ABS_PREFIX/lib/libsecp256k1.a" \
        -o "$ABS_PREFIX/lib/libsecp256k1.so"
    cd -
}

build_blst() {
    if [ -z "$FORCE" ] && [ -f "$ABS_PREFIX/lib/libblst.a" ] && [ -f "$ABS_PREFIX/lib/libblst.so" ] \
        && [ -f "$ABS_PREFIX/lib/pkgconfig/libblst.pc" ]; then
        echo "already installed in $ABS_PREFIX, skipping (use --force to rebuild)"
        return
    fi
    local dir="$ABS_SRC_ROOT/blst"
    checkout_rev "$BLST_REPO" "$dir" "$BLST_REV"
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
      first_obj="$(find . -maxdepth 1 -name '*.o' -print -quit)" && \
      [ -n "$first_obj" ] && file "$first_obj" | grep -q WebAssembly )
    local rc=$?
    rm -rf "$tmp"
    if [ "$rc" -ne 0 ]; then
        echo "ERROR: $archive does not appear to contain wasm objects" >&2
        exit 1
    fi
}

# Line-buffered and portable replacement for 'sed -u' (BSD sed has no -u).
prefix_log() {
    while IFS= read -r line; do printf '%s > %s\n' "$1" "$line"; done
}

# Report which library was being built when a failure aborts the script.
# ('set -E' above makes functions inherit this ERR trap; wrapping the build
# in 'if !' instead would disable 'set -e' inside the build functions.)
CURRENT_LIB=""
trap '[ -z "$CURRENT_LIB" ] || [ "$BASH_SUBSHELL" != 0 ] || printf "ERROR: building %s failed, see the log above.\n" "$CURRENT_LIB" >&2' ERR

run_build() {
    CURRENT_LIB="$1"
    "$2" 2>&1 | prefix_log "$(printf '%s%-9s\033[0m' "$3" "$1")"
    CURRENT_LIB=""
}

run_build libsodium build_libsodium $'\033[1;32m'
run_build secp256k1 build_secp256k1 $'\033[1;36m'
run_build blst      build_blst      $'\033[1;33m'

verify_wasm "$ABS_PREFIX/lib/libsodium.a"
verify_wasm "$ABS_PREFIX/lib/libsecp256k1.a"
verify_wasm "$ABS_PREFIX/lib/libblst.a"

# Run the same checks that cabal will run during dependency resolution, so a
# broken pkg-config setup is reported here with a clear message instead of
# surfacing later as a confusing solver error. The libblst bound mirrors
# cardano-crypto-class's 'pkgconfig-depends: libblst >= 0.3.14'.
if ! PKG_CONFIG_PATH="$ABS_PREFIX/lib/pkgconfig" \
        pkg-config --exists 'libblst >= 0.3.14' libsodium libsecp256k1; then
    cat >&2 <<EOF
ERROR: pkg-config cannot resolve the installed libraries from
$ABS_PREFIX/lib/pkgconfig. Check the .pc files in that directory.
EOF
    exit 1
fi

echo
echo "Done. Installed to: $ABS_PREFIX"

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
echo "Wrote project fragment: $FRAGMENT"

CABAL_PROJECT="$PROJECT_DIR/cabal.project"

case "$FRAGMENT" in
    "$PROJECT_DIR"/*) IMPORT_PATH="${FRAGMENT#"$PROJECT_DIR"/}" ;;
    *)                IMPORT_PATH="$FRAGMENT" ;;
esac

if grep -qF "wasm-libs-without-nix.cabal" "$CABAL_PROJECT"; then
    echo "cabal.project already imports wasm-libs-without-nix.cabal — nothing to do."
else
    if [ -n "$ASSUME_YES" ]; then
        answer=y
    elif [ -t 0 ]; then
        read -r -p "Add 'if arch(wasm32) / import: $IMPORT_PATH' block to cabal.project? [y/N] " answer || answer=""
    else
        answer=""
    fi
    case "$answer" in
        [yY]|[yY][eE][sS])
            cat >> "$CABAL_PROJECT" <<EOF

if arch(wasm32)
  import: $IMPORT_PATH
EOF
            echo "Added to $CABAL_PROJECT"
            ;;
        *)
            cat <<EOF
Skipped (pass --yes to add it without prompting). To enable, append to
cabal.project:

if arch(wasm32)
  import: $IMPORT_PATH
EOF
            ;;
    esac
fi

cat <<EOF

Next steps — to build the wasm module:

  # cardano-crypto-class and cardano-crypto-praos locate the libraries
  # through pkg-config, and the directory below is not a standard
  # pkg-config location:
  export PKG_CONFIG_PATH=$ABS_PREFIX/lib/pkgconfig

  wasm32-wasi-cabal update
  wasm32-wasi-cabal build cardano-wasm
EOF
