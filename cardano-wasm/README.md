# cardano-wasm

Part of an effort at IOG to build Cardano Haskell libraries to Wasm.

## Building the wasm module

### Setting up build environment with nix

Enter the Nix shell by writing `nix develop .#wasm` on a shell in this folder and move on to the [Compiling `cardano-wasm` section](#compiling-cardano-wasm).

### Setting up build environment without nix

For the installation we will need some dependencies. In Debian based distros we can install them using apt like this:

```console
sudo apt install happy pkgconf libtool git wget curl jq unzip zstd tar gzip
```

#### Installing `ghc` for wasm

Then it is necessary to get `ghc` for wasm, and you see how to do that [here](https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta#getting-started-without-nix). At the moment, it is necessary to install a version of `ghc` that has `base <= 4.20`, so I would recommend installing `wasm32-wasi-9.10` like this:

```console
wget "https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/-/archive/master/ghc-wasm-meta-master.tar.gz"
tar -zxf ghc-wasm-meta-master.tar.gz
cd ghc-wasm-meta-master
FLAVOUR=9.10 ./setup.sh
source ~/.ghc-wasm/env
cd ..
```

After installing `ghc` for wasm, `~/.ghc-wasm` should contain all the installed tools, and `~/.ghc-wasm/wasm32-wasi-ghc/bin` should be in your `PATH` environment variable. You can check that this is the case by running the following command:
```console
$ wasm32-wasi-ghc --version
The Glorious Glasgow Haskell Compilation System, version 9.10.1.20250327
```

And that should not return:
```console
wasm32-wasi-ghc: command not found
```

Then we need to compile three libraries to WASM: `libblst`, `libsodium`, and `libsecp256k1`.

In order to not interfere with the system library installation, we will create a folder to serve as our prefix:

```console
mkdir -p ~/prefix/{lib/pkgconfig,include}
```

#### Installing `libblst`

We can obtain `libblst` from GitHub [here](https://github.com/supranational/blst). So we can use `git` to get its source code:

```console
git clone "https://github.com/supranational/blst.git"
```

Then we get into the downloaded folder and we build it as follows:

```console
cd blst
./build.sh
```

And we copy the result and includes to our prefix as follows:

```console
cp libblst.a ~/prefix/lib/
cp bindings/{blst.h,blst_aux.h} ~/prefix/include/
```

We generate a dynamic version of the library:

```console
wasm32-wasi-clang -shared -Wl,--whole-archive ~/prefix/lib/libblst.a -o ~/prefix/lib/libblst.so
```

And finally we write an entry for `pkgconfig`, so that later `ghc` can find our prefix:

```
cat <<EOF > $HOME/prefix/lib/pkgconfig/libblst.pc
prefix=$HOME/prefix
exec_prefix=\${prefix}
libdir=\${exec_prefix}/lib
includedir=\${prefix}/include

Name: libblst
Description: blst (pronounced 'blast') is a BLS12-381 signature library focused on performance and security
URL: https://github.com/supranational/blst
Version: 0.3.15

Cflags: -I\${includedir}
Libs: -L\${libdir} -lblst
Libs.private:
EOF
```

Finally we leave the folder:

```console
cd ..
```

#### Installing `libsodium`

We can also obtain `libsodium` from its website [here](https://libsodium.org). We can use `wget` to get the source code for one of its releases. For example:

```console
wget "https://download.libsodium.org/libsodium/releases/libsodium-1.0.20-stable.tar.gz"
```

Then we extract it, get into the created folder and compile it as follows:

```console
tar -zxf libsodium-1.0.20-stable.tar.gz
cd libsodium-stable
./configure --host=wasm32-wasi --prefix=$HOME/prefix
make
make install
```

Finally we generate a dynamic version of the library, and we leave the folder:

```console
wasm32-wasi-clang -shared -Wl,--whole-archive ~/prefix/lib/libsodium.a -o ~/prefix/lib/libsodium.so
cd ..
```

#### Installing `libsecp256k1`

We can obtain `libsecp256k1 ` from GitHub [here](https://github.com/bitcoin-core/secp256k1). So we can use `git` to get its source code:

```console
git clone "https://github.com/bitcoin-core/secp256k1.git"
```

Then we get into the downloaded folder, and we build it as follows:

```console
cd secp256k1
./autogen.sh
./configure --prefix=$HOME/prefix --host=wasm32-wasi --enable-module-schnorrsig SECP_CFLAGS=-fPIC
make
make install
```

Finally we generate a dynamic version of the library, and we leave the folder:

```console
wasm32-wasi-clang -shared -Wl,--whole-archive ~/prefix/lib/libsecp256k1.a -o  ~/prefix/lib/libsecp256k1.so
cd ..
```

#### Set up `pkg-config`

First we make sure we have `pkg-config` installed, in Debian based distros this can be done with apt:

```console
sudo apt install pkgconf
```

And we set the variable `PKG_CONFIG_PATH` to inform `pkg-config` of where the entries for wasm are stored:

```console
export PKG_CONFIG_PATH=$HOME/prefix/lib/pkgconfig
```

Then we get into the `cardano-wasm` subfolder of the clone of `cardano-api`.

```console
cd cardano-api/cardano-wasm
```

### Compiling `cardano-wasm`

Once we have the environment set up we can procede to build the wasm module as follows:

```console
wasm32-wasi-cabal update
wasm32-wasi-cabal build
```

That will generate the `wasm` module, and you can find where it was generated by using the following command:

```console
echo "$(env -u CABAL_CONFIG wasm32-wasi-cabal list-bin exe:cardano-wasm | tail -n1)"
```

And you can see the exported functions by using the following command:

```console
wasm-dis "$(env -u CABAL_CONFIG wasm32-wasi-cabal list-bin exe:cardano-wasm | tail -n1)" | grep "export "
```

To generate a post-link module with the exports you can write:

```console
$(wasm32-wasi-ghc --print-libdir)/post-link.mjs -i "$(env -u CABAL_CONFIG wasm32-wasi-cabal list-bin exe:cardano-wasm | tail -n1)" -o cardano-wasm.js
```

That will create it with the name `cardano-wasm.js` in the current folder.

You can find more information in [this url](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html).

