# cardano-wasm

Part of an effort at IOG to build Cardano Haskell libraries to Wasm.

## Building the wasm module

### Setting up build environment with nix

Enter the Nix shell by writing `nix develop .#wasm` on a shell in this folder and move on to the [Compiling `cardano-wasm` section](#compiling-cardano-wasm).

### Setting up build environment without nix

For the installation we will need some dependencies. In Debian based distros we can install them using apt like this:

```bash
sudo apt install happy pkgconf libtool git wget curl jq unzip zstd tar gzip
```

#### Installing `ghc` for wasm

Then it is necessary to get `ghc` for wasm, and you see how to do that [here](https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta#getting-started-without-nix). At the moment, it is necessary to install a version of `ghc` that has `base <= 4.20`, so I would recommend installing `wasm32-wasi-9.10` like this:

```bash
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

```bash
mkdir -p ~/prefix/{lib/pkgconfig,include}
```

#### Installing `libblst`

We can obtain `libblst` from GitHub [here](https://github.com/supranational/blst). So we can use `git` to get its source code:

```bash
git clone "https://github.com/supranational/blst.git"
```

Then we get into the downloaded folder and we build it as follows:

```bash
cd blst
./build.sh
```

And we copy the result and includes to our prefix as follows:

```bash
cp libblst.a ~/prefix/lib/
cp bindings/{blst.h,blst_aux.h} ~/prefix/include/
```

We generate a dynamic version of the library:

```bash
wasm32-wasi-clang -shared -Wl,--whole-archive ~/prefix/lib/libblst.a -o ~/prefix/lib/libblst.so
```

And finally we write an entry for `pkgconfig`, so that later `ghc` can find our prefix:

```bash
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

```bash
cd ..
```

#### Installing `libsodium`

We can also obtain `libsodium` from its website [here](https://libsodium.org). We can use `wget` to get the source code for one of its releases. For example:

```bash
wget "https://download.libsodium.org/libsodium/releases/libsodium-1.0.20-stable.tar.gz"
```

Then we extract it, get into the created folder and compile it as follows:

```bash
tar -zxf libsodium-1.0.20-stable.tar.gz
cd libsodium-stable
./configure --host=wasm32-wasi --prefix=$HOME/prefix
make
make install
```

Finally we generate a dynamic version of the library, and we leave the folder:

```bash
wasm32-wasi-clang -shared -Wl,--whole-archive ~/prefix/lib/libsodium.a -o ~/prefix/lib/libsodium.so
cd ..
```

#### Installing `libsecp256k1`

We can obtain `libsecp256k1 ` from GitHub [here](https://github.com/bitcoin-core/secp256k1). So we can use `git` to get its source code:

```bash
git clone "https://github.com/bitcoin-core/secp256k1.git"
```

Then we get into the downloaded folder, and we build it as follows:

```bash
cd secp256k1
./autogen.sh
./configure --prefix=$HOME/prefix --host=wasm32-wasi --enable-module-schnorrsig SECP_CFLAGS=-fPIC
make
make install
```

Finally we generate a dynamic version of the library, and we leave the folder:

```bash
wasm32-wasi-clang -shared -Wl,--whole-archive ~/prefix/lib/libsecp256k1.a -o  ~/prefix/lib/libsecp256k1.so
cd ..
```

#### Set up `pkg-config`

First we make sure we have `pkg-config` installed, in Debian based distros this can be done with apt:

```bash
sudo apt install pkgconf
```

And we set the variable `PKG_CONFIG_PATH` to inform `pkg-config` of where the entries for wasm are stored:

```bash
export PKG_CONFIG_PATH=$HOME/prefix/lib/pkgconfig
```

Then we get into the `cardano-wasm` subfolder of the clone of `cardano-api`.

```bash
cd cardano-api/cardano-wasm
```

### Compiling `cardano-wasm`

Once we have the environment set up we can proceed to build the wasm module as follows:

```bash
wasm32-wasi-cabal update
wasm32-wasi-cabal build cardano-wasm
```

That will generate the `wasm` module, and you can find where it was generated by using the following command:

```bash
echo "$(env -u CABAL_CONFIG wasm32-wasi-cabal list-bin exe:cardano-wasm | tail -n1)"
```

And you can see the exported functions by using the following command:

```bash
wasm-dis "$(env -u CABAL_CONFIG wasm32-wasi-cabal list-bin exe:cardano-wasm | tail -n1)" | grep "export "
```

To generate a post-link module with the exports you can write:

```bash
$(wasm32-wasi-ghc --print-libdir)/post-link.mjs -i "$(env -u CABAL_CONFIG wasm32-wasi-cabal list-bin exe:cardano-wasm | tail -n1)" -o cardano-wasm.js
```

That will create it with the name `cardano-wasm.js` in the current folder.

You can find more information in [this url](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html).

And you can find an example of how to use it in the `example` subfolder. This example assumes that the generated `.wasm` and `.js` files as well as the files from the `lib-wrapper` subfolder, all reside in the same folder as the code in `example` subfolder.

### Troubleshooting guide

#### `Failed to load dynamic interface file for ...` GHC-47808 error

When you're seeing an error similar to:
```console
[1 of 4] Compiling Data.Constraint.Compose ( src/Data/Constraint/Compose.hs, dist/build/Data/Constraint/Compose.o, dist/build/Data/Constraint/Compose.dyn_o )
src/Data/Constraint/Compose.hs:14:1: error: [GHC-47808]
    Failed to load dynamic interface file for Data.Constraint:
      Exception when reading interface file  /home/mgalazyn/.local/share/cabal/store/ghc-9.10.1.20250327-inplace/constraints-0.14.2-7fc45a1b31889530ee7f295fd8bc8535f521e6eaac4bbc3cc5df34e174a5eacf/lib/Data/Constraint.dyn_hi
        /home/mgalazyn/.local/share/cabal/store/ghc-9.10.1.20250327-inplace/constraints-0.14.2-7fc45a1b31889530ee7f295fd8bc8535f521e6eaac4bbc3cc5df34e174a5eacf/lib/Data/Constraint.dyn_hi: withBinaryFile: does not exist (No such file or directory)
   |
14 | import Data.Constraint
   | ^^^^^^^^^^^^^^^^^^^^^^
```

Most likely the reason is that `cabal` store is polluted with non-wasm and/or nix and non-nix package information and `ghc-pkg` is unable to find the right dependencies.
You can try to isolate `cabal` environment for WASM build from other builds.
In other words, paths in `cabal path` command output should be different than for other builds.

##### When using `nix`

Try entering development shell in an isolated environment, and setting `$HOME` to a different directory, making `cabal` use different state directory.
```bash
nix develop -i .#wasm
export HOME=`pwd`  # or some other path that you want cabal state in
wasm32-wasi-cabal update
wasm32-wasi-cabal build cardano-wasm
```

#### Cabal error Cabal-7125 `Failed to download ... The exception was: user error (https not supported)`

This can happen when using `nix develop -i`. To work around that add to your `cabal.project.local`:

```
http-transport: curl
```
thus making `cabal` use `curl` for HTTP connections instead.

#### Build is getting stuck at one of the dependencies

It may happen that the build gets stuck on one of the dependencies, and if you check the CPU usage of the compiler process, you'll see a value close to 0.
To work around that, disable build concurrency:

```bash
wasm32-wasi-cabal build cardano-wasm -j1 --ghc-options="-j1" --no-semaphore
```

# Examples

You can find examples of how to use the `cardano-wasm` library in the [examples](./examples) subfolder.

