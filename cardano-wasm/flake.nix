{
  inputs = {
    nixpkgs.follows = "ghc-wasm-meta/nixpkgs";
    flake-utils.follows = "ghc-wasm-meta/flake-utils";
    ghc-wasm-meta.url = "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org";
    iohk-nix.url = "github:input-output-hk/iohk-nix";
  };
  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = inputs.nixpkgs.legacyPackages.${system};

      wasi-sdk =
        inputs.ghc-wasm-meta.packages.${system}.wasi-sdk;
      wasm = {
        libsodium =
          pkgs.callPackage ./nix/libsodium.nix { inherit wasi-sdk; };
        secp256k1 =
          (pkgs.callPackage ./nix/secp256k1.nix { inherit wasi-sdk; }).overrideAttrs (_: {
            src = inputs.iohk-nix.inputs.secp256k1;
          });
        blst =
          (pkgs.callPackage ./nix/blst.nix { inherit wasi-sdk; }).overrideAttrs (_: {
            src = inputs.iohk-nix.inputs.blst;
          });
      };
    in
    {
      devShells.default = pkgs.mkShell {
        packages = [
          inputs.ghc-wasm-meta.packages.${system}.all_9_10

          pkgs.pkg-config
          wasm.libsodium
          wasm.secp256k1
          wasm.blst
        ];
      };
      packages = wasm;
    });
}
