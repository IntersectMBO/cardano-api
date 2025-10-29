{
  description = "cardano-api";

  inputs = {
    hackageNix = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };
    haskellNix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.hackage.follows = "hackageNix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # blst fails to build for x86_64-darwin
    # nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    nixpkgs.url = "github:NixOS/nixpkgs/4284c2b73c8bce4b46a6adf23e16d9e2ec8da4bb";
    iohkNix.url = "github:input-output-hk/iohk-nix";
    flake-utils.url = "github:hamishmack/flake-utils/hkm/nested-hydraJobs";
    incl.url = "github:divnix/incl";
    # non-flake nix compatibility
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    CHaP = {
      url = "github:intersectmbo/cardano-haskell-packages?ref=repo";
      flake = false;
    };

    # wasm specific inputs
    wasm-nixpkgs.follows = "ghc-wasm-meta/nixpkgs";
    ghc-wasm-meta.url = "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org";
  };

  outputs = inputs: let
    supportedSystems = [
      "x86_64-linux"
      "x86_64-darwin"
      "aarch64-linux"
      "aarch64-darwin"
    ];

    # see flake `variants` below for alternative compilers
    defaultCompiler = "ghc9102";
    # Used for cross compilation, and so referenced in .github/workflows/release-upload.yml. Adapt the
    # latter if you change this value.
    crossCompilerVersion = "ghc967";
  in
    inputs.flake-utils.lib.eachSystem supportedSystems (
      system: let
        # setup our nixpkgs with the haskell.nix overlays, and the iohk-nix
        # overlays...
        nixpkgs = let
          abseilOverlay = final: prev:
            prev.lib.optionalAttrs prev.stdenv.hostPlatform.isWindows {
              abseil-cpp = prev.abseil-cpp.overrideAttrs (finalAttrs: previousAttrs: {
                buildInputs = previousAttrs.buildInputs ++ [prev.pkgs.windows.mingw_w64_pthreads];
              });
            };
        in
          import inputs.nixpkgs {
            overlays = [
              # iohkNix.overlays.crypto provide libsodium-vrf, libblst and libsecp256k1.
              inputs.iohkNix.overlays.crypto
              # haskellNix.overlay can be configured by later overlays, so need to come before them.
              inputs.haskellNix.overlay
              # configure haskell.nix to use iohk-nix crypto librairies.
              inputs.iohkNix.overlays.haskell-nix-crypto
              abseilOverlay
            ];
            inherit system;
            inherit (inputs.haskellNix) config;
          };
        inherit (nixpkgs) lib;

        proto-js-bundle-drv = import ./nix/proto-to-js.nix {pkgs = nixpkgs;};
        wasm-typedoc-drv = import ./nix/typedoc.nix {pkgs = nixpkgs;};

        # We use cabalProject' to ensure we don't build the plan for
        # all systems.
        cabalProject = nixpkgs.haskell-nix.cabalProject' ({config, ...}: {
          src = ./.;
          name = "cardano-api";
          compiler-nix-name = lib.mkDefault defaultCompiler;

          # we also want cross compilation to windows on linux (and only with default compiler).
          crossPlatforms = p:
            lib.optional (system == "x86_64-linux" && config.compiler-nix-name == crossCompilerVersion)
            p.mingwW64;

          # CHaP input map, so we can find CHaP packages (needs to be more
          # recent than the index-state we set!). Can be updated with
          #
          #  nix flake lock --update-input CHaP
          #
          inputMap = {
            "https://chap.intersectmbo.org/" = inputs.CHaP;
          };
          # Also currently needed to make `nix flake lock --update-input CHaP` work.
          cabalProjectLocal = ''
            repository cardano-haskell-packages-local
              url: file:${inputs.CHaP}
              secure: True
            active-repositories: hackage.haskell.org, cardano-haskell-packages-local
          '';
          shell.packages = p: [
            # Packages in this repo
            p.cardano-api
            p.cardano-api-gen
            # Work around for issue created by our inability to register sublibs.
            # This package may need to be built and we need to make sure its dependencies
            # are included in `ghc-pkg list` (in particular `compact`)
            p.ouroboros-consensus-cardano
          ];
          # tools we want in our shell, from hackage
          shell.tools =
            {
              cabal = "3.14.1.1";
            }
            // lib.optionalAttrs (config.compiler-nix-name == defaultCompiler) {
              # tools that work only with default compiler
              ghcid = "0.8.9";
              cabal-gild = "1.3.1.2";
              fourmolu = "0.18.0.0";
              haskell-language-server = "latest";
              hlint = "3.10";
            };
          # and from nixpkgs or other inputs
          shell.nativeBuildInputs = with nixpkgs; [gh git jq yq-go actionlint shellcheck snappy protobuf];
          # disable Hoogle until someone request it
          shell.withHoogle = false;
          # Skip cross compilers for the shell
          shell.crossPlatforms = _: [];
          shell.shellHook = ''
            export LD_LIBRARY_PATH="${nixpkgs.snappy}/lib:$LD_LIBRARY_PATH"
            export PATH="$(git rev-parse --show-toplevel)/scripts/devshell:$PATH"
          '';

          # package customizations as needed. Where cabal.project is not
          # specific enough, or doesn't allow setting these.
          modules = [
            ({...}: {
              packages.cardano-api = {
                configureFlags = ["--ghc-option=-Werror"];
                components = {
                  tests.cardano-api-golden = {
                    preCheck = ''
                      export CREATE_GOLDEN_FILES=1
                    '';
                  };
                };
              };
            })
            ({
              pkgs,
              config,
              ...
            }: let
              generatedExampleFiles = map (f: "cardano-wasm/lib-wrapper/${f}") (builtins.filter (f: lib.strings.hasSuffix ".d.ts" f) (builtins.attrNames (builtins.readDir ./cardano-wasm/lib-wrapper)));
              exportWasmPath = "export CARDANO_WASM=${config.hsPkgs.cardano-wasm.components.exes.cardano-wasm}/bin/cardano-wasm${pkgs.stdenv.hostPlatform.extensions.executable}";
            in {
              packages.cardano-wasm.components.tests.cardano-wasm-golden.preCheck = let
                filteredProjectBase = inputs.incl ./. generatedExampleFiles;
              in ''
                ${exportWasmPath}
                cp -r ${filteredProjectBase}/* ..
              '';
            })
            {
              packages.crypton-x509-system.postPatch = ''
                substituteInPlace crypton-x509-system.cabal --replace 'Crypt32' 'crypt32'
              '';
            }
            ({pkgs, ...}: {
              packages.proto-lens-protobuf-types.components.library.build-tools = [pkgs.buildPackages.protobuf];
              packages.cardano-rpc.components.library.build-tools = [pkgs.buildPackages.protobuf];
            })
          ];
        });
        # ... and construct a flake from the cabal project
        flake = cabalProject.flake (
          lib.optionalAttrs (system == "x86_64-linux") {
            # on linux, build/test other supported compilers
            variants = lib.genAttrs [crossCompilerVersion] (compiler-nix-name: {
              inherit compiler-nix-name;
            });
          }
        );
        # wasm shell
        wasmShell = let
          wasm-pkgs = inputs.wasm-nixpkgs.legacyPackages.${system};
          wasi-sdk = inputs.ghc-wasm-meta.packages.${system}.wasi-sdk;
          wasm = {
            libsodium =
              wasm-pkgs.callPackage ./nix/libsodium.nix {inherit wasi-sdk;};
            secp256k1 = (wasm-pkgs.callPackage ./nix/secp256k1.nix {inherit wasi-sdk;}).overrideAttrs (_: {
              src = inputs.iohkNix.inputs.secp256k1;
            });
            blst = (wasm-pkgs.callPackage ./nix/blst.nix {inherit wasi-sdk;}).overrideAttrs (_: {
              src = inputs.iohkNix.inputs.blst;
            });
          };
        in
          lib.optionalAttrs (system != "x86_64-darwin") {
            wasm = wasm-pkgs.mkShell {
              packages =
                [
                  wasm-pkgs.pkg-config
                  wasm-pkgs.curl
                  wasm-pkgs.git
                  inputs.ghc-wasm-meta.packages.${system}.all_9_10
                  wasm.libsodium
                  wasm.secp256k1
                  wasm.blst
                ]
                ++ lib.optional (system == "x86_64-linux" || system == "aarch64-linux") wasm-pkgs.envoy;
            };
          };
        playwrightShell = let
          playwright-pkgs = inputs.nixpkgs.legacyPackages.${system};
        in {
          playwright = playwright-pkgs.mkShell {
            packages = [
              playwright-pkgs.playwright-test
              playwright-pkgs.python313Packages.docopt
              playwright-pkgs.python313Packages.httpserver
            ];
          };
        };
        flakeWithWasmShell = nixpkgs.lib.recursiveUpdate flake {
          devShells = wasmShell;
          hydraJobs = {devShells = wasmShell;};
        };
        flakeWithPlaywrightShell = nixpkgs.lib.recursiveUpdate flakeWithWasmShell {
          devShells = playwrightShell;
          hydraJobs = {devShells = playwrightShell;};
        };
      in
        nixpkgs.lib.recursiveUpdate flakeWithPlaywrightShell rec {
          project = cabalProject;
          # add a required job, that's basically all hydraJobs.
          hydraJobs =
            nixpkgs.callPackages inputs.iohkNix.utils.ciJobsAggregates
            {
              ciJobs =
                flakeWithPlaywrightShell.hydraJobs
                // {
                  # This ensure hydra send a status for the required job (even if no change other than commit hash)
                  revision = nixpkgs.writeText "revision" (inputs.self.rev or "dirty");
                  proto-js-bundle = proto-js-bundle-drv;
                  wasm-typedoc = wasm-typedoc-drv;
                };
            }
            // {
              packages =
                {wasm-typedoc = wasm-typedoc-drv;}
                // lib.optionalAttrs (system != "aarch64-darwin") {
                  proto-js-bundle = proto-js-bundle-drv;
                };
            };
          legacyPackages = {
            inherit cabalProject nixpkgs;
            # also provide hydraJobs through legacyPackages to allow building without system prefix:
            inherit hydraJobs;
          };
          packages =
            lib.optionalAttrs (system != "aarch64-darwin") {
              proto-js-bundle = proto-js-bundle-drv;
            }
            // {
              wasm-typedoc = wasm-typedoc-drv;
            };
          devShells = let
            # profiling shell
            profilingShell = p: {
              # `nix develop .#profiling` (or `.#ghc927.profiling): a shell with profiling enabled
              profiling = (p.appendModule {modules = [{enableLibraryProfiling = true;}];}).shell;
            };
          in
            profilingShell cabalProject;
          # formatter used by nix fmt
          formatter = nixpkgs.alejandra;
        }
    );

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    allow-import-from-derivation = true;
  };
}
