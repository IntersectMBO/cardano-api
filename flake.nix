{
  description = "cardano-api";

  inputs = {
    hackageNix = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };
    haskellNix = {
      url = "github:input-output-hk/haskell.nix?ref=2024.09.15";
      inputs.hackage.follows = "hackageNix";
    };
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    iohkNix.url = "github:input-output-hk/iohk-nix";
    flake-utils.url = "github:hamishmack/flake-utils/hkm/nested-hydraJobs";
    # non-flake nix compatibility
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    cardano-mainnet-mirror = {
      url = "github:input-output-hk/cardano-mainnet-mirror";
      flake = false;
    };

    CHaP = {
      url = "github:intersectmbo/cardano-haskell-packages?ref=repo";
      flake = false;
    };
  };

  outputs = inputs: let
    supportedSystems = [
      "x86_64-linux"
      "x86_64-darwin"
      "aarch64-linux"
      "aarch64-darwin"
    ];

    # see flake `variants` below for alternative compilers
    defaultCompiler = "ghc982";
    # Used for cross compilation, and so referenced in .github/workflows/release-upload.yml. Adapt the
    # latter if you change this value.
    crossCompilerVersion = "ghc966";
  in
    inputs.flake-utils.lib.eachSystem supportedSystems (
      system: let
        # setup our nixpkgs with the haskell.nix overlays, and the iohk-nix
        # overlays...
        nixpkgs = import inputs.nixpkgs {
          overlays = [
            # iohkNix.overlays.crypto provide libsodium-vrf, libblst and libsecp256k1.
            inputs.iohkNix.overlays.crypto
            # haskellNix.overlay can be configured by later overlays, so need to come before them.
            inputs.haskellNix.overlay
            # configure haskell.nix to use iohk-nix crypto librairies.
            inputs.iohkNix.overlays.haskell-nix-crypto
          ];
          inherit system;
          inherit (inputs.haskellNix) config;
        };
        inherit (nixpkgs) lib;

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
              haskell-language-server.src = nixpkgs.haskell-nix.sources."hls-2.9";
              hlint = "3.8";
              stylish-haskell = "0.14.6.0";
            };
          # and from nixpkgs or other inputs
          shell.nativeBuildInputs = with nixpkgs; [gh jq yq-go actionlint shellcheck];
          # disable Hoogle until someone request it
          shell.withHoogle = false;
          # Skip cross compilers for the shell
          shell.crossPlatforms = _: [];
          shell.shellHook = ''
            export PATH="$(git rev-parse --show-toplevel)/scripts/devshell:$PATH"
          '';

          # package customizations as needed. Where cabal.project is not
          # specific enough, or doesn't allow setting these.
          modules = [
            ({pkgs, ...}: {
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
            {
              packages.crypton-x509-system.postPatch = ''
                substituteInPlace crypton-x509-system.cabal --replace 'Crypt32' 'crypt32'
              '';
            }
          ];
        });
        # ... and construct a flake from the cabal project
        flake = cabalProject.flake (
          lib.optionalAttrs (system == "x86_64-linux") {
            # on linux, build/test other supported compilers
            variants = lib.genAttrs ["ghc8107" crossCompilerVersion] (compiler-nix-name: {
              inherit compiler-nix-name;
            });
          }
        );
      in
        nixpkgs.lib.recursiveUpdate flake rec {
          project = cabalProject;
          # add a required job, that's basically all hydraJobs.
          hydraJobs =
            nixpkgs.callPackages inputs.iohkNix.utils.ciJobsAggregates
            {
              ciJobs =
                flake.hydraJobs
                // {
                  # This ensure hydra send a status for the required job (even if no change other than commit hash)
                  revision = nixpkgs.writeText "revision" (inputs.self.rev or "dirty");
                };
            };
          legacyPackages = {
            inherit cabalProject nixpkgs;
            # also provide hydraJobs through legacyPackages to allow building without system prefix:
            inherit hydraJobs;
          };
          devShells = let
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
