{
  description = "PyF";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:nixos/nixpkgs";
  inputs.treefmt-nix.url = "github:numtide/treefmt-nix";

  nixConfig.extra-substituters = [ "https://guibou.cachix.org" ];
  nixConfig.extra-trusted-public-keys = [
    "guibou.cachix.org-1:GcGQvWEyTx8t0KfQac05E1mrlPNHqs5fGMExiN9/pbM="
  ];

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , treefmt-nix
    , ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        treefmtEval = treefmt-nix.lib.evalModule pkgs ./treefmt.nix;
        pyfBuilder =
          hPkgs:
          let
            shell = pkg.env.overrideAttrs (old: {
              nativeBuildInputs = old.nativeBuildInputs ++ (with pkgs; [ cabal-install ]);
            });

            # Shell with haskell language server
            shell_hls = shell.overrideAttrs (old: {
              nativeBuildInputs = old.nativeBuildInputs ++ [ hPkgs.haskell-language-server ];
            });

            pkg = ((hPkgs.callCabal2nix "PyF" ./. { })).overrideAttrs (oldAttrs: {
              buildInputs = oldAttrs.buildInputs;
              passthru = oldAttrs.passthru // {
                inherit shell shell_hls;
              };
            });
          in
          # Add the GHC version in the package name
          pkg.overrideAttrs (old: {
            pname = "PyF-ghc${hPkgs.ghc.version}";
            name = "PyF-ghc${hPkgs.ghc.version}-${old.version}";
          });

      in
      with pkgs;
      rec {
        checks = {
          inherit (packages)
            pyf_94
            pyf_96
            pyf_98
            pyf_910
            pyf_912
            ;

          formatting = treefmtEval.config.build.check self;
        };

        packages = {
          # GHC 8.6 is tested with stack, I'm stopping the testing with nix.
          # GHC 8.6..9.2 are not in nixpkgs anymore.

          pyf_94 = pyfBuilder haskell.packages.ghc94;
          pyf_96 = pyfBuilder haskell.packages.ghc96;
          pyf_98 = pkgs.haskell.lib.dontCheck (pyfBuilder haskell.packages.ghc98);

          pyf_910 = pyfBuilder haskell.packages.ghc910;
          pyf_912 = pyfBuilder haskell.packages.ghc912;
          pyf_914 = pyfBuilder
            (haskell.packages.ghc914.override {
              overrides = self: super:
              {
                call-stack = haskell.lib.dontCheck super.call-stack;
              };});

              default = pyfBuilder haskellPackages;
            };

            formatter = treefmtEval.config.build.wrapper;

          devShells = (builtins.mapAttrs (name: value: value.shell) packages) // {
            treesitter = pkgs.mkShell {
              buildInputs = [
                pkgs.tree-sitter
                pkgs.nodejs
              ];
            };
            default = packages.default.shell_hls;
          };
        }
          );
      }
