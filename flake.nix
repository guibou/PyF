{
  description = "PyF";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/haskell-updates";

  # Broken: see https://github.com/NixOS/nix/issues/5621
  #nixConfig.allow-import-from-derivation = true;
  nixConfig.extra-substituters = [ "https://guibou.cachix.org" ];
  nixConfig.extra-trusted-public-keys =
    [ "guibou.cachix.org-1:GcGQvWEyTx8t0KfQac05E1mrlPNHqs5fGMExiN9/pbM=" ];

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
        pyfBuilder = hPkgs:
          let
            shell = pkg.env.overrideAttrs (old: {
              nativeBuildInputs = old.nativeBuildInputs
                ++ (with pkgs; [ cabal-install ]);
            });

            # Shell with haskell language server
            shell_hls = shell.overrideAttrs (old: {
              nativeBuildInputs = old.nativeBuildInputs
                ++ [ hPkgs.haskell-language-server ];
            });

            pkg = (
              (hPkgs.callCabal2nix "PyF" ./. { })).overrideAttrs
              (oldAttrs: {
                buildInputs = oldAttrs.buildInputs;
                passthru = oldAttrs.passthru // { inherit shell shell_hls; };
              });
            # Add the GHC version in the package name
          in pkg.overrideAttrs (old: {
            pname = "PyF-ghc${hPkgs.ghc.version}";
            name = "PyF-ghc${hPkgs.ghc.version}-${old.version}";
          });

      in with pkgs; rec {
        checks = {
          inherit (packages) 
            pyf_810
            pyf_90
            pyf_92
            pyf_94
            pyf_96
            pyf_98
            pyf_910
            pyf_912;
        };

        packages = {
          # GHC 8.6 is tested with stack, I'm stopping the testing with nix.
          # GHC 8.8 is not in nixpkgs anymore.

          pyf_810 = pyfBuilder haskell.packages.ghc810;
          pyf_90 = pyfBuilder haskell.packages.ghc90;
          pyf_92 = pyfBuilder haskell.packages.ghc92;
          pyf_94 = pyfBuilder haskell.packages.ghc94;
          pyf_96 = pyfBuilder haskell.packages.ghc96;
          pyf_98 = pkgs.haskell.lib.dontCheck (pyfBuilder haskell.packages.ghc98);

          pyf_910 = pyfBuilder haskell.packages.ghc910;
          pyf_912 = pyfBuilder haskell.packages.ghc912;

          default = pyfBuilder haskellPackages;
        };

        apps = {
          run-ormolu = {
            type = "app";
            program = "${writeScript "pyf-ormolu" ''
              ${ormolu}/bin/ormolu --mode inplace $(git ls-files | grep '\.hs$')
              exit 0
            ''}";
          };
        };

        devShells = (builtins.mapAttrs (name: value: value.shell) packages) // {
          treesitter = pkgs.mkShell { buildInputs = [ pkgs.tree-sitter pkgs.nodejs ]; };
          default = packages.default.shell_hls;
        };
      });
}
