{
  description = "PyF";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.hls.url = "github:haskell/haskell-language-server";
  inputs.nixpkgs.url = "github:nixos/nixpkgs";

  # Broken: see https://github.com/NixOS/nix/issues/5621
  #nixConfig.allow-import-from-derivation = true;
  nixConfig.extra-substituters = [ "guibou.cachix.org" ];
  nixConfig.extra-trusted-public-keys =
    [ "guibou.cachix.org-1:GcGQvWEyTx8t0KfQac05E1mrlPNHqs5fGMExiN9/pbM=" ];

  outputs = { self, nixpkgs, flake-utils, hls }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in with pkgs; rec {
        # Explicit list of used files. Else there is always too much and
        # cache is invalidated.
        sources = lib.sourceByRegex ./. [
          "PyF.cabal$"
          ".*.hs$"
          ".*.md$"
          ".*.golden$"
          "src"
          "app"
          "src/PyF"
          "src/PyF/Internal"
          "test"
          "test/golden"
          "LICENSE"
        ];

        pyfBuilder = hPkgs:
          let
            shell = pkg.env.overrideAttrs (old: {
              nativeBuildInputs = old.nativeBuildInputs
                ++ [ cabal-install python3 ];
            });

            # Shell with haskell language server
            shell_hls = shell.overrideAttrs (old: {
              nativeBuildInputs = old.nativeBuildInputs
                ++ [ hPkgs.haskell-language-server ];
            });

            pkg = (haskell.lib.buildFromSdist
              (hPkgs.callCabal2nix "PyF" sources { })).overrideAttrs
              (oldAttrs: {
                buildInputs = oldAttrs.buildInputs;
                passthru = oldAttrs.passthru // { inherit shell shell_hls; };
              });
            # Add the GHC version in the package name
          in pkg.overrideAttrs (old: { name = "PyF-ghc${hPkgs.ghc.version}"; });

        packages = rec {
          pyf_86 = (pyfBuilder (haskell.packages.ghc865Binary.override {
            overrides = self: super: with haskell.lib; { };
          })).overrideAttrs (old: {
            passthru.shell = old.passthru.shell.overrideAttrs (old: {
              # for some reasons, ncurses is not part of the dependencies of ghc...
              buildInputs = old.buildInputs ++ [ pkgs.ncurses ];
            });
          });

          pyf_88 = pyfBuilder (haskell.packages.ghc884.override {
            overrides = self: super: with haskell.lib; { };
          });

          pyf_810 = pyfBuilder (haskell.packages.ghc8107.override {
            overrides = self: super: with haskell.lib; { };
          });

          pyf_90 = pyfBuilder (haskell.packages.ghc902.override {
            overrides = self: super: with haskell.lib; { };
          });

          pyf_92 = pyfBuilder (haskell.packages.ghc923.override {
            overrides = self: super: with haskell.lib; rec { };
          });

          # GHC 9.4
          pyf_HEAD = pyfBuilder ((haskell.packages.ghcHEAD.override {
            overrides = self: super:
              with haskell.lib; {
                # Disabling tests breaks the loop between primitive and its tests
                # which indirectly depends on primitive.
                primitive = haskell.lib.dontCheck
                  (super.callHackage "primitive" "0.7.4.0" { });
                # Tests depends on mockery which does not build with GHC 9.4
                temporary = haskell.lib.dontCheck super.temporary;

                hspec = haskell.lib.dontCheck
                  (super.callHackage "hspec" "2.10.0" { });
                hspec-core = haskell.lib.dontCheck
                  (super.callHackage "hspec-core" "2.10.0" { });
                hspec-meta = haskell.lib.dontCheck
                  (super.callHackage "hspec-meta" "2.9.3" { });
                base-compat = haskell.lib.dontCheck
                  (super.callHackage "base-compat" "0.12.1" { });
                hspec-discover = haskell.lib.dontCheck
                  (super.callHackage "hspec-discover" "2.10.0" { });
              };
          }));

          pyf_all = linkFarmFromDrvs "all_pyf" [
            pyf_810
            pyf_88
            pyf_86
            pyf_90
            pyf_92
            pyf_HEAD
          ];

          # That the current version for developement
          # We use the current version of nixpkgs in order to reduce build time.
          pyf_current = pyf_90;

          # Only the current build is built with python3 support
          # (i.e. extended tests)
          pyf = haskell.lib.enableCabalFlag (pyf_current.overrideAttrs
            (old: { buildInputs = old.buildInputs ++ [ python3 ]; }))
            "python_test";
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

        defaultPackage = packages.pyf;
        devShell = packages.pyf.shell_hls;
        devShells = builtins.mapAttrs (name: value: value.shell) packages;
      });
}
