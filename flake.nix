{
  description = "PyF";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:sternenseemann/nixpkgs/ghc-9.6.1";

  # Broken: see https://github.com/NixOS/nix/issues/5621
  #nixConfig.allow-import-from-derivation = true;
  nixConfig.extra-substituters = [ "guibou.cachix.org" ];
  nixConfig.extra-trusted-public-keys =
    [ "guibou.cachix.org-1:GcGQvWEyTx8t0KfQac05E1mrlPNHqs5fGMExiN9/pbM=" ];

  outputs = { self, nixpkgs, flake-utils }:
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
              (haskell.lib.dontCheck (hPkgs.callCabal2nix "PyF" sources { }))).overrideAttrs
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

          pyf_88 = pyfBuilder (haskell.packages.ghc88.override {
            overrides = self: super: with haskell.lib; { };
          });

          pyf_810 = pyfBuilder (haskell.packages.ghc810.override {
            overrides = self: super: with haskell.lib; { };
          });

          pyf_90 = pyfBuilder (haskell.packages.ghc90.override {
            overrides = self: super: with haskell.lib; { };
          });

          pyf_92 = pyfBuilder (haskell.packages.ghc92.override {
            overrides = self: super: with haskell.lib; rec { };
          });

          # The current version for debug
          pyf_current = pyfBuilder (haskellPackages.override {
            overrides = self: super: with haskell.lib; rec { };
          });

          pyf_96 = pyfBuilder (haskell.packages.ghc96.override {
            overrides = self: super: with haskell.lib; rec {
              # ghcWithPackages = p: (super.ghcWithPackages p).overrideAttrs(old: {
              #   postBuild = if old ? postBuild then builtins.trace old.postBuild old.postBuild else "";
              # });

            # Tests depends on mockery which does not build with GHC >= 9.4
            temporary = haskell.lib.dontCheck super.temporary;

            splitmix = haskell.lib.doJailbreak super.splitmix;
            # Disabling tests breaks the loop between primitive and its tests
            # which indirectly depends on primitive.
            primitive = haskell.lib.doJailbreak (haskell.lib.dontCheck
              (super.callHackage "primitive" "0.7.4.0" { }));
            };
          });

          # GHC 9.4
          pyf_94 = pyfBuilder ((haskell.packages.ghc94.override {
            overrides = self: super:
              with haskell.lib; {
                splitmix = doJailbreak super.splitmix;

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
            pyf_current
            pyf_92
            pyf_94
          ];

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
        devShells = (builtins.mapAttrs (name: value: value.shell) packages) // {
          treesitter = pkgs.mkShell { buildInputs = [ pkgs.tree-sitter pkgs.nodejs ]; };
        };
      });
}
