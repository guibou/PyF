{
  description = "PyF";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  # Broken: see https://github.com/NixOS/nix/issues/5621
  #nixConfig.allow-import-from-derivation = true;

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

          pyf_90 = pyfBuilder (haskell.packages.ghc901.override {
            overrides = self: super: with haskell.lib; { };
          });

          pyf_92 = pyfBuilder (haskell.packages.ghc921.override {
              overrides = self: super: with haskell.lib; rec { };
          });

          # GHC 9.4
          pyf_HEAD = pyfBuilder (haskell.packages.ghcHEAD.override {
            overrides = self: super: with haskell.lib; { };
          });

          all_pyf = linkFarmFromDrvs "all_pyf" [pyf_810 pyf_88 pyf_86 pyf_90 pyf_92];

          # That the current version for developement
          # We use the current version of nixpkgs in order to reduce build time.
          pyf_current = pyf_810;

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
              ${ormolu}/bin/ormolu --mode inplace $(git ls-files | grep '\.hs$' | grep -v ParserEx)
              exit 0
            ''}";
          };
        };

        defaultPackage = packages.pyf;
        devShell = packages.pyf.shell_hls;
        devShells = builtins.mapAttrs (name: value: value.shell) packages;
      });
}
