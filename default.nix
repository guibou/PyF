{ pkgs ? import ./nixpkgs.nix {} }:
with pkgs;
rec {
  inherit pkgs;

  # Explicit list of used files. Else there is always too much and
  # cache is invalidated.
  sources = lib.sourceByRegex ./.  [
    "PyF\.cabal$"
    ".*\.hs$"
    ".*\.md$"
    ".*\.golden$"
    "src"
    "app"
    "src/PyF"
    "src/PyF/Internal"
    "test"
    "test/failureCases"
    "test/golden"
    "LICENSE"
  ];

  pyfBuilder = hPkgs: let
    shell = pkg.env.overrideAttrs (old: {
      # The haskell environment does not come with cabal-install
      #nativeBuildInputs = old.nativeBuildInputs ++ [root.pkgs.cabal-install root.pkgs.python3 hPkgs.haskell-language-server ];
      nativeBuildInputs = old.nativeBuildInputs ++ [cabal-install python3 ];
    });

    # Shell with haskell language server
    shell_hls = shell.overrideAttrs (old: {
      nativeBuildInputs = old.nativeBuildInputs ++ [hPkgs.haskell-language-server];
    });

    pkg = (haskell.lib.buildFromSdist (hPkgs.callCabal2nix "PyF" sources {})).overrideAttrs(
    oldAttrs: {
      buildInputs = oldAttrs.buildInputs;
      passthru = oldAttrs.passthru // {
        inherit shell shell_hls;
      };
    });
    # Add the GHC version in the package name
  in pkg.overrideAttrs(old : {
    name = let x = builtins.parseDrvName old.name;
           in "${x.name}-ghc${hPkgs.ghc.version}-${x.version}";
  });

  pyf_86 = pyfBuilder (haskell.packages.ghc865.override {
    overrides = self: super: with haskell.lib; {
      ghc-lib-parser-ex = haskell.lib.disableCabalFlag (haskell.lib.enableCabalFlag super.ghc-lib-parser-ex "no-ghc-lib") "auto";
    };
  });

  pyf_88 = pyfBuilder (haskell.packages.ghc884.override {
    overrides = self: super: with haskell.lib; {
      ghc-lib-parser-ex = haskell.lib.disableCabalFlag (haskell.lib.enableCabalFlag super.ghc-lib-parser-ex "no-ghc-lib") "auto";
    };
  });

  pyf_810 = pyfBuilder (haskell.packages.ghc8103.override {
    overrides = self: super: with haskell.lib; {
      # In order to use the true GHC lib
      # ghc-lib-parser-ex = haskell.lib.addBuildDepend (haskell.lib.disableCabalFlag (haskell.lib.disableCabalFlag super.ghc-lib-parser-ex "no-ghc-lib") "auto") super.ghc-lib-parser;
    };
  });

  # disable tests, the golden ones are broken with GHC 9.0.
  # they are correct, but the error messages changed a bit.
  pyf_91 = haskell.lib.dontCheck (pyfBuilder (haskell.packages.ghc901.override {
    overrides = self: super: with haskell.lib; {
      ghc-lib-parser-ex = haskell.lib.dontCheck (haskell.lib.disableCabalFlag (haskell.lib.enableCabalFlag super.ghc-lib-parser-ex "no-ghc-lib") "auto");
    };
  }));

  # That the current version for developement
  # We use the current version of nixpkgs in order to reduce build time.
  pyf_current = pyf_810;

  # Only the current build is built with python3 support
  # (i.e. extended tests)
  pyf = haskell.lib.enableCabalFlag (pyf_current.overrideAttrs(old: {
    buildInputs = old.buildInputs ++ [python3];
  })) "python_test";

  # Run hlint on the codebase
  hlint = runCommand "hlint-pyf" {
    nativeBuildInputs = [haskellPackages.hlint];
  }
  ''
  cd ${sources}
  hlint .
  mkdir $out
  '';

  # Run ormolu on the codebase
  # Fails if there is something to format
  ormolu = runCommand "ormolu-pyf" {
    nativeBuildInputs = [haskellPackages.ormolu];
  }
  ''
  cd ${sources}
  ormolu --mode check $(find -name '*.hs' | grep -v ParserEx)
  mkdir $out
  '';

  hlint-fix = mkShell {
    nativeBuildInputs = [haskellPackages.hlint git haskellPackages.apply-refact];
    shellHook = ''
      for file in $(git ls-files | grep '\.hs$')
      do
        hlint  --refactor --refactor-options='-i' $file
      done
      exit 0
    '';
  };

  ormolu-fix = mkShell {
    nativeBuildInputs = [haskellPackages.ormolu git];
    shellHook = ''
      ormolu --mode inplace $(git ls-files | grep '\.hs$' | grep -v ParserEx)
      exit 0
    '';
  };

  pyf_all = [pyf_88 pyf_86 pyf_810 pyf_91];
}
