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

  pyfBuilder = hPkgs: (haskell.lib.buildFromSdist (hPkgs.callCabal2nix "PyF" sources {})).overrideAttrs(
    oldAttrs: {
      buildInputs = oldAttrs.buildInputs;
    });

  pyf_86 = pyfBuilder haskell.packages.ghc865;
  pyf_88 = pyfBuilder (haskell.packages.ghc884.override {
    overrides = self: super: with hasell.lib; {
    };
  });

  pyf_810 = pyfBuilder (haskell.packages.ghc8103.override {
    overrides = self: super: with haskell.lib; {
    };
  });

  # disable tests, the golden ones are broken with GHC 9.0.
  # they are correct, but the error messages changed a bit.
  pyf_91 = haskell.lib.dontCheck (pyfBuilder (haskell.packages.ghc901.override {
    overrides = self: super: with haskell.lib; {
      haskell-src-meta = dontCheck ((doJailbreak super.haskell-src-meta).overrideAttrs ( old: {
        patches = [
          (fetchpatch
            {
              url = "https://gitlab.haskell.org/ghc/head.hackage/-/raw/master/patches/haskell-src-meta-0.8.5.patch?inline=false";
              sha256 = "03hiwx2dwaa4z2miqyfxx70yj1l2g48ld599fadx2w4i7z1p244j";
            }
          )
        ];
      }));

      th-expand-syns = dontCheck ((doJailbreak super.th-expand-syns).overrideAttrs ( old: {
        patches = [
          (fetchpatch
            {
              url = "https://gitlab.haskell.org/ghc/head.hackage/-/raw/master/patches/th-expand-syns-0.4.6.0.patch?inline=false";
              sha256 = "09lxvqhrhrxhfmcfwmsms490g54x0x2kgwm95ywn7ikiiwxnhynb";
            }
          )
        ];
      }));
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
  ormolu --mode check $(find -name '*.hs')
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
      ormolu --mode inplace $(git ls-files | grep '\.hs$')
      exit 0
    '';
  };

  pyf_all = [pyf_88 pyf_86 pyf_810 pyf_91];
}
