{ pkgs ? import ./nixpkgs.nix {}}:
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
    "test/golden"
    "LICENSE"
  ];

  pyfBuilder = hPkgs: let
    shell = pkg.env.overrideAttrs (old: {
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

  pyf_86 = (pyfBuilder (haskell.packages.ghc865Binary.override {
    overrides = self: super: with haskell.lib; {
    };
  })).overrideAttrs(old: {
    passthru.shell = old.passthru.shell.overrideAttrs(old: {
      # for some reasons, ncurses is not part of the dependencies of ghc...
      buildInputs = old.buildInputs ++ [pkgs.ncurses];
    });
  });

  pyf_88 = pyfBuilder (haskell.packages.ghc884.override {
    overrides = self: super: with haskell.lib; {
    };
  });

  pyf_810 = pyfBuilder (haskell.packages.ghc8106.override {
    overrides = self: super: with haskell.lib; {
    };
  });

  pyf_90 = pyfBuilder (haskell.packages.ghc901.override {
    overrides = self: super: with haskell.lib; {
    };
  });

  # GHC 9.2 is not released yet. Hence a lot of fix for dependencies
  # Note that most fix impact the "test" phase, PyF do not need these libraries
  # for build.
  pyf_92 = haskell.lib.dontHaddock (pyfBuilder (haskell.packages.ghc921.override {
    overrides = self: super: with haskell.lib; rec {
      tagged = super.tagged.overrideAttrs(old: {
        configurePhase = ''
          sed -i 's/2.18/3/' tagged.cabal
        '' + old.configurePhase;
      });
      hashable = doJailbreak ((super.callHackage "hashable" "1.3.1.0" {}).overrideAttrs(old: {
        configurePhase = ''
          sed -i 's/.*Option.*//' src/Data/Hashable/Class.hs
        '' + old.configurePhase;
      }));
      splitmix = doJailbreak super.splitmix;
      regex-base = doJailbreak super.regex-base;

      # it pulls crappy dependencies when testing
      temporary = dontCheck super.temporary;

      # build fails because of GHC 9.2
      # Test uses it indirectly...
      primitive2 = doJailbreak (dontCheck (super.callCabal2nix "primitive" (pkgs.fetchzip {
        url = "https://github.com/haskell/primitive/archive/0cbb62aeb6e5d9f3660a7c7b1b731bce48214c5a.tar.gz";
        sha256 = "11bjwixd2672hlbimxy5w5gwy7j7qdd5dvycm6vi4ajxjqhl9ggj";
      }) {}));

      tf-random = super.tf-random.override { primitive = primitive2; };

      # random 1.2 depends on primitive
      random = super.callHackage "random" "1.1" {};
    };
  }));

  # GHC 9.4
  pyf_HEAD = pyfBuilder (haskell.packages.ghcHEAD.override {
    overrides = self: super: with haskell.lib; {
    };
  });

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
  hlint $(find -name '*.hs' | grep -v ParserEx)
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

  ormolu-fix = mkShell {
    nativeBuildInputs = [haskellPackages.ormolu git];
    shellHook = ''
      ormolu --mode inplace $(git ls-files | grep '\.hs$' | grep -v ParserEx)
      exit 0
    '';
  };

  pyf_all = [pyf_86 pyf_88 pyf_810 pyf_90 pyf_92];
}
