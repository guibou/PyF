{ nixpkgs ? ./nixpkgs.nix }:
with import nixpkgs {};
rec {
  pyfBuilder = hPkgs: hPkgs.developPackage {
    name = "PyF";

    # Filter temp files, git files, .ghc.environment and .nix files
    # which are not part of the build
    root = lib.sources.cleanSourceWith {
      filter = name: type: let baseName = baseNameOf (toString name); in
         !(lib.hasPrefix ".ghc.environment." baseName) && (builtins.match ".+\\.nix" (toString name) == null);
      src = lib.sources.cleanSource ./.;
    };

    overrides = self : super : {
      megaparsec = super.callHackageDirect {
        pkg = "megaparsec";
        ver = "8.0.0";
        sha256 = "1bk4jsa69maryj97jcvxxc211icvnkr21xrj2bqq9ddfizkq5lg0";
      } {};
    };
  };

  pyf_86 = pyfBuilder haskell.packages.ghc865;
  pyf_88 = pyfBuilder haskell.packages.ghc881;

  pyf = pyf_88;

  pyf-sdist = haskell.lib.buildFromSdist pyf;
}
