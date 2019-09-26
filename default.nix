{ nixpkgs ? ./nixpkgs.nix }:
with import nixpkgs {};
rec {
  pyf = haskellPackages.developPackage {
    name = "PyF";

    # Filter temp files, git files, .ghc.environment and .nix files
    # which are not part of the build
    root = lib.sources.cleanSourceWith {
      filter = name: type: let baseName = baseNameOf (toString name); in
         !(lib.hasPrefix ".ghc.environment." baseName) && (builtins.match ".+\\.nix" (toString name) == null);
      src = lib.sources.cleanSource ./.;
    };

    overrides = self : super : {
    };
  };

  pyf-sdist = haskell.lib.buildFromSdist pyf;
}
