{ nixpkgs ? ./nixpkgs.nix }:
with import nixpkgs {};
haskellPackages.developPackage {
  name = "PyF";

  # Filter temp files, git files, .ghc.environment and .nix files
  # which are not part of the build
  root = lib.sources.cleanSourceWith {
    filter = name: type: let baseName = baseNameOf (toString name); in
    !(lib.hasPrefix ".ghc.environment." baseName) && (baseName != "default.nix");
    src = lib.sources.cleanSource ./.;
  };

  overrides = self: super: {
    # PyF is not compatible yet with megaparsec 7
    megaparsec = super.megaparsec_6_5_0;
  };
}
