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

  overrides = self : super : {
    # haskell src meta < 0.8.2 does not correctly handle TypeApplication
    # see: https://github.com/DanBurton/haskell-src-meta/issues/4
    haskell-src-meta = super.callCabal2nix "haskell-src-meta" (fetchzip {
      url = "http://hackage.haskell.org/package/haskell-src-meta-0.8.2/haskell-src-meta-0.8.2.tar.gz";
      sha256 = "1sw1s78cdwx4bg5x6ngl6iz8lml51iya0j7apc2cniqawd02fhv2";
    }) {};
  };
}
