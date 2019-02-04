{ nixpkgs ? ./nixpkgs.nix }:
with import nixpkgs {};
let
  hspec-golden = haskellPackages.developPackage {
    name = "hspec_golden";
    returnShellEnv = false;

    root = pkgs.fetchzip {
      url = "https://github.com/guibou/hspec-golden/archive/71dea329ed2175390798f0aee0602ec05b483978.tar.gz";
      sha256 = "1gnmw5xsf2zxkyssi8l5hqwvzdk5caj55gbvd9is2ys15bgpq2n8";
    };
  };
in
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
    hspec-golden = hspec-golden;
  };
}
