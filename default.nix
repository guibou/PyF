{ pkgs ? import (builtins.fetchTarball https://github.com/NixOS/nixpkgs/archive/e53ebf3a31e.tar.gz) {} }:
with pkgs.haskell.packages.ghc822;
developPackage {
  root = ./.;
  overrides = self: super: {
    megaparsec = megaparsec_6_2_0;
  };
}
