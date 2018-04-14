{ pkgs ? import (builtins.fetchTarball https://github.com/NixOS/nixpkgs/archive/ee28e35ba37ab285fc29e4a09f26235ffe4123e2.tar.gz) {} }:
with pkgs.haskellPackages;
(developPackage {
  root = ./.;
}).overrideAttrs (oldAttrs: {
    buildInputs = oldAttrs.buildInputs ++ [pkgs.python3];
    })
