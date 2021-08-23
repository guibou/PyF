let
  # nixpkgs unstable 23-09-2021
  rev = "e0ce3c683ae6";
  sha256 = "08ans3w6r4fbs1wx6lzlp4xwhy6p04x3spvvrjz5950w8mzxqm61";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/nixos/nixpkgs/archive/${rev}.tar.gz";
})
