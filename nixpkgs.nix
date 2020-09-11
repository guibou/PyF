let
  # nixpkgs-unstable 2020-09-11
  sha256 = "1vx7kyaq0i287dragjgfdj94ggwr3ky2b7bq32l8rkd2k3vc3gl5";
  rev = "3c0e3697520cbe7d9eb3a64bfd87de840bf4aa77";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
