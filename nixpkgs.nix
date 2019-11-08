let
  sha256 = "1w7qi0kan6afr6iz1rkwmij3isdvvc3cmx1jj5hqj6p7d9lcdb3n";
  rev = "b937c4c734afffe5cf7bf83d1f85e861b7a8c68c";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
