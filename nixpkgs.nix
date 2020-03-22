let
  # nixpkgs-unstable 2020-03-21
  sha256 = "05n27wz5ln9ni5cy5rhjcy612i44gmblkq5m0g827v8pd0nk00da";
  rev = "d96bd3394b734487d1c3bfbac0e8f17465e03afe";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
