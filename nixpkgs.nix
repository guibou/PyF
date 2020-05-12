let
  # nixpkgs-unstable 2020-03-21
  sha256 = "1x4j31ii2n7hdlqwsg4pmcah10m0d7dhvzzd9iwmvb3ys13b6pf5";
  rev = "b970793dbf9e41701db81a97ac53bfc5d5a33938";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
