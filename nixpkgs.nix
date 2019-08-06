let
  sha256 = "095l3gqpyh1c6xijhin2bry8qq2w7yzidiiaadyzzwjn6vvqm9xi";
  rev = "002b853782e";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
