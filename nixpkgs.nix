let
  # haskell-update of 2021-02-06
  sha256 = "0pwqmh3vw887f0gfpjmbslrsj8228d343f223psl68ba6af42c0c";
  rev = "6ec544cb489d5c203204e363e343175fbaa04dac";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
