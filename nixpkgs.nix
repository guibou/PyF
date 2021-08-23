let
  # https://github.com/NixOS/nixpkgs/pull/124999
  rev = "eeab902e1ae4ea9df2c432f9864775e866e585cd";
  sha256 = "05wg68kwfb3k78jq1i357j2lzj2vzg1zmrwbvx7l90gbs8wv4zmw";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/nixos/nixpkgs/archive/${rev}.tar.gz";
})
