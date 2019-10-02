let
  sha256 = "1a7bjv8wfp38dmb7aq1awnx94sdifw0461g8985hjb1wbmkj9whz";
  rev = "b8ff7bc092315cc2950837b5844a2b3b1688c754";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
