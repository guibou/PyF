with import ./. {};
pyf.env.overrideAttrs (old: {
   # The haskell environment does not come with cabal-install
   nativeBuildInputs = old.nativeBuildInputs ++ [pkgs.cabal-install pkgs.python3 pkgs.haskellPackages.haskell-language-server ];
})
