{ ... }:
{
  projectRootFile = "flake.nix";

  programs.ormolu.enable = true;
  programs.nixfmt.enable = true;
  programs.yamlfmt.enable = true;
  programs.cabal-fmt.enable = true;
  programs.mdformat.enable = true;

  settings.excludes = [
    "*.golden"
    "flake.lock"
    "*.png"
    ".gitignore"
    ".envrc"
    "LICENSE"
    "tree-sitter-pyf/*"
  ];
}
