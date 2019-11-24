let
  sources = import ./sources.nix;
  nixpkgs = import sources.nixpkgs { };
  spago2nix = import sources.spago2nix { };
in nixpkgs // {
  pkgs = nixpkgs;
  node2nix = nixpkgs.nodePackages.node2nix;
  inherit spago2nix;
}
