let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };
in { xmonad-custom = import ./xmonad-custom { inherit pkgs; }; }
