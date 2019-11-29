let
  sources = import ./nix/sources.nix;
  nixpkgs = import sources.nixpkgs { };
  easy-purescript-nix = import sources.easy-purescript-nix { };
  spago2nix = import sources.spago2nix { };
  niv = import sources.niv { };
in {
  inherit (nixpkgs) stdenv nix writeShellScriptBin runCommand writeText;
  pkgs = nixpkgs;
  parcel-bundler = nixpkgs.nodePackages.parcel-bundler;
  typescript = nixpkgs.nodePackages.typescript;
  inherit (easy-purescript-nix) spago purescript;
  inherit spago2nix;
  inherit niv;
}
