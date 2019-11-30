let
  sources = import ./nix/sources.nix;
  nixpkgs = import sources.nixpkgs { };
  easy-purescript-nix = import sources.easy-purescript-nix { };
  spago2nix = import sources.spago2nix { };
  niv = import sources.niv { };
  yarn2nix = import sources.yarn2nix { };
in {
  inherit (nixpkgs) stdenv nix writeShellScriptBin runCommand writeText;
  pkgs = nixpkgs;
  parcel-bundler = nixpkgs.nodePackages.parcel-bundler;
  typescript = nixpkgs.nodePackages.typescript;
  yarn = nixpkgs.nodePackages.yarn;
  inherit (easy-purescript-nix) spago purescript;
  inherit spago2nix niv yarn2nix;
}
