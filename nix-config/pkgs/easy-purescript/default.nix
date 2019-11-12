{pkgs ? import <nixpkgs> {} }:
let
  easyPS = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "aa94aeac3a6ad9b4dfa0e807ad1421097d74f663";
    sha256 = "3547d95dbf3e0b60475afdde76d8d6f1f97b9f033a38f27e31c13da6b389d0cd";
  }) {
    inherit pkgs;
  };
in
  {
  inherit (easyPS)
    purs
    spago
    spago2nix
    purty;
}
