{ sources ? import ../../nix/sources.nix }:
let pkgs = import sources.nixpkgs { };
in {
  monorepo-tools-add = pkgs.writeShellScriptBin "monorepo-tools-add" ''
    ${sources.monorepo-tools}/monorepo_add.sh $@
  '';
}
