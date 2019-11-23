{ pkgs, nodejs, python, alsaLib, runCommand, node2nix, stdenv, spago2nix
, writeShellScriptBin, writeText, purescript, parcel-bundler, ... }:

let

  nix-gen = (writeShellScriptBin "nix-gen" ''
    ${spago.generate}/bin/spago-gen
  '');

  nodePkg = let
    derivation = runCommand "node2nix" { } ''
      mkdir $out
      cp ${./package.json} $out/package.json
      cp ${./package-lock.json} $out/package-lock.json

      cd $out
      ${node2nix}/bin/node2nix --lock
    '';
    expression = import derivation { inherit nodejs; };
  in expression.package.override { buildInputs = [ python alsaLib ]; };

  spago = rec {
    generate = writeShellScriptBin "spago-gen" ''
      ${spago2nix}/bin/spago2nix generate
      mv spago-packages.nix -t nix/spago2nix
    '';

    spagoPkgs = import ./nix/spago2nix/spago-packages.nix { inherit pkgs; };

    projectOutput = spagoPkgs.mkBuildProjectOutput {
      src = ./src;
      purs = purescript;
    };
  };

  executable = let
    entry = writeText "index.js" ''
      var Main = require("${spago.projectOutput}/output/Main");
      Main.main()
    '';
  in writeShellScriptBin "fad9-cli" ''
    export NODE_PATH=${nodePkg}/lib/node_modules/fad9-cli/node_modules
    ${nodejs}/bin/node ${entry} $@
  '';

  default = stdenv.mkDerivation {
    name = "fad9-cli";
    buildInputs = [ python alsaLib nodejs nix-gen parcel-bundler ];
    buildCommand = "ln -s ${executable} $out";
  };

in default
