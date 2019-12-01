{ stdenv, spago, purescript, niv, nix, writeShellScriptBin, spago2nix
, runCommand, pkgs, parcel-bundler, writeText, typescript, yarn, yarn2nix }:
let
  tsc-wrapped = writeShellScriptBin "tsc-wrapped" ''
    ${typescript}/bin/tsc $@
    find src -type f -name '*.js' | xargs sed -i 's/exports.__esModule = true;//g'
  '';

  generateNix = writeShellScriptBin "generate-nix" ''
    ${spago2nix}/bin/spago2nix generate
    mkdir -p nix/spago2nix
    mv spago-packages.nix nix/spago2nix
  '';

  buildYarn = yarn2nix.mkYarnModules {
    pname = "browse-graph";
    name = "browse-graph";
    version = "1.0.0";

    packageJSON = ../package.json;
    yarnLock = ../yarn.lock;
  };

  buildPureScript = rec {
    spagoPkgs = import ./spago2nix/spago-packages.nix { inherit pkgs; };
    src = runCommand "compile-typescript" { } ''
      TMP=`mktemp -d`
      ln -s ${buildYarn}/node_modules $TMP/node_modules
      ln -s ${../tsconfig.json} $TMP/tsconfig.json
      cp -r ${../src} $TMP/src
      chmod -R +w $TMP/src
      cd $TMP
      ${tsc-wrapped}/bin/tsc-wrapped
      cp -r $TMP/src $out
    '';
    projectOutput = spagoPkgs.mkBuildProjectOutput {
      inherit src;
      purs = purescript;
    };
  };

  bundle = let
    entry = writeText "index.js" ''
      var Main = require("./output/Main")
      Main.main()
    '';
  in runCommand "bundle" { } ''
    TMP=`mktemp -d`
    ln -s ${entry} $TMP/index.js
    ln -s ${buildPureScript.projectOutput}/output $TMP/output
    mkdir $out
    ${parcel-bundler}/bin/parcel \
      build \
      --out-dir $out \
      $TMP/index.js \
  '';
in stdenv.mkDerivation {
  name = "browse-graph";
  buildInputs = [
    spago
    purescript
    niv.niv
    generateNix
    parcel-bundler
    typescript
    yarn
    yarn2nix.yarn2nix
    tsc-wrapped
  ];
  buildCommand = ''
    mkdir $out
    cp ${bundle}/index.js $out/index.js
    cp -r ${../assets}/* -t $out
  '';
}
