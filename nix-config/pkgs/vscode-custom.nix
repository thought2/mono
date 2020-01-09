{ pkgs }:
let
  extensions = with pkgs.vscode-extensions; pkgs.vscode-utils.extensionsFromVscodeMarketplace [
    {
      name = "code-runner";
      publisher = "formulahendry";
      version = "0.6.33";
      sha256 = "166ia73vrcl5c9hm4q1a73qdn56m0jc7flfsk5p5q41na9f10lb0";
    }];
in
  pkgs.vscode-with-extensions.override {
      vscodeExtensions = extensions;
    }
