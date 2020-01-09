{ pkgs }:
let
  extensions = with pkgs.vscode-extensions;
    [ bbenoist.Nix ] ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
      {
        name = "nixfmt-vscode";
        publisher = "brettm12345";
        version = "0.0.1";
        sha256 = "07w35c69vk1l6vipnq3qfack36qcszqxn8j3v332bl0w6m02aa7k";
      }
      {
        name = "ide-purescript";
        publisher = "nwolverson";
        version = "0.20.8";
        sha256 = "16avxmb1191l641r6pd99lw2cgq8gdfipb9n7d0czx1g9vfjr3ip";
      }
      {
        name = "vscode-purty";
        publisher = "mvakula";
        version = "0.4.1";
        sha256 = "021r5cg6h229d2gfgd5a06iy0w5fw9563vxpfcs045nn559xpwxr";
      }
    ];
in pkgs.vscode-with-extensions.override { vscodeExtensions = extensions; }
