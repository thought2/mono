{pkgs, config, ...}: 
let
  rootDir = pkgs.runCommand "rootdir" {} ''
    mkdir $out
    echo "foo" > $out/bar
  '';

  subDir = pkgs.runCommand "subdir" {} ''
    mkdir $out
    echo "baz" > $out/index.html
  '';
in
{
  networking.firewall.allowedTCPPorts = [ 80 22 ];

  services.nginx.enable = true;

  services.nginx = {

  };

  services.nginx.virtualHosts."localhost" = {
    addSSL = false;
    enableACME = false;
    # root = rootDir;

    locations = {
       "= /sub" = {
         alias = subDir + "/index.html";
       };
       "^~ /sub/" = {
         alias = subDir + "/";
      };
    };
  };

  services.openssh = {
    enable = true;
    permitRootLogin = "yes";
    passwordAuthentication = true;
  };

  users.mutableUsers = false;
  
  users.users.root = {
    initialPassword = "root";
    openssh.authorizedKeys.keyFiles = [ ./keys.nix ];
  };


}
