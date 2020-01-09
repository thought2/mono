{ pkgs, ... }:
{
  nix.binaryCaches = [ "https://thought2.cachix.org" ];

  nix.binaryCachePublicKeys = [
    "thought2.cachix.org-1:+c5/EQBPB9m64uxauVdL116qje6xeWElqZK78xaUddE="
  ];

  systemd.services."cachix" = {
    enable = false;
    description = "";
    wantedBy = [ "default.target" ];
    serviceConfig.Restart = "always";
    serviceConfig.RestartSec = 2;
    serviceConfig.ExecStart = "${pkgs.cachix}/bin/cachix push thought2 --watch-store";
  };
}
