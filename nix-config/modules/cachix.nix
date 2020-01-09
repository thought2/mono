{ pkgs, ... }:
{
  nix.binaryCaches = [ "https://thought2.cachix.org" ];

  nix.binaryCachePublicKeys = [
    "thought2.cachix.org-1:+c5/EQBPB9m64uxauVdL116qje6xeWElqZK78xaUddE="
  ];

  systemd.user.services.cachix-daemon = {
    description = "IPFS Daemon";
    serviceConfig = {
       Type = "forking";
       ExecStart = "${pkgs.cachix}/bin/cachix push --watch-store";
       Restart = "on-failure";
     };
     wantedBy = [ "default.target" ];
   };
}
