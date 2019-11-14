{ pkgs, ... }: {
  services.xserver = {

    layout = "us";
    enable = true;
    exportConfiguration = true;
    # xkbVariant = "altgr-intl";
    # xkbOptions = "ctrl:nocaps, eurosign:e, compose:ralt";
    # xkbOptions = "eurosign:e,caps:none, keypad:pointerkeys";
  };

  environment.systemPackages = [
    (pkgs.writeShellScriptBin "keyboard-intl-on" ''
      ${pkgs.xorg.setxkbmap}/bin/setxkbmap -variant "altgr-intl"
    '')
    (pkgs.writeShellScriptBin "keyboard-intl-off" ''
      ${pkgs.xorg.setxkbmap}/bin/setxkbmap -variant ""
    '')

  ];

}
