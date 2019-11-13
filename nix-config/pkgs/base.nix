{ pkgs, ... }:

with import ../util;

let

  node2nixPkgs = import ./node2nix { };

  easy-purescript = pkgs.easy-purescript;

  elmTools = import (pkgs.fetchFromGitHub {
    owner = "turboMaCk";
    repo = "nix-elm-tools";
    rev = "41b5045587f84d993a7ee55972cfd61152cafc48";
    sha256 = "1ns02xxj3zijf6myaxk8azgs8v69gpc2b0v080m2xjf1pvv6hd75";
  }) { inherit pkgs; };

in with pkgs;

lib.flatten (builtins.attrValues {

  aliases = mkAliases {
    # elm-format = "${elmPackages.elm}/bin/elm-format-0.18";
    greyBg = "xsetroot -solid '#c5c8c9'";
    blackBg = "xsetroot -solid '#000000'";
  };

  custom = [ (import ./monorepo-tools.nix { }).monorepo-tools-add ];

  desktop = [
    xorg.xinit
    xmonad
    xzoom
    rofi
    zathura
    # playonlinux
    xfce.thunar
    libreoffice
  ];

  web = [
    tdesktop
    chromium
    # opera
    firefox
    tor
    thunderbird
    torbrowser
    youtube-dl
  ];

  dev = [
    (import ./unison.nix { })
    jekyll
    bundler
    bundix
    vscode
    atom
    franz
    jetbrains.idea-community
    terraform
    chamber
    nailgun
    gitAndTools.pre-commit
    # stack2nix broken?
    dhall
    awscli
    aws-vault
    docker_compose
    cloc
    travis
    emacs
    # emacs-nw
    emacs-client
    mysql
    cabal-install
    vim
    gcc
    # racket
    pythonExt
    # zeal
    stack
    openjdk
    leiningen
    git
    pkgs.boot
    ghc
    node2nixPkgs.cross-env
    node2nixPkgs.localtunnel
    # node2nixPkgs.elm-doc-preview
    # node2nixPkgs.elm-live
    # node2nixPkgs.create-elm-app
    # node2nixPkgs.pulp
    # unstable.purescript
    node2nixPkgs.selenium-webdriver
    node2nixPkgs.ts-node
    # node2nixPkgs.elm-test
    nodePackages.prettier
    # node2nixPkgs.pre-commit
    node2nixPkgs.http-server
    nodePackages.bower
    nodePackages.pulp
    node2nixPkgs.chalk-cli
    nodePackages.typescript
    # haskellPackages.idris
    haskellPackages.stylish-haskell
    yarn
    chromedriver
    gitAndTools.gitflow
    nodejs-10_x
    elmPackages.elm
    elmPackages.elm-format
    # elmPackages.elm
    # elmPackages.elm-format
    # node2nixPkgs.elm-oracle
    # purescript
    sbt
    swiProlog
    glslviewer
    docker
    python3Packages.notebook
    coq
    rustc
    cargo
    rustup
    scala
    ocamlPackages.merlin
    ocaml
    elmTools.elm-test
    elmTools.elm-verify-examples
    elmTools.elm-analyse
    elmTools.elm-doc-preview
    purs
    spago
    spago2nix
    purty
  ];

  gfx = [ blender imagemagick gimp graphviz ];

  nix = [
    nixops
    # npm2nix
    nix-prefetch-git
    # cabal2nix
    nodePackages.node2nix
  ];

  system = [ acpi ];

  cli = [
    xclip
    jq
    fish
    bash
    coreutils
    tree
    which
    jp
    pstree
    bc
    file
    rlwrap
    inotify-tools
    libnotify
    parallel
    iotop
    jnettop
    ag
    pastebinit
  ];

  utils = [
    zip
    unzip
    pass
    psmisc # e.g. killall
    alsaUtils
    ncftp
    rxvt
    konsole
    lxterminal
    sshpass
    usbutils
  ];

  media = [ vlc scrot feh espeak sox ];

  office = [
    # libreoffice
    aspellDicts.de
    aspellDicts.en
    aspell
  ];

  crypto = [ gnupg ];

  vm = [ virtualbox ];

  security = [ clamav ];

})
