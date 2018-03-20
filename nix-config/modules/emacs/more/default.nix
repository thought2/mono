{ pkgs, lib, ... }:

with lib;
with pkgs.emacs25PackagesNg;

{ programs.emacs =
    { pkgs = flatten
      [ aggressive-indent
        auto-complete
        better-defaults
        cider
        clojure-mode
        helm-dash
        company
        elm-mode
        flycheck
        flycheck-elm
        melpaPackages.nix-mode
        mmm-mode
        magit
        paredit
        smartparens
        tide
        web-mode
        direx
        [haskell-mode hindent]
        nixos-options
        helm-nixos-options
        company-nixos-options
        nix-sandbox
        js2-mode
        prettier-js
        indium
        yasnippet
        elfeed
        [ ensime sbt-mode ] # scala
        [ ivy-pass helm-pass pass ]
        dictcc
      ];
      init = readFile ./default.el;
    };
}
