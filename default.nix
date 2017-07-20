# from https://nixos.org/nixos/manual/index.html#module-services-emacs

{ pkgs ? import <nixpkgs> {} }:

let
  myEmacs = pkgs.emacs;
  emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;
in
  emacsWithPackages (epkgs: (with epkgs.melpaStablePackages; [
  ]) ++ (with epkgs; [
    use-package
    diminish
    dash
    s
    f
    hydra

    buffer-move
    eyebrowse
    rainbow-delimiters

    company
    company-auctex
    company-nixos-options
    company-quickhelp

    helm
    helm-ag
    helm-swoop

    which-key
    golden-ratio
    anzu
    nlinum

    solarized-theme
    reykjavik-theme
    spike-theme
    zerodark-theme
    boron-theme
    darktooth-theme
    smart-mode-line
    smart-mode-line-powerline-theme

    magit
    git-timemachine
    git-gutter
    git-gutter-fringe

    htmlize
    restclient

    org-plus-contrib
    org-bullets
    ob-restclient
    ob-mongo
    org-tree-slide

    projectile
    helm-projectile

    dired-subtree
    dired-rainbow
    dired-filter
    stripe-buffer
    whitespace-cleanup-mode
    move-text
    yasnippet
    helm-c-yasnippet
    expand-region
    multiple-cursors
    paredit
    ggtags
    helm-gtags
    emmet-mode
    web-mode
    rainbow-mode
    adoc-mode
    yaml-mode
    goto-chg
    geiser
    markdown-mode
    flymd
    plantuml-mode
    groovy-mode
    js2-mode
    scala-mode
    ensime
    elm-mode
    clojure-mode
    monroe
    cider
    slime
    nix-mode
    ess
    stumpwm-mode
    emms
    password-store
    pass
    magnatune
    chee
    dictcc
    jabber
    elfeed
    undo-tree
    beacon
    auctex
    nameless
    slack
    excorporate
    play-routes-mode
  ]))
