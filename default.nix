# from https://nixos.org/nixos/manual/index.html#module-services-emacs

{ pkgs ? import <nixpkgs> {} }:

let
  myEmacs = pkgs.emacs;
  emacsPackagesNg = (pkgs.emacsPackagesNgGen myEmacs);
  emacsWithPackages = emacsPackagesNg.emacsWithPackages;
  customPackages = import ./pkgs { inherit pkgs emacsPackagesNg; };
in
  emacsWithPackages (epkgs: customPackages ++ (with epkgs.melpaStablePackages; [
  ]) ++ (with epkgs.orgPackages; [

    org-plus-contrib

  ]) ++ (with epkgs.elpaPackages; [

    nlinum
    rainbow-mode
    auctex
    excorporate
    hyperbole

  ]) ++ (with epkgs.melpaPackages; [
    use-package
    diminish
    dash
    s
    f
    hydra

    buffer-move
    eyebrowse
    rainbow-delimiters
    hide-lines

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
    nyan-mode

    solarized-theme
    reykjavik-theme
    soft-stone-theme
    soft-charcoal-theme
    sublime-themes
    boron-theme
    darktooth-theme
    leuven-theme
    eziam-theme
    sexy-monochrome-theme
    badger-theme
    smart-mode-line
    smart-mode-line-powerline-theme
    spacemacs-theme
    gruvbox-theme
    autumn-light-theme

    magit
    forge
    git-gutter
    git-gutter-fringe

    htmlize
    restclient

    org-bullets
    org-tree-slide
    org-journal
    org-jira
    ob-restclient
    ob-elvish
    ob-mongo
    ox-asciidoc
    ox-gfm
    ox-jira
    ox-mediawiki
    ox-pandoc
    ox-twbs

    projectile
    helm-projectile
    neotree

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
    adoc-mode
    yaml-mode
    sass-mode
    goto-chg
    geiser
    markdown-mode
    flymd
    plantuml-mode
    groovy-mode
    js2-mode
    scala-mode
    elm-mode
    clojure-mode
    elvish-mode
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
    #undo-tree
    beacon
    nameless
    slack
    play-routes-mode
    logview
    swagger-to-org
    scad-mode
    ansible
    fish-mode
  ]))
