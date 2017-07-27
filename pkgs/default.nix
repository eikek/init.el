{ pkgs, emacsPackagesNg }:

[ (import ./helm-org-clock.nix {inherit pkgs emacsPackagesNg;})
]
