{ pkgs, emacsPackagesNg }:

[ (import ./helm-org-clock.nix {inherit pkgs emacsPackagesNg;})
  (import ./org-expenses.nix {inherit pkgs emacsPackagesNg;})
  (import ./erosiond-theme.nix {inherit pkgs emacsPackagesNg;})
  (import ./colonoscopy-theme.nix {inherit pkgs emacsPackagesNg;})
]
