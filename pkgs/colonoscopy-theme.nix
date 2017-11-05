{ pkgs, emacsPackagesNg }:

emacsPackagesNg.trivialBuild rec {

  pname = "emacs-colonoscopy-theme";

  version = "0.0.1";

  src = pkgs.fetchFromGitHub {
    owner = "emacsfodder";
    repo = "${pname}";
    rev = "64bbb322b13dae91ce9f1e3581f836f94f800ead";
    sha256 = "1r0is6zjkzikm565fvmj0gx8ms5ig9l5xihnka4fig7jy6ak33z5";
  };

  packageRequires = with emacsPackagesNg; [ ];
}
