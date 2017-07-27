{ pkgs, emacsPackagesNg }:

emacsPackagesNg.trivialBuild rec {

  pname = "helm-org-clock";

  version = "0.0.1";

  src = pkgs.fetchFromGitHub {
    owner = "WorldsEndless";
    repo = "${pname}";
    rev = "8f5975e9cc6cf8f4d84101ed8fbca943d9a4a8cb";
    sha256 = "0zdiqyyvydqdxwgcr6v02wpjbdzbf9394jwijv34h29vyw22ics5";
  };

}
