{ pkgs, emacsPackagesNg }:

emacsPackagesNg.trivialBuild rec {

  pname = "flucui-theme";

  version = "0.0.1";

  src = pkgs.fetchFromGitHub {
    owner = "MetroWind";
    repo = "${pname}";
    rev = "eba54099a8a42524ccf41fe724ad495d528abcd2";
    sha256 = "0zhni4q5kcg91dr7qgdszgsdpl3jkk2md0sjgpa8571j9q47iijl";
  };

  packageRequires = with emacsPackagesNg; [ ];
}
