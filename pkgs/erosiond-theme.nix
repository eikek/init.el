{ pkgs, emacsPackagesNg }:

emacsPackagesNg.trivialBuild rec {

  pname = "erosiond-theme";

  version = "0.0.1";

  src = pkgs.fetchFromGitHub {
    owner = "darrik";
    repo = "${pname}";
    rev = "9c085d4595bf38e030abc9bd174c33fc5e7b82b8";
    sha256 = "1jvd1zb9w1b9i55wrdi8xv8p2ij0vhwz31662jx7bnlpk7yz6gfs";
  };

  packageRequires = with emacsPackagesNg; [ ];
}
