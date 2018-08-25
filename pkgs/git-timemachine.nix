{ pkgs, emacsPackagesNg }:

emacsPackagesNg.trivialBuild rec {

  pname = "git-timemachine";

  version = "4.6";

  src = pkgs.fetchFromGitLab {
    owner = "pidu";
    repo = "${pname}";
    rev = "90a980578249c102da3e904fccdc9a2a5a0e7bcc";
    sha256 = "027zpl603kad2x59wzygiblpc84cn945fnqpm7m1yi3lw4smxsbr";
  };

  packageRequires = [ ];
}
