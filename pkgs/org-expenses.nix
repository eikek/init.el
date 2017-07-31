{ pkgs, emacsPackagesNg }:

emacsPackagesNg.melpaBuild rec {

  pname = "org-expenses";

  version = "0.0.1";

  src = pkgs.fetchFromGitHub {
    owner = "eikek";
    repo = "${pname}";
    rev = "47f972fd14516291ea33d0590f183583956b7888";
    sha256 = "14wy56lnfqnpp2l94n3yqdcarncs5nc23g0ak51ahi36aykl60gv";
  };

  packageRequires = with emacsPackagesNg; [ dash s org ];
}
