{
  alexandria,
  anaphora,
  collectors,
  fetchFromGitHub,
  lib,
  lisp,
  mkDerivation,
  optima,
  parse-declarations-1-0,
}:

mkDerivation {
  pname = "cl-environments";
  version = "20220526-git";

  src = fetchFromGitHub {
    owner = "alex-gutev";
    repo = "cl-environments";
    rev = "e6cd3b5e1cabd27e1162d1c33d2d8c841e5158d4";
    hash = "sha256-NVLRnS8nnbwl6ADzUCrPbDGP87shuiEouPg3DBiZTl0=";
  };

  propagatedBuildInputs = [
    alexandria
    anaphora
    collectors
    optima
  ] ++ lib.optional (lisp.pname == "sbcl") parse-declarations-1-0;

  meta = {
    description = "Implements the CLTL2 environment access API.";
    license = lib.licenses.mit;
  };
}
