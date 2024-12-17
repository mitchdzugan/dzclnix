{
  agutil,
  alexandria,
  anaphora,
  arrows,
  cl-environments,
  fetchFromGitHub,
  introspect-environment,
  lib,
  mkDerivation,
  optima,
}:

mkDerivation {
  pname = "cl-form-types";
  version = "20220526-git";

  src = fetchFromGitHub {
    owner = "alex-gutev";
    repo = "cl-form-types";
    rev = "daba0821df80fce51b5f6fb5f9c3f2d5932de428";
    hash = "sha256-oQ2N/3zW3pJwBsat5HnmrQ093WrxhChm+ZaEKJNvieE=";
  };

  propagatedBuildInputs = [
    agutil
    alexandria
    anaphora
    arrows
    cl-environments
    introspect-environment
    optima
  ];

  meta = {
    description = "Library for determining the types of Common Lisp forms based on information stored in the environment.";
    license = lib.licenses.mit;
  };
}
