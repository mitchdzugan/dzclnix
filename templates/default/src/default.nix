{
  mkDerivation,
  mkShell,

  alexandria,
  clnix-swank-server,
  iterate,
}:

let
  self = mkDerivation {
    pname = "rename-me";
    version = "0.1.0";
    src = ./.;

    propagatedBuildInputs = [
      alexandria
      iterate
    ];

    passthru.devShell = mkShell {
      inputsFrom = [ self ];
      nativeBuildInputs = [ clnix-swank-server ];
    };
  };

in
self
