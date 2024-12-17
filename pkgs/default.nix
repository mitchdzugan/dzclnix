{ lispPackages }:

{
  clnix-swank-server = lispPackages.callPackage ./clnix-swank-server.nix { };
  cl-environments = lispPackages.callPackage ./cl-environments.nix { };
  cl-form-types = lispPackages.callPackage ./cl-form-types.nix { };
  coalton = lispPackages.callPackage ./coalton.nix { };
  immutable = lispPackages.callPackage ./immutable.nix { };
}
