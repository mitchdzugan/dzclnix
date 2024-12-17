{
  inputs.clnix = {
    url = "sourcehut:~remexre/clnix";
    inputs = {
      flake-utils.follows = "flake-utils";
      nixpkgs.follows = "nixpkgs";
    };
  };
  outputs =
    {
      self,
      clnix,
      flake-utils,
      nixpkgs,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        sbcl = clnix.packages.${system}.sbcl;
      in
      rec {
        devShells.default = pkgs.mkShell {
          inputsFrom = builtins.attrValues packages;
          nativeBuildInputs = [ sbcl.packages.clnix-swank-server ];
        };

        packages = {
          default = packages.rename-me;
          rename-me = sbcl.packages.callPackage ./src { };
        };
      }
    );
}
