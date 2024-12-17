{
  asdf,
  ecl,
  lib,
}:

let
  self = ecl.overrideAttrs (old: {
    postPatch = ''
      cp ${asdf}/lib/common-lisp/asdf/build/asdf.lisp ./contrib/asdf/asdf.lisp
    '';

    passthru = {
      evalCommand =
        files:
        lib.escapeShellArgs (
          [ (lib.getExe self) ]
          ++ builtins.concatMap (file: [
            "--eval"
            "${file}"
          ]) files
          ++ [ "--" ]
        );
      loadCommand =
        files:
        lib.escapeShellArgs (
          [ (lib.getExe self) ]
          ++ builtins.concatMap (file: [
            "--load"
            "${file}"
          ]) files
          ++ [ "--" ]
        );
    };
  });
in
self
