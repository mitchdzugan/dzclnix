{
  asdf,
  ccl,
  lib,
}:

let
  self = ccl.overrideAttrs (old: {
    postPatch =
      old.postPatch
      + ''
        cp ${asdf}/lib/common-lisp/asdf/build/asdf.lisp ./tools/asdf.lisp
      '';

    passthru = {
      evalCommand =
        files:
        lib.escapeShellArgs (
          [
            (lib.getExe self)
            "--batch"
          ]
          ++ builtins.concatMap (file: [
            "--eval"
            "${file}"
          ]) files
          ++ [ "--" ]
        );
      loadCommand =
        files:
        lib.escapeShellArgs (
          [
            (lib.getExe self)
            "--batch"
          ]
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
