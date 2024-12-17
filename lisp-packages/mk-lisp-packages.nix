{
  callPackage,
  lib,
  linkFarm,
  lisp,
  lispPackages,
  quicklisp,
  stdenv,
  writeText,
}:

let
  mkDerivation = callPackage ./make-derivation.nix { inherit lisp; };
  otherPackages = callPackage ../pkgs { inherit lispPackages; };
  quicklispOverrides = callPackage ../quicklisp/overrides.nix { inherit lispPackages; };

  # Branches on whether a list contains zero, one, or greater than one element.
  zoi =
    {
      zero,
      one ? (x: x),
      n,
    }:
    xs:
    let
      count = builtins.length xs;
    in
    if count == 0 then
      zero
    else if count == 1 then
      one (builtins.head xs)
    else
      n xs;

  # an attrset of attrsets, mapping system names to system info attrsets
  qlSystemInfo = builtins.mapAttrs (
    systemName:
    zoi {
      zero = builtins.abort "No system named ${systemName}; this is a bug in clnix";
      n = _: builtins.abort "Multiple Quicklisp projects provide system ${systemName}";
    }
  ) quicklisp.systems;

  # an attrset of strings, mapping project names to their version strings.
  qlVersions = builtins.mapAttrs (
    projectName: projectInfo:
    let
      inherit (projectInfo) prefix;
    in
    if lib.hasPrefix (projectName + "-") prefix then
      lib.removePrefix (projectName + "-") prefix
    else if lib.hasPrefix (projectName + "_") prefix then
      lib.removePrefix (projectName + "_") prefix
    else
      prefix
  ) quicklisp.projects;

  # "escapes" the name of a system
  escapeSystemName =
    builtins.replaceStrings
      [
        "'"
        "+"
        "."
        "/"
      ]
      [
        "-"
        "-"
        "-"
        "-"
      ];

  # an attrset of derivations, mapping system names to derivations that build
  # them (but do not test them)
  qlSystems = lib.pipe qlSystemInfo [
    builtins.attrNames
    (builtins.map (
      systemName:
      let
        systemInfo = qlSystemInfo.${systemName};
        projectName = systemInfo.project;
        projectInfo = quicklisp.projects.${projectName};

        asdFileBasename = systemInfo.system-file-basename;
        asdFile = zoi {
          zero = builtins.abort "No .asd files in project ${projectName} defined the system ${systemName}";
          n =
            _: builtins.abort "Multiple .asd files in project ${projectName} defined the system ${systemName}";
        } (builtins.filter (asdPath: baseNameOf asdPath == "${asdFileBasename}.asd") projectInfo.asd-files);

        inherit (projectInfo) prefix;
        depSystemNames = projectInfo.system-deps.${systemName};
        depSystems = lib.pipe depSystemNames [
          (builtins.filter (systemName: systemName != "asdf"))
          (builtins.map (depSystemName: self.${escapeSystemName depSystemName}))
        ];
        args = {
          pname = lib.strings.sanitizeDerivationName systemName;
          version = qlVersions.${projectName};

          src = projectInfo.src;
          nativeBuildInputs = [ lisp ];
          propagatedBuildInputs = depSystems;

          setSourceRoot = ''
            mkdir -p $out/src
            mv ${prefix} $out/src/${prefix}
            ln -s $out/src/${prefix}/${asdFile} $out/src/
            sourceRoot=$out/src
          '';

          configurePhase = ''
            runHook preConfigure

            mkdir -p $out/lib
            CL_SOURCE_REGISTRY="$out/src:''${CL_SOURCE_REGISTRY:-}"
            ASDF_OUTPUT_TRANSLATIONS="$out/bin:$out/bin:''${ASDF_OUTPUT_TRANSLATIONS:-}"
            ASDF_OUTPUT_TRANSLATIONS="$out/src:$out/lib:''${ASDF_OUTPUT_TRANSLATIONS:-}"
            export CL_SOURCE_REGISTRY ASDF_OUTPUT_TRANSLATIONS

            runHook postConfigure
          '';

          asdfSystemNames = [ systemName ];
          buildPhase = ''
            runHook preBuild

            while [[ ! -f "$NIX_BUILD_TOP/.clnix-finished-binaries" ]]; do
              ${
                lisp.loadCommand [
                  ./common.lisp
                  ./build-phase.lisp
                ]
              }
            done

            runHook postBuild
          '';

          installPhase = ''
            runHook preInstall
            runHook postInstall
          '';

          setupHook = writeText "setup-hook" ''
            CL_SOURCE_REGISTRY="@out@/src:''${CL_SOURCE_REGISTRY:-}"
            ASDF_OUTPUT_TRANSLATIONS="@out@/src:@out@/lib:''${ASDF_OUTPUT_TRANSLATIONS:-}"
            export CL_SOURCE_REGISTRY ASDF_OUTPUT_TRANSLATIONS
          '';
        };
      in
      {
        name = escapeSystemName systemName;
        value = stdenv.mkDerivation (args // ((quicklispOverrides.systems.${systemName} or (_: { })) args));
      }
    ))
    builtins.listToAttrs
  ];

  # a derivation, which is simply a linkFarm of all the systems in Quicklisp,
  # for easily testing that everything works.
  #
  # TODO: Add a checkPhase (or a flake check?) that runs all their tests too.
  allQuicklispSystems =
    let
      allSystemNames = builtins.attrNames qlSystems;

      systemNameNotExcluded =
        systemName:
        !(builtins.elem systemName quicklispOverrides.exclude-from-allQuicklispSystems)
        && !(lib.hasPrefix "clml" systemName);
      allNonExcludedSystemNames = builtins.filter systemNameNotExcluded allSystemNames;

      allLinkFarmEntries = builtins.map (systemName: {
        name = systemName;
        path = qlSystems.${systemName};
      }) allNonExcludedSystemNames;
    in
    linkFarm "all-quicklisp-systems" (
      builtins.trace "allQuicklispSystems has ${toString (builtins.length allLinkFarmEntries)} systems" allLinkFarmEntries
    );

  self = {
    inherit allQuicklispSystems lisp mkDerivation;
  } // qlSystems // otherPackages;
in
self
