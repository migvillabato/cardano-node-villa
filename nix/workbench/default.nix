{ pkgs
, lib, jq, runCommand
, db-analyser
, cardanoNodePackages
, cardano-world
}:

let

  workbench' = tools:

    /* The function `mkDerivation` in the Nixpkgs standard environment is a
       wrapper around `derivation` that adds a default value for system and
       always uses Bash as the `builder`, to which the supplied builder is
       passed as a command-line argument. See the Nixpkgs manual for details.
    */
    pkgs.stdenv.mkDerivation {

      /* Specifying a `name` and a `src` is the absolute minimum Nix requires.
         For convenience, you can also use `pname` and `version` attributes and
         `mkDerivation` will automatically set `name` to "${pname}-${version}"
         by default. Since [RFC 0035](https://github.com/NixOS/rfcs/pull/35),
         this is preferred for packages in Nixpkgs, as it allows us to reuse the
         version easily:
      */
      pname = "workbench";
      version = "0.1";

      # The list of source files or directories to be unpacked or copied.
      src = ./.;

      /* Many packages have dependencies that are not provided in the standard
         environment. It’s usually sufficient to specify those dependencies in
         the `buildInputs` attribute.
         This attribute ensures that the bin subdirectories of these packages
         appear in the PATH environment variable during the build, that their
         include subdirectories are searched by the C compiler, and so on.
      */
      buildInputs = [ jq pkgs.makeWrapper ];

      /* Often it is necessary to override or modify some aspect of the build.
         To make this easier, the standard environment breaks the package build
         into a number of phases, all of which can be overridden or modified
         individually: unpacking the sources, applying patches, configuring,
         building, and installing. (There are some others; see Section 6.5,
         [Phases](https://nixos.org/manual/nixpkgs/stable/#sec-stdenv-phases).)
      */

      buildPhase = ''
        patchShebangs .
      '';

      /* The fixup phase performs some (Nix-specific) post-processing actions on
         the files installed under $out by the install phase.
      */
      postFixup = ''
        wrapProgram "$out/bin/wb" --argv0 wb --prefix PATH ":" ${lib.makeBinPath tools}
      '';

      installPhase = ''
        mkdir -p         $out/bin
        cp -a wb chain-filters profiles *.sh *.jq $out/bin
      '';

      /* (Note the use of ''-style string literals, which are very convenient
         for large multi-line script fragments because they don’t need escaping
         of " and \, and because indentation is intelligently removed.)
      */

      # If set libraries and executables are not stripped. By default, they are.
      dontStrip = true;

    };

  workbench = with cardanoNodePackages; with pkgs; workbench' (
    [ git graphviz
      jq
      moreutils
      procps

      cardano-cli
      cardano-topology
    ] ++ lib.optional (!pkgs.stdenv.hostPlatform.isDarwin) db-analyser ++ [
      locli
    ]);

  runWorkbench =
    name: command:
    runCommand name {} ''
      ${workbench}/bin/wb ${command} > $out
    '';

  runWorkbenchJqOnly =
    name: command:
    runCommand name {} ''
      ${workbench' [jq pkgs.moreutils]}/bin/wb ${command} > $out
    '';

  runJq =
    name: args: query:
    runCommand name {} ''
      args=(${args})
      ${jq}/bin/jq '${query}' "''${args[@]}" > $out
    '';

  profile-names-json =
    runWorkbenchJqOnly "profile-names.json" "profiles list";

  profile-names =
    __fromJSON (__readFile profile-names-json);

  all-profiles =
    ## The backend is an attrset of AWS/supervisord-specific methods and parameters.
    { backend

    ## Environment arguments:
    ##   - either affect semantics on all backends equally,
    ##   - or have no semantic effect
    , envArgs
    }:
    rec {
      mkProfile =
        profileName:
        pkgs.callPackage ./profiles
          { inherit
              pkgs
              runWorkbenchJqOnly runJq workbench
              backend
              profileName;
          };

      value = lib.genAttrs profile-names mkProfile;

      JSON = pkgs.writeText "all-profiles.json" (__toJSON (lib.mapAttrs (_: x: x.value) value));
    };

  ## materialise-profile :: ProfileNix -> BackendProfile -> Profile
  materialise-profile      = import ./profile.nix  { inherit pkgs lib; };
  ## profile-topology :: ProfileNix -> Topology
  profile-topology         = import ./topology.nix { inherit pkgs; };
  ## profile-topology :: ProfileNix -> Topology -> Genesis
  profile-topology-genesis = import ./genesis.nix  { inherit pkgs; };

  with-profile =
    { backend, envArgs, profileName }:
    let
      ps = all-profiles { inherit backend envArgs; };

      profileNix = ps.value."${profileName}"
        or (throw "No such profile: ${profileName};  Known profiles: ${toString (__attrNames ps.value)}");

      profile = materialise-profile
        { inherit profileNix workbench;
          backendProfile =
            backend.materialise-profile { inherit profileNix; };
        };

      topology = profile-topology { inherit profileNix profile; };

      genesis = profile-topology-genesis { inherit profileNix profile topology; };
    in {
      inherit
        profileNix profile
        topology
        genesis;
    };

  run-analysis = import ./analyse.nix;

in {
  inherit runJq;

  inherit workbench' workbench runWorkbench runWorkbenchJqOnly;

  inherit all-profiles profile-names profile-names-json with-profile;

  inherit run-analysis;
}
