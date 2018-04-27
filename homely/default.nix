{ stdenv, pkgs, callPackage }:
rec {
  homely = callPackage ../lib/default.nix { };
  user = builtins.getEnv "USER";
  mkHomely = { directory }:
    let
      directoryTree = "$out/${user}.json";
      callHomely = "${homely}/bin/homely";
    in
      stdenv.mkDerivation {
        name = "${user}-homely";
        builder = pkgs.writeText "builder.sh" ''
          . $stdenv/setup
          mkdir -p $out
          ${callHomely} bundle ${directory} ${directoryTree}
        '';
      };
}
