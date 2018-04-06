{ stdenv, pkgs, callPackage }:
rec {
  homely = callPackage ../lib/default.nix { };
  mkHomely = { user, directory }:
    let
      directoryTree = "$out/${user}.json";
      deliveryPath = "/nix/var/nix/profiles/per-user/${user}/homely/";
      callHomely = "${homely}/bin/homely";
    in
      stdenv.mkDerivation {
        name = "${user}-homely";
        builder = pkgs.writeText "builder.sh" ''
          . $stdenv/setup
          mkdir -p $out
          ${callHomely} bundle  ${directory}     ${directoryTree}
          ${callHomely} deliver ${directoryTree} $out
        '';
      };
}
