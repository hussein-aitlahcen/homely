{ stdenv, pkgs, callPackage }:
rec {
  homely = callPackage ../lib/default.nix { };
  user = builtins.getEnv "USER";
  mkHomely = { dotfiles }:
    let
      directory = stdenv.mkDerivation {
        name = "${user}-dotfiles";
        src = dotfiles;
        installPhase = ''
          mkdir $out
          cp -a . $out
        '';
      };
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
