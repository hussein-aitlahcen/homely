with import <nixpkgs> {};
with import ./homely { inherit stdenv; inherit pkgs; inherit callPackage; };
mkHomely rec {
  user = "hussein";
  directory = stdenv.mkDerivation {
    name = "${user}-dotfiles";
    src = fetchgit {
      url = "https://github.com/hussein-aitlahcen/dotfiles";
      rev = "edc83816e45003c4b9b949dc955bffa973a548e8";
      sha256 = "0f5hvldwjrn6f791kafnz3rhnvax2sjks9hyjb755zcvhz9h6i2c";
    };
    installPhase = ''
      mkdir $out
      cp -a . $out
    '';
  };
}
