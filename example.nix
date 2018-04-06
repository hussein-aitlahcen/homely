with import <nixpkgs> {};
with import ./homely { inherit stdenv; inherit pkgs; inherit callPackage; };
mkHomely {
  user = "hussein";
  directory = stdenv.mkDerivation {
    name = "hussein-dotfiles";
    src = fetchgit {
      url = "https://github.com/hussein-aitlahcen/dotfiles";
      rev = "c2af9631b263af768da9e1dc7e17fb22ef6ebe5e";
      sha256 = "1djw7gmrjlna8dl9mmr2gklszmgpiry31vr3w0bhxfhiv4khdmsd";
    };
    installPhase = ''
      mkdir $out
      cp -a . $out
    '';
  };
}
