with import <nixpkgs> {};
with import ./homely { inherit stdenv; inherit pkgs; inherit callPackage; };
mkHomely {
  dotfiles = fetchgit {
    url = "https://github.com/hussein-aitlahcen/dotfiles";
    rev = "d43a5ba1124d7603b4714b6007d9271efbbf160e";
    sha256 = "190w6h4fwkv17m1rm1sf1c9m6b59dpanwnfm8yrbwaxq1f5vcjar";
  };
}
