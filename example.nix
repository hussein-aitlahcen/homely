with import <nixpkgs> {};
with import ./homely { inherit stdenv; inherit pkgs; inherit callPackage; };
let
  latest = builtins.fromJSON (builtins.readFile ./dotfiles.json);
in
  mkHomely {
    dotfiles = fetchgit {
      url    = latest.url;
      rev    = latest.rev;
      sha256 = latest.sha256;
    };
  }
