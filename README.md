# Homely

NixOS is our home.

This tool is experimental.

Minimalistic behavior: it download you dotfiles, make a `json` definition of it and symlink everything in your home.

Files are readonly thanks to nix store.

## Getting started

1. Supply your own **dotfiles** repository in `example.nix`
2. Run `./install.sh`
3. Feel like if you were at home
