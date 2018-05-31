# Replace the dotfiles_url with yours, and let's go

DOTFILES_URL=https://github.com/hussein-aitlahcen/dotfiles
DOTFILES_DEFINITION_OUTPUT=./dotfiles.json

function write_dotfiles_definition {
    nix-prefetch-git --fetch-submodules $1 > $2
}

function build_dotfiles_definition {
    nix build -f example.nix --out-link output/definition
}

function build_homely {
    nix build -f ./lib/shell.nix --out-link output/homely
}

function deliver_dotfiles {
    ./output/homely/bin/homely deliver ./output/definition/`whoami`.json ~/
}

write_dotfiles_definition $DOTFILES_URL $DOTFILES_DEFINITION_OUTPUT && \
    build_dotfiles_definition && \
    build_homely && \
    deliver_dotfiles
