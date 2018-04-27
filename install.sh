nix build -f example.nix --out-link output/definition && \
nix build -f ./lib/default.nix --out-link output/homely && \
./output/homely/bin/homely deliver ./output/definition/`whoami`.json ~/
