#! /usr/bin/env nix-shell
#! nix-shell -i bash -p nodePackages.node2nix

cd "$(dirname "${BASH_SOURCE[0]}")"

node2nix -18 -i node-packages.json

nix fmt .
