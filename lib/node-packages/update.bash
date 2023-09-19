#! /usr/bin/env nix-shell
#! nix-shell -i bash -p bash nodePackages.node2nix

node2nix -18 -i node-packages.json
