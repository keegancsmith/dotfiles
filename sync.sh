#!/bin/bash

# Place this file in the same directory as all your config files.

function conditionallink {
    if [ ! -e "$1" ]; then
        echo "Error: $1 does not exist"
        return
    fi
    if [ -e "$2" ] && [ ! -h "$2" ]; then
        echo "Error: $2 exists and is not a symbolic link"
        return
    fi

    rm -f "$2"
    mkdir -p "$(dirname $2)"
    ln -vs "$1" "$2"
}

cd $(dirname $0)
REPO=$(pwd)

dotfiles=".bash_logout .bash_profile .bashrc .emacs .mu4e.el \
          .hgrc .inputrc .screenrc .vimrc .xsession .gitconfig \
          .beetsconfig .bash_darwin .gitignore_global .zshrc"
for f in $dotfiles; do
    conditionallink "${REPO}/${f}" "${HOME}/${f}"
done

# kitty
conditionallink "${REPO}/kitty.conf" "${HOME}/.config/kitty/kitty.conf"
conditionallink "${REPO}/dracula.conf" "${HOME}/.config/kitty/dracula.conf"

# starship
conditionallink "${REPO}/starship.toml" "${HOME}/.config/starship.toml"

# youtube-dl
conditionallink "${REPO}/youtube-dl.conf" "${HOME}/.config/youtube-dl/config"

# tridactyl (firefox vim mode)
conditionallink "${REPO}/tridactylrc" "${HOME}/.config/tridactyl/tridactylrc"

# Karabiner-Elements
conditionallink "${REPO}/karabiner.json" "${HOME}/.config/karabiner/karabiner.json"

# xmonad
conditionallink "${REPO}/xmonad.hs" "${HOME}/.xmonad/xmonad.hs"
conditionallink "${REPO}/xmobarrc"  "${HOME}/.xmonad/xmobarrc"

# misc files used by configs
cd "${REPO}/misc"
for f in *; do
    conditionallink "${REPO}/misc/${f}" "${HOME}/.misc/${f}"
done
