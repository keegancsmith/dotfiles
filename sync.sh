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

dotfiles=".bash_logout .bash_profile .bashrc .emacs .emacs.d \
          .hgrc .inputrc .xsession .vimrc"

for f in $dotfiles; do
    conditionallink "${REPO}/${f}" "${HOME}/${f}"
done

# SSH
conditionallink "${REPO}/sshconfig" "${HOME}/.ssh/config"

# xmonad
conditionallink "${REPO}/xmonad.hs" "${HOME}/.xmonad/xmonad.hs"
conditionallink "${REPO}/xmobarrc"  "${HOME}/.xmonad/xmobarrc"

# misc files used by configs
cd "${REPO}/misc"
for f in *; do
    conditionallink "${REPO}/misc/${f}" "${HOME}/.misc/${f}"
done
