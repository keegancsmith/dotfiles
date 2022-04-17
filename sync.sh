#!/usr/bin/env bash

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

dotfiles=".bash_logout .bash_profile .bashrc .emacs.d \
          .hgrc .inputrc .screenrc .vimrc .gitconfig \
          .bash_darwin .gitignore_global .zshrc .zshenv \
          .notmuch-config .tmux.conf"
for f in $dotfiles; do
    conditionallink "${REPO}/${f}" "${HOME}/${f}"
done

# fish
conditionallink "${REPO}/config.fish" "${HOME}/.config/fish/config.fish"

# kitty
conditionallink "${REPO}/kitty.conf" "${HOME}/.config/kitty/kitty.conf"

# starship
conditionallink "${REPO}/starship.toml" "${HOME}/.config/starship.toml"

# youtube-dl
conditionallink "${REPO}/youtube-dl.conf" "${HOME}/.config/youtube-dl/config"

# mpv
conditionallink "${REPO}/mpv.conf" "${HOME}/.config/mpv/mpv.conf"

# Karabiner-Elements
conditionallink "${REPO}/karabiner.json" "${HOME}/.config/karabiner/karabiner.json"

# qutebrowser
case $OSTYPE in
    darwin*)
        conditionallink "${REPO}/qutebrowser" "${HOME}/.qutebrowser"
        ;;
    *)
        conditionallink "${REPO}/qutebrowser" "${HOME}/.config/qutebrowser"
        ;;
esac

# i3
conditionallink "${REPO}/i3" "${HOME}/.config/i3"
conditionallink "${REPO}/i3status" "${HOME}/.config/i3status"

# notmuch hooks
conditionallink "${REPO}/notmuch-hooks" "${HOME}/.mail/.notmuch/hooks"

# https://github.com/gauteh/lieer gmail<->maildir syncer
conditionallink "${REPO}/lieer-gmail.json" "${HOME}/.mail/gmail/.gmailieer.json"
conditionallink "${REPO}/lieer-sourcegraph.json" "${HOME}/.mail/sourcegraph/.gmailieer.json"

# Ensure we have maildirs setup for lieer
mkdir -p "${HOME}"/.mail/{gmail,sourcegraph}/mail/{new,cur,tmp}

# misc files used by configs
cd "${REPO}/misc"
for f in *; do
    conditionallink "${REPO}/misc/${f}" "${HOME}/.misc/${f}"
done
