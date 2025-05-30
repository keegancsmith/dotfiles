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
          .inputrc .screenrc .vimrc .gitconfig \
          .bash_darwin .gitignore_global .zshrc .zshenv \
          .notmuch-config .tmux.conf .hammerspoon"
for f in $dotfiles; do
    conditionallink "${REPO}/${f}" "${HOME}/${f}"
done

# kitty
conditionallink "${REPO}/kitty" "${HOME}/.config/kitty"

# direnv
conditionallink "${REPO}/direnvrc" "${HOME}/.config/direnv/direnvrc"

# starship
conditionallink "${REPO}/starship.toml" "${HOME}/.config/starship.toml"

# youtube-dl
conditionallink "${REPO}/youtube-dl.conf" "${HOME}/.config/youtube-dl/config"

# mpv
conditionallink "${REPO}/mpv.conf" "${HOME}/.config/mpv/mpv.conf"

# Karabiner-Elements
conditionallink "${REPO}/karabiner.json" "${HOME}/.config/karabiner/karabiner.json"

# i3
conditionallink "${REPO}/i3" "${HOME}/.config/i3"
conditionallink "${REPO}/i3status" "${HOME}/.config/i3status"

# rofi-pass
conditionallink "${REPO}/rofi-pass.conf" "${HOME}/.config/rofi-pass/config"

# ghostty
conditionallink "${REPO}/ghostty" "${HOME}/.config/ghostty/config"

# volumeicon
conditionallink "${REPO}/volumeicon" "${HOME}/.config/volumeicon/volumeicon"

# notmuch hooks
conditionallink "${REPO}/notmuch-hooks" "${HOME}/.mail/.notmuch/hooks"

# https://github.com/gauteh/lieer gmail<->maildir syncer
conditionallink "${REPO}/lieer-gmail.json" "${HOME}/.mail/gmail/.gmailieer.json"
conditionallink "${REPO}/lieer-sourcegraph.json" "${HOME}/.mail/sourcegraph/.gmailieer.json"

# Ensure we have maildirs setup for lieer
mkdir -p "${HOME}"/.mail/{gmail,sourcegraph}/mail/{new,cur,tmp}

# Per OS destinations
case $OSTYPE in
    darwin*)
        qutebrowser="${HOME}/.qutebrowser"
        vscode="${HOME}/Library/Application Support/Code/User"
        claude="${HOME}/Library/Application Support/Claude"
        ;;
    *)
        qutebrowser="${HOME}/.config/qutebrowser"
        vscode="${HOME}/.config/Code/User"
        claude="${HOME}/.config/Claude"
        ;;
esac

# vscode
conditionallink "${REPO}/vscode/settings.json" "${vscode}/settings.json"
conditionallink "${REPO}/vscode/keybindings.json" "${vscode}/keybindings.json"

# qutebrowser
conditionallink "${REPO}/qutebrowser" "${qutebrowser}"

# claude-desktop
conditionallink "${REPO}/claude_desktop_config.json" "${claude}/claude_desktop_config.json"
