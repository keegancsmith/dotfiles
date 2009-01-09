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
    mkdirhier "$(dirname $2)"
    ln -vs "$1" "$2"
}

cd $(dirname $0)
REPO=$(pwd)

for f in .bash_logout .bash_profile .bashrc .emacs .emacs.d .hgrc; do
    conditionallink "${REPO}/${f}" "${HOME}/${f}"
done

conditionallink "${REPO}/sshconfig" "${HOME}/.ssh/config"
