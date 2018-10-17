#!/bin/bash

set -ex

# brew leaves
brew install \
     bash \
     bash-completion \
     codemod \
     git \
     go \
     graphviz \
     htop \
     ispell \
     jq \
     kitty \
     mosh \
     python \
     redis \
     ripgrep \
     screen \
     vcprompt \
     watchman \
     youtube-dl

# brew cask list
brew cask install \
     docker \
     emacs \
     google-backup-and-sync \
     google-chrome \
     iterm2 \
     kap \
     keybase \
     keycastr \
     mpv \
     muzzle \
     slack \
     spectacle \
     spotify \
     virtualbox \
     wkhtmltopdf

brew install --with-short-names kubectx

brew tap homebrew/cask-versions
brew cask install visual-studio-code-insiders
