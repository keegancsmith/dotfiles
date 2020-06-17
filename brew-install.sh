#!/bin/bash

set -ex

# brew leaves | sort | awk 'BEGIN{printf "brew install"}{printf " \\\n     %s",$1}END{printf "\n"}' | pbcopy
brew install \
     aria2 \
     asciidoc \
     automake \
     awscli \
     bash \
     bash-completion \
     black \
     cmake \
     codemod \
     coreutils \
     csshx \
     direnv \
     dos2unix \
     gawk \
     git \
     github/gh/gh \
     gnu-sed \
     go \
     goreleaser \
     graphviz \
     hledger \
     htop \
     imagemagick \
     ispell \
     johanhaleby/kubetail/kubetail \
     jq \
     kubectx \
     lastpass-cli \
     ledger \
     libarchive \
     libpq \
     libssh \
     libzip \
     make \
     minikube \
     mkcert \
     mosh \
     python \
     netcat \
     nginx \
     nmap \
     node@12 \
     pandoc \
     parallel \
     pkg-config \
     pngcrush \
     postgresql \
     prettier \
     pypy \
     python@2 \
     redis \
     ripgrep \
     screen \
     shellcheck \
     starship \
     terraform \
     tnftp \
     tree \
     vegeta \
     watch \
     watchman \
     wget \
     yarn \
     youtube-dl \
     zsh

# Build recent version of Emacs
brew tap d12frosted/emacs-plus
brew install emacs-plus@27 --with-jansson --with-modern-icon --with-no-titlebar

# brew cask list --full-name | sort | awk 'BEGIN{printf "brew cask install"}{printf " \\\n     %s",$1}END{printf "\n"}' | pbcopy
brew cask install \
     1password \
     1password-cli \
     adobe-acrobat-reader \
     homebrew/cask-fonts/font-cascadia \
     homebrew/cask-fonts/font-fira-code \
     homebrew/cask-fonts/font-gomono-nerd-font \
     homebrew/cask-fonts/font-hack \
     homebrew/cask-fonts/font-input \
     homebrew/cask-fonts/font-jetbrains-mono \
     homebrew/cask-fonts/font-office-code-pro \
     day-o \
     discord \
     docker \
     dropbox \
     firefox-developer-edition \
     google-chrome \
     handbrake \
     intellij-idea \
     kap \
     karabiner-elements \
     keybase \
     keycastr \
     kitty \
     minikube \
     mpv \
     muzzle \
     phantomjs \
     plex-media-server \
     signal \
     skype \
     slack \
     spectacle \
     spotify \
     transmission \
     virtualbox \
     visual-studio-code \
     vlc \
     whatsapp \
     wireshark \
     wkhtmltopdf
