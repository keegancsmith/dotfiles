#!/bin/bash

set -ex

# brew leaves
brew install bash bash-completion codemod git graphviz htop ispell jq mosh \
     redis ripgrep screen watchman youtube-dl python vcprompt go

# brew cask list
brew cask install docker emacs iterm2 kap keybase keycastr mpv muzzle slack \
     spectacle virtualbox google-backup-and-sync spotify google-chrome

brew tap homebrew/cask-versions
brew cask install visual-studio-code-insiders
