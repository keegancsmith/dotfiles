==========
 Dotfiles
==========

Collection of my settings. Targetting OS X, but should work on Linux.

Usage
=====

Just run `./sync.sh` and make sure the external programs are installed.

OS X
====

I use brew to install all the program I care about: `brew bundle`

Additionally the following settings are toggled:

- Accessibility -> Displays -> Reduce Transparency (faster window switches)

I use a daskeyboard. To get it working how I want on OS X I need to swap
command and option in "modifier keys" in the keyboard settings.

Org-Mode
========

Emacs expect a directory called `org-files` in your home directory. I sync
this with a private GitHub repository::

  $ git clone https://github.com/keegancsmith/org-files ~/org-files

Go
===

Install go tooling::

  $ go get -v \
     github.com/rogpeppe/godef \
     golang.org/x/tools/cmd/goimports
