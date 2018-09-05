==========
 Dotfiles
==========

Collection of my settings. Targetting OS X, but should work on Linux.

Usage
=====

Just run `./sync.sh` and make sure the external programs are installed.

OS X
====

I use brew to install all the program I care about: `./brew-install.sh`

Additionally the following settings are toggled:

- Accessibility -> Displays -> Reduce Transparency (faster window switches)
  
Org-Mode
========

Emacs expect a directory called `org-files` in your home directory. I sync
this with google drive::

  $ ln -s ~/Drive/org-files/ ~/

