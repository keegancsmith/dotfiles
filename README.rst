==========
 Dotfiles
==========

Collection of my linux settings. This should all work with the latest Ubuntu
currently out. There are some external programs I use which are not in
Ubuntu. These should be on somewhere on the $PATH

Usage
=====

Just run `./sync.sh` and make sure the external programs are installed.

OS X
====

I use brew and pip to install all the program I care about::

  $ brew install bash bash-completion codemod git graphviz htop ispell jq mosh redis ripgrep screen watchman youtube-dl python vcprompt
  $ brew cask install docker emacs kap keybase keycastr mpv muzzle slack spectacle virtualbox visual-studio-code-insiders
 
.. brew leaves
   brew cask list
  
Org-Mode
========

Emacs expect a directory called `org-files` in your home directory. I sync
this with dropbox::

  $ ln -s ~/Dropbox/org-files/ ~/

