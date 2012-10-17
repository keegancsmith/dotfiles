==========
 Dotfiles
==========

Collection of my linux settings. This should all work with the latest Ubuntu
currently out. There are some external programs I use which are not in
Ubuntu. These should be on somewhere on the $PATH

Usage
=====

Just run `./sync.sh` and make sure the external programs are installed.

External Programs
=================

* http://github.com/sargon/trayer-srg - trayer fork for notification area in
  xmonad (sudo aptitude install libgtk2.0-dev libxmu-dev)
* http://vc.gerg.ca/hg/vcprompt/ - Display version control information about
  your current directory in bash. If it is missing $PS1 won't include it.

Notes
=====

Ubuntu Oneiric seemed to introduce a bug were the menu bar in gnome terminal
is shown by default even if the profile disables it, this is a workaround::

  $ sudo apt-get remove appmenu-gtk3 appmenu-gtk appmenu-qt

General setup on Ubuntu
=======================

First I install aptitude::

  $ sudo apt-get install aptitude

Then I install everything else::

  $ sudo aptitude install mercurial emacs emacs-goodies-el vim \
     fonts-inconsolata screen xmonad build-essential haskell-mode \
     mercurial-git git python-virtualenv virtualenvwrapper python-pip ipython

I then compile the stuff in custom programs section and put symbolic links
into ~/bin.

Then I setup my dotfiles::

  $ mkdir ~/repos; cd ~/repos
  $ hg clone https://bitbucket.org/keegan_csmith/dotfiles
  $ cd dotfiles
  $ ./sync.sh

This will say which files didn't copy due to already existing. Just go ahead
and delete those and run sync again.
