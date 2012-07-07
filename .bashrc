# /etc/skel/.bashrc:
#
# This file is sourced by all *interactive* bash shells on startup,
# including some apparently interactive shells such as scp and rcp
# that can't tolerate any output.  So make sure this doesn't display
# anything or bad things will happen !


# Test for an interactive shell.  There is no need to set anything
# past this point for scp and rcp, and it's important to refrain from
# outputting anything in those cases.
if [[ $- != *i* ]] ; then
    # Shell is non-interactive.  Be done now!
    return
fi

# Enable colors for ls, etc.  Prefer ~/.dir_colors #64489
if [[ -f ~/.dir_colors ]]; then
    eval `dircolors -b ~/.dir_colors`
elif [[ -f /etc/DIR_COLORS ]]; then
    eval `dircolors -b /etc/DIR_COLORS`
else
    eval "`dircolors -b`"
fi
alias ls="ls --color=auto"

# Change the window title of X terminals
case $TERM in
    xterm*|rxvt*|Eterm)
        PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME%%.*}:${PWD/$HOME/~}\007"'
        ;;
    screen)
        PROMPT_COMMAND='echo -ne "\033_${USER}@${HOSTNAME%%.*}:${PWD/$HOME/~}\033\\"'
        ;;
esac

# Optionally put vcprompt into PS1
if ! which vcprompt > /dev/null; then
    alias vcprompt='echo -n'
fi

# Prompt
#PS1='\[\033[01;32m\]\u@\h\[\033[01;34m\] \W \$\[\033[00m\] '
RED=`tput setaf 1`
GREEN=`tput setaf 2`
YELLOW=`tput setaf 3`
BLUE=`tput setaf 4`
MAGENTA=`tput setaf 5`
CYAN=`tput setaf 6`
WHITE=`tput setaf 7`
BOLD=`tput bold`
RST=`tput sgr0`
PS1='\[$BOLD$BLUE\][\w] \[$RST\]\[$GREEN\]$(vcprompt)\n\[$BOLD$GREEN\]\u@\h \[$YELLOW\]$ \[$RST\]'

# Alias's
alias la="ls -A"
alias ll="ls -l"
alias grep="grep --color=auto"
alias o="xdg-open"

# Completion
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

# Some customizations
export PATH=$HOME/bin:"$PATH"
export EDITOR=vim
export HISTCONTROL=ignoreboth
export HISTFILESIZE=8000
export HISTSIZE=2000
export MAILCHECK=0

# Virtualenv
export PIP_RESPECT_VIRTUALENV=true
export PIP_DOWNLOAD_CACHE=$HOME/.pip/cache
source ~/.misc/lazyvirtualenvwrapper.sh

shopt -s histappend
