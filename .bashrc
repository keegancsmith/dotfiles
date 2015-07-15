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
elif hash dircolors 2>/dev/null; then
    eval "`dircolors -b`"
fi
alias ls="ls -G"

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
if ! which vcprompt &> /dev/null; then
    alias vcprompt='echo -n'
fi

# Prompt
RED=`tput setaf 1`
GREEN=`tput setaf 2`
YELLOW=`tput setaf 3`
BLUE=`tput setaf 4`
MAGENTA=`tput setaf 5`
CYAN=`tput setaf 6`
WHITE=`tput setaf 7`
BOLD=`tput bold`
RST=`tput sgr0`
get_exit_status(){
    es=$?
    if [ $es -ne 0 ]; then
        echo -n "$RED"
    else
        echo -n "$YELLOW"
    fi
}
PROMPT_COMMAND='exitStatus=$(get_exit_status)'
PS1='\[$BOLD$BLUE\][\w] \[$RST\]\[$GREEN\]$(vcprompt)\n\[$BOLD$GREEN\]\u@\h \[$exitStatus\]$ \[$RST\]'

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
export HISTCONTROL=ignoreboth:erasedups
export HISTFILESIZE=80000
export HISTSIZE=20000
export MAILCHECK=0

# I hate it when I accidently lock the terminal in screen
export LOCKPRG=/bin/true

# Virtualenv
export PIP_RESPECT_VIRTUALENV=true
source ~/.misc/lazyvirtualenvwrapper.sh

# Docker convenience functions
function docker-shell-image { docker run -ti --entrypoint /bin/bash $1 -s; }
function docker-shell-container { docker exec -ti $1 /bin/bash -s; }

# Go workspace
export GOPATH=$HOME/go
export GOBIN=$GOPATH/bin
[ -d $GOPATH ] && export PATH="$PATH":$GOBIN || unset GOPATH GOBIN
function gofetch {
    pushd $GOPATH/src
    find . -maxdepth 4 -type d -name .git -print -execdir git fetch -a \;
    popd
}

# Create PYTHONPATH if it doesn't exist. A hack to ensure hg can see pip
# installed packages
if [ -z "$PYTHONPATH" ] && hash pip &> /dev/null; then
    export PYTHONPATH="$(pip -V | grep -o '/.*site-packages')"
fi

shopt -s histappend
