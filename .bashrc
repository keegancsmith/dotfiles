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
alias ack-go="ack --ignore-dir=vendor --type=go"
alias kname='kubectl get -o jsonpath={.items[0].metadata.name}'

# Completion
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

# Some customizations
export LC_ALL=en_US.UTF-8
export EDITOR=vim
export HISTCONTROL=ignoreboth:erasedups
export HISTFILESIZE=80000
export HISTSIZE=20000
export MAILCHECK=0

# I hate it when I accidently lock the terminal in screen
export LOCKPRG=/bin/true

# Create PYTHONPATH if it doesn't exist. A hack to ensure hg can see pip
# installed packages
#if [ -z "$PYTHONPATH" ] && hash pip &> /dev/null; then
#    export PYTHONPATH="$(pip -V | grep -o '/.*site-packages')"
#fi

shopt -s histappend

# Check for local install of gcloud
if [[ -d ~/google-cloud-sdk ]]; then
  source ~/google-cloud-sdk/path.bash.inc
  source ~/google-cloud-sdk/completion.bash.inc
fi

# Virtualenv
#export PIP_RESPECT_VIRTUALENV=true
#source ~/.misc/lazyvirtualenvwrapper.sh

[[ $OSTYPE == darwin* ]] && . ~/.bash_darwin

#function _update_ps1() {
#    PS1="$(powerline-go -error $? -mode compatible -modules venv,cwd,perms,gitlite,hg,jobs,exit,root)"
#}
#if [ "$TERM" != "linux" ]; then
#    PROMPT_COMMAND="_update_ps1"
#fi

alias cd-src="cd ~/go/src/github.com/sourcegraph/sourcegraph"
alias cd-sg="cd ~/go/src/github.com/sourcegraph/sourcegraph"
alias cd-infra="cd ~/go/src/github.com/sourcegraph/infrastructure"
alias cd-zoekt="cd ~/go/src/github.com/google/zoekt"

export TERM=xterm-256color

export SRCPATH=$HOME/go/src:$HOME/src

eval "$(direnv hook bash)"
eval "$(starship init bash)"
