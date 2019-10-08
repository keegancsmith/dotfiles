HISTFILE=~/.histfile
HISTSIZE=20000
SAVEHIST=20000

# Alias's
alias ls="ls -G"
alias grep="grep --color=auto"
alias kname='kubectl get -o jsonpath={.items[0].metadata.name}'

# Shortcuts to dirs I go to often
alias cd-src="cd ~/go/src/github.com/sourcegraph/sourcegraph"
alias cd-sg="cd ~/go/src/github.com/sourcegraph/sourcegraph"
alias cd-infra="cd ~/go/src/github.com/sourcegraph/infrastructure"
alias cd-zoekt="cd ~/go/src/github.com/google/zoekt"

export LC_ALL=en_US.UTF-8
export EDITOR=vim
export MAILCHECK=0
export TERM=xterm-256color

# I hate it when I accidently lock the terminal in screen
export LOCKPRG=/bin/true

export SRCPATH=$HOME/go/src:$HOME/src

eval "$(starship init zsh)"

source ~/.zplug/init.zsh

zplug 'zplug/zplug',      hook-build:'zplug --self-manage'

zplug "lib/completion",   from:oh-my-zsh
zplug "lib/git",          from:oh-my-zsh
zplug "lib/history",      from:oh-my-zsh
zplug "lib/key-bindings", from:oh-my-zsh

zplug 'dracula/zsh', as:theme

zplug load
