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

# jump to repo
function repo {
    IFS=':' read -a srcpaths <<< ${SRCPATH}
    d=$(counsel-repo ${srcpaths[@]} | fzf --no-sort --select-1 --query="$1")
    for base in ${srcpaths[@]}; do
        if [ -d $base/$d ]; then
            cd $base/$d
            return 0
        fi
    done
}

# Alias's
alias la="ls -A"
alias ll="ls -l"
alias grep="grep --color=auto"
alias o="xdg-open"
alias ack-go="ack --ignore-dir=vendor --type=go"
alias kname='kubectl get -o jsonpath={.items[0].metadata.name}'

shopt -s histappend

alias cd-src="cd ~/go/src/github.com/sourcegraph/sourcegraph"
alias cd-sg="cd ~/go/src/github.com/sourcegraph/sourcegraph"
alias cd-infra="cd ~/go/src/github.com/sourcegraph/infrastructure"
alias cd-zoekt="cd ~/go/src/github.com/google/zoekt"

eval "$(direnv hook bash)"
eval "$(starship init bash)"
