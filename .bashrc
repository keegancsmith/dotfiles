# Some customizations
export LC_ALL=en_US.UTF-8
export EDITOR=nvim
export BROWSER=qutebrowser
export HISTCONTROL=ignoreboth:erasedups
export HISTFILESIZE=80000
export HISTSIZE=20000
export MAILCHECK=0

export SRCPATH="$HOME/src:$HOME/org-files"

# I hate it when I accidently lock the terminal in screen
export LOCKPRG=/bin/true

# Go
export GOPATH=$HOME/go
export GOBIN=$GOPATH/bin

# Check for local install of gcloud
if [[ -d ~/google-cloud-sdk ]]; then
  source ~/google-cloud-sdk/path.bash.inc
  source ~/google-cloud-sdk/completion.bash.inc
fi

# PATH dirs to add if they exist
paths=(
    "/opt/homebrew/bin"
    "/usr/local/opt/findutils/libexec/gnubin"
    "/usr/local/opt/openjdk/bin"
    "/usr/local/sbin"
    "/usr/local/bin"
    "/usr/local/opt/go/libexec/bin"
    "/nix/var/nix/profiles/default/bin"
    "/var/run/current-system/sw/bin"
    "/run/current-system/sw/bin"
    "$HOME/.nix-profile/bin"
    "$HOME/.cargo/bin"
    "$GOBIN"
    "$HOME/bin"
)
for p in "${paths[@]}"; do
    [ -d "$p" ] && [[ "$PATH" != *"$p"* ]] && export PATH="$p":"$PATH"
done

# eat checks that TERM is eat-*, so source it before changing our TERM below.
[ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \
    source "$EAT_SHELL_INTEGRATION_DIR/bash"

# Fallback to more general TERM for terminal emulators I use. Mostly useful
# for SSH onto machines without the corresponding terminfo.
case $TERM in
    eat*)
        export TERM=xterm-256color
        ;;
    alacritty*|*kitty*)
        export TERM=xterm-256color
        ;;
    screen*)
        export TERM=screen-256color
        ;;
esac

# Enable colors for ls, etc.
if hash dircolors 2>/dev/null; then
    eval "`dircolors -b`"
fi
export CLICOLOR=1

# White background
export MCFLY_LIGHT=TRUE

# Test for an interactive shell.  There is no need to set anything
# past this point for scp and rcp, and it's important to refrain from
# outputting anything in those cases.
if [[ $- != *i* ]] ; then
    # Shell is non-interactive.  Be done now!
    return
fi

# Change the window title of X terminals
case $TERM in
    xterm*|rxvt*|Eterm|alacritty)
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

# pass with fzf
function pass-fzf {
    d=$(find ~/.password-store/ -name '*.gpg' -printf '%P\n' | rev | cut -b 5- | rev | fzf --select-1)
    echo
    pass "$@" "$d"
}

# Alias's
alias la="ls -A"
alias ll="ls -l"
alias grep="grep --color=auto"
alias o="xdg-open"
alias kname='kubectl get -o jsonpath={.items[0].metadata.name}'
alias vim=nvim

if ! command -v open &> /dev/null; then
    alias open='xdg-open'
fi

shopt -s histappend

eval "$(direnv hook bash)"

# avoid fancy prompt for automated interactive ssh such as tramp.
[[ $TERM == "dumb" ]] || eval "$(starship init bash)"

# Completion
[ -f /etc/bash_completion ] && . /etc/bash_completion

true
