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

# Here be dragons. I have a prefered order for my PATH. In particular I want
# this preference of PATHS: direnv > HOME > nix > homebrew > system.
#
# This logic will screw things up below, especially since anything in "paths"
# will end up less important than anything custom which may be unexpected by a
# calling program which executes bash -c. However, this is what works so far
# for me in the weird world of editors trying to guess your PATH + direnv.

# PATH dirs to add if they exist
paths=(
    "$HOME/bin"
    "$GOBIN"
    "$HOME/.cargo/bin"
    "$HOME/.nix-profile/bin"
    "/run/wrappers/bin"
    "/var/run/wrappers/bin"
    "/run/current-system/sw/bin"
    "/var/run/current-system/sw/bin"
    "/nix/var/nix/profiles/default/bin"
    "/usr/local/opt/go/libexec/bin"
    "/usr/local/bin"
    "/usr/local/sbin"
    "/usr/local/opt/openjdk/bin"
    "/usr/local/opt/findutils/libexec/gnubin"
    "/opt/homebrew/bin"
    "/usr/bin"
    "/bin"
    "/usr/sbin"
    "/sbin"
)

# Start with system PATH
initPATH=$PATH

# Clear PATH to rebuild it
unset PATH

# Add our preferred paths in order
for p in "${paths[@]}"; do
    if [ -d "$p" ]; then
        if [ -z "${PATH}" ]; then
            PATH="$p"
        elif [[ ":${PATH}:" != *":$p:"* ]]; then
            PATH="$PATH:$p"
        fi
    fi
done

# Add any remaining paths from the original PATH to the front
IFS=':' read -ra OLDPATH <<< "$initPATH"
for p in "${OLDPATH[@]}"; do
    if [ -n "$p" ] && [ -d "$p" ] && [[ ":${PATH}:" != *":$p:"* ]]; then
        PATH="$p:$PATH"
    fi
done

export PATH

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
    screen*|tmux*)
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

# quick jumping to directory in sourcegraph/cloud
function cloud_jump {
    local dir=$(git grep "displayName:" -- ":/**/config.yaml" | \
        awk -F': *' '{print $1 "\t" $3}' | \
        fzf --query "$1" --with-nth 2 | \
        cut -f1)
    if [ -n "$dir" ]; then
        cd "$(dirname "$dir")"
    fi
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

if ! command -v pbpaste &> /dev/null; then
    alias pbpaste='xclip -o'
fi

shopt -s histappend

eval "$(direnv hook bash)"

# avoid fancy prompt for automated interactive ssh such as tramp.
[[ $TERM == "dumb" ]] || eval "$(starship init bash)"

[ -f ~/.sourcegraph/sg.bash_autocomplete ] && eval "PROG=sg source ~/.sourcegraph/sg.bash_autocomplete"

# Completion
[ -f /etc/bash_completion ] && . /etc/bash_completion

true
