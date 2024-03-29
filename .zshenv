export LC_ALL=en_US.UTF-8
export EDITOR=nvim
export MAILCHECK=0
export TERM=xterm-256color

# I hate it when I accidently lock the terminal in screen
export LOCKPRG=/bin/true

export SRCPATH=$HOME/go/src:$HOME/src

# PATH
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/.nimble/bin:$PATH"
export PATH="/usr/local/bin:$PATH"
export PATH=$HOME/bin:"$PATH"

# Go workspace
export GOPATH=$HOME/go
export GOBIN=$GOPATH/bin
[ -d $GOPATH ] && export PATH="$PATH":$GOBIN || unset GOPATH GOBIN
[ -d /usr/local/opt/go/libexec/bin ] && export PATH=/usr/local/opt/go/libexec/bin:$PATH

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/keegan/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/keegan/google-cloud-sdk/path.zsh.inc'; fi
if [ -e /Users/keegan/.nix-profile/etc/profile.d/nix.sh ]; then . /Users/keegan/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
