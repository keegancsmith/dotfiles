# /etc/skel/.bash_profile

# This file is sourced by bash for login shells.  The following line
# runs your .bashrc and is recommended by the bash info pages.
[[ -f ~/.bashrc ]] && . ~/.bashrc

export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/.nimble/bin:$PATH"

# Completion
[ -f /etc/bash_completion ] && . /etc/bash_completion

[[ $OSTYPE == darwin* ]] && . ~/.bash_darwin

# Check for local install of gcloud
if [[ -d ~/google-cloud-sdk ]]; then
  source ~/google-cloud-sdk/path.bash.inc
  source ~/google-cloud-sdk/completion.bash.inc
fi

export PATH=$HOME/bin:"$PATH"

# Go workspace
export GOPATH=$HOME/go
export GOBIN=$GOPATH/bin
[ -d $GOPATH ] && export PATH="$PATH":$GOBIN || unset GOPATH GOBIN
[ -d /usr/local/opt/go/libexec/bin ] && export PATH=/usr/local/opt/go/libexec/bin:$PATH
