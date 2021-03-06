HISTFILE=~/.histfile
HISTSIZE=20000
SAVEHIST=20000

# Alias's
alias ls="ls -G"
alias grep="grep --color=auto"
alias kname='kubectl get -o jsonpath={.items[0].metadata.name}'
alias gotestchanged='git status --porcelain | sed s/^.../.\\// | xargs -n1 dirname | sort | uniq | xargs go test'

# Shortcuts to dirs I go to often
alias cd-src="cd ~/go/src/github.com/sourcegraph/sourcegraph"
alias cd-sg="cd ~/go/src/github.com/sourcegraph/sourcegraph"
alias cd-infra="cd ~/go/src/github.com/sourcegraph/infrastructure"
alias cd-zoekt="cd ~/go/src/github.com/google/zoekt"

# jump to repo
function repo {
    d=$(counsel-repo ${(s.:.)SRCPATH} | fzf --no-sort --select-1 --query="$1")
    for base in ${(s.:.)SRCPATH}; do
        if [ -d $base/$d ]; then
            cd $base/$d
            return 0
        fi
    done
}

# Set window title to current dir
function set_win_title(){
    echo -ne "\033]0; $(basename "$PWD") \007"
}
precmd_functions+=(set_win_title)

eval "$(direnv hook zsh)"
eval "$(starship init zsh)"

source ~/.zplug/init.zsh

zplug 'zplug/zplug',      hook-build:'zplug --self-manage'

zplug "lib/completion",   from:oh-my-zsh
zplug "lib/git",          from:oh-my-zsh
zplug "lib/history",      from:oh-my-zsh
zplug "lib/key-bindings", from:oh-my-zsh

zplug load

# The next line enables shell command completion for gcloud.
if [ -f '/Users/keegan/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/keegan/google-cloud-sdk/completion.zsh.inc'; fi
