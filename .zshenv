typeset -U path
# export PATH=/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin
path+=(/usr/local/bin /usr/local/sbin /usr/bin /bin /usr/sbin /sbin)
# export PATH=$PATH:$HOME/cascode/github.com/xiki/bin:/usr/local/sbin
path+=($HOME/cascode/github.com/xiki/bin /usr/local/sbin)
# export PATH="$PATH:$HOME/.fzf/bin"
path+=($HOME/.fzf/bin)
# export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
path+=($HOME/.rvm/bin) # Add RVM to PATH for scripting

if uname | grep -q Linux ; then
    export EDITOR='emacsclient -t'
else
    export EDITOR='emacsclient -n'
fi
export GOROOT=/usr/local/opt/go/libexec/
export GOPATH=$HOME/.go
if { uname | grep -q Linux; } && [ -e $HOME/liuxiang ] ; then
    export ZDOTDIR=$HOME/liuxiang
    export PYTHONPATH=$HOME/.local/lib/python2.7/site-packages
    export FPATH="$FPATH:$HOME/liuxiang/local/share/zsh/site-functions"
    [ -n "$EMACS" ] && source $HOME/liuxiang/.zprofile
else
    export PYTHONPATH=$HOME/Library/Python/2.7/lib/python/site-packages
    export FPATH="$FPATH:/usr/local/share/zsh/site-functions"
    [ -n "$EMACS" ] && source $HOME/.zprofile
    source $HOME/.config/private/homebrew_github_api_token.sh
fi
export XAPIAN_CJK_NGRAM=1
export SDKMAN_DIR="$HOME/.sdkman"
export HOMEBREW_CASK_OPTS=--caskroom=/opt/homebrew-cask/Caskroom
path+=($GOROOT/bin $GOPATH/bin)

