typeset -U path
# export PATH=/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin
path+=(/usr/local/bin /usr/local/sbin /usr/bin /bin /usr/sbin /sbin)
# export PATH=$PATH:$HOME/cascode/github.com/xiki/bin:/usr/local/sbin
path+=($HOME/cascode/github.com/xiki/bin /usr/local/sbin)
# export PATH="$PATH:$HOME/.fzf/bin"
path+=($HOME/.fzf/bin)
# export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
path+=($HOME/.rvm/bin) # Add RVM to PATH for scripting
export MANPATH="$MANPATH:$HOME/.fzf/man:$HOME/.linux-man"
export FPATH="$FPATH:/usr/local/share/zsh/site-functions"
export PYTHONPATH=/Users/liuxiang/Library/Python/2.7/lib/python/site-packages
export XAPIAN_CJK_NGRAM=1
export SDKMAN_DIR="$HOME/.sdkman"
export HOMEBREW_CASK_OPTS=--caskroom=/opt/homebrew-cask/Caskroom
source $HOME/.config/private/homebrew_github_api_token.sh

[ -n "$EMACS" ] && source $HOME/.zprofile
