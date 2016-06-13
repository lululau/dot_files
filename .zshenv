export PATH=/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin
export PATH=$PATH:$HOME/cascode/github.com/xiki/bin
export PATH="$PATH:$HOME/.fzf/bin"
export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
export MANPATH="$MANPATH:$HOME/.fzf/man"
export FPATH="$FPATH:/usr/local/share/zsh/site-functions"
export XAPIAN_CJK_NGRAM=1
export SDKMAN_DIR="$HOME/.sdkman"
source $HOME/.config/private/homebrew_github_api_token.sh

[ -n "$EMACS" ] && source $HOME/.zprofile
