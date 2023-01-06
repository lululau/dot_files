typeset -U path
# export PATH=/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin
path+=($HOME/.local/bin)
path+=$(echo ~/.stack/programs/*/*/bin(NOn[1]))
path+=($HOME/.cargo/bin)
path+=(/usr/local/bin /usr/local/sbin /usr/bin /bin /usr/sbin /sbin)
# export PATH=$PATH:$HOME/cascode/github.com/xiki/bin:/usr/local/sbin
path+=($HOME/cascode/github.com/xiki/bin /usr/local/sbin)
# export PATH="$PATH:$HOME/.fzf/bin"
path+=($HOME/.fzf/bin)
# export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
path+=($HOME/.rvm/bin) # Add RVM to PATH for scripting
path+=($HOME/perl5/bin)

if uname | grep -q Linux ; then
    export EDITOR='emacsclient -t'
else
    export EDITOR='emacsclient'
fi
export GOROOT=/usr/local/opt/go/libexec/
export GOPATH=$HOME/.go
export GO111MODULE=auto
export GOPROXY=https://goproxy.cn,direct
if { uname | grep -q Linux; } && [ -e $HOME/liuxiang ] ; then
    export ZDOTDIR=$HOME/liuxiang
    # export PYTHONPATH=$HOME/.local/lib/python2.7/site-packages
    export FPATH="$FPATH:$HOME/liuxiang/local/share/zsh/site-functions"
    [ -n "$INSIDE_EMACS" ] && source $HOME/liuxiang/.zprofile
else
    # export PYTHONPATH=$HOME/Library/Python/2.7/lib/python/site-packages
    export FPATH="$FPATH:/usr/local/share/zsh/site-functions"
    [ -n "$EMACS" ] && source $HOME/.zprofile
    [ -e $HOME/.config/private/homebrew_github_api_token.sh ] && source $HOME/.config/private/homebrew_github_api_token.sh
    # export JAVA_HOME=/Library/Java/JavaVirtualMachines/CurrentJDK/Contents/Home
fi
export XAPIAN_CJK_NGRAM=1
export SDKMAN_DIR="$HOME/.sdkman"
# export HOMEBREW_CASK_OPTS=--caskroom=/opt/homebrew-cask/Caskroom
path+=($GOROOT/bin $HOME/.go/bin)
export FZF_TMUX=0
export PYSPARK_DRIVER_PYTHON=ipython

export GREP_COLOR=$'\e[43;30'
export AUTOJUMP_KEEP_SYMLINKS=1
export CLICOLOR=1
export LANG=zh_CN.UTF-8
export MAGICK_HOME=/opt/local
export GROOVY_HOME=/usr/lib/groovy
export TERM2NARROW=false
export LESS_TERMCAP_mb=$'\E[05;34m'       # begin blinking
export LESS_TERMCAP_md=$'\E[01;34m'       # begin bold
export LESS_TERMCAP_me=$'\E[0m'           # end mode
export LESS_TERMCAP_se=$'\E[0m'           # end standout-mode
export LESS_TERMCAP_so=$'\E[44;33m'       # begin standout-mode
export LESS_TERMCAP_ue=$'\E[0m'           # end underline
export LESS_TERMCAP_us=$'\E[04;33m'       # begin underline
export PAGER='less -R'
export RI='-f ansi'
export LSCOLORS=exfxcxdxcxegedabagacad
export LS_COLORS='di=01;36'
export PERL_MB_OPT="--install_base \"$HOME/perl5\""
export PERL_MM_OPT="INSTALL_BASE=$HOME/perl5"
export PERL5LIB=$HOME/perl5/lib/perl5
export NULLCMD=:
# export HOMEBREW_BOTTLE_DOMAIN=https://mirrors.ustc.edu.cn/homebrew-bottles
# export HOMEBREW_BOTTLE_DOMAIN=https://mirrors.tuna.tsinghua.edu.cn/homebrew-bottles
export HOMEBREW_BAT=1
export VISUAL=${ZDOTDIR:-$HOME}/bin/emacsclient-for-visual
[[ -e $HOME/.secretenv ]] && source $HOME/.secretenv
export LESSOPEN="|/usr/local/bin/lesspipe.sh %s" LESS_ADVANCED_PREPROCESSOR=1
export BAT_CONFIG_PATH=$HOME/.config/.batrc
export RUST_SRC_PATH=/usr/local/Cellar/rust/1.38.0/share/rust/rust_src
export RUSTUP_DIST_SERVER=https://mirrors.tuna.tsinghua.edu.cn/rustup
export curl_ca_bundle=$HOME/.mitmproxy/mitmproxy-ca.pem
export RUBY_YJIT_ENABLE=true
