typeset -U path
# export PATH=/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin
path+=($HOME/.local/bin)
path+=($HOME/.stack/snapshots/x86_64-osx/lts-12.0/8.4.3/bin)
path+=($HOME/.stack/compiler-tools/x86_64-osx/ghc-8.4.3/bin)
path+=($HOME/.stack/programs/x86_64-osx/ghc-8.4.3/bin)
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
export GOPATH=$HOME/.go:$HOME/cascode/go
if { uname | grep -q Linux; } && [ -e $HOME/liuxiang ] ; then
    export ZDOTDIR=$HOME/liuxiang
    # export PYTHONPATH=$HOME/.local/lib/python2.7/site-packages
    export FPATH="$FPATH:$HOME/liuxiang/local/share/zsh/site-functions"
    [ -n "$EMACS" ] && source $HOME/liuxiang/.zprofile
else
    # export PYTHONPATH=$HOME/Library/Python/2.7/lib/python/site-packages
    export FPATH="$FPATH:/usr/local/share/zsh/site-functions"
    [ -n "$EMACS" ] && source $HOME/.zprofile
    source $HOME/.config/private/homebrew_github_api_token.sh
    export JAVA_HOME=/Library/Java/JavaVirtualMachines/CurrentJDK/Contents/Home
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
export PERL_MB_OPT="--install_base \"/Users/liuxiang/perl5\""
export PERL_MM_OPT="INSTALL_BASE=/Users/liuxiang/perl5"
export PERL5LIB=$HOME/perl5/lib/perl5
export NULLCMD=:
export HOMEBREW_BOTTLE_DOMAIN=https://mirrors.ustc.edu.cn/homebrew-bottles
export VISUAL=${ZDOTDIR:-$HOME}/bin/emacsclient-for-visual
source $HOME/.secretenv
export LESSOPEN="|/usr/local/bin/lesspipe.sh %s" LESS_ADVANCED_PREPROCESSOR=1
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
export BAT_CONFIG_PATH=$HOME/.config/.batrc
