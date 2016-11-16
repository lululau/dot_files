# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

export AUTOJUMP_KEEP_SYMLINKS=1

alias -- +x='chmod +x '
alias -- '--=git checkout -'
alias l1='ls -1'
alias cdg='cd ~liuxiang/git-umeng'
alias guse='rvm gemset use'
alias gemls='rvm gemset list'
alias ls="ls -G"
# alias go="open"
alias grep="grep --color=auto"
alias ggrep="ggrep --color=auto"
export GREP_COLOR=$'\e[43;30'
alias tomcat="/usr/lib/tomcat/bin/catalina.sh run"
alias tree="tree -AC"
alias s="screen -e^jj"
alias odx='od -Ad -tx1'
alias odc='od -Ad -tc'
alias rvmdefault='rvm use default'
export CLICOLOR=1
# export LSCOLORS=ExFxCxDxBxegedabagacad
export LANG=zh_CN.UTF-8
export MAGICK_HOME=/opt/local
# export GOOS=darwin
# export GOARCH=amd64
# export GOBIN=$HOME/gobin
export GROOVY_HOME=/usr/lib/groovy
export TERM2NARROW=false



export LESS_TERMCAP_mb=$'\E[05;34m'       # begin blinking
export LESS_TERMCAP_md=$'\E[01;34m'       # begin bold
export LESS_TERMCAP_me=$'\E[0m'           # end mode
export LESS_TERMCAP_se=$'\E[0m'           # end standout-mode
export LESS_TERMCAP_so=$'\E[44;33m'       # begin standout-mode
export LESS_TERMCAP_ue=$'\E[0m'           # end underline
export LESS_TERMCAP_us=$'\E[04;33m'       # begin underline



# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="liuxiang"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
alias a='cd ..'
alias aa='cd ../../'
alias aaa='cd ../../../'
alias aaaa='cd ../../../../'
alias aaaaa='cd ../../../../../'
alias aaaaaa='cd ../../../../../../'
alias aaaaaaa='cd ../../../../../../../'
alias aaaaaaaa='cd ../../../../../../../../'
alias aaaaaaaaa='cd ../../../../../../../../../'
alias rspec='rspec -I. -fd --color'
alias vih="sudo vim /etc/hosts"
export PAGER='less -R'
export RI='-f ansi'

# Set to this to use case-sensitive completion
#CASE_SENSITIVE="true"

# Comment this out to disable bi-weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment to change how often before auto-updates occur? (in days)
# export UPDATE_ZSH_DAYS=13

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want to disable command autocorrection
# DISABLE_CORRECTION="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Uncomment following line if you want to disable marking untracked files under
# VCS as dirty. This makes repository status check for large repositories much,
# much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
autoload -U compinit; compinit
plugins=(git autojump battery colorize colored-man command-not-found compleat cp cpanm encode64 gem github gnu-utils go golang history jruby macports mvn mysql-macports node npm osx perl pip python rails rake rsync ruby rvm safe-paste scala screen svn terminalapp terminitor textmate themes bundler httpie ack2 funcfind gemcd alibas vagrant)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...

compdef _ack2_completion ack

function emacs() {
  /usr/local/bin/emacs "$@"
  echo $'\033]50;CursorShape=0\007'
}

function powerline_precmd() {
  export PS1="$(~/.powerline-shell.py $? --shell zsh 2> /dev/null)"
}

function install_powerline_precmd() {
  for s in "${precmd_functions[@]}"; do
    if [ "$s" = "powerline_precmd" ]; then
      return
    fi
  done
  precmd_functions+=(powerline_precmd)
}

function zz() {
  if [ "$TERM2NARROW" = 'true' ]; then
    TERM2NARROW=false
  else
    TERM2NARROW=true
  fi
}

# install_powerline_precmd

alias pc='pc.sh'
stty -ixon -ixoff


function omz_termsupport_preexec () {
  emulate -L zsh
  setopt extended_glob
  local CMD=${1[(wr)^(*=*|sudo|ssh|rake|-*)]:gs/%/%%}
  local LINE="${2:gs/%/%%}"
  title $ZSH_THEME_TERM_TAB_TITLE_IDLE $ZSH_THEME_TERM_TITLE_IDLE
}


function jj() {
  cd "$({dirs -pl; j -s | gsed -n '/^_______/!p; /^_______/q'  | cut -d$'\t' -f2; } | fzf)"
}

export LSCOLORS=exfxcxdxcxegedabagacad

PERL_MB_OPT="--install_base \"/Users/liuxiang/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/Users/liuxiang/perl5"; export PERL_MM_OPT;
export PERL5LIB=/Users/liuxiang/perl5/lib/perl5/

# function rm() {
#   echo '~~~~ Use mm instead in interactvie shell!  ~~~~'
#   echo
#   mm --help
# }

set -o interactivecomments
export NULLCMD=:
function gi() { curl -L -s https://www.gitignore.io/api/$@ ;}
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
export FZF_DEFAULT_OPTS='-x -m'

function command_not_found_handler() {
  if echo -n "$1" | grep -q '^-[0-9]\{1,2\}$'; then
    num=$1
    shift
    CLICOLOR_FORCE= ls -l "$@" | tail $num
  elif echo -n "$1" | grep -q '^[0-9]\{1,2\}$'; then
    num=$1
    shift
    CLICOLOR_FORCE= ls -l "$@" | head -$num
  elif [ $# -eq 1  ]; then
    if echo $1 | grep -q  '^[0-9]\{6\}$'; then
      alibas Unode221-boss.et2 "$1"
    elif echo $1 | grep -q  '^[0-9]\{7\}$'; then
      alibas web"${1:0:1}" "${1:1}"
    elif echo $1 | grep -q  '^[0-9]\{8\}$'; then
      if [ "${1:0:1}" = w ]; then
          alibas web"${1:1:1}" "${1:2}"
      elif [ "${1:0:1}" = j ]; then
          alibas job"${1:1:1}" "${1:2}"
      elif [ "${1:0:1}" = d ]; then
          alibas db"${1:1:1}" "${1:2}"
      fi
    elif echo "$1" | grep -q '^ '; then
      ruby_exp=$(echo "$1" | perl -e '$x = join("", <>); $x =~ s/#(?!{)(.*?)#(?!{)/;system(%{$1});/gs; print $x')
      ruby -e "$ruby_exp"
    else
        return 127
    fi
  else
    return 127
  fi
}

if [[ "$TERM" == "dumb" ]]
then
    unsetopt zle
    unsetopt prompt_cr
    unsetopt prompt_subst
    unfunction precmd
    unfunction preexec
    PS1='$ '
fi

# for shell-pop
if [ -n "$EMACS" ]
then
  chpwd() { print -P "\033AnSiTc %d" }
  print -P "\033AnSiTu %n"
  print -P "\033AnSiTc %d"
  source $HOME/.zlogin
fi

# export HOMEBREW_BOTTLE_DOMAIN=https://mirrors.ustc.edu.cn/homebrew-bottles

# source ~/.xsh

source /Users/liuxiang/.iterm2_shell_integration.zsh

# source ~/.profile
