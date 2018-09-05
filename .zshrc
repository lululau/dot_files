# Path to your oh-my-zsh configuration.
if { uname | grep -q Linux; } && [ -e $HOME/liuxiang ] ; then
    ZSH=$HOME/liuxiang/.oh-my-zsh
    export CONFIGDIR=$HOME/liuxiang/.config
else
    ZSH=$HOME/.oh-my-zsh
    export CONFIGDIR=$HOME/.config
fi

source $CONFIGDIR/.zsh-aliases.zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="liuxiang"

# Set to this to use case-sensitive completion
#CASE_SENSITIVE="true"

# Comment this out to disable bi-weekly auto-update checks
DISABLE_AUTO_UPDATE="true"

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

# Which plugins would you like to load? (plugins can be found in
# ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
autoload -U compinit; compinit
autoload -U zmv
# plugins=(git autojump battery colorize colored-man command-not-found compleat cp
#          cpanm encode64 gem github gnu-utils go golang history jruby macports
#          mvn mysql-macports node npm osx perl pip python rails rake rsync ruby
#          rvm safe-paste scala screen svn terminalapp terminitor textmate themes
#          bundler httpie ack2 funcfind gemcd alibas vagrant tmux)

plugins=(ack2 alibas autojump autopair
         bd brew bundler
         colored-man colorize compleat cp cpanm common-aliases copybuffer
         docker docker-compose docker-machine
         encode64 emoji
         funcfind
         gem gemcd git github go golang gradle
         history httpie
         jruby
         lein
         mvn
         node npm nvm
         osx
         perl pip python
         rails rake rsync ruby rvm
         safe-paste sbt scala screen svn systemadmin systemd
         terminalapp terminitor themes tig tmux tmux-pane-words tmuxinator
         vagrant virtualenv
         xcode
         yum
         project-root
         zsh-autosuggestions zsh-brew-services zsh_reload zsh-completions)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...

unalias ping
unalias fd
unalias rb

# Linux Specific Config
if uname | grep -q Linux; then
    if [ -e $HOME/liuxiang/.zshrc ]; then
        export SHELL=/home/deploy/liuxiang/local/bin/zsh
        alias tmux='tmux -L liuxiang -f /home/deploy/liuxiang/.tmux.conf'
        alias vim="vim -u ~/liuxiang/.vimrc"
        export SCREENRC=$HOME/liuxiang/.screenrc
    fi
    alias ta='tmux attach -t'
    alias tad='tmux attach -d -t'
    alias ts='tmux new-session -s'
    alias tl='tmux list-sessions'
    alias tksv='tmux kill-server'
    alias tkss='tmux kill-session -t'
    alias e='emacsclient -t'
    alias gls=ls
    alias gsed=sed
fi

bindkey '^Xk' autosuggest-clear
bindkey '^X^k' autosuggest-clear
bindkey "^X^X" vi-cmd-mode

compdef _ack2_completion ack

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

stty -ixon -ixoff


function omz_termsupport_preexec () {
  emulate -L zsh
  setopt extended_glob
  local CMD=${1[(wr)^(*=*|sudo|ssh|rake|-*)]:gs/%/%%}
  local LINE="${2:gs/%/%%}"
  title $ZSH_THEME_TERM_TAB_TITLE_IDLE $ZSH_THEME_TERM_TITLE_IDLE
}


function jj() {
  cd "$({dirs -pl; j -s | sed -n '/^_______/!p; /^_______/q'  | cut -d$'\t' -f2; } | fzf)"
}

# function rm() {
#   echo '~~~~ Use mm instead in interactvie shell!  ~~~~'
#   echo
#   mm --help
# }

set -o interactivecomments
function gi() { curl -L -s https://www.gitignore.io/api/$@ ;}

autoload -U perl-subs
zle -N perl-subs
bindkey '^xs' perl-subs

source $HOME/.space.zsh

source $ZSH/functions/zce.zsh

export FZF_TMUX=1
export FZF_TMUX_HEIGHT=40%
export FZF_DEFAULT_OPTS="-x -m --history=$HOME/.fzf_history --history-size=10000 --bind 'ctrl-n:down,ctrl-p:up,alt-n:next-history,alt-p:previous-history,ctrl-l:jump,alt-a:select-all,ctrl-alt-j:half-page-down,ctrl-alt-k:half-page-up,alt-j:page-down,alt-k:page-up',ctrl-t:top"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# function command_not_found_handler() {
#   if echo -n "$1" | grep -q '^-[0-9]\{1,2\}$'; then
#     num=$1
#     shift
#     CLICOLOR_FORCE= ls -l "$@" | tail $num
#   elif echo -n "$1" | grep -q '^[0-9]\{1,2\}$'; then
#     num=$1
#     shift
#     CLICOLOR_FORCE= ls -l "$@" | head -$num
#   elif [ $# -eq 1  ]; then
#     if echo $1 | grep -q  '^[0-9]\{6\}$'; then
#       alibas Unode221-boss.et2 "$1"
#     elif echo $1 | grep -q  '^[0-9]\{7\}$'; then
#       alibas web"${1:0:1}" "${1:1}"
#     elif echo $1 | grep -q  '^[0-9]\{8\}$'; then
#       if [ "${1:0:1}" = w ]; then
#           alibas web"${1:1:1}" "${1:2}"
#       elif [ "${1:0:1}" = j ]; then
#           alibas job"${1:1:1}" "${1:2}"
#       elif [ "${1:0:1}" = d ]; then
#           alibas db"${1:1:1}" "${1:2}"
#       fi
#     elif echo "$1" | grep -q '^ '; then
#       ruby_exp=$(echo "$1" | perl -e '$x = join("", <>); $x =~ s/#(?!{)(.*?)#(?!{)/;system(%{$1});/gs; print $x')
#       ruby -e "$ruby_exp"
#     else
#         return 127
#     fi
#   else
#     return 127
#   fi
# }

precmd() {
  pwd > /tmp/iterm2_pwd
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
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=8'
export ZSH_THEME_TERM_TAB_TITLE_IDLE="%20<..<%~%<<" #20 char left truncated PWD

# for shell-pop
if [ -n "$EMACS" ]
then
  export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=5'
  alias ag='ag --color-match=33'
  chpwd() { print -P "\033AnSiTc %d" }
  print -P "\033AnSiTu %n"
  print -P "\033AnSiTc %d"
  source $HOME/.zlogin
fi

# source ~/.xsh

if [ -z "$EMACS" ]; then
  if { uname | grep -q Linux; } && [ -e $HOME/liuxiang ] ; then
    source $HOME/liuxiang/.iterm2_shell_integration.zsh
  else
    source ~/.iterm2_shell_integration.zsh
  fi
fi

# source ~/.profile
