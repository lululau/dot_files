[[ `uname` == Darwin ]] && ulimit -n 200000
ulimit -u 2128

setopt +o nomatch

[[ $TERM == "tramp" ]] && unsetopt zle && PS1='$ ' && return

# Path to your oh-my-zsh configuration.
if { uname | grep -q Linux; } && [ -e $HOME/liuxiang ] ; then
    ZSH=$HOME/liuxiang/.oh-my-zsh
    export CONFIGDIR=$HOME/liuxiang/.config
else
    ZSH=$HOME/.oh-my-zsh
    export CONFIGDIR=$HOME/.config
fi

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
# ZSH_THEME="liuxiang"
# ZSH_THEME="spaceship"

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
# plugins=(git autojump battery colorize colored-man command-not-found compleat cp
#          cpanm encode64 gem github gnu-utils go golang history jruby macports
#          mvn mysql-macports node npm macos perl pip python rails rake rsync ruby
#          rvm safe-paste scala screen svn terminalapp terminitor textmate themes
#          bundler httpie ack2 funcfind gemcd alibas vagrant tmux)

UNBUNDLED_COMMANDS=(rubocop)

plugins=(ack2 alibas autojump autopair
         bd brew bundler
         colored-man-pages colorize compleat cp cpanm common-aliases copybuffer
         docker docker-compose docker-machine zsh-docker-aliases
         encode64 emoji
         funcfind
         gem gemcd git github golang gradle
         history httpie
         jruby
         lein
         mvn
         node npm nvm
         # macos
         perl pip python
         rails rake rsync ruby rvm
         safe-paste sbt scala screen svn systemadmin systemd
         terminitor themes tig tmux tmux-pane-words tmuxinator
         vagrant virtualenv
         xcode
         yum
         project-root
         kubectl
         minikube
         rust
         zsh-autosuggestions zsh-brew-services zsh-completions
         you-should-use)

[ -z "$INSIDE_EMACS" ] && plugins+=(fast-syntax-highlighting)

source $ZSH/oh-my-zsh.sh

autoload -U compinit; compinit -d
autoload -U zmv

# Customize to your needs...

[ -e $CONFIGDIR/.zsh-aliases.zsh ] && source $CONFIGDIR/.zsh-aliases.zsh || source $HOME/.zsh-aliases.zsh
unalias ping
# unalias fd
unalias vd
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
if [ -n "$INSIDE_EMACS" ]
   export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=102'
then
  # alias ag='ag --color-match=33'
  # chpwd() { print -P "\033AnSiTc %d" }
  # print -P "\033AnSiTu %n"
  # print -P "\033AnSiTc %d"
  source $HOME/.zlogin
fi

# source ~/.xsh

if [ -z "$INSIDE_EMACS" ]; then
  if { uname | grep -q Linux; } && [ -e $HOME/liuxiang ] ; then
    source $HOME/liuxiang/.iterm2_shell_integration.zsh
  else
    source ~/.iterm2_shell_integration.zsh
  fi
fi

function sat() {
  lines=$[$(tput lines) - 4]
  cat $@ | head -n $lines
}

function hat() {
  lines=$[$(tput lines)/2 - 2]
  cat $@ | head -n $lines
}

function px() {
  if [ "$1" = on ]; then
    export http_proxy=http://127.0.0.1:1087;export https_proxy=http://127.0.0.1:1087;
    echo "http_proxy=$http_proxy\nhttps_proxy=$https_proxy"
  elif [ "$1" = off ]; then
    unset http_proxy; unset https_proxy
    echo "http_proxy=$http_proxy\nhttps_proxy=$https_proxy"
  elif [ "$1" =~ '^[0-9]+$' ]; then
    export http_proxy=http://127.0.0.1:$1;export https_proxy=http://127.0.0.1:$1;
    echo "http_proxy=$http_proxy\nhttps_proxy=$https_proxy"
  elif [ "$2" =~ '^[0-9]+$' ]; then
    export http_proxy=http://$1:$2;export https_proxy=http://$1:$2;
    echo "http_proxy=$http_proxy\nhttps_proxy=$https_proxy"
  else
    echo "http_proxy=$http_proxy\nhttps_proxy=$https_proxy"
  fi
}

source $HOME/.oh-my-zsh/custom/plugins/zsh-histdb/sqlite-history.zsh
autoload -Uz add-zsh-hook
add-zsh-hook precmd histdb-update-outcome
# source $HOME/.oh-my-zsh/custom/plugins/zsh-histdb/histdb-interactive.zsh
# bindkey '^r' _histdb-isearch

_zsh_autosuggest_strategy_histdb_top() {
  local query="PRAGMA case_sensitive_like = 1; select commands.argv from
history left join commands on history.command_id = commands.rowid
left join places on history.place_id = places.rowid
where commands.argv LIKE '$(sql_escape $1)%'
group by commands.argv
order by places.dir != '$(sql_escape $PWD)', count(*) desc limit 1"
  suggestion=$(_histdb_query "$query")
}

_zsh_autosuggest_strategy_histdb_recent() {
  local query="PRAGMA case_sensitive_like = 1; select commands.argv from
history left join commands on history.command_id = commands.rowid
left join places on history.place_id = places.rowid
where commands.argv LIKE '$(sql_escape $1)%'
order by history.start_time desc limit 1"
  suggestion=$(_histdb_query "$query")
}

ZSH_AUTOSUGGEST_STRATEGY=histdb_recent

# source ~/.profile
if [ -e ~/.jenv/bin ]; then
  export PATH="$HOME/.jenv/bin:$PATH"
  eval "$(jenv init -)"
fi

eval "$(starship init zsh)"

[ -e "$HOME/Library/Preferences/org.dystroy.broot/launcher/bash/br" ] && source $HOME/Library/Preferences/org.dystroy.broot/launcher/bash/br

[ -n "$SSH_CLIENT" ] && eval `ssh-agent` &> /dev/null

zle-keymap-select () {
  zle reset-prompt
  case $KEYMAP in
    vicmd) printf "\e[2 q";;
    viins|main) printf "\e[5 q";;
  esac
}
zle -N zle-keymap-select

vterm_printf(){
  if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
    # Tell tmux to pass the escape sequences through
    printf "\ePtmux;\e\e]%s\007\e\\" "$1"
  elif [ "${TERM%%-*}" = "screen" ]; then
    # GNU screen (screen, screen-256color, screen-256color-bce)
    printf "\eP\e]%s\007\e\\" "$1"
  else
    printf "\e]%s\e\\" "$1"
  fi
}


vterm_cmd() {
  local vterm_elisp
  vterm_elisp=""
  while [ $# -gt 0 ]; do
    vterm_elisp="$vterm_elisp""$(printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
    shift
  done
  vterm_printf "51;E$vterm_elisp"
}

vterm_set_directory() {
  vterm_cmd update-pwd "$PWD/"
}

me() {
  local file=$1
  if [ -n "$file" ]; then
    if [ "$file[1]" != "/" ]; then
      file="$PWD/$file"
    fi
  else
    file="$PWD"
  fi
  local host_name=$HOST
  vterm_cmd find-remote-file "$file" "$host_name"
}

sme() {
  local file=$1
  if [ -n "$file" ]; then
    if [ "$file[1]" != "/" ]; then
      file="$PWD/$file"
    fi
  else
    file="$PWD"
  fi
  local host_name=$HOST
  vterm_cmd sudo-find-remote-file "$file" "$host_name"
}


# if is_inside_emacs | grep -q true; then
#   # zle-keymap-select () {
#   #   starship_render
#   #   zle reset-prompt
#   #   case $KEYMAP in
#   #     vicmd) printf "\e]51;Elx/run-in-vterm/set-green-box-cursor\e\\";;
#   #     viins|main) printf "\e]51;Elx/run-in-vterm/set-blue-bar-cursor\e\\";;
#   #   esac
#   # }
# fi

autoload -U add-zsh-hook
add-zsh-hook -Uz chpwd (){ vterm_set_directory }
add-zsh-hook -Uz precmd (){ vterm_set_directory }

autoload -U +X bashcompinit && bashcompinit
complete -o nospace -C /usr/local/bin/bitcomplete bit

export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/shims:$PATH"
if type pyenv &> /dev/null; then
  eval "$(pyenv init -)"
  # eval "$(pyenv virtualenv-init -)"
fi

function set_vim_cursor_type() {
  echo "$3" | grep -q '^\(vim\|vi\|nvim\|=vim\)' && echo -ne '\e[2 q'
}

preexec_functions+=(set_vim_cursor_type)

HISTDB_VTERM_SESSION=$RANDOM

function save_history_to_vterm() {
  local cmd="${1[0, -2]}"

  for boring in "${_BORING_COMMANDS[@]}"; do
    if [[ "$cmd" =~ $boring ]]; then
      return 0
    fi
  done

  local pwd=$PWD
  local started=$(date +%s)
  local host_name=$HOST
  vterm_cmd save-zsh-history "$HISTDB_VTERM_SESSION" "$host_name" "$cmd" "$pwd" "$started"
  return 0
}

function update_history_outcome_to_vterm() {
  local retval=$?
  local finished=$(date +%s)
  local host_name=$HOST
  vterm_cmd update-zsh-history-outcome "$HISTDB_VTERM_SESSION" "$host_name" "$retval" "$finished"
}

# autoload -Uz add-zsh-hook
add-zsh-hook precmd update_history_outcome_to_vterm
zshaddhistory_functions+=(save_history_to_vterm)

function download() {
  local file=$1
  if [ "$file[1]" != "/" ]; then
    file="$PWD/$file"
  fi
  vterm_cmd download "$file"
}

function upload() {
  vterm_cmd upload "$PWD"
}

function helm-dired-history-update() {
  emacsclient -n -q --eval "(helm-dired-history--update \"$PWD\")" &> /dev/null
}

function recentf-add-file() {
  local first_arg=$1
  local arg
  for arg (${(z)first_arg}); do
    if [ -d $arg ]; then
      emacsclient -n -q --eval "(helm-dired-history--update \"${(Q)arg:a}\")" &> /dev/null
    elif [ -f "$arg" ]; then
      emacsclient -n -q --eval "(recentf-add-file \"${(Q)arg:a}\")" &> /dev/null
    fi
  done
}

chpwd_functions+=(helm-dired-history-update)
preexec_functions+=(recentf-add-file)

# textra
export TEXTRA_INSTALL="$HOME/.textra"
export PATH="$TEXTRA_INSTALL/bin:$PATH"
### Codex CLI setup - start
export CODEX_CLI_PATH=/Users/liuxiang/.cli-co-pilot
source "$CODEX_CLI_PATH/scripts/zsh_plugin.zsh"
bindkey '^G' create_completion
### Codex CLI setup - end
