[[ `uname` == Darwin ]] && ulimit -n 200000
ulimit -u 2128

export LIGHT_ZSH=true

setopt +o nomatch


# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh
export CONFIGDIR=$HOME/.config

# Comment this out to disable bi-weekly auto-update checks
DISABLE_AUTO_UPDATE="true"

UNBUNDLED_COMMANDS=(rubocop)

plugins=(autopair
         git
         safe-paste
         project-root
         zsh-autosuggestions
         zsh-completions
         poetry)

source $ZSH/oh-my-zsh.sh

autoload -U compinit; compinit -d
autoload -U zmv


[ -e $CONFIGDIR/.zsh-aliases.zsh ] && source $CONFIGDIR/.zsh-aliases.zsh || source $HOME/.zsh-aliases.zsh

bindkey '^Xk' autosuggest-clear
bindkey '^X^k' autosuggest-clear
bindkey "^X^X" vi-cmd-mode

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

set -o interactivecomments

source $HOME/.space.zsh

export FZF_TMUX=1
export FZF_TMUX_HEIGHT=40%
export FZF_DEFAULT_OPTS="-x -m --history=$HOME/.fzf_history --history-size=10000 --bind 'ctrl-n:down,ctrl-p:up,alt-n:next-history,alt-p:previous-history,ctrl-l:jump,alt-a:select-all,ctrl-alt-j:half-page-down,ctrl-alt-k:half-page-up,alt-j:page-down,alt-k:page-up',ctrl-t:top"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

precmd() {
  pwd > /tmp/iterm2_pwd
}

export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=8'
export ZSH_THEME_TERM_TAB_TITLE_IDLE="%20<..<%~%<<" #20 char left truncated PWD


if [ -n "$INSIDE_EMACS" ]; then
   export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=102'
fi

source $HOME/.oh-my-zsh/custom/plugins/zsh-histdb/sqlite-history.zsh
autoload -Uz add-zsh-hook
add-zsh-hook precmd histdb-update-outcome

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

# autoload -U +X bashcompinit && bashcompinit
# complete -o nospace -C /usr/local/bin/bitcomplete bit

export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/shims:$PATH"
if type pyenv &> /dev/null; then
  eval "$(pyenv init -)"
fi

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
