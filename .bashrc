#!/bin/bash

if [ -z "$HOMEBREW_PREFIX" ]; then
  if [ -e "/opt/homebrew/bin/brew" ]; then
    HOMEBREW_PREFIX="/opt/homebrew"
  else
    HOMEBREW_PREFIX="/usr/local"
  fi
fi

ulimit -n 200000
ulimit -u 2128

export PATH=$HOMEBREW_PREFIX/bin:$PATH

if [ -f $(brew --prefix)/etc/bash_completion ]; then
    . $(brew --prefix)/etc/bash_completion
fi

[[ -s `brew --prefix`/etc/autojump.sh ]] && . `brew --prefix`/etc/autojump.sh


export EDITOR=vim
export LESS=i
export HISTTIMEFORMAT='[%F %T] '
export HISTSIZE=100000
export HISTFILESIZE=100000
alias s="screen -e^jj"
alias tmdir='mkdir $(date "+%F %T" | perl -pe "s#\\D##g;s#.{6}\$#.\$&#")'
alias hibernateon='pmset -a hibernatemode 5'
alias hibernateoff='pmset -a hibernatemode 3'
alias vih="sudo vim /etc/hosts"
alias pc="pc.sh"
alias g=git
alias rspec='rspec -I. -fd --color'
export PERLLIB="/Users/liuxiang/casecode/mygithub/cliutils"
case $TERM in
    screen*)
       # This is the escape sequence ESC k \w ESC
       # Use current dir as the title
       SCREENTITLE='\[\ek\W\e\\\]'
       PS1="${SCREENTITLE}${PS1}"
       ;;
    *)
       ;;
esac

_up()
{
    pwd=$PWD/
    pwd=$(echo -n $pwd | perl -pe '
$i=0;
s#/#//#g;
$_ = reverse;
$i++ while s#//#reverse "{${i}}/"#e;
$_=reverse;
')
    COMPREPLY=($pwd " ")
}

function up() {
    if [ "$#" -eq 0 ]
    then
  count=2
    elif [ "$#" -eq 1 ] && echo $1 | grep -q '^[0-9]\+$'
    then
  count=$1
    else
  echo 'up <count>' >&2
  return
    fi
    for ((i = 0; i < count; i++))
    do
  dir=$dir"../"
    done
    dir=${dir:=.}
    cd $dir
    unset dir
}


complete -o nospace -F _up up a

alias a='up 1'
alias aa='up'
alias aaa='up 3'
alias aaaa='up 4'
alias aaaaa='up 5'
alias i2=iterm2
bind 'set match-hidden-files off'

# Don't define aliases in plain Bourne shell
[ -n "${BASH_VERSION}${KSH_VERSION}${ZSH_VERSION}" ] || return 0
alias mcd='. /opt/local/libexec/mc/mc-wrapper.sh'
alias jr=jruby
export JRUBY_HOME=/usr/lib/jruby
export PAGER='less -R'
export RI='-f ansi'
### chsdir start ###
#. $HOME/bin/chs_completion
#export CHSDIR="{'n':'l'}"
### chsdir finish. ###

function _update_ps1()
{
     export PS1="$(~/.powerline-bash.py $?)"
     echo -ne "\033]0;${PWD/$HOME/~}\007"
}

# if [ $TERM = xterm-256color ]
# then
#   export PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND ;} _update_ps1"
# else
#   export PS1='\[[01;32m\]\u@MacBookPro: \w $ \[[00m\]'
# fi

# export PS1=$'\e[01;32m'"➜  $(rvm-prompt | gsed 's/ruby-//')"$'\e[0m \e[01;31m'"\\w"$'\e[0m '

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"
if [ -f /opt/local/etc/bash_completion ]
then
    . /opt/local/etc/bash_completion
fi
[[ -r "$rvm_path/scripts/completion" ]] && . "$rvm_path/scripts/completion"


export ORACLE_HOME='/usr/local/instantclient'
# export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$ORACLE_HOME
# export DYLD_LIBRARY_PATH=$LD_LIBRARY_PATH

source ~liuxiang/bin/completion-ruby/completion-ruby-all

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/Users/liuxiang/.sdkman"
[[ -s "/Users/liuxiang/.sdkman/bin/sdkman-init.sh" ]] && source "/Users/liuxiang/.sdkman/bin/sdkman-init.sh"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

export STARSHIP_CONFIG=$HOME/.config/starship.bash.toml

eval "$(starship init bash)"
export BASH_SILENCE_DEPRECATION_WARNING=1

function poe() {
  if [ "$1" = "active" -o "$1" = "a" -o "$1" = use ]; then
    source $(poetry env info --path)/bin/activate
  elif [ "$1" = "deactive" -o "$1" = "d" -o "$1" = unuse ]; then
    deactivate
  else
    poetry "$@"
  fi
}

function poea() {
  poe active
}


