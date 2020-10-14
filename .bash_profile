#!/bin/bash

# Mark sure '/usr/local/bin' have higher priority than '/usr/bin'
PATH=${PATH/\/usr\/bin/swap_usr_local_bin_and_user_bin}
PATH=${PATH/\/usr\/local\/bin/\/usr\/bin}
PATH=${PATH/swap_usr_local_bin_and_user_bin/\/usr\/local\/bin}

alias cdg='cd ~liuxiang/git-umeng'
alias e='emacs -q'
alias guse='rvm gemset use'
alias gls='rvm gemset list'
alias ls="ls -G"
alias go="open"
alias grep="grep --color=auto"
alias ggrep="ggrep --color=auto"
export GREP_COLOR=$'\e[43;30'
alias tomcat="/usr/lib/tomcat/bin/catalina.sh run"
# export PS1="\u@MacBookPro: \w $ "
alias tree="tree -AC"
alias s="screen -e^jj"
export CLICOLOR=1
export LSCOLORS=ExFxCxDxBxegedabagacad
export CLASSPATH=.:/usr/local/javalib/mysql-jdbc.jar:/usr/local/javalib/sqlite3-jdbc.jar
export LANG=zh_CN.UTF-8
#export MAGICK_HOME=/Applications/ImageMagick
export MAGICK_HOME=/opt/local
#PATH=/Users/liuxiang/casecode/mygithub/rakudo/parrot_install/bin:$PATH:$MAGICK_HOME/bin:/usr/local/mysql/bin
PATH=/Users/liuxiang/Applications/Postgres.app/Contents/MacOS/bin:/Users/liuxiang/casecode/mygithub/rakudo/parrot_install/bin:$PATH:/usr/local/mysql/bin
# export DYLD_LIBRARY_PATH=$DYLD_LIBRARY_PATH:/$MAGICK_HOME/lib
# export DYLD_LIBRARY_PATH=$DYLD_LIBRARY_PATH
export GOROOT=$HOME/go
export GOOS=darwin
export GOARCH=amd64
export GOBIN=$HOME/gobin


##
# Your previous /Users/liuxiang/.bash_profile file was backed up as /Users/liuxiang/.bash_profile.macports-saved_2009-10-18_at_17:38:50
##

# MacPorts Installer addition on 2009-10-18_at_17:38:50: adding an appropriate PATH variable for use with MacPorts.
export GROOVY_HOME=/usr/lib/groovy
export PATH=$PATH:/opt/local/bin:/opt/local/sbin:${GROOVY_HOME}/bin:$GOBIN
# Finished adapting your PATH environment variable for use with MacPorts.


##
# Your previous /Users/liuxiang/.bash_profile file was backed up as /Users/liuxiang/.bash_profile.macports-saved_2010-01-28_at_23:57:57
##

# MacPorts Installer addition on 2010-01-28_at_23:57:57: adding an appropriate PATH variable for use with MacPorts.
# Finished adapting your PATH environment variable for use with MacPorts.


##
# Your previous /Users/liuxiang/.bash_profile file was backed up as /Users/liuxiang/.bash_profile.macports-saved_2010-03-09_at_23:56:34
##

# MacPorts Installer addition on 2010-03-09_at_23:56:34: adding an appropriate PATH variable for use with MacPorts.

# Finished adapting your PATH environment variable for use with MacPorts.

#if [ -f /opt/local/etc/bash_completion ]
#then
#   . /opt/local/etc/bash_completion
#fi


export LESS_TERMCAP_mb=$'\E[05;34m'       # begin blinking
export LESS_TERMCAP_md=$'\E[01;34m'       # begin bold
export LESS_TERMCAP_me=$'\E[0m'           # end mode
export LESS_TERMCAP_se=$'\E[0m'           # end standout-mode
export LESS_TERMCAP_so=$'\E[44;33m'       # begin standout-mode
export LESS_TERMCAP_ue=$'\E[0m'           # end underline
export LESS_TERMCAP_us=$'\E[04;33m'       # begin underline

export PATH=$PATH:$JRUBY_HOME/bin:/usr/local/texlive/2010/bin/x86_64-darwin
PATH=$PATH:$HOME/bin
PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
. ~/.bashrc

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/Users/liuxiang/.sdkman"
[[ -s "/Users/liuxiang/.sdkman/bin/sdkman-init.sh" ]] && source "/Users/liuxiang/.sdkman/bin/sdkman-init.sh"

export EA_EDITOR='/usr/local/bin/emacsclient -a "" -c'
complete -C /usr/local/bin/bitcomplete bit
