alias -- +x='chmod +x '
alias -- '--=git checkout -'
alias l1='ls -1'
alias lA='ls -la'
alias LA='ls -la'
alias cdg='cd ~liuxiang/git-umeng'
alias guse='rvm gemset use'
alias gemls='rvm gemset list'
if [ "$(uname)" = Darwin ]; then
  alias ls="ls -G"
else
  alias ls="ls --color"
fi
alias grep="grep --color=auto"
alias ggrep="ggrep --color=auto"
alias tomcat="$HOMEBREW_PREFIX/opt/tomcat@7/bin/catalina run"
alias tomcat7="$HOMEBREW_PREFIX/opt/tomcat@7/bin/catalina run"
alias tomcat8="$HOMEBREW_PREFIX/opt/tomcat@8/bin/catalina run"
alias s="screen -e^jj"
alias -g ODX='| od -Ad -tx1'
alias -g ODC='| od -Ad -tc'
alias -s tgz='tar -zxf'
alias -s tar.gz='tar -zxf'
alias rvmdefault='rvm use default'
alias k='kill'

alias gls='gls --color --quoting-style=literal '
alias lf='gls --color --quoting-style=literal -ldU '
alias pn='print -l'
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
alias lv=lnav
alias bslist='brew services list'
alias bsstart='brew services start'
alias bsstop='brew services stop'
alias blist='brew list'
alias bag='brew list | ag '
alias bsearch='brew search'
alias binstall='brew install'
alias binfo='brew info'
alias vg=vagrant
alias pc='pc.sh'
alias kc=kubectl
alias mk=minikube
alias f=fd
alias shu=tree
alias rm=trash
alias mm=trash
alias sleep-wake-log="pmset -g log | grep -e ' Sleep  ' -e ' Wake  ' | ag --passthru ' Wake  '"
alias re=rexe
alias nuc='nu -c'
alias gu=gitui
alias lwl='ls | wc -l'
alias wlwl='watch -n1 "ls | wc -l"'
alias cdm='cd "$(tmux display-message -p "#{pane_current_path}")"'
