alias -- +x='chmod +x '
alias -- '--=git checkout -'
alias l1='ls -1'
alias lA='ls -la'
alias LA='ls -la'
if [ "$(uname)" = Darwin ]; then
  alias ls="ls -G"
else
  alias ls="ls --color"
fi
alias -g ODX='| od -Ad -tx1'
alias -g ODC='| od -Ad -tc'
alias -s tgz='tar -zxf'
alias -s tar.gz='tar -zxf'
alias k='kill'

alias gls='gls --color --quoting-style=literal '
alias lf='gls --color --quoting-style=literal -ldU '
alias pn='print -l'
alias rspec='rspec -I. -fd --color'
alias lv=lnav
alias pc='pc.sh'
alias kc=kubectl
alias mk=minikube
alias f=fd
alias rm=trash
alias mm=trash
alias sleep-wake-log="pmset -g log | grep -e ' Sleep  ' -e ' \(Dark\)\?Wake  ' | ag --passthru ' Wake  '"
alias re=rexe
alias nuc='nu -c'
alias lwl='ls | wc -l'
alias wlwl='LC_CTYPE=en_US.UTF-8 viddy -n 1 "ls | wc -l"'
alias cdm='cd "$(tmux display-message -p "#{pane_current_path}")"'
alias groovysh='JAVA_OPTS=-Djava.awt.headless=true groovysh'
alias gsh='JAVA_OPTS=-Djava.awt.headless=true groovysh'
alias ports='sudo lsof -Pn -iTCP -sTCP:LISTEN | tee >(head -1)'
alias vi=nvim
alias vim=nvim
alias view='nvim -R'
alias dev=./bin/dev
alias ❯=''
alias html2pdf=wkhtmltopdf
alias nv=open-with-neovide.sh
alias cs='cargo search --registry=crates-io'
alias lzd=lazydocker
alias p=pbpaste
alias -g L="| view"
alias -g M='| nvim +Man!'
alias -g V="| view"
alias -g C='| wc -l'
alias -g LL="2>&1 | view"
alias -g G='| rg'
alias -g J='| jless'
alias -g P='| pbcopy'
alias pp='pbpaste | view'
alias pj='pbpaste | jless'
alias yless="jless --yaml"
alias claude!='clear; claude --dangerously-skip-permissions'
alias cl='clear; claude --dangerously-skip-permissions'
alias cursor='env -u TMUX -u TMUX_PANE command cursor'
alias mdui=ekphos
alias oc=opencode
