# export PATH=$HOME/ServerApps/bin:$HOME/bin:$HOME/Library/Python/2.7/bin:$PATH
if { uname | grep -q Linux; } && [ -e $HOME/liuxiang ] ; then
    _new_path=($HOME/liuxiang/bin $HOME/liuxiang/local/bin $HOME/.local/bin)
    for i in "${path[@]}"
    do
        if [ $HOME/liuxiang/bin != "$i" -a $HOME/liuxiang/local/bin != "$i" -a $HOME/.local/bin != "$i" ]; then
            _new_path+=$i
        fi
    done
else
  _new_path=($HOME/ServerApps/bin $HOME/bin $HOME/.local/bin $HOME/Library/Python/3.12/bin $HOME/Library/Python/2.7/bin)
    for i in "${path[@]}"
    do
      if [ $HOME/ServerApps/bin != "$i" -a $HOME/bin != "$i" -a $HOME/Library/Python/2.7/bin != "$i" -a $HOME/Library/Python/3.12/bin != "$i" ]; then
            _new_path+=$i
        fi
    done
fi

path=(${_new_path})

# GPG configuration
# Check for the gpg-agent socket, and set SSH_AUTH_SOCK and GPG_TTY
# environment variables accordingly:
if [ "$USER" = vagrant ]; then
    if [[ -S "${HOME}/.gnupg/S.gpg-agent.ssh" ]]; then
        export GPG_TTY=$(tty)
        export GPG_TTY
        if [[ ${SSH_AUTH_SOCK} != "${HOME}/.gnupg/S.gpg-agent.ssh" ]]; then
            export SSH_AUTH_SOCK="${HOME}/.gnupg/S.gpg-agent.ssh"
        fi
        if [ -f "${HOME}/.gpg-agent-info" ]; then
            . "${HOME}/.gpg-agent-info"
            export GPG_AGENT_INFO
        fi
    fi
fi

# Added by OrbStack: command-line tools and integration
source ~/.orbstack/shell/init.zsh 2>/dev/null || :
