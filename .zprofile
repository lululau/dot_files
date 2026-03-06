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
if [ -e ~/.orbstack/shell/init.zsh ]; then
  source ~/.orbstack/shell/init.zsh 2>/dev/null || :
fi

# Added by swiftly
if [ -e ~/.swiftly/env.sh ]; then
  source ~/.swiftly/env.sh
fi
