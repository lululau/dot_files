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
