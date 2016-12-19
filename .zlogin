
# if [ -n "$ITERM_SESSION_ID"  ]
# then
#     eval "$(icrt.rb "${ITERM_SESSION_ID%%:*}")"
# fi

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
[[ -s $HOME/.sdkman/bin/sdkman-init.sh ]] && source $HOME/.sdkman/bin/sdkman-init.sh
[[ -s $HOME/.rvm/scripts/rvm ]] && source $HOME/.rvm/scripts/rvm

if { uname | grep -q Linux; } && [ -e $HOME/.autojump ] ; then
    [[ -s $HOME/.autojump/etc/profile.d/autojump.sh ]] && source $HOME/.autojump/etc/profile.d/autojump.sh
fi
