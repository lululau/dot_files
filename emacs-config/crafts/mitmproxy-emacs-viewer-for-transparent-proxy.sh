#!/bin/bash

# Put the following line in your sudoers file:
# nobody ALL= (YOUR_USERNAME) NOPASSWD: /usr/local/bin/emacsclient

if [ -z "$HOMEBREW_PREFIX" ]; then
  if [ -e "/opt/homebrew/bin/brew" ]; then
    HOMEBREW_PREFIX="/opt/homebrew"
  else
    HOMEBREW_PREFIX="/usr/local"
  fi
fi

dirname=$(cd "$(dirname $0)"; pwd)
user=$(cat "$dirname/.vterm-mitmproxy-current-user")
chmod +r $1
sudo -u "$user" $HOMEBREW_PREFIX/bin/emacsclient --eval "(split-window)" &> /dev/null
sudo -u "$user" $HOMEBREW_PREFIX/bin/emacsclient -n "$@"
