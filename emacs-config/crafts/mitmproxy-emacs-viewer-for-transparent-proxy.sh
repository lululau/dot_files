#!/bin/bash

# Put the following line in your sudoers file:
# nobody ALL= (YOUR_USERNAME) NOPASSWD: /usr/local/bin/emacsclient

dirname=$(cd "$(dirname $0)"; pwd)
user=$(cat "$dirname/.vterm-mitmproxy-current-user")
chmod +r $1
sudo -u "$user" /usr/local/bin/emacsclient --eval "(split-window)" &> /dev/null
sudo -u "$user" /usr/local/bin/emacsclient -n "$@"
