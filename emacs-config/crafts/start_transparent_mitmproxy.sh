#!/bin/bash

dirname=$(cd "$(dirname $0)"; pwd)

sudo sysctl -w net.inet.ip.forwarding=1
sudo pfctl -e -f "$dirname"/mitmproxy.pf.conf
echo -n "$USER" > "$dirname/.vterm-mitmproxy-current-user"
sudo -u nobody PAGER="$dirname"/mitmproxy-emacs-viewer-for-transparent-proxy.sh mitmproxy -m transparent --showhost -k -p 8888
sudo pfctl -d
