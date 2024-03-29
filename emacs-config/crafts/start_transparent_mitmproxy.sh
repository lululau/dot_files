#!/bin/bash

dirname=$(cd "$(dirname $0)"; pwd)

sudo sysctl -w net.inet.ip.forwarding=1
sudo pfctl -e -f "$dirname"/mitmproxy.pf.conf
echo -n "$USER" > "$dirname/.vterm-mitmproxy-current-user"
sudo -u nobody bash -c "ulimit -n 200000; ulimit -u 2128; export PAGER=$dirname/mitmproxy-emacs-viewer-for-transparent-proxy.sh; mitmproxy -m transparent --showhost -k --set console_palette_transparent=true --set console_palette=dark -p 8888"
sudo pfctl -d
