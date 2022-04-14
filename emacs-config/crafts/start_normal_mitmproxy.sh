#!/bin/bash

dirname=$(cd "$(dirname $0)"; pwd)

if [ "$1" = "true" ]; then
  SERVICE_GUID=`printf "open\nget State:/Network/Global/IPv4\nd.show" | scutil | grep "PrimaryService" | awk '{print $3}'`
  SERVICE_NAME=`printf "open\nget Setup:/Network/Service/$SERVICE_GUID\nd.show" | scutil | grep "UserDefinedName" | awk -F': ' '{print $2}'`
  sudo networksetup -setwebproxy "$SERVICE_NAME" 127.0.0.1 8888
  sudo networksetup -setsecurewebproxy "$SERVICE_NAME" 127.0.0.1 8888
fi


ulimit -n 200000
ulimit -u 2128
PAGER="$dirname"/mitmproxy-emacs-viewer.sh mitmproxy --showhost -k --set console_palette_transparent=true --set console_palette=dark -p 8888

if [ "$1" = "true" ]; then
  sudo networksetup -setwebproxystate "$SERVICE_NAME" off
  sudo networksetup -setsecurewebproxystate "$SERVICE_NAME" off
fi
