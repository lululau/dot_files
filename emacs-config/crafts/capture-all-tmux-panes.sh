#!/bin/bash

word=$1
sessions=$(tmux list-sessions -F '#S')
windows=$(for s in $sessions; do tmux list-windows -t "$s" -F '#S:#I'; done)
panes=$(for w in $windows; do tmux list-panes -t "$w" -F '#S:#I.#P'; done)
for p in $panes; do
  tmux capture-pane -t "$p" -S- -E- -p -J | awk -v RS='[│\n]' 'index($0, "'"$word"'") > 0 {sub(/[ \t]+$/, "");print}'
done
