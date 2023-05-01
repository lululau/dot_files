#!/bin/bash

word=$1
panes=$(tmux list-panes -F '#S:#I.#P')
for p in $panes; do
  tmux capture-pane -t "$p" -S- -E- -p -J | awk -v RS='[│\n]' 'index($0, "'"$word"'") > 0 {sub(/[ \t]+$/, "");print}'
done
