#!/bin/bash

emacsclient --eval "(split-window)" &> /dev/null
emacsclient -n "$@"
