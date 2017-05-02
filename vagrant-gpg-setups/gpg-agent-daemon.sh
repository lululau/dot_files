#!/bin/bash
gpg-agent --quiet --daemon --enable-ssh-support --write-env-file "${HOME}/.gpg-agent-info"
