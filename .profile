ulimit -n 200000
ulimit -u 2128

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/Users/liuxiang/.sdkman"
[[ -s "/Users/liuxiang/.sdkman/bin/sdkman-init.sh" ]] && source "/Users/liuxiang/.sdkman/bin/sdkman-init.sh"

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
[[ -s $HOME/.rvm/scripts/rvm ]] && source $HOME/.rvm/scripts/rvm

. "$HOME/.grit/bin/env"
