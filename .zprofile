# export PATH=$HOME/bin:$HOME/Library/Python/2.7/bin:$PATH
if { uname | grep -q Linux; } && [ -e $HOME/liuxiang ] ; then
    _new_path=($HOME/liuxiang/bin $HOME/liuxiang/local/bin $HOME/.local/bin)
    for i in "${path[@]}"
    do
        if [ $HOME/liuxiang/bin != "$i" -a $HOME/liuxiang/local/bin != "$i" -a $HOME/.local/bin != "$i" ]; then
            _new_path+=$i
        fi
    done
else
    _new_path=($HOME/bin $HOME/Library/Python/2.7/bin/)
    for i in "${path[@]}"
    do
        if [ $HOME/bin != "$i" -a $HOME/Library/Python/2.7/bin != "$i" ]; then
            _new_path+=$i
        fi
    done
fi

path=(${_new_path})
