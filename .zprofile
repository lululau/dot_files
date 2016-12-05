# export PATH=$HOME/bin:$HOME/Library/Python/2.7/bin:$PATH
_new_path=($HOME/liuxiang/bin $HOME/liuxiang/local/bin $HOME/.local/bin)
for i in "${path[@]}"
do
  if [ $HOME/liuxiang/bin != "$i" -a $HOME/liuxiang/local/bin != "$i" -a $HOME/.local/bin != "$i" ]; then
    _new_path+=$i
  fi
done
path=(${_new_path})
