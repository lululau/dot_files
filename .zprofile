# export PATH=$HOME/bin:$HOME/Library/Python/2.7/bin:$PATH
_new_path=($HOME/bin $HOME/Library/Python/2.7/bin)
for i in "${path[@]}"
do
  if [ $HOME/bin != "$i" -a $HOME/Library/Python/2.7/bin != "$i" ]; then
    _new_path+=$i
  fi
done
path=(${_new_path})
