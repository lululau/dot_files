_new_path=($HOME/ServerApps/bin $HOME/bin $HOME/Library/Python/3.10/bin $HOME/Library/Python/2.7/bin)
for i in "${path[@]}"
do
  if [ $HOME/ServerApps/bin != "$i" -a $HOME/bin != "$i" -a $HOME/Library/Python/2.7/bin != "$i" -a $HOME/Library/Python/3.10/bin != "$i" ]; then
    _new_path+=$i
  fi
done

path=(${_new_path})
