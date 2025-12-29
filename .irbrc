print "\e[5 q"

if $PROGRAM_NAME != 'kernel'
  require 'pry'
  Pry.start
  exit
end
