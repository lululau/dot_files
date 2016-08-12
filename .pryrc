unless $USER_PRYRC_LOADED

  if defined?(Rails) && Rails.const_defined?("ConsoleMethods") && Rails.env
    extend Rails::ConsoleMethods 
  end

  Pry.config.editor = "vim"

  # if defined?(PryNav)
  #   Pry.commands.rename_command 'c', 'continue'
  #   Pry.commands.rename_command 's', 'step'
  #   Pry.commands.rename_command 'n', 'next'
  # end

  # if defined?(PryDebugger) || defined? (PryByebug) 
  #   Pry.commands.rename_command 'b', 'break'
  #   Pry.commands.rename_command 'c', 'continue'
  #   Pry.commands.rename_command 's', 'step'
  #   Pry.commands.rename_command 'n', 'next'
  #   Pry.commands.rename_command 'f', 'finish'
  # end

  Pry.commands.rename_command 'b', 'break' if Pry.commands.valid_command?('break')
  Pry.commands.rename_command 'c', 'continue' if Pry.commands.valid_command?('continue')
  Pry.commands.rename_command 's', 'step' if Pry.commands.valid_command?('step')
  Pry.commands.rename_command 'n', 'next' if Pry.commands.valid_command?('next')
  Pry.commands.rename_command 'f', 'finish' if Pry.commands.valid_command?('finish')

  Pry.commands.alias_command 'ed', 'edit'
  Pry.commands.alias_command 'w', 'whereami'
  Pry.commands.alias_command 'pwd', 'nesting'
  Pry.commands.alias_command 'a', 'cd ..'
  Pry.commands.alias_command 'aa', 'cd ../..'
  Pry.commands.alias_command 'aaa', 'cd ../../..'
  Pry.commands.alias_command 'aaaa', 'cd ../../../..'
  Pry.commands.alias_command 'aaaaa', 'cd ../../../../..'
  Pry.commands.alias_command 'aaaaaa', 'cd ../../../../../..'
  Pry.commands.alias_command 'aaaaaaa', 'cd ../../../../../../..'
  Pry.commands.alias_command 'aaaaaaaa', 'cd ../../../../../../../..'
  Pry.commands.alias_command 'aaaaaaaaa', 'cd ../../../../../../../../..'

  Pry.commands.block_command 'require-active-support' do
    require "active_support/all"
  end

  Pry.commands.block_command 'require-factories' do
    Dir.glob(File.join(Dir.pwd, "spec/factories/**/**/*.rb")).each do |factory|
      require factory
    end
  end

  Pry.commands.alias_command 'load-factories', 'require-factories'

  def rm(const)
    Object.send :remove_const, const
  end

  def umeng_signin(email="admin@umeng.com", password=111111)
    app.post '/people/sign_in', "person[email]=#{email}&person[password]=#{password}"
  end

  def pbp
    p = IO.popen("pbpaste")
    lines = p.readlines.map(&:chomp)
    p.close
    lines
  end

  def pbp_str
    `pbpaste` 
  end

  def pc(contents)
    p = IO.popen("pbcopy", "w")
    p.write(contents)
    p.close
  end

  def with_out_str
    require "stringio"
    $stdout = x = StringIO.new
    yield
    x.rewind
    result = x.read
    $stdout = STDOUT
    x.close
    result
  end

  def with_out_arr(&block)
    with_out_str(&block).split("\n")
  end

  $USER_PRYRC_LOADED=true

  if ENV['INSIDE_EMACS']
    IRB.conf[:USE_READLINE] = false
    Pry.config.pager = false
    Pry.config.correct_indent = false
    Pry.config.editor = "emacsclient"
  end
end
