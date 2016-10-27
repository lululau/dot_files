unless $USER_PRYRC_LOADED

  if defined?(Rails) && Rails.const_defined?("ConsoleMethods") && Rails.env
    extend Rails::ConsoleMethods
  end

  if defined?(Rails) && Rails.root.to_s =~ /^#{ENV["HOME"]}\/kt\/(baton|notab)/
    BA = BaseAsset
    VA = VirtualAsset
    RP = RegisteredProduct
    AU = AssetUnit
    PS = PackageStrategy
    PCR = ProductCodeRange
    CI = ConsigneeInterest
    AF = AttachFile
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

  Pry.commands.alias_command 'rc', 'reload-code'
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

  Pry.commands.block_command 'toggle-mongoid-query-log' do
    if Object.const_defined?(:Moped)
      if $pry_previous_mongo_logger
        $pry_previous_mongo_logger, Moped.logger = Moped.logger, $pry_previous_mongo_logger
      else
        $pry_previous_mongo_logger, Moped.logger = Moped.logger, Logger.new(STDOUT, :debug)
      end
    else
      if $pry_previous_mongo_logger
        $pry_previous_mongo_logger, Mongo::Logger.logger = Mongo::Logger.logger, $pry_previous_mongo_logger
      else
        $pry_previous_mongo_logger, Mongo::Logger.logger = Mongo::Logger.logger, Logger.new(STDOUT, :debug)
      end
    end
  end

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

  class Pry::Command::Ls::LocalVars
    def colorized_assignment_style(lhs, rhs, desired_width = 7)
      colorized_lhs = color(:local_var, lhs)
      color_escape_padding = colorized_lhs.to_s.size - lhs.to_s.size
      pad = desired_width + color_escape_padding
      "%-#{pad}s = %s" % [color(:local_var, colorized_lhs), rhs]
    end
  end

  $USER_PRYRC_LOADED=true

  if ENV['INSIDE_EMACS']
    IRB.conf[:USE_READLINE] = false
    Pry.config.pager = false
    Pry.config.correct_indent = false
    Pry.config.editor = "emacs-other-window"
  end
end
