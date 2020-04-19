require 'rubygems'
Gem::Deprecate.skip = true # Don't print out gem Deprecation warnings
require 'logger'

begin
  require File.join(File.dirname(__FILE__), 'debundle.rb')

  require 'irbtools/configure'
  # Move IRB's history file to an XDG compliant configuration directory
  IRB.conf[:HISTORY_FILE] = File.expand_path File.join(ENV['XDG_DATA_HOME'], 'irb_history')
  Irbtools.welcome_message = "Welcome to irb. You are using #{ RUBY_DESCRIPTION }."

  #################################################
  # Supplemental gems
  Irbtools.add_library 'net-http-spy', thread: :myIRB do
    # These logger settings need to be redefined
    # https://github.com/martinbtt/net-http-spy/issues/2
    if defined? Net::HTTP::Persistent::SSLReuse
      Net::HTTP::Persistent::SSLReuse.http_logger = Logger.new(STDOUT)
      Net::HTTP::Persistent::SSLReuse.http_logger_options = {:body => true, :verbose => true, :trace => true}
    end
  end

  if File.exists?(ENV['CONFIG_RU'] || 'config.ru')
    Irbtools.add_library 'racksh/init', thread: :myIRB do
      ENV['CONFIG_RU'] ||= 'config.ru'
      ENV['RACK_ENV'] ||= 'development'
      Rack::Shell.init
    end
  end

  require File.join(File.dirname(__FILE__), 'sugar.rb')

  # Don't need coderay support within irb to syntax highlight files, I don't use it
  Irbtools.remove_library :coderay

  require 'irbtools/more'
  Irbtools.start

  IRB.conf[:PROMPT][:ARROWSHAPES] = {
    :PROMPT_I => ">> ",    # normal
    :PROMPT_N => "|> ",    # indenting
    :PROMPT_C => "…> ",    # continuing a statement
    :PROMPT_S => "%l> ",   # continuing a string
    :RETURN   => "=> %s \n",
    :AUTO_INDENT => true,
  }

  IRB.conf[:PROMPT_MODE] = :ARROWSHAPES

# Make some noise whenever there is an exception, of any kind.
# Otherwise IRB won't report any error and just stop executing irbrc, leaving me
# confused as to why some of the RC file hasn't been loaded.
rescue Exception => e
  puts e.full_message
end
