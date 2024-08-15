require 'irbtools/configure'

Irbtools.welcome_message = "Welcome to irb. 💎 You are using #{ RUBY_DESCRIPTION }."

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

if File.exist?(ENV['CONFIG_RU'] || 'config.ru')
  Irbtools.add_library 'racksh/init', thread: :myIRB do
    ENV['CONFIG_RU'] ||= 'config.ru'
    ENV['RACK_ENV'] ||= 'development'
    Rack::Shell.init
  end
end

# Don't need coderay support within irb to syntax highlight files, I don't use it
Irbtools.remove_library :coderay
Irbtools.start





