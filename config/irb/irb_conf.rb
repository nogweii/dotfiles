begin
    # Check if the load path has been properly set.
    if $LOAD_PATH.grep(Regexp.new(File.join("config", "irb"))).count == 0
        $LOAD_PATH.unshift File.join(ENV['HOME'], ".config", "irb")
    end

    def tramp_require(what, &block)
        loaded, require_result = false, nil

        begin
            require_result = require what
            loaded = true

        rescue Exception => ex
            puts "** Unable to require '#{what}'"
            puts "--> #{ex.class}: #{ex.message}"
        end

        yield if loaded and block_given?

        require_result
    end

    tramp_require 'irb/ext/save-history' do
        # Save history across sessions to a file
        IRB.conf[:HISTORY_FILE] = File.expand_path File.join(ENV['XDG_DATA_HOME'], "irb_history") # Where to store the history
        IRB.conf[:SAVE_HISTORY] = 500 # How many lines to save in the history
    end
    tramp_require 'irb/completion' # Tab complete objects/methods

    tramp_require 'rubygems'
    tramp_require 'method_extensions' # Awesome extensions to Method
    tramp_require 'method_extensions/method/super' # Mods core classes!
    tramp_require 'bond' do
        Bond.start
    end
    tramp_require 'hirb' do
        # # Hirb has custom bond completion
        # Bond.load_gems 'hirb'
    end
    tramp_require 'boson' do
        Boson.start
    end
    tramp_require 'net-http-spy' # Spy on Net::HTTP requests
    tramp_require 'wirble' do
        Wirble.init
        Wirble.colorize
    end
    tramp_require File.expand_path(File.join((ENV['XDG_CONFIG_DIR'] || "~/.config"), '/irb/irb_rocket'))
    tramp_require "ap"
    tramp_require "looksee"
    if File.exists? (ENV['CONFIG_RU'] || "config.ru")
        tramp_require "racksh/init" do
            ENV['CONFIG_RU'] ||= 'config.ru'
            ENV['RACK_ENV'] ||= 'development'
            Rack::Shell.init
        end
    end

    Thread.abort_on_exception = true
    ARGV.concat ["--readline", "--prompt-mode", "simple"]

    IRB.conf[:USE_READLINE] = true # Use readline
    IRB.conf[:IGNORE_EOF] = true # Ignore Ctrl-D, require 'exit' (like I have in zsh)

    # http://ozmm.org/posts/time_in_irb.html
    def time(times = 1)
        return nil unless block_given?
        require 'benchmark'
        ret = nil
        Benchmark.bm { |x| x.report { times.times { ret = yield } } }
        ret
    end

    # thanks rtomayko
    def IRB.reload
        load __FILE__
    end

    def copy(str)
        IO.popen('xclip -i', 'w') { |f| f << str.to_s }
    end

    def paste
        `xclip -o`
    end

    def ep
        eval(paste)
    end

    HASH = { :red => "blue", "blue" => :green, 3 => "four" } unless defined?(HASH)
    ARRAY = HASH.keys unless defined?(ARRAY)
    STRING = "blue" unless defined?(STRING)
    SYMBOL = :green unless defined?(SYMBOL)

# Make some noise whenever there is an exception, of any kind.
# Otherwise IRB won't report any error and just stop executing irbrc, leaving me
# confused as to why some of the RC file hasn't been loaded.
rescue Exception => e
    puts "Error! Error! #{e.class} raised!"
    puts "> #{e.message}"
    puts e.backtrace.join("\n")
end
