begin
    require 'irb/ext/save-history' # Save history across sessions to a file
    require 'irb/completion' # Tab complete objects/methods

    require 'method_extensions' # Awesome extensions to Method
    require 'method_extensions/method/super' # Mods core classes!
    require 'bond'; Bond.start
    Bond.load_gems 'hirb'; require 'hirb' # Hirb has custom bond completion
    require 'boson'; Boson.start
    require 'net-http-spy' # Spy on Net::HTTP requests
    require 'wirble'
    require File.join(ENV['HOME'], '.irb/irb_rocket')
    require "ap"
    require "looksee"

    Thread.abort_on_exception = true
    ARGV.concat ["--readline", "--prompt-mode", "simple"]

    IRB.conf[:HISTORY_FILE] = File.expand_path File.join(ENV['XDG_DATA_HOME'], "irb_history") # Where to store the history
    IRB.conf[:SAVE_HISTORY] = 500 # How many lines to save in the history
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

# Make some noise whenever there is an exception, of any kind.
# Otherwise IRB won't report any error and just stop executing irbrc, leaving me
# confused as to why some of the file hasn't been loaded.
rescue Exception => e
    puts "Error! Error! #{e.class} raised!"
    puts "> #{e.message}"
    puts e.backtrace.join("\n")
end
