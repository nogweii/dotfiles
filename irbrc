begin
    require 'irb/ext/save-history'
    require 'irb/completion'

    Thread.abort_on_exception = true
    ARGV.concat ["--readline", "--prompt-mode", "simple"]

    IRB.conf[:SAVE_HISTORY] = 500
    IRB.conf[:HISTORY_FILE] = File.expand_path File.join(%w[~ .data irb_history])

    require 'pp'
    pp IRB.conf[:HISTORY_FILE]

    # http://ozmm.org/posts/time_in_irb.html
    def time(times = 1)
        require 'benchmark'
        ret = nil
        Benchmark.bm { |x| x.report { times.times { ret = yield } } }
        ret
    end

    # thanks rtomayko
    def IRB.reload
        load __FILE__
    end
rescue Exception => e
    puts "Error! Error! #{e.class} raised!"
    puts "> #{e.message}"
    puts e.backtrace.join("\n")
end
