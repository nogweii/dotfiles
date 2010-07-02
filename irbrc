# Saves a lot of guess work if a background thread fails
require 'irb/completion'
require 'irb/ext/save_history'

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

begin
  require "ap"
  IRB::Irb.class_eval do
    def output_value
      ap @context.last_value
    end
  end
rescue LoadError => e
  puts "ap gem not found.  Try typing 'gem install awesome_print' to get super-fancy output."
end

