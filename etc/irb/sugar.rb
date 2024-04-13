# Extra methods to make some tasks in IRB easier

class Float
  # Transform a number into a string with commas every 3 digits
  # (English/American style). Makes it easier to read large numbers that way
  def commify
    to_s.reverse.gsub(/(\d\d\d)(?=\d)(?!\d*\.)/, '\1,').reverse
  end
end

class Integer
  # Transform a number into a string with commas every 3 digits
  # (English/American style). Makes it easier to read large numbers that way
  def commify
    to_s.gsub(/(\d)(?=(\d{3})+$)/, '\1,')
  end
end

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
