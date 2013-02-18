#!/usr/bin/ruby

require 'uri'

# A utility function that will take a string (parameter 'query') and try to find
# the appropriate search engine to use. These engines are provided by surfraw,
# the text user's revolution against the web.
def search_engine(query)
  # The default elvi, if the search terms don't begin with a recognized one
  default_elvi = 'google'

  # split up the query to check if there is an elvi
  elvi, *query = query.split(' ')

  if @elvis.include? elvi
    puts "[go.rb] elvi '#{elvi}' is included" if $DEBUG
    # the word is in the @elvis list, so we assume it's valid & recognized. Call
    # surfraw and have it just print out final URL
    search_query = query.join(' ')
    url = %x{surfraw -p -browser=echo #{elvi} "#{search_query}"}
  else
    puts "[go.rb] #{elvi} is not an elvi" if $DEBUG
    # nope, not a recognized elvi, so we use the default elvi. We include the
    # 'elvi' variable as it's part of the user's input, even though we didn't
    # recognize it
    search_query = "#{elvi} #{query.join(' ')}"
    url = %x{surfraw -p -browser=echo #{default_elvi} "#{search_query}"}
  end

  # A quick append to the search history file; used by another script which just
  # does a search
  open("#{ENV['XDG_DATA_HOME']}/uzbl/search_history", 'a').puts search_query.strip
  
  return url
end

# Getting a list of elvi installed
begin
  # a list of all the installed elvi
  @elvis = Dir.chdir('/usr/lib/surfraw') { Dir['*'] }
  if File.directory? "#{ENV['XDG_CONFIG_HOME']}/surfraw/elvi"
      # append all of the user's elvi
      @elvis.push(*Dir.chdir("#{ENV['XDG_CONFIG_HOME']}/surfraw/elvi") { Dir['*'] })
      @elvis.uniq! # shrink the array, removing duplicates
  end
  # not to worry, though. surfraw will use per-user elvi first, before the
  # system-wide ones are. Neat!
rescue Errno::ENOENT
  # surfraw isn't installed, so we won't match any specialized search engines
  @elvis = []
end

# build the history array
history = []
# first, read from the history file
File.readlines("#{ENV['XDG_DATA_HOME']}/uzbl/history").each do |history_line|
  columns = history_line.strip.split(' ')

  history << "#{columns[3..-1].join(' ')} (#{columns[2]})"
end

# build a hash that contains the count of each unique history item in the array
history_count = history.inject(Hash.new(0)) { |hash, element| hash[element] += 1; hash }
# sort the count of history by how many times they appeared, backwards (so the
# most frequent sites bubble up to the top), or alphabetical if they've shown up
# equal times
history_count = history_count.sort {|a, b| (a[1] <=> b[1]) * -1}
# and now, get just the page's title & URL, as a sorted array
history = history_count.map {|count_pair| count_pair[0]}

# the default value. Only stays nil after popen() if the user quit dmenu without
# making a choice
chosen = nil

# with the various sources prepared, execute and fill in dmenu's stdin
IO.popen('dmenu -b -l 15 -p "Open:"', 'w+') do |menu_io|
  # first, we put in the history
  history.each do |hist|
    menu_io.puts hist
  end

  # then the elvis come after
  @elvis.each do |elvi|
    menu_io.puts elvi
  end

  # close the write stream (stdin, EOF), also dmenu picks up on that and will at
  # this point immediately display with the full list of choices
  menu_io.close_write

  # and we wait for the user to make their choice. Note: they could still
  # escape, which means chosen will be nil (as it won't be set by this line)
  chosen = menu_io.read.strip
end

# The user hit escape, so we can just quit here
if chosen.nil? or chosen == ""
  exit 0
end

# matching a *basic* url, as formatted earlier from history. Basically, the user
# chose something from the history
if chosen =~ / \(([a-z]+:.*)\)$/
  chosen = $1
  puts "[go.rb] recognized history #{chosen}" if $DEBUG
end

# short-circut: if the query includes a space, jump to the search engine logic
if chosen.include? ' '
  chosen = search_engine chosen
  puts "[go.rb] short circut to search_engine #{chosen}" if $DEBUG
end

# Completing an incomplete URL, i.e. the user typed only the domain
if chosen.include? '.'
  # just a domain, like 'google.com', so prepend http (safe bet) and go
  if not chosen.include? ':'
    chosen = 'http://' + chosen
    puts "[go.rb] prepending http:// #{chosen}" if $DEBUG
  end
end

# the url doesn't have something that looks like a scheme. This is possible if
# the user inputs a single word. If it's an elvi, that will give a safe default
# URL or otherwise search for that word.
#if URI.parse(chosen).is_a? URI::Generic
unless chosen =~ /^[a-z]+:/i
  # failing anything else, just search for the string
  chosen = search_engine chosen
  puts "[go.rb] fall back, search_engine #{chosen}" if $DEBUG
end

puts chosen
