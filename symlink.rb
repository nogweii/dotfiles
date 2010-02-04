#!/usr/bin/ruby

# Makes symlinks for all the configuration files except for this script and any
# file with README in it.

require 'fileutils'
[Dir["*"], Dir["config/*"]].flatten.reject do |file|
    file =~ /README/ or
    file == $0 or
    file =~ %r[^config/?$] or
    file =~ /website/
end.each do |file|
    dotfile = File.expand_path(File.join(ENV['HOME'], ".#{file}"))

     unless File.exists? dotfile
         FileUtils.ln_s(File.expand_path(file), dotfile, :verbose => true)
     else
         warn "`#{dotfile}' already exists, skipping"
     end
end

