#!/usr/bin/ruby
 
# Makes symlinks for all the configuration files except for this script and any
# file with README in it.
 
require 'fileutils'
Dir["*"].reject {|file| file =~ /README/ or file == $0 }.each do |file|
  FileUtils.ln_s(File.expand_path(File.join(File.dirname(file), file)), File.expand_path(File.join(ENV['HOME'], ".#{file}")), :verbose => true) unless File.exists? File.expand_path(File.join(ENV['HOME'], ".#{file}"))
end

