#!/usr/bin/ruby

system("git submodule init")
system("git submodule update")

# Makes symlinks for all the configuration files except for this script and any
# file with README in it.

require 'fileutils'
[Dir["*"], Dir["config/*"], Dir["data/*"]].flatten.reject do |file|
    file =~ /README/ or
    file == $0 or
    file =~ %r[^config/?$] or
    file =~ /website/
end.sort.each do |file|
    dotfile = File.expand_path(File.join(ENV['HOME'], ".#{file}"))

     unless File.exists? dotfile
         FileUtils.ln_s(File.expand_path(file), dotfile, :verbose => true)
     else
         warn "`#{dotfile}' already exists, skipping"
     end
end

# Symlink ~/.xsession to ~/.xinitrc so graphical display managers also work
unless File.exists? File.expand_path(File.join(ENV['HOME'], ".xsession"))
    FileUtils.ln_s(File.expand_path(File.join(ENV['HOME'], ".xinitrc")),
              File.expand_path(File.join(ENV['HOME'], ".xsession")), :verbose => true)
end
unless File.exists? File.expand_path(File.join(ENV['HOME'], ".config/pianobar/ctl"))
    system("mkfifo ~/.config/pianobar/ctl")
end
