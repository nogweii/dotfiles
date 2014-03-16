require 'rake/clean'

def here(*paths)
  File.expand_path(File.join(File.dirname(__FILE__), *paths))
end

def dotfiles
  Dir[here('*')].map do |path|
    File.basename(path)
  end.reject do |path|
    path == "Rakefile" or path =~ /^README/
  end.sort
end
p dotfiles

DOTFILES = []

dotfiles.each do |dotfile|
  # Absolute path to the symlink in $HOME
  link_path = File.expand_path "~/.#{dotfile}"

  # Override the path. I hope you didn't have anything important there!
  file link_path do
    rm link_path if File.exists? link_path
    symlink here(dotfile), link_path
  end

  # This is a file we're managing, so append it to the list of files that we
  # can clean
  DOTFILES << link_path
end

task :default => :dotfiles

desc "Symlinks all my dotfiles"
task :dotfiles => [:submodules, :prepare, DOTFILES].flatten do
end

desc "Removes all my dotfile symlinks"
task :clean do
  dotfiles.each do |dotfile|
    link = File.expand_path("~/.#{dotfile}")

    if File.symlink?(link)
      rm link
    end
  end
end

desc "Initialize all submodules"
task :submodules do
  sh 'git submodule update --init --recursive'
end

MAKE_DIRS = [here("vim/tmp"), File.expand_path("~/.local/cache")]

MAKE_DIRS.each do |dir|
  directory dir do
    mkdir_p dir
  end
end

CLOBBER << [DOTFILES].flatten
CLEAN << [MAKE_DIRS].flatten

desc "Prepare extra directories"
task :prepare => MAKE_DIRS do
  # compile youcompleteme
  # compile command-t
end
