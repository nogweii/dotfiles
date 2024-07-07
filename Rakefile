# frozen_string_literal: true

require "pathname"

@extra_information = (verbose == true) || (Rake.application.options.trace == true)

Dir["lib/rake-tasks/*.rb"].each do |rake_task_file|
  require_relative rake_task_file
end

def debug(message)
  puts message if @extra_information
end

# A small alias to the function since it gets called a lot in here
def exp(path)
  File.expand_path path
end

DOTFILES_DIR = exp File.dirname(__FILE__)

# cd to the top of the git repo
gitdir = `git rev-parse --show-toplevel`.strip
Dir.chdir gitdir

# Only show shell commands if we didn't run rake with -v or -t
RakeFileUtils.verbose_flag = false unless @extra_information

DOTFILE_SOURCES = Dir["share/*"] + Dir["etc/*"]
DOTFILE_EXCLUSIONS = ["etc/ssh"]

DOTFILES = (DOTFILE_SOURCES - DOTFILE_EXCLUSIONS).flatten.sort
DOTFILE_TARGETS = DOTFILES.inject({}) do |collection, val|
  collection.merge({ exp("./#{val}") => exp("~/.local/#{val}") })
end
DOTFILE_TARGETS[exp("./bin")] = exp("~/.local/bin")
DOTFILE_TARGETS[exp("./etc/ssh/config")] = exp("~/.ssh/config")
DOTFILE_TARGETS[exp("./etc/ssh/99-defaults.conf")] = exp("~/.ssh/config.d/99-defaults.conf")

task default: [:submodules, :prepare, :dircolors, :dotfiles, :unnecessary]

# ~/.<dotfile> can be one of 3 states:
#  - Doesn't exist
#  - Is a symlink to somewhere in this repo
#  - Is a regular file/directory
#
# I handle it as such:
# - If it doesn't exist already, it's assumed that we can manage it, and
#   therefore do whatever we want.
# - If it's a regular file/directory, we assume that there is content that you
#   want to keep, and therefore we should not touch the file
# - All symlinks are free game. We replace the symlink with whatever we want.
desc "Symlinks all my dotfiles"
task :dotfiles do
  DOTFILE_TARGETS.each do |source, destination|
    if File.symlink?(destination) and (not File.exist? destination)
      debug "Replacing #{destination} as it's a broken symlink"
      rm_r destination
      symlink source, destination
    elsif not File.exist? destination
      debug "Creating new symlink at #{destination}"
      symlink source, destination
    elsif File.symlink?(destination) and File.readlink(destination) != source
      debug "Overriding the symlink at #{destination} with #{source}"
      rm_r destination
      symlink source, destination
    elsif not File.symlink?(destination)
      warn "File '#{destination}' exists but is not a symlink. Not touching it!"
    end
  end
end

desc "List of everything this rake file will try managing"
task :list do
  puts "Symlink these files:"
  DOTFILE_TARGETS.sort.each do |source, destination|
    puts " - #{source.delete_prefix Dir.pwd + "/"} => #{destination}"
  end
  puts ""
  puts "Create these directories:"
  MAKE_DIRS.each do |dir|
    puts " - #{dir}"
  end
  puts ""
  puts "Clean up these old things:"
  OLD_CLEANUP.each do |thing|
    puts " - #{thing}"
  end
  puts ""
  puts "These files will not be symlinked:"
  DOTFILE_EXCLUSIONS.each do |thing|
    print " - #{thing}"
    if not File.exist? thing
      puts " (doesn't exist!)"
    else
      puts ""
    end
  end
end

desc "Use vivid to generate dircolors"
task :dircolors do
  require "erb"

  unless system("which vivid", { out: File::NULL })
    puts "vivid is not installed! no dircolors for you."
    next
  end
  vivid_colors = `vivid generate ./etc/dircolors/bamboo.yml`.strip
  template = ERB.new File.open("./etc/dircolors/zsh_template.erb").read
  File.open("./etc/zsh/conf/00_z_dircolors.zsh", "w") do |f|
    f.puts template.result(binding)
  end
end

desc "Rebuild some zsh caches"
task zsh: %i[zsh:functions zsh:completion]

namespace :zsh do
  desc "My personal function collection"
  task :functions do
    system("zsh -ic zfunccompile")
  end

  desc "Completion engine"
  task :completion do
    system("zsh -ic compsupercache")
  end
end
