require 'rake/clean'

ENV['HOME'] = ENV['DOTFILES_HOME_DIR'] if ENV.key? 'DOTFILES_HOME_DIR'
extra_information = (verbose == true) || (Rake.application.options.trace == true)

# cd to the top of the git repo
$gitdir = `git rev-parse --show-toplevel`.strip
Dir.chdir $gitdir

# Only show shell commands if we didn't run rake with -v or -t
RakeFileUtils.verbose_flag = false unless extra_information

def dotfiles
  (Dir['*'] - %w[Rakefile README.md config]).sort
end

DOTFILES = []

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
#
# This function builds a file task for Rake that resolves the above conditions.
# Takes a local file path (relative to the root of the repository) and will
# attempt to symlink the matching location (relative to $HOME, with a dot
# prefixing it) to the source file within the repository.
#
# @param [String] dotfile The local file in the repo to be symlinked to $HOME
def dottask(dotfile)
  # Absolute path to where we're going to symlink in $HOME
  home_abs_path = File.expand_path "~/.#{dotfile}"

  # Absolute path to the real file, stored in the repository
  repo_abs_path = File.expand_path "./#{dotfile}"

  # Build the file task that sets up the symlink
  file home_abs_path do
    if not File.exists? home_abs_path
      symlink repo_abs_path, home_abs_path
    elsif File.symlink? home_abs_path
      rm_r home_abs_path
      symlink repo_abs_path, home_abs_path
    else
      warn "File '#{home_abs_path}' exists but is not a symlink. Not touching it!"
    end
  end

  # This is a file we're managing, so append it to the list of files that we
  # can clean
  DOTFILES << home_abs_path
end

dotfiles.each do |dotfile|
  dottask dotfile
end

task :default => :dotfiles

desc "Symlinks all my dotfiles"
task :dotfiles => [:submodules, :prepare, DOTFILES, ].flatten

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
  sh 'git submodule update --init --recursive >/dev/null'
end

# All the submodules
# File.foreach('.gitmodules') {|line| print $1 if line =~ /^\[submodule
# \"(.*)\"\]/ }

MAKE_DIRS = ["vim/tmp", File.expand_path("~/.local/cache")]

File.open("config/user-dirs.dirs").readlines.each do |user_dir|
  next if user_dir =~ /^#/
  expand_path = user_dir.gsub(/.*="\$HOME\/(.*)"\n/, "#{ENV['HOME']}/\\1")
  MAKE_DIRS << expand_path
end

File.open("profile").readlines.each do |profile_line|
  next unless profile_line =~ / # dir-make/
  expand_path = profile_line.gsub(/.*="\$\{HOME\}\/(.*)".*\n/, "#{ENV['HOME']}/\\1")
  MAKE_DIRS << expand_path
end

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
  sh 'tic terminfo/screen-256color-italitc.terminfo'
end

desc "List of everything this rake file will try managing"
task :list do
  require 'pp'
  puts "Symlink these files:"
  pp DOTFILES
  puts "Create these directories:"
  pp MAKE_DIRS
end
