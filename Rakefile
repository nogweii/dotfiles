require 'rake/clean'

@extra_information = (verbose == true) || (Rake.application.options.trace == true)

def log(message)
  puts "\e[31m** dotfile:\e[0m #{message}" if @extra_information
end

# cd to the top of the git repo
$gitdir = `git rev-parse --show-toplevel`.strip
Dir.chdir $gitdir

# dotfile black magic to make Rake behave a bit better for our particular
# situation
module Rake
  class FileTask < Task
    # 'name' is the $HOME-relative path to a dotfile.
    def needed?
      begin
        symlinked_to_us = !!(File.readlink(name) =~ /#{$gitdir}/)
      rescue Errno::EINVAL, Errno::ENOENT
        symlinked_to_us = false
      end

      is_needed = ! symlinked_to_us ||
                  ! File.exist?(name) ||
                  out_of_date?(timestamp)

      warn "#{name} exists! Not touching it" if is_needed && !File.symlink?(name)

      return is_needed && ! File.directory?(name)
    end
  end
end

# Only show shell commands if we didn't run rake with -v or -t
RakeFileUtils.verbose_flag = false unless @extra_information

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
  home_rel_path = File.expand_path "~/.#{dotfile}"
  # Absolute path to the real file, stored in the repository
  repo_rel_path = File.expand_path "./#{dotfile}"

  log "Making dotfile task for #{dotfile} (#{home_rel_path} -> #{repo_rel_path})"

  # Build the file task that sets up the symlink
  file home_rel_path do
    rm_r home_rel_path if File.exists? home_rel_path
    symlink repo_rel_path, home_rel_path
  end

  # This is a file we're managing, so append it to the list of files that we
  # can clean
  DOTFILES << home_rel_path
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
  sh 'git submodule update --init --recursive'
end

# All the submodules
# File.foreach('.gitmodules') {|line| print $1 if line =~ /^\[submodule
# \"(.*)\"\]/ }

MAKE_DIRS = ["vim/tmp", File.expand_path("~/.local/cache")]

MAKE_DIRS.each do |dir|
  directory dir do
    mkdir_p dir
  end
end

CLOBBER << [DOTFILES].flatten
CLEAN << [MAKE_DIRS].flatten

desc "Prepare extra directories"
task :prepare => MAKE_DIRS do
  symlink "#{$gitdir}/share/rbenv-plugins", "share/rbenv/plugins"
  # compile youcompleteme
  # compile command-t
end
