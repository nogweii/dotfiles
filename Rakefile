require 'rake/clean'

ENV['HOME'] = ENV['DOTFILES_HOME_DIR'] if ENV.key? 'DOTFILES_HOME_DIR'
extra_information = (verbose == true) || (Rake.application.options.trace == true)

# cd to the top of the git repo
gitdir = `git rev-parse --show-toplevel`.strip
Dir.chdir gitdir

# Only show shell commands if we didn't run rake with -v or -t
RakeFileUtils.verbose_flag = false unless extra_information

def dotfiles
  (
    # Start with everything in this directory, but not recursively
    Dir['*'] -
    # Don't symlink the following
    %w[Rakefile README.md config Brewfile Gemfile Gemfile.lock] +
    # Add these extra to the list to be symlink'd
    %w[config/git config/conky config/nvim config/krb5_ipa.conf]
  ).sort
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
    if not File.exist? home_abs_path
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

task default: :dotfiles

desc 'Symlinks all my dotfiles'
task dotfiles: [:submodules, :prepare, DOTFILES].flatten

desc 'Removes all my dotfile symlinks'
task :clean do
  dotfiles.each do |dotfile|
    link = File.expand_path("~/.#{dotfile}")

    rm link if File.symlink?(link)
  end
end

desc 'Initialize all submodules'
task :submodules do
  sh 'git submodule update --init --recursive >/dev/null'
end

# All the submodules
# File.foreach('.gitmodules') {|line| print $1 if line =~ /^\[submodule
# \"(.*)\"\]/ }

MAKE_DIRS = ['vim/tmp',
             File.expand_path('~/.local/cache'),
             File.expand_path('~/.local'),
             File.expand_path('~/media')]

File.open('config/user-dirs.dirs').readlines.each do |user_dir|
  next if user_dir =~ /^#/
  expand_path = user_dir.gsub(/.*="\$HOME\/(.*)"\n/, "#{ENV['HOME']}/\\1")
  MAKE_DIRS << expand_path
end

File.open('profile').readlines.each do |profile_line|
  next unless profile_line =~ / # dir-make/
  expand_path = profile_line.gsub(/.*="\$\{HOME\}\/(.*)".*\n/, "#{ENV['HOME']}/\\1")
  MAKE_DIRS << expand_path
end

MAKE_DIRS.sort!
MAKE_DIRS.uniq!

MAKE_DIRS.each do |dir|
  directory dir do
    mkdir_p dir
  end
end

CLOBBER << [DOTFILES].flatten
CLEAN << [MAKE_DIRS].flatten

desc 'Prepare extra directories'
task :prepare => MAKE_DIRS do
  ENV['TERMINFO'] = File.expand_path('~/.local/terminfo')
  unless File.exist? File.join(ENV['TERMINFO'], 's', 'screen-256color-italic')
    sh 'tic terminfo/screen-256color-italitc.termcap'
  end
  unless File.exist? File.join(ENV['TERMINFO'], 't', 'tmux-italics')
    sh 'tic terminfo/tmux-italics.termcap'
  end
end

desc 'List of everything this rake file will try managing'
task :list do
  require 'pp'
  puts 'Symlink these files:'
  pp DOTFILES
  puts 'Create these directories:'
  pp MAKE_DIRS
end

namespace :vim do
  desc 'add a vim plugin as a submodule'
  task :add do
    require 'readline'
    require 'octokit'
    require 'pp'
    stty_save = %x`stty -g`.chomp
    trap('INT') { system 'stty', stty_save; exit }

    puts 'Creating a new submodule in vim/bundle/ from github'
    buf = Readline.readline('Github project: ', false)
    latest_tag = Octokit.tags(buf)[0]
    bundle_dir_name = buf.split('/')[1].sub(/(^vim-?|[-.]?vim$)/, '')
    if latest_tag
      puts "Adding vim project #{bundle_dir_name} v#{latest_tag[:name]}"
      sh "git submodule add https://github.com/#{buf} vim/bundle/#{bundle_dir_name} --branch #{latest_tag[:name]}"
    else
      puts "Adding vim project #{bundle_dir_name} HEAD"
      sh "git submodule add https://github.com/#{buf} vim/bundle/#{bundle_dir_name}"
    end

    sh "git commit -m 'New vim plugin: #{buf}'"
  end
end

desc 'Check for any extra elements missing from this system'
task :doctor => ['doctor:binaries', 'doctor:fonts']

namespace :doctor do

  desc 'Find any missing CLI tools to fully make me comfortable'
  task :binaries do

    %w[jq ag rg npm pip grc keychain go youtube-dl streamlink mpv pamu2fcfg wget
       curl vim nvim yarn irb].each do |binary|

      next if ENV['PATH'].split(':').any? do |path|
        File.exists? File.join(path, binary)
      end
      warn "Missing binary: #{binary}"
    end

  end

  desc 'Check that all of my favorite fonts are available'
  task :fonts do
    all_fonts = `fc-list`.split("\n")

    [
      /Noto Color Emoji/,
      /nerd font/i
    ].each do |font|
      next if all_fonts.any? font
      warn "Missing font matching #{font}"
    end
  end

end
