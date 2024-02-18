# frozen_string_literal: true

require 'rake/clean'
require 'pathname'

ENV['HOME'] = ENV['DOTFILES_HOME_DIR'] if ENV.key? 'DOTFILES_HOME_DIR'
@extra_information = (verbose == true) || (Rake.application.options.trace == true)

def debug(message)
  puts message if @extra_information
end

# cd to the top of the git repo
gitdir = `git rev-parse --show-toplevel`.strip
Dir.chdir gitdir

# Only show shell commands if we didn't run rake with -v or -t
RakeFileUtils.verbose_flag = false unless @extra_information

DOTFILES = [
  # Start with everything in this directory, but not recursively
  Dir['*'] -
  # Don't symlink the following
  %w[Rakefile README.md config Brewfile Brewfile.lock.json Gemfile Gemfile.lock xdg-data LICENSE] +
  # Add these extra to the list to be symlink'd
  %w[config/git config/conky config/nvim config/krb5_ipa.conf config/alacritty config/pylint.rc.toml config/gemrc config/tmux config/irb config/fd config/kitty config/yamlfmt]
].flatten.sort

task default: [:submodules, :prepare, :dotfiles, :unnecessary]

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
desc 'Symlinks all my dotfiles'
task :dotfiles  do
  DOTFILES.each do |dotfile|
    # Absolute path to where we're going to symlink in $HOME
    home_abs_path = File.expand_path "~/.#{dotfile}"

    # Absolute path to the real file, stored in the repository
    repo_abs_path = File.expand_path "./#{dotfile}"

    if not File.exist? home_abs_path
      symlink repo_abs_path, home_abs_path
    elsif File.symlink?(home_abs_path) and File.readlink(home_abs_path) != repo_abs_path
      rm_r home_abs_path
      symlink repo_abs_path, home_abs_path
    elsif not File.symlink?(home_abs_path)
      warn "File '#{home_abs_path}' exists but is not a symlink. Not touching it!"
    end
  end
end

desc 'Initialize all submodules'
task :submodules do
  sh 'git submodule update --init --recursive >/dev/null'
end

# All the submodules
# File.foreach('.gitmodules') {|line| print $1 if line =~ /^\[submodule
# \"(.*)\"\]/ }

MAKE_DIRS = [
  File.expand_path('~/.local'),
  File.expand_path('~/.local/cache'),
  File.expand_path('~/media'),
  File.expand_path('~/.local/cache/zsh'),
  File.expand_path('~/.local/share/nvim/backup'),
]
MAKE_DIRS.sort!
MAKE_DIRS.uniq!

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

  kitty_macos_path = Pathname.new '/Applications/kitty.app/Contents/Resources/terminfo/78/xterm-kitty'
  kitty_terminfo_path = Pathname.new(ENV['TERMINFO']) + kitty_macos_path.parent.basename + kitty_macos_path.basename
  if kitty_macos_path.exist? && !kitty_terminfo_path.exist?
    FileUtils.mkdir_p File.join(ENV['TERMINFO'], kitty_macos_path.parent.basename)
    FileUtils.symlink kitty_macos_path, File.join(ENV['TERMINFO'], kitty_macos_path.parent.basename, kitty_macos_path.basename)
  end
end

OLD_CLEANUP = [
  File.expand_path('~/.local/share/nvim/site/pack/packer'),
  File.expand_path('./config/nvim/lua/packer_compiled.lua'),
  File.expand_path('~/.vim'),
  File.expand_path('~/.local/share/vim'),
]
OLD_CLEANUP.sort!
OLD_CLEANUP.uniq!

desc 'Delete no longer necessary files and directories'
task :unnecessary do
  # I'm deleting things, be loud about what that is
  RakeFileUtils.verbose_flag = true

  OLD_CLEANUP.each do |path|
    rm_r path if File.exist? path or File.symlink? path
  end

  # Find various symlinks in my home directory that point to this folder but
  # wouldn't be recreated normally. (Often a result of a file that's been
  # deleted in the repo yet the symlink still remains)

  home_links = Dir.entries(File.expand_path('~')).select do |home_path|
    abs_path = File.join File.expand_path('~'), home_path
    File.symlink? abs_path and File.readlink(abs_path).start_with?(File.expand_path('.'))
  end.map { |item| item.sub(/^\./, '') }

  config_links = Dir.entries(File.expand_path('~/.config')).select do |home_path|
    abs_path = File.join File.expand_path('~/.config'), home_path
    File.symlink? abs_path and File.readlink(abs_path).start_with?(File.expand_path('.'))
  end.map { |item| 'config/' + item.sub(/^\./, '') }

  ((home_links + config_links) - DOTFILES).each do |path|
    rm_r File.join(File.expand_path('~'), '.' + path)
  end
end

desc 'List of everything this rake file will try managing'
task :list do
  puts 'Symlink these files:'
  DOTFILES.each do |file|
    home_abs_path = File.expand_path "~/.#{file}"
    puts " - #{file} => #{home_abs_path}"
  end
  puts ''
  puts 'Create these directories:'
  MAKE_DIRS.each do |dir|
    puts " - #{dir}"
  end
  puts ''
  puts 'Clean up these old things:'
  OLD_CLEANUP.each do |thing|
    puts " - #{thing}"
  end
end

desc 'Check for any extra elements missing from this system'
task :doctor => ['doctor:binaries', 'doctor:archlinux']

namespace :doctor do
  desc 'Find any missing CLI tools to fully make me comfortable'
  task :binaries do
    %w[jq rg npm pip irb bundle grc go youtube-dl streamlink mpv pamu2fcfg
    wget curl nvim yarn fzf fd lsd neomutt docker ansible sudo tmux
    dfc ncdu git sqlite3 ksshaskpass cryfs ctags bundle pry
    shellcheck neovim-ruby-host nc youtube-dl zk].each do |binary|

      next if ENV['PATH'].split(':').any? do |path|
        File.exist? File.join(path, binary)
      end
      warn "Missing binary: #{binary}"
    end
  end

  arch_ns = namespace :archlinux do
    desc 'Check that all of my favorite fonts are available'
    task :fonts do
      all_fonts = `fc-list`.split("\n")

      [
        /Noto Color Emoji/,
        /nerd font/i,
        /Fira Sans:style=Regular/,
        /Fira Code:style=Regular/
      ].each do |font|
        next if all_fonts.any? font

        warn "Missing font matching #{font}"
      end
    end

    task :repo_key do
      aether_key_fingerprint = '739AA6E3A03B25494C16379E65462C4BAE7384AD'
      `pacman-key -f '#{aether_key_fingerprint}' >/dev/null 2>&1`
      if $?.exitstatus == 1
        puts "No trust of aether-aur repo PGP key. Run pacman-key to trust it:"
        puts "  sudo pacman-key --keyserver 'hkps://keys.openpgp.org' -r '#{aether_key_fingerprint}'"
        puts "  sudo pacman-key --keyserver 'hkps://keys.openpgp.org' --lsign-key '#{aether_key_fingerprint}'"
      else
        debug "GPG key #{aether_key_fingerprint} is trusted"
      end
    end

    task :repo do
      f = File.open("/etc/pacman.conf")
      found = false
      f.each_line do |line|
        found = true if line == "[aether-aur]\n"
      end

      puts "aether-aur repository configuration missing in pacman.conf" unless found
    end

    desc 'Check various Arch packages are installed'
    task :packages do
      [
        "lsd",
        "alacritty",
        # "base-devel",
        "mpv",
        "pacman-contrib",
        "python-pynvim",
        "noto-fonts-emoji",
        "ttf-nerd-fonts-symbols",
        "ttf-cascadia-code",
        "ttf-fira-code",
        "otf-fira-sans",
        "ripgrep",
        "jq",
        "fzf",
        "tmux",
        "docker",
        "shellcheck",
        "openbsd-netcat"
      ].each do |package_name|
        if not system("pacman -Qiq #{package_name} >/dev/null 2>&1")
          puts "Package #{package_name} not installed"
        end
      end
    end
  end

  desc 'Run archlinux specific tasks, but only on arch systems.'
  task :archlinux do
    if File.exist? "/etc/arch-release"
      arch_ns.tasks.each do |arch_task|
        arch_task.invoke
      end
    else
      debug "Skipping archlinux tasks because this does not look like an Arch system"
    end
  end
end
