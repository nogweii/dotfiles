# frozen_string_literal: true

require 'rake/clean'
require 'pathname'

ENV['HOME'] = ENV['DOTFILES_HOME_DIR'] if ENV.key? 'DOTFILES_HOME_DIR'
@extra_information = (verbose == true) || (Rake.application.options.trace == true)

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

# Start with everything in this directory, but not recursively
DOTFILE_SOURCES = Dir['*'] +
  # And load everything under config/
  Dir['config/*']


# Don't symlink the following files.
# The files under config/ that are excluded are because I can specify their absolute path with ${DOTSDIR}
DOTFILE_EXCLUSIONS = %w[
  Rakefile README.md config Brewfile Brewfile.lock.json Gemfile
  Gemfile.lock _typos.toml LICENSE bin

  config/bundle config/inputrc config/npmrc config/psqlrc config/wgetrc
  config/ripgreprc config/gemrc config/dircolors config/atuin/
]

DOTFILES = (DOTFILE_SOURCES - DOTFILE_EXCLUSIONS).flatten.sort
DOTFILE_TARGETS = DOTFILES.inject({}) do |collection, val|
  collection.merge({exp("./#{val}") => exp("~/.#{val}")})
end
DOTFILE_TARGETS[exp("./bin")] = exp("~/bin")

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
desc 'Symlinks all my dotfiles'
task :dotfiles  do
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
  File.expand_path('~/.bin'),
]
OLD_CLEANUP.sort!
OLD_CLEANUP.uniq!

desc 'Delete no longer necessary files and directories'
task :unnecessary do
  # I'm deleting things, be loud about what that is
  RakeFileUtils.verbose_flag = true

  OLD_CLEANUP.each do |path|
    if File.exist? path or File.symlink? path
      debug "Deleting old path #{path}"
      rm_r path
    end
  end

  `git status --porcelain=2`.split("\n").grep(/^? zsh\/plugins/).each do |old_zsh|
    path = old_zsh.split(' ')[1]
    debug "Deleting zsh plugin #{old_zsh} that is no longer tracked"
    rm_r path
  end

  # Find various symlinks in my home directory that point to this folder but
  # wouldn't be recreated normally. (Often a result of a file that's been
  # deleted in the repo yet the symlink still remains)

  home_links = Dir.entries(File.expand_path('~')).select do |home_path|
    abs_path = File.join exp('~'), home_path
    File.symlink? abs_path and File.readlink(abs_path).start_with?(DOTFILES_DIR)
  end.map { |item| File.join exp('~'), item }

  config_links = Dir.entries(File.expand_path('~/.config')).select do |home_path|
    abs_path = File.join exp('~/.config'), home_path
    File.symlink? abs_path and File.readlink(abs_path).start_with?(DOTFILES_DIR)
  end.map { |item| File.join exp('~/.config'), item }

  ((home_links + config_links) - DOTFILE_TARGETS.values).each do |path|
    debug "Deleting unnecessary path #{path}"
    rm_r File.join(File.expand_path('~'), '.' + path)
  end
end

desc 'List of everything this rake file will try managing'
task :list do
  puts 'Symlink these files:'
  DOTFILE_TARGETS.each do |source, destination|
    puts " - #{source} => #{home_abs_path}"
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
  puts ''
  puts 'These files will not be symlinked:'
  DOTFILE_EXCLUSIONS.each do |thing|
    print " - #{thing}"
    if not File.exist? thing
      puts " (doesn't exist!)"
    else
      puts ''
    end
  end
end

desc 'Check for any extra elements missing from this system'
task :doctor => ['doctor:binaries', 'doctor:archlinux', 'doctor:macos']

namespace :doctor do
  desc 'Find any missing CLI tools to fully make me comfortable'
  task :binaries do
    %w[jq rg npm pip irb bundle grc go mpv trash wget curl nvim yarn fzf fd lsd
      neomutt docker ansible sudo tmux dfc ncdu git sqlite3 bundle pry
      shellcheck neovim-ruby-host nc trash kitty].each do |binary|

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

  mac_ns = namespace :macos do
    desc 'Check that all of my favorite fonts are available'
    task :fonts do
      all_fonts = `atsutil fonts -list`.split("\n")

      [
        /Symbols Nerd Font/i,
        /Source Code Pro/i,
      ].each do |font|
        next if all_fonts.any? font

        warn "Missing font matching #{font}"
      end
    end
  end

  desc 'Run ArchLinux-specific tasks'
  task :archlinux do
    if File.exist? "/etc/arch-release"
      arch_ns.tasks.each do |arch_task|
        arch_task.invoke
      end
    else
      debug "Skipping archlinux tasks because this does not look like an Arch system"
    end
  end

  desc 'Run MacOS specific tasks.'
  task :macos do
    if RbConfig::CONFIG["host_os"] =~ /darwin/
      mac_ns.tasks.each do |mac_task|
        mac_task.invoke
      end
    else
      debug "Skipping mac tasks because this does not look like a MacOS system"
    end
  end
end

desc 'Use vivid to generate dircolors'
task :dircolors do
  require 'erb'

  unless system('which vivid', {out: File::NULL})
    puts "vivid is not installed! no dircolors for you."
    next
  end
  vivid_colors = `vivid generate ./config/dircolors/bamboo.yml`.strip
  template = ERB.new File.open("./config/dircolors/zsh_template.erb").read
  File.open("./zsh/00_z_dircolors.zsh", "w") do |f|
    f.puts template.result(binding)
  end
end
