desc "Initialize all submodules"
task :submodules do
  sh "git submodule update --init --recursive >/dev/null"
end

MAKE_DIRS = [
  File.expand_path("~/.local"),
  File.expand_path("~/.local/cache"),
  File.expand_path("~/.local/etc"),
  File.expand_path("~/.local/var"),
  File.expand_path("~/.local/share"),
  File.expand_path("~/.local/cache/zsh"),
  File.expand_path("~/.local/share/nvim/backup"),
  File.expand_path("~/.local/share/opentofu/plugins"),
  File.expand_path("~/.ssh/"),
  File.expand_path("~/.ssh/config.d/"),
  File.expand_path("~/.ssh/keys/"),
  File.expand_path("~/.ssh/c/"),
  File.expand_path("~/.ssh/tmp/")
]

File.open("etc/user-dirs.dirs").each_line do |user_dir|
  next if /^#/.match?(user_dir)

  expand_path = user_dir.gsub(%r{.*="\$HOME/(.*)"\n}, "#{ENV["HOME"]}/\\1")
  MAKE_DIRS << expand_path
end

MAKE_DIRS.sort!
MAKE_DIRS.uniq!

MAKE_DIRS.each do |dir|
  directory dir do
    mkdir_p dir
  end
end

desc "Prepare extra directories"
task prepare: MAKE_DIRS do
  ENV["TERMINFO"] = File.join(ENV["XDG_DATA_HOME"] || exp("~/.local/share"), "terminfo")
  debug "TERMINFO directory is '#{ENV["TERMINFO"]}'"

  unless File.exist? File.join(ENV["TERMINFO"], "s", "screen-256color-italic")
    sh "tic share/terminfo/screen-256color-italitc.termcap"
  end
  sh "tic share/terminfo/tmux-italics.termcap" unless File.exist? File.join(ENV["TERMINFO"], "t", "tmux-italics")

  kitty_macos_path = Pathname.new "/Applications/kitty.app/Contents/Resources/terminfo/78/xterm-kitty"
  kitty_terminfo_path = Pathname.new(ENV["TERMINFO"]) + kitty_macos_path.parent.basename + kitty_macos_path.basename
  if kitty_macos_path.exist? && !kitty_terminfo_path.exist?
    FileUtils.mkdir_p File.join(ENV["TERMINFO"], kitty_macos_path.parent.basename)
    FileUtils.symlink kitty_macos_path,
                      File.join(ENV["TERMINFO"], kitty_macos_path.parent.basename, kitty_macos_path.basename)
  end
end
