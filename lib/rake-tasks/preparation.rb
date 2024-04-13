desc 'Initialize all submodules'
task :submodules do
  sh 'git submodule update --init --recursive >/dev/null'
end

MAKE_DIRS = [
  File.expand_path('~/.local'),
  File.expand_path('~/.local/cache'),
  File.expand_path('~/.local/etc'),
  File.expand_path('~/.local/var'),
  File.expand_path('~/.local/share'),
  File.expand_path('~/.local/cache/zsh'),
  File.expand_path('~/.local/share/nvim/backup'),
]

File.open('etc/user-dirs.dirs').readlines.each do |user_dir|
  next if user_dir =~ /^#/
  expand_path = user_dir.gsub(/.*="\$HOME\/(.*)"\n/, "#{ENV['HOME']}/\\1")
  MAKE_DIRS << expand_path
end

MAKE_DIRS.sort!
MAKE_DIRS.uniq!

MAKE_DIRS.each do |dir|
  directory dir do
    mkdir_p dir
  end
end

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
