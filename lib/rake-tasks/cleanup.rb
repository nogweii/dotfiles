OLD_CLEANUP = [
  File.expand_path('~/.local/share/nvim/site/pack/packer'),
  File.expand_path('./etc/nvim/lua/packer_compiled.lua'),
  File.expand_path('~/.vim'),
  File.expand_path('~/.local/share/vim'),
  File.expand_path('~/.bin'),
  File.expand_path('~/.ssh/conf.d/'),
  File.expand_path('~/.ssh/ansible_inventory.ssh_config'),
  File.expand_path('~/.ssh/tmp/'),
  File.expand_path('~/bin'),
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

  home_links = []
  ["~", "~/.config", "~/.local/share", "~/.local/etc"].each do |scan_target|
    home_links << Dir.entries(exp(scan_target)).select do |home_path|
      abs_path = File.join exp(scan_target), home_path
      File.symlink? abs_path and File.readlink(abs_path).start_with?(DOTFILES_DIR)
    end.map { |item| File.join exp(scan_target), item }
  end

  (home_links.flatten.uniq - DOTFILE_TARGETS.values).each do |path|
    debug "Deleting unnecessary path #{path}"
    rm_r path
  end
end
