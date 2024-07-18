desc "Check for any extra elements missing from this system"
task :doctor => ["doctor:binaries", "doctor:archlinux", "doctor:macos"]

namespace :doctor do
  desc "Find any missing CLI tools to fully make me comfortable"
  task :binaries do
    %w[jq rg npm pip irb bundle grc go mpv trash wget curl nvim yarn fzf fd lsd
       neomutt docker ansible sudo tmux dfc ncdu git sqlite3 bundle pry
       shellcheck neovim-ruby-host nc kitty].each do |binary|
      next if ENV["PATH"].split(":").any? do |path|
        File.exist? File.join(path, binary)
      end
      warn "Missing binary: #{binary}"
    end

    # These neovim (or rather, my selected plugins) care about
    %w[luarocks git make unzip wget curl gzip tar bash sh cargo
       ruby python php go node composer gem java npm javac pip
       gh gcc bundle wl-copy pbcopy rg grep fd
    ].each do |binary|
      next if ENV["PATH"].split(":").any? do |path|
        File.exist? File.join(path, binary)
      end
      warn "Neovim will sad it can't find: #{binary}"
    end
  end

  arch_ns = namespace :archlinux do
    desc "Check that all of my favorite fonts are available"
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
      aether_key_fingerprint = "739AA6E3A03B25494C16379E65462C4BAE7384AD"
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

    desc "Check various Arch packages are installed"
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
    desc "Check that all of my favorite fonts are available"
    task :fonts do
      all_fonts = `atsutil fonts -list`.split("\n")

      [
        /Symbols Nerd Font/i,
        /Source Code Pro/i
      ].each do |font|
        next if all_fonts.any? font

        warn "Missing font matching #{font}"
      end
    end
  end

  desc "Run ArchLinux-specific tasks"
  task :archlinux do
    if File.exist? "/etc/arch-release"
      arch_ns.tasks.each do |arch_task|
        arch_task.invoke
      end
    else
      debug "Skipping archlinux tasks because this does not look like an Arch system"
    end
  end

  desc "Run MacOS specific tasks."
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
