require 'etc'

desc 'Configure the system to find these dotfiles'
task :sysinstall do
  # rubygems has example regexes to match
  # https://github.com/rubygems/rubygems/blob/master/lib/rubygems/platform.rb#L85
  if RUBY_PLATFORM =~ /linux/
    Rake::Task["sysinstall:linux"].invoke
  elsif RUBY_PLATFORM =~ /darwin/
    Rake::Task["sysinstall:macos"].invoke
  end
end

namespace :sysinstall do
  task :linux do
    pp USER.uid
    # TODO: create a /etc/systemd/system/user@{uid}.service.d/xdg.conf file
    # and set XDG_CONFIG_DIR (and maybe ZDOTDIR?)
    # but also as sudo. (sub-sub command?)
  end

  task :macos do
    plist_path = File.join(ENV['HOME'], 'Library', 'LaunchAgents', "#{LAUNCHD_NAME}.plist")
    mkdir_p File.dirname plist_path
    system("launchctl bootout gui/#{USER.uid} #{plist_path}") if File.exist? plist_path

    File.open(plist_path, "w") do |file|
      file.puts LAUNCHD_PLIST
    end
    system("launchctl bootstrap gui/#{USER.uid} #{plist_path}")
  end
end

USER = Etc.getpwuid

LAUNCHD_NAME = 'net.nogweii.systemd-env'
# Launch ~/.local/bin/launchctl-set-systemd-env.rb when I log in to MacOS,
# causing a bunch of environment variables (see ./etc/environment.d/*) to be
# set for the session.
LAUNCHD_PLIST = %{<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>Label</key>
    <string>#{LAUNCHD_NAME}</string>
    <key>ProgramArguments</key>
    <array>
        <string>#{USER.dir}/.local/bin/launchctl-set-systemd-env.rb</string>
        <string>-x</string>
    </array>
    <key>RunAtLoad</key>
    <true/>
    <key>StandardOutPath</key>
    <string>#{USER.dir}/.local/tmp/systemd-env.stdout</string>
    <key>StandardErrorPath</key>
    <string>#{USER.dir}/.local/tmp/systemd-env.stderr</string>
</dict>
</plist>}
