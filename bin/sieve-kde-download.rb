#!/usr/bin/env ruby

require "bundler/inline"
require "pathname"
require "fileutils"

gemfile do
  source "https://rubygems.org"
  gem "ruby-managesieve", require: "managesieve"
  gem "iniparse"
  gem "ruby-dbus", require: "dbus"
end

kde_sieve_editor_config = IniParse.parse(File.read("#{ENV["XDG_CONFIG_HOME"]}/sieveeditorrc"))
server_config = kde_sieve_editor_config["ServerSieve 0"]
sieve_secret_name = "sieveeditor/#{server_config["UserName"]}@#{server_config["ServerName"]}"

# Connect to the Secrets Service
dbus_secrets_service = DBus.session_bus["org.freedesktop.secrets"]
secrets_service = dbus_secrets_service["/org/freedesktop/secrets"]
session = secrets_service["org.freedesktop.Secret.Service"].OpenSession("plain", "")
secrets_collection = dbus_secrets_service["/org/freedesktop/secrets/aliases/default"]["org.freedesktop.Secret.Collection"]

# Search through all of the items in the default collection
password = nil
secrets_collection["Items"].each do |secret_dbus_path|
  secret = dbus_secrets_service[secret_dbus_path]["org.freedesktop.Secret.Item"]
  if secret["Label"] == sieve_secret_name
    # Found it based on the label, now retrieve the bytes of the secret
    password = secret.GetSecret(session[1])[2].pack("C*")
    break
  end
end

# whew, I finally have everything to connect to the sieve server:
sieve = ManageSieve.new({
  host: server_config["ServerName"],
  port: server_config["Port"].to_i,
  user: server_config["UserName"],
  password: password,
  auth: "PLAIN"
})

# Then download the script marked as active
active_script_name = sieve.scripts.find { |s| s[1] == "ACTIVE" }[0]

# Possible sieve folder paths:
# - ~/.dovecot.sieve/
# - ~/sieve/
# - 'sieve' folder underneath Maildir
# - somewhere else I choose
#@sieve_path = Pathname.new("#{ENV['HOME']}") / '.dovecot.sieve'
#@sieve_path = Pathname.new("#{ENV['HOME']}") / 'mail' / '.sieve'
@sieve_path = Pathname.new("#{ENV["XDG_CONFIG_HOME"]}") / "sieve"
FileUtils.mkdir_p @sieve_path

ESC = "\u001B["
OSC = "\u001B]"
BEL = "\u0007"

def update_message(script_name)
  hyperlink = [OSC, "8", ";", ";", "file:///#{@sieve_path / (script_name + ".sieve")}", BEL, script_name + ".sieve", OSC, "8", ";", ";", BEL].join

  puts "Downloaded #{hyperlink}"
end

sieve.scripts.each do |script|
  script_name = script[0]
  active_status = script[1] == "ACTIVE"
  script_contents = sieve.get_script(script_name)
  script_path = @sieve_path / (script_name + ".sieve")

  File.open(script_path, "w") do |fp|
    fp.write(script_contents)
  end

  # Provide a symlink to automatically run the active script
  FileUtils.symlink(script_path, @sieve_path / ".active.sieve", force: true) if active_status

  update_message script_name
end
