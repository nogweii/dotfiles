# Various gems I use on every system

source "https://rubygems.org"

ruby "~> 3"

gem "binman"
gem "md2man"
gem "rainbow"
gem "tty"
gem "uniscribe"

gem "net-ntp"
gem "optimist"

# These gems I use within irb. Check config/irb/irb_conf.rb for details.
group :irb do
  gem "irb"
  gem "irbtools"
  gem "looksee"
  gem "net-http-spy"
  gem "pry" # because it's so damn neat and useful
  gem "racksh"
end

group :development do
  gem "octokit"
  gem "rubocop"
  gem "standard"

  # This is the ruby Language Server -- bringing IDE functionality to every editor
  gem "ruby-lsp"
  gem "solargraph"
end

# These gems I like to have available for my machine or are commonly installed.
group :common do
  gem "faraday"
  gem "nokogiri"
  gem "patron"
  gem "typhoeus"
  gem "yard"
end

# Neovim gem enables ruby support in neovim
gem "neovim"

# better tags generation using Ripper, the Ruby parser library
gem "ripper-tags"

gem "rufo"
