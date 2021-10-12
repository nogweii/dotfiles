# Various gems I use on every system

source 'https://rubygems.org'

ruby '~> 3'

gem 'uniscribe'
gem 'md2man'
gem 'binman'

gem 'tty'
gem 'rainbow'

# These gems I use within irb. Check config/irb/irb_conf.rb for details.
group :irb do
  gem 'ruby-terminfo'
  gem 'racksh'
  gem 'net-http-spy'

  gem 'pry' # because it's so damn neat and useful

  gem 'irbtools'
  gem 'irbtools-more'
  gem 'looksee'
end

group :development do
  gem 'octokit'
  gem 'rubocop'
end

# These gems I like to have available for my machine or are commonly installed.
group :common do
  gem 'faraday'
  gem 'patron'
  gem 'typhoeus'
  gem 'nokogiri'
  gem 'yard'
end

# This is the ruby Language Server -- bringing IDE functionality to every editor
gem 'solargraph'

# Neovim gem enables ruby support in neovim
gem 'neovim'

# better tags generation using Ripper, the Ruby parser library
gem "ripper-tags"
