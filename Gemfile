# Various gems I use on every system

source 'https://rubygems.org'

ruby '~> 2.4'

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

  gem 'irbtools', "~> 3.0.2"
  gem 'irbtools-more', '~> 2.4.0'
  gem 'looksee', "~> 4.2.0"
end

group :development do
  gem 'octokit', "~> 4.0"
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
