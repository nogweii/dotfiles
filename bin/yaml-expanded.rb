#!/usr/bin/env ruby

# frozen_string_literal: true

require "yaml"
require "json"

if ARGV[0]
  path = ARGV[0]
else
  warn "You need to specify a file"
  exit 1
end

original_yaml = YAML.load_file(path, aliases: true)
json_vers = original_yaml.to_json
expanded_yaml = YAML.load(json_vers)
puts expanded_yaml.to_yaml
