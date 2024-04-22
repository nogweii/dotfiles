#!/usr/bin/env ruby

# Parse a bunch of environment.d(5) files to get a dictionary of final values.
# Then, spit out a series of launchctl commands to set those environment variables
# in a MacOS context.
#
# Yup, this is a compatibility shim for loading all of the systemd environment
# variable config files in a MacOS system. Because it's meant to be so early in
# the boot process, make sure this file is compatible with **Ruby 2.6**. Yes,
# that old. MacOS ships a very old version and won't update it.
#
# Big thanks to the dotenv ruby library, these regexes are based on it!
# SPDX-License-Identifier: MIT

require 'shellwords'

LINE = /
      (?:^|\A)              # beginning of line
      \s*                   # leading whitespace
      ([^\#][A-Z0-9_]+)          # key
      (?:\s*=\s*?|:\s+?)    # separator
      (                     # optional value begin
        \s*'(?:\\'|[^'])*'  #   single quoted value
        |                   #   or
        \s*"(?:\\"|[^"])*"  #   double quoted value
        |                   #   or
        [^\#\r\n]+          #   unquoted value
      )?                    # value end
      \s*                   # trailing whitespace
      (?:$|\z)              # end of line
    /xi

VARIABLE = /
  (?<!\\)\$          # literal $ that is not prefixed with a backslash
  (?:
    (?<varname>[A-Z0-9_]+) # basic variable reference
  |
    \{           # opening brace wrapping
    (?<varname>[A-Z0-9_]+) # variable name
    (?:
      (?<colontype>:[+-]) # the type of replacement string
      (?<altvalue>.*) # the inner value of said replacement string
    )?
    \}+?           # closing brace
  )
/xi

# This is a hash that contains all of the environment variables we look up
# values for
source_env = ENV.to_h
source_env['HOME'] = Dir.home
# And this is final set of values that will be printed out, ones that were
# specified in the files
result_env = {}

Dir[File.join(Dir.home, ".local", "etc", "environment.d", "*.conf")].sort.each do |env_file|
  File.open(env_file, 'r').read.scan(LINE) do |env_var, env_value|
    # Remove surrounding quotes
    env_value = env_value.strip.sub(/\A(['"])(.*)\1\z/m, '\2')
    # Replace literal \n and \r with enough backslashes that they'll get through everything else
    env_value = env_value.gsub('\n', "\\\\\\n").gsub('\r', "\\\\\\r")
    # Unescape any not-$ characters
    env_value.gsub!(/\\([^$])/, '\1')

    while VARIABLE.match? env_value
      env_value.gsub!(VARIABLE) do |var|
        match = Regexp.last_match
        if not match[:colontype]
          source_env.fetch(match[:varname], '')
        elsif match[:colontype] == ":-"
          if source_env.has_key?(match[:varname]) and not source_env[match[:varname]].nil? and not source_env[match[:varname]].empty?
            # there is a useful string in the environment named :varname, use it
            source_env[match[:varname]]
          else
            # guess it isn't set, use the alternative string provided instead
            match[:altvalue]
          end
        elsif match[:colontype] == ":+"
          if source_env.has_key?(match[:varname]) and not source_env[match[:varname]].nil? and not source_env[match[:varname]].empty?
            # there is a valid environment variable named :varname, which means
            # we need to use the alternate value instead
            match[:altvalue]
          else
            # there is no such variable named :varname, so return an empty string
            ''
          end
        end
      end
    end

    result_env[env_var] = env_value
    source_env[env_var] = env_value
  end
end

result_env.each do |var, value|
  if ARGV[0].nil? || ARGV[0] == "-h"
    puts "does a bunch of `launchctl setenv` calls based on environment.d/"
    puts "  -p => print what it would do"
    puts "  -x => execute launchctl a lot"
    exit 0
  elsif ARGV[0] == "-p"
    puts "launchctl setenv #{var} #{Shellwords.escape(value)}"
  elsif ARGV[0] == "-x"
    system("launchctl", "setenv", var, value)
  else
    puts "i don't understand. run #{$0} -h"
    exit 2
  end
end
