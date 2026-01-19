#!/usr/bin/env ruby

# This script is meant to be used in conjunction with go-hass-agent as an additional
# control

# Toggle the screen using DPMS via `kscreen-doctor` based on the first argument
if ARGV[0] == "ON"
  `kscreen-doctor --dpms on`
  sleep 1
elsif ARGV[0] == "OFF"
  `kscreen-doctor --dpms off`
  sleep 1
end

# Then report the new status to stdout, with the assumption that any line
# ending in 'off' means all of the screens are off
dpms_status = `kscreen-doctor --dpms show`
sensor_state = !/off$/m.match?(dpms_status)
puts sensor_state ? "ON" : "OFF"
