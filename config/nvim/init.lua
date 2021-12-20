-- My neovim configuration!

-- The configuration is split up into multiple other files; see each of them
-- under the `lua/me/` subfolder.

-- A method to dump an object and print it out
function _G.dump(...)
    local objects = vim.tbl_map(vim.inspect, {...})
    print(unpack(objects))
end

-- Taking a function reference as a parameter, run the function and return a
-- table describing how long that took to run
function _G.profile(func)
  local start_time = vim.loop.hrtime() -- gets a nanosecond precision timestamp
  local return_value = func()
  local end_time = vim.loop.hrtime()

  return {
    start_time = start_time,
    return_value = return_value,
    end_time = end_time,
    duration = ((end_time - start_time) / 1e6) -- return as milliseconds
  }
end

require('impatient').enable_profile()

require('me.options')
require('me.desktop_notify')
require('me.plugins')
require('packer_compiled')
require('me.maps')
require('me.settings')
require('me.autocommands')
