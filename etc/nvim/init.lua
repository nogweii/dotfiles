-- My neovim configuration!

-- The configuration is split up into multiple other files; see each of them
-- under the `lua/me/` subfolder.

-- A method to dump an object and print it out
function _G.dump(...)
  local objects = vim.tbl_map(vim.inspect, { ... })
  print(unpack(objects))
end

vim.loader.enable()

require('me.options')
-- require('me.desktop_notify')
require('me.maps')
require('me.settings')
require('me.exrc_trust')
require('me.autocommands').create_au()
require('plugin_manager')
