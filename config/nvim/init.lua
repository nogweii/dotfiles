-- My neovim configuration!

-- The configuration is split up into multiple other files; see each of them
-- under the `lua/me/` subfolder.

-- A method to dump an object and print it out
function _G.dump(...)
    local objects = vim.tbl_map(vim.inspect, {...})
    print(unpack(objects))
end

require('me.options')
require('me.plugins')
require('me.maps')
require('me.settings')
require('me.autocommands')
require('me.desktop_notify')
