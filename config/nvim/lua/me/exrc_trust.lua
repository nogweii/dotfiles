-- Automaticly trust .nvim.lua files in my dotfiles repo, as well as my projects in ~/code/

--#region This section copied from nvim lua code
-- See nvim's runtime in lua/vim/secure.lua (or online: https://github.com/neovim/neovim/blob/master/runtime/lua/vim/secure.lua)
-- spdx-license-identifier: Apache-2.0

--- Reads trust database from $XDG_STATE_HOME/nvim/trust.
---
---@return table<string, string> Contents of trust database, if it exists. Empty table otherwise.
local function read_trust()
  local trust = {} ---@type table<string, string>
  local f = io.open(vim.fn.stdpath('state') .. '/trust', 'r')
  if f then
    local contents = f:read('*a')
    if contents then
      for line in vim.gsplit(contents, '\n') do
        local hash, file = string.match(line, '^(%S+) (.+)$')
        if hash and file then
          trust[file] = hash
        end
      end
    end
    f:close()
  end
  return trust
end

--- Writes provided {trust} table to trust database at
--- $XDG_STATE_HOME/nvim/trust.
---
---@param trust table<string, string> Trust table to write
local function write_trust(trust)
  vim.validate({ trust = { trust, 't' } })
  local f = assert(io.open(vim.fn.stdpath('state') .. '/trust', 'w'))

  local t = {} ---@type string[]
  for p, h in pairs(trust) do
    t[#t + 1] = string.format('%s %s\n', h, p)
  end
  f:write(table.concat(t))
  f:close()
end

--#endregion

local nvim_lua_config = vim.fn.getcwd() .. '/.nvim.lua'

if
  (not vim.startswith(nvim_lua_config, vim.env.HOME .. '/code/'))
  and (not vim.startswith(nvim_lua_config, vim.env.HOME .. '/dotfiles/'))
then
  return
end

if vim.fn.filereadable(nvim_lua_config) then
  local trust = read_trust()

  local contents ---@type string?
  do
    local f = io.open(nvim_lua_config, 'r')
    if not f then
      return nil
    end
    contents = f:read('*a')
    f:close()
  end
  local hash = vim.fn.sha256(contents)

  trust[nvim_lua_config] = hash

  if trust[nvim_lua_config] ~= hash then
    trust[nvim_lua_config] = hash
    write_trust(trust)
    vim.notify(nvim_lua_config .. ' has changed, updated trust store', 'info')
  elseif not trust[nvim_lua_config] then
    trust[nvim_lua_config] = hash
    write_trust(trust)
    vim.notify('New exrc file found: ' .. nvim_lua_config .. ', trusting it!', 'info')
  end
end
