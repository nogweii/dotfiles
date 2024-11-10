-- Automatically load .nvim.lua files in my dotfiles repo, as well as my projects in ~/code/
-- RISKY! 'exrc' is not enabled by default for a reason.

local nvim_lua_config = vim.fn.getcwd() .. '/.nvim.lua'

if
    (vim.startswith(nvim_lua_config, vim.env.HOME .. '/code/')
      or vim.startswith(nvim_lua_config, vim.env.HOME .. '/dotfiles/'))
    and (vim.fn.filereadable(nvim_lua_config) == 1)
then
  dofile(nvim_lua_config)
end
