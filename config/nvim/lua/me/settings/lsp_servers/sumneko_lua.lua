-- check if the Lua buffer opened just now is related neovim
-- (a config file underneath the correct directory)
local neovim_parent_dir = vim.fn.resolve(vim.fn.stdpath("config"))
local buffer_file_name = vim.fn.expand("%:p")

-- TODO: check if the config file is beneath ~/.config/neovim OR ~/dotfiles/config/neovim

if string.sub(buffer_file_name, 1, string.len(neovim_parent_dir)) == neovim_parent_dir then
  -- yup, so load the awesome settings from folke to teach the LSP about
  -- neovim's APIs
  return require("lua-dev").setup({})
else
  -- not a neovim config file, so no special settings
  return {}
end
