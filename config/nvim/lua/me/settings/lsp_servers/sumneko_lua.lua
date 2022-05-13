local buffer_file_name = vim.fn.resolve(vim.fn.expand("%:p"))

local setup_options = {}

-- check if the Lua buffer opened just now is related neovim
-- (a config file underneath the correct directory)
local directories = vim.split(vim.o.packpath, ",")
local is_a_neovim_dir = false
for _, dir in pairs(directories) do
  local dir_absolute = vim.fn.resolve(dir)

  if string.sub(buffer_file_name, 1, string.len(dir_absolute)) == dir_absolute then
    is_a_neovim_dir = true
    break
  end
end

if is_a_neovim_dir then
  -- yup, so load the awesome settings from folke to teach the LSP about
  -- neovim's APIs
  return require("lua-dev").setup(setup_options)
else
  -- not a neovim config file, so no special settings
  return setup_options
end
