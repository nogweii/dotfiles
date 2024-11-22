local M = {}

--- Add an entry to the list of things that must be installed.
--- Assumes a top-level option "ensure_installed" is a list-table.
---@param additional string|string[] The thing to add to the list
---@return function(any, PluginOpts)
M.add_ensure_installed = function(additional)
  if type(additional) ~= "table" then
    additional = { additional }
  end

  ---@param _plugin LazyPlugin
  ---@param opts PluginOpts
  ---@return PluginOpts
  return function(_plugin, opts)
    opts.ensure_installed = opts.ensure_installed or {}

    if vim.env.SSH_CONNECTION ~= nil and vim.env.SSH_CONNECTION ~= '' then
      -- Only ensure things are installed when running locally
      vim.list_extend(opts.ensure_installed, additional)
    end

    return opts
  end
end

--- Configure a none-ls tool to use a configuration file in my dotfiles if
--- there is no project-specific config.
---@param project_confs string[] the list of filenames that indicate project specific config
---@param fallback_filename string what filename (under ~nvim/linters/) to use in case
---@return nil|string # the path to the global fallback config file if it should be used, nil otherwise
M.alternative_config_file = function(project_confs, fallback_filename)
  local utils = require("null-ls.utils")
  local cond = utils.make_conditional_utils()
  if not cond.root_has_file(project_confs) then
    return vim.fs.joinpath(vim.fn.stdpath("config"), "linters", fallback_filename)
  end
end

return M
