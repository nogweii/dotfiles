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

    local new_list = vim.iter({ opts.ensure_installed, additional }):flatten():totable()
    opts.ensure_installed = vim.fn.uniq(new_list)

    return opts
  end
end

return M
