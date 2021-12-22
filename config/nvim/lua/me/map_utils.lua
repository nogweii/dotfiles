_M = {}

-- A little utility function to make nvim_set_keymap a bit more ergonomic
function _M.map(args)
  args = vim.tbl_extend("keep", args, {
    mode = "", -- The vim mode for this map
    keys = nil, -- The input sequence of keys to activate this mapping
    to = nil, -- Sequence to keys to 'type' into the editor

    recurse = false, -- set to true to not include noremap
    silent = true, -- set to false to not include <silent>, e.g. the map will not be echoed to the command line
    expression = false, -- set to true if the output is to be evaluated rather than typed
    plugins = false, -- set to true if the mapping requires plugins and should be disabled when packer wasn't loaded
  })

  if (args.plugins) and (not packer_exists) then
    return
  end

  vim.api.nvim_set_keymap(args.mode, args.keys, args.to, { noremap = not args.recurse, silent = args.silent, expr = args.expression })
end

-- A wrapper around map for a common pattern of binding to a <Plug>
function _M.plug_map(args)
  _M.map{keys = args.keys, to = '<Plug>(' .. args.command .. ')', mode = args.mode or 'n', silent = true, recurse = true, plugins = true}
end

-- A wrapper around map for a common pattern of binding to a :command
function _M.cmd_map(args)
  _M.map{keys = args.keys, to = '<cmd>' .. args.command .. '<CR>', mode = args.mode or 'n', silent = true, plugins = args.plugins == nil and true or args.plugins}
end

return _M
