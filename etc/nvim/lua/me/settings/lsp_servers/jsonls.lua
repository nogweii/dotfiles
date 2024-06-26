local setup_options = {}

if vim.fn.executable('vscode-json-languageserver') == 1 then
  -- the Arch Linux package installs the language server with a slightly
  -- different name in $PATH
  setup_options['cmd'] = { 'vscode-json-languageserver', '--stdio' }
end

setup_options['settings'] = {
  json = {
    validate = { enable = true },
  },
}

-- only load SchemaStore.nvim when the LSP config is finally being built
setup_options['on_new_config'] = function(new_config)
  new_config = vim.tbl_deep_extend('force', new_config or {}, {
    settings = {
      json = {
        schemas = require('schemastore').json.schemas(),
      },
    },
  })
end

return setup_options
