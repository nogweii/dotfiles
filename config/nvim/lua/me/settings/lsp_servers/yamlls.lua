local schemas = require('schemastore').json.schemas()

local setup_options = {
  settings = {
    yaml = {
      schemaStore = {
        -- disable the schemas shipped with the LSP to use the schemastore plugin
        -- which can be configured in more ways than a basic on/off
        enable = false,
      },
      schemas = schemas,
    },
  },

  --- Ran whenever yamlls attaches to a buffer, look at it and see if it is a file
  --- that would not really work well with the LSP. Stuff like Helm, Jinja, and Go
  --- template files are examples of that where it would be better to disable
  --- the LSP instead.
  ---@param client vim.lsp.client
  ---@param bufnr number
  on_attach = function(client, bufnr)
    local filename = vim.api.nvim_buf_get_name(bufnr)
    if filename:match('charts/.+/templates/.+%.ya?ml$') then
      vim.defer_fn(function()
        vim.lsp.buf_detach_client(bufnr, client.id)
      end, 100)
    end
  end,
}

return setup_options
