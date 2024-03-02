local schemas = require('schemastore')

local setup_options = {
  settings = {
    yaml = {
      schemaStore = {
        -- disable the schemas shipped with the LSP to use the schemastore plugin
        -- which can be configured in more ways than a basic on/off
        enable = false,
        -- url = 'https://www.schemastore.org/api/json/catalog.json',
        url = '',
      },
      schemas = schemas.yaml.schemas(),

      -- I use yamlfmt, yamlls uses prettier. hm....
      format = { enabled = false },
      keyOrdering = false,
    },
  },

  -- Tell yamlls that we do, in fact, support line folding
  capabilities = {
    textDocument = {
      foldingRange = {
        dynamicRegistration = false,
        lineFoldingOnly = true,
      },
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

    vim.api.nvim_create_user_command('YamlCurrentSchema', function()
      local schema = require('yaml-companion').get_buf_schema(0)
      if schema.result[1].name == 'none' then
        print('No schema loaded')
        return
      end
      print('Current schema is ' .. schema.result[1].name)
    end, {})

    -- TODO: implement this correctly; actually set the schema. not just present a selector and do nothing
    vim.api.nvim_create_user_command('YamlChangeSchema', 'lua require("yaml-companion").open_ui_select()', {})
  end,
}

-- TODO: do I want to add lazy loading of the schemas? https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/plugins/extras/lang/yaml.lua#L36C1-L43C15
-- TODO: do I add a hover text filter for html escape codes? https://github.com/hazelweakly/nixos-configs/blob/main/dots/nvim/lua/_/lsp/yamlls.lua#L4-L26
--  and https://github.com/redhat-developer/yaml-language-server/pull/844
-- TODO: add a bunch of yaml schema mappings that aren't shipped in schemastore
--    e.g. https://github.com/arsham/shark/blob/9888a40f7fde3a0ccb51fe073799974c3b52a312/lua/plugins/lsp/config/schemas.lua
--    also, `.gitlab/ci/*.yaml` should be registered as gitlab-ci schema
-- TODO: extend the anti-yamlls detection to scan the file for {{ }} - https://github.com/towolf/vim-helm/issues/15

return require('yaml-companion').setup({
  lspconfig = setup_options,
  schemas = vim.tbl_map(function(schema)
    return { name = schema.name, uri = schema.url }
  end, schemas.json.schemas()),
})
