return {
  settings = {
    yaml = {
      schemaStore = {
        -- disable the schemas shipped with the LSP to use the schemastore plugin
        -- which can be configured in more ways than a basic on/off
        enable = false,
        url = '',
      },

      validate = true,

      -- I use yamlfmt, yamlls uses prettier. hm....
      format = { enabled = false },
      keyOrdering = false,
    },
    redhat = { telemetry = { enabled = false } },
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
      -- looks like a helm chart's template, detach soon after
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

    vim.api.nvim_create_user_command('YamlChangeSchema', 'lua require("yaml-companion").open_ui_select()', {})
  end,

  -- Lazily configure yamlls with schemastore and yaml-companion.
  -- This way I don't immediately load these two plugins at neovim
  -- start (since this whole file is *not* lazily loaded)
  on_new_config = function(new_config, _root_dir)
    local schemastore = require('schemastore')
    local yaml_companion = require('yaml-companion')

    local yamlls_with_schemas = vim.tbl_deep_extend('keep', new_config, {
      settings = {
        yaml = {
          schemas = schemastore.yaml.schemas(),
        },
      },
    })

    local companion_schemas = vim.tbl_map(function(schema)
      return { name = schema.name, uri = schema.url }
    end, schemastore.json.schemas())

    return vim.tbl_deep_extend('keep', yamlls_with_schemas, yaml_companion.setup({ schemas = companion_schemas }))
  end,
}

-- TODO: do I add a hover text filter for html escape codes? https://github.com/hazelweakly/nixos-configs/blob/main/dots/nvim/lua/_/lsp/yamlls.lua#L4-L26
--  and https://github.com/redhat-developer/yaml-language-server/pull/844
-- TODO: add a bunch of yaml schema mappings that aren't shipped in schemastore
--    e.g. https://github.com/arsham/shark/blob/9888a40f7fde3a0ccb51fe073799974c3b52a312/lua/plugins/lsp/config/schemas.lua
--    also, `.gitlab/ci/*.yaml` should be registered as gitlab-ci schema
-- TODO: extend the anti-yamlls detection to scan the file for {{ }} - https://github.com/towolf/vim-helm/issues/15
