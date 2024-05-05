---@type lspconfig.Config
return {
  settings = {
    Lua = {
      workspace = {
        checkThirdParty = false,
      },
      completion = {
        callSnippet = 'Replace',
      },
      diagnostics = {
        -- Don't complain about unused local variables if they begin with an underscore
        unusedLocalExclude = { '_*' },
      },
      -- Do not send telemetry data containing a randomized but unique identifier
      telemetry = {
        enable = false,
      },
    },
  },
  on_new_config = function(new_config, root_dir)
    require('neodev.lsp').on_new_config(new_config, root_dir)
  end,
}
