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
  before_init = require('neodev.lsp').before_init,
}
