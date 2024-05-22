local add_ensure = require('me.utils').add_ensure_installed

---@type LazySpec[]
return {

  -- Additional LSP setup for the neovim nvim lua API.
  -- see etc/nvim/lua/me/settings/lsp_servers/lua_ls.lua for additional details
  {
    'folke/neodev.nvim',
    lazy = true,
    opts = {
      library = {
        enabled = true,
        plugins = true,
      },
      lspconfig = false,
      pathStrict = true,
    },
    event = 'LspAttach',
  },

  {
    "WhoIsSethDaniel/mason-tool-installer.nvim",
    opts = add_ensure({ 'stylua' }),
  },
}
