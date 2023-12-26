---@type LazySpec
return {
  -- a collection of LSP configs
  {
    'neovim/nvim-lspconfig',
    dependencies = {
      -- Additional LSP setup for the neovim nvim lua API.
      -- see config/nvim/lua/me/settings/lsp_servers/lua_ls.lua for additional details
      'folke/neodev.nvim',

      -- a downloaded copy of the SchemaStore.org catalog
      -- (used by JSON & YAML LSPs)
      'b0o/schemastore.nvim',
      'hrsh7th/cmp-nvim-lsp',
    },
    config = function()
      require('me.settings.lsp')
    end,
    version = false, -- use latest commit rather than version
  },

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
  },

  {
    'mrded/nvim-lsp-notify',
    requires = { 'rcarriga/nvim-notify' },
  },
}
