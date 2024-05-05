---@type LazySpec[]
return {
  -- a collection of LSP configs
  {
    'neovim/nvim-lspconfig',
    config = function()
      require('me.settings.lsp')
    end,
    version = false, -- use latest commit rather than version
  },

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
  },

  {
    'mrded/nvim-lsp-notify',
    dependencies = { 'rcarriga/nvim-notify' },
  },

  {
    'soulis-1256/eagle.nvim',
  },
}
