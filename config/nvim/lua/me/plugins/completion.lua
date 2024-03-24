---@type LazySpec[]
return {
  -- snippets engine
  {
    'L3MON4D3/LuaSnip',
    dependencies = {
      'rafamadriz/friendly-snippets',
      { 'honza/vim-snippets', version = false },
    },
    config = function()
      local luasnip = require('luasnip')

      luasnip.config.set_config({
        updateevents = 'TextChanged,TextChangedI',
      })

      -- One peculiarity of honza/vim-snippets is that the file containing global
      -- snippets is _.snippets, so we need to tell luasnip that the filetype "_"
      -- contains global snippets:
      luasnip.filetype_extend('all', { '_' })

      require('luasnip.loaders.from_vscode').lazy_load()
      require('luasnip.loaders.from_snipmate').lazy_load()
      require('luasnip.loaders.from_lua').lazy_load()
    end,
    version = 'v2.*',
  },

  -- advanced & flexible completion menu
  {
    'hrsh7th/nvim-cmp',
    config = function()
      require('me.settings.cmp')
    end,
    dependencies = { 'L3MON4D3/LuaSnip' },
    version = false,
  },
  -- additional sources for cmp, lazily loaded
  { 'saadparwaiz1/cmp_luasnip' },
  { 'hrsh7th/cmp-nvim-lsp' },
  { 'hrsh7th/cmp-buffer' },
  { 'hrsh7th/cmp-path' },
  { 'hrsh7th/cmp-cmdline' },
  { 'petertriho/cmp-git' },
  { 'dmitmel/cmp-cmdline-history' },
  { 'hrsh7th/cmp-emoji' },
}
