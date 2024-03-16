---@type LazySpec[]
return {
  -- snippets engine
  {
    'L3MON4D3/LuaSnip',
    dependencies = { 'rafamadriz/friendly-snippets' },
    config = function()
      require('me.settings.luasnip')
    end,
  },
  -- a bunch of community maintained snippets
  { 'rafamadriz/friendly-snippets' },

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
