---@type LazySpec[]
return {

  -- a downloaded copy of the SchemaStore.org catalog
  -- (used by JSON & YAML LSPs)
  {
    'b0o/SchemaStore.nvim',
    lazy = true,
    version = false, -- last release is way too old
  },

  {
    'someone-stole-my-name/yaml-companion.nvim',
    requires = {
      { 'neovim/nvim-lspconfig' },
      { 'nvim-lua/plenary.nvim' },
      { 'nvim-telescope/telescope.nvim' },
    },
    lazy = true,
    version = false, -- get the latest commit, the last release is >2 yrs ago
  },
}
