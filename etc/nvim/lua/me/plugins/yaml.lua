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
    dependencies = {
      'neovim/nvim-lspconfig',
      'nvim-lua/plenary.nvim',
      'nvim-telescope/telescope.nvim',
      'b0o/SchemaStore.nvim',
    },
    lazy = true,
    version = false, -- get the latest commit, the last release is >2 yrs ago
    -- ft = { 'yaml' },
    ft = { '*yaml*' },
    config = nil,
  },

  {
    "cuducos/yaml.nvim",
    ft = { "*yaml*" },
    dependencies = {
      "nvim-treesitter/nvim-treesitter",
      "nvim-telescope/telescope.nvim",
    },
    opts = {
      -- tell the plugin to expect these filetypes:
      ft = {
        "yaml",
        "eruby.yaml",
        "yaml.ansible"
      }
    }
  }
}
