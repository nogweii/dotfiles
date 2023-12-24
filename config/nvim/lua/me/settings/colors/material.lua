vim.g.material_style = 'darker'
require('material').setup({
  styles = {
    comments = { italic = true },
  },

  plugins = {
    -- Uncomment the plugins that you use to highlight them
    -- Available plugins:
    -- "dap",
    -- "dashboard",
    'gitsigns',
    -- "hop",
    -- "indent-blankline",
    -- "lspsaga",
    -- "mini",
    'neogit',
    'nvim-cmp',
    -- "nvim-navic",
    -- "nvim-tree",
    -- "sneak",
    'telescope',
    'trouble',
    'which-key',
  },
})
vim.cmd('colorscheme material')
