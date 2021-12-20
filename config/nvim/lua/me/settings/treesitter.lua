require('nvim-treesitter.configs').setup {
  -- automatically install all of the maintained treesitter modules
  ensure_installed = "maintained",

  -- Enable some modules shipped with nvim-treesitter
  highlight = {
    enable = true,
    additional_vim_regex_highlighting = false,
  },
  indent = {
    enable = true
  },

  -- Colorful brackets to help visualize nest depth
  -- (provided by p00f/nvim-ts-rainbow)
  rainbow = {
    enable = true
  },

  -- Smarter 'commentstring' in files with multiple languages at once (like HTML)
  -- (provided by JoosepAlviste/nvim-ts-context-commentstring)
  context_commentstring = {
    enable = true
  },
}

vim.wo.foldmethod = 'expr'
vim.wo.foldexpr = 'nvim_treesitter#foldexpr()'
