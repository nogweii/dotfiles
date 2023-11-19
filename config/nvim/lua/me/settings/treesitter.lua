vim.g.skip_ts_context_commentstring_module = true

require("nvim-treesitter.configs").setup({
  -- Enable some modules shipped with nvim-treesitter
  highlight = {
    enable = true,
    additional_vim_regex_highlighting = true,
  },
  indent = {
    enable = true,
  },
})

vim.wo.foldmethod = "expr"
vim.wo.foldexpr = "nvim_treesitter#foldexpr()"

require('ts_context_commentstring').setup {}
