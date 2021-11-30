local cat = require('catppuccin')
cat.setup({
  styles = {
    functions = "NONE",
    keywords = "NONE"
  },
  integrations = {
    ts_rainbow = "NONE",
  },
})
-- Set the colorscheme
vim.cmd [[colorscheme catppuccin]]
