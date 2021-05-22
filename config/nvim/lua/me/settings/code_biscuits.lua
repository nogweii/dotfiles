local colors = require('zephyr')

require('nvim-biscuits').setup({
  default_config = {
    max_length = 30,
    min_distance = (vim.o.lines - 3) / 2,
    prefix_string = " "
  },
})

vim.cmd [[highlight clear BiscuitColor]]
vim.cmd([[highlight BiscuitColor gui=italic guifg=]] .. colors.base6)
